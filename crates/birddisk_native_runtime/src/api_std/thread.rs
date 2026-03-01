use crate::rt_core::*;
use crate::TraceFrame;

type EntryI64_0 = extern "C-unwind" fn(i64) -> i64;
type EntryI64_1 = extern "C-unwind" fn(i64, i64) -> i64;
type EntryI64_2 = extern "C-unwind" fn(i64, i64, i64) -> i64;

fn thread_handle_value(rt: &Runtime, raw: u64, op: &'static str) -> Option<HeapHandle> {
    if raw > u32::MAX as u64 {
        thread_error(rt, op);
        return None;
    }
    Some(HeapHandle::from_u32(raw as u32))
}

fn run_thread_entry<F>(layout: Vec<Vec<usize>>, trace_frames: Vec<TraceFrame>, call: F) -> ThreadOutcome
where
    F: FnOnce(&mut Runtime) -> i64,
{
    let mut child = Runtime::new();
    child.set_layout(layout);
    child.set_trace(trace_frames);
    let result = call(&mut child);
    match child.take_error() {
        Some(trap) => ThreadOutcome::Trap(NativeTrap {
            code: trap.code,
            message: trap.message,
            message_handle: None,
            trace: trap.trace,
        }),
        None => ThreadOutcome::Ok(result),
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_thread_store(rt: *mut Runtime, handle: u64, result: i64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match heap_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    if header.kind() != HeapKind::Object {
        thread_error(rt, "std::thread::spawn expected a Thread handle.");
        return;
    }
    rt.register_thread_handle(handle, result);
}

#[no_mangle]
pub extern "C-unwind" fn bd_thread_spawn_i64_0(rt: *mut Runtime, handle: u64, entry: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match thread_handle_value(rt, handle, "std::thread::spawn expected a Thread handle.") {
        Some(value) => value,
        None => return 0,
    };
    if entry == 0 {
        thread_error(rt, "std::thread::spawn failed: invalid entry function.");
        return 0;
    }
    let layout = rt.layout_clone();
    let trace_frames = rt.trace_frames_clone();
    let entry_fn: EntryI64_0 = unsafe { std::mem::transmute(entry as usize) };
    let join = std::thread::spawn(move || {
        run_thread_entry(layout, trace_frames, |child| {
            let child_ptr = (child as *mut Runtime as usize) as i64;
            entry_fn(child_ptr)
        })
    });
    rt.register_thread_host_handle(handle, join);
    1
}

#[no_mangle]
pub extern "C-unwind" fn bd_thread_spawn_i64_1(
    rt: *mut Runtime,
    handle: u64,
    entry: u64,
    arg0: i64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match thread_handle_value(rt, handle, "std::thread::spawn expected a Thread handle.") {
        Some(value) => value,
        None => return 0,
    };
    if entry == 0 {
        thread_error(rt, "std::thread::spawn failed: invalid entry function.");
        return 0;
    }
    let layout = rt.layout_clone();
    let trace_frames = rt.trace_frames_clone();
    let entry_fn: EntryI64_1 = unsafe { std::mem::transmute(entry as usize) };
    let join = std::thread::spawn(move || {
        run_thread_entry(layout, trace_frames, |child| {
            let child_ptr = (child as *mut Runtime as usize) as i64;
            entry_fn(child_ptr, arg0)
        })
    });
    rt.register_thread_host_handle(handle, join);
    1
}

#[no_mangle]
pub extern "C-unwind" fn bd_thread_spawn_i64_stream_i64_2(
    rt: *mut Runtime,
    handle: u64,
    entry: u64,
    stream_handle: u64,
    arg1: i64,
) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match thread_handle_value(rt, handle, "std::thread::spawn expected a Thread handle.")
    {
        Some(value) => value,
        None => return 0,
    };
    if entry == 0 {
        thread_error(rt, "std::thread::spawn failed: invalid entry function.");
        return 0;
    }
    let stream_handle = match heap_handle(rt, stream_handle) {
        Some(value) => value,
        None => {
            thread_error(rt, "std::thread::spawn failed: TcpStream handle is invalid.");
            return 0;
        }
    };
    let stream_book_id = match heap_header(rt, stream_handle) {
        Some(header) if header.kind() == HeapKind::Object => header.type_id(),
        _ => {
            thread_error(rt, "std::thread::spawn failed: TcpStream handle is invalid.");
            return 0;
        }
    };
    let stream = match rt.take_tcp_stream(stream_handle) {
        Some(value) => value,
        None => {
            thread_error(rt, "std::thread::spawn failed: TcpStream handle is invalid.");
            return 0;
        }
    };
    let layout = rt.layout_clone();
    let trace_frames = rt.trace_frames_clone();
    let entry_fn: EntryI64_2 = unsafe { std::mem::transmute(entry as usize) };
    let join = std::thread::spawn(move || {
        run_thread_entry(layout, trace_frames, |child| {
            maybe_collect(child);
            let child_stream_handle = match child.heap_mut().alloc_object(stream_book_id, 0) {
                Some(value) => value,
                None => {
                    oom_error(child);
                    return 0;
                }
            };
            child.register_tcp_stream(child_stream_handle, stream);
            let child_ptr = (child as *mut Runtime as usize) as i64;
            entry_fn(child_ptr, child_stream_handle.as_u32() as i64, arg1)
        })
    });
    rt.register_thread_host_handle(handle, join);
    1
}

#[no_mangle]
pub extern "C-unwind" fn bd_thread_join(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let raw_handle = handle;
    if raw_handle > u32::MAX as u64 {
        set_error(
            rt,
            "E0405",
            format!("Thread handle is invalid (raw={raw_handle})."),
            None,
        );
        return 0;
    }
    let handle = HeapHandle::from_u32(raw_handle as u32);
    let id = handle.as_u32();
    match rt.join_thread_handle(handle) {
        Ok(value) => value,
        Err(ThreadJoinError::Missing) => {
            set_error(rt, "E0405", format!("Thread handle is invalid (id={id})."), None);
            0
        }
        Err(ThreadJoinError::Running) => {
            set_error(rt, "E0405", format!("Thread is still running (id={id})."), None);
            0
        }
        Err(ThreadJoinError::AlreadyJoined) => {
            set_error(
                rt,
                "E0405",
                format!("Thread has already been joined (id={id})."),
                None,
            );
            0
        }
        Err(ThreadJoinError::Panicked) => {
            set_error(rt, "E0405", format!("Thread panicked (id={id})."), None);
            0
        }
        Err(ThreadJoinError::Trap(trap)) => {
            set_error(rt, trap.code, trap.message, None);
            0
        }
    }
}
