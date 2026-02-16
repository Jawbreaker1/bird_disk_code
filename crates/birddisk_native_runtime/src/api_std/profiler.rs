use crate::rt_core::*;

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_uptime_ms(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.now_ms()
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_alloc_count(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().alloc_count as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_bytes_allocated(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().bytes_allocated as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_bytes_in_use(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().bytes_in_use as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_peak_bytes_in_use(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().peak_bytes_in_use as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_gc_runs(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().gc_runs as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_last_freed(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().last_freed as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_last_live(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().last_live as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_last_freed_bytes(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().last_freed_bytes as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_profiler_last_live_bytes(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.heap_stats().last_live_bytes as i64
}
