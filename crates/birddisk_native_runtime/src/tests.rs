use crate::rt_core::{
    split_lines, Heap, HeapHeader, HeapKind, RootStack, RootValue, Runtime, ThreadJoinError,
};

#[test]
fn heap_header_encodes_kind_and_type() {
    let header = HeapHeader::new(HeapKind::Object, 42, 3, 0);
    assert_eq!(header.kind(), HeapKind::Object);
    assert_eq!(header.type_id(), 42);
}

#[test]
fn root_stack_tracks_frames() {
    let mut roots = RootStack::new();
    let base = roots.push_frame(2).expect("root frame allocation");
    roots.set_slot(base, RootValue::Null);
    roots.set_slot(base + 1, RootValue::Null);
    roots.pop_frame(2);
    assert!(roots.slots().is_empty());
}

#[test]
fn gc_reclaims_unrooted_values() {
    let mut heap = Heap::new();
    let keep = heap.alloc_string(3).expect("alloc keep");
    let drop = heap.alloc_string(2).expect("alloc drop");

    let mut roots = RootStack::new();
    let base = roots.push_frame(1).expect("root frame");
    roots.set_slot(base, RootValue::Ptr(keep));

    let report = heap.gc(&roots);
    assert_eq!(report.freed, 1);
    assert_eq!(heap.header(drop).kind(), HeapKind::Free);
}

#[test]
fn split_lines_strips_cr() {
    let lines = split_lines("123\r\n456\r\n");
    let collected: Vec<String> = lines.into_iter().collect();
    assert_eq!(
        collected,
        vec!["123".to_string(), "456".to_string(), "".to_string()]
    );
}

#[test]
fn thread_registry_join_bookkeeping() {
    let mut rt = Runtime::new();
    let thread_id = rt.register_thread();
    assert_eq!(rt.join_thread(thread_id), Err(ThreadJoinError::Running));
    rt.complete_thread(thread_id, 42);
    assert_eq!(rt.join_thread(thread_id), Ok(42));
    assert_eq!(
        rt.join_thread(thread_id),
        Err(ThreadJoinError::AlreadyJoined)
    );
    assert_eq!(rt.join_thread(9_999), Err(ThreadJoinError::Missing));
}
