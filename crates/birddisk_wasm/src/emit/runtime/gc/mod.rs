mod collect;
mod free_list;
mod layout;
mod roots;
mod tests;
mod worklist;

use crate::emit::WatEmitter;

pub(in crate::emit) fn emit_gc_layout_runtime(
    emitter: &mut WatEmitter,
    book_count: i32,
    offsets_base: i32,
    counts_base: i32,
    fields_base: i32,
    heap_start: i32,
    root_ptr_offset: i32,
    root_data_offset: i32,
    root_slots: i32,
    mark_ptr_offset: i32,
    mark_data_offset: i32,
    mark_slots: i32,
    seen_ptr_offset: i32,
    seen_data_offset: i32,
    seen_slots: i32,
) {
    layout::emit_gc_layout(emitter, book_count, offsets_base, counts_base, fields_base);
    roots::emit_gc_roots(emitter, root_ptr_offset, root_data_offset, root_slots);
    worklist::emit_gc_worklist(
        emitter,
        mark_ptr_offset,
        mark_data_offset,
        mark_slots,
        seen_ptr_offset,
        seen_data_offset,
        seen_slots,
    );
    free_list::emit_gc_free_list(emitter);
    collect::emit_gc_collect(
        emitter,
        heap_start,
        root_ptr_offset,
        root_data_offset,
        seen_ptr_offset,
    );
    tests::emit_gc_tests(emitter);
}
