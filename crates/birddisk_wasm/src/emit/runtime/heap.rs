use crate::emit::{
    WatEmitter, ARRAY_HEADER_SIZE, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET, HEAP_HEADER_SIZE,
    HEAP_KIND_FREE, HEAP_KIND_SHIFT, HEAP_LEN_OFFSET, TRACE_STACK_DATA_OFFSET,
    TRACE_STACK_PTR_OFFSET, TRACE_STACK_SLOTS, TRAP_ARRAY_OOM, TRAP_TRACE_OOM,
};

pub(in crate::emit) fn emit_heap_runtime(
    emitter: &mut WatEmitter,
    export_memory: bool,
    needs_validate_utf8: bool,
    needs_io: bool,
    heap_start: i32,
) {
    emitter.push_line("(import \"env\" \"bd_trap\" (func $bd_trap (param i32)))");
    if needs_validate_utf8 {
        emitter.push_line(
            "(import \"env\" \"bd_validate_utf8\" (func $bd_validate_utf8 (param i32 i32) (result i32)))",
        );
    }
    if needs_io {
        emitter.push_line("(import \"env\" \"bd_print\" (func $bd_print (param i32 i32)))");
        emitter.push_line(
            "(import \"env\" \"bd_read_line_len\" (func $bd_read_line_len (result i32)))",
        );
        emitter.push_line(
            "(import \"env\" \"bd_read_line_fill\" (func $bd_read_line_fill (param i32 i32)))",
        );
    }
    emitter.push_line("(memory $mem 1)");
    if export_memory {
        emitter.push_line("(export \"memory\" (memory $mem))");
    }
    emitter.push_line(format!("(global $heap (mut i32) (i32.const {heap_start}))"));
    emitter.push_line("(global $free_list (mut i32) (i32.const 0))");
    emitter.push_line("(global $gc_last_freed (mut i32) (i32.const 0))");
    emitter.push_line("(global $prof_alloc_count (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_bytes_allocated (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_bytes_in_use (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_peak_bytes_in_use (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_gc_runs (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_last_freed (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_last_live (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_last_freed_bytes (mut i64) (i64.const 0))");
    emitter.push_line("(global $prof_last_live_bytes (mut i64) (i64.const 0))");
    emitter.push_line("(global $gc_threshold (mut i64) (i64.const 65536))");
    emitter.push_line("(global $error_kind (mut i32) (i32.const 0))");
    emitter.push_line("(global $error_msg (mut i32) (i32.const 0))");
    emitter.push_line("(global $error_trace (mut i32) (i32.const 0))");

    emitter.push_line(
        "(func $bd_alloc_from_free (param $size i32) (result i32) (local $prev i32) (local $cur i32) (local $cur_size i32) (local $next i32) (local $remain i32) (local $split i32)",
    );
    emitter.indent();
    emitter.push_line("global.get $free_list");
    emitter.push_line("local.set $cur");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $prev");
    let free_exit = "free_exit";
    let free_loop = "free_loop";
    emitter.push_line(format!("block ${free_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${free_loop}"));
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line("i32.eqz");
    emitter.push_line(format!("br_if ${free_exit}"));
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $cur_size");
    emitter.push_line("local.get $cur_size");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $next");
    emitter.push_line("local.get $cur_size");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $remain");
    emitter.push_line("local.get $remain");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $split");
    emitter.push_line("local.get $split");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_FREE << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $split");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $split");
    emitter.push_line("local.get $remain");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $split");
    emitter.push_line("local.get $next");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $prev");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $split");
    emitter.push_line("global.set $free_list");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $prev");
    emitter.push_line("local.get $split");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $cur");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $cur_size");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $prev");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $next");
    emitter.push_line("global.set $free_list");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $prev");
    emitter.push_line("local.get $next");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $cur");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $cur");
    emitter.push_line("local.set $prev");
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $cur");
    emitter.push_line(format!("br ${free_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("i32.const 0");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_alloc (param $size i32) (result i32) (local $ptr i32) (local $new_heap i32) (local $pages_needed i32) (local $cur_pages i32) (local $grow_by i32) (local $payload i32)");
    emitter.indent();
    emitter.push_line("local.get $size");
    emitter.push_line("i32.const 7");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const -8");
    emitter.push_line("i32.and");
    emitter.push_line("local.set $size");
    emitter.push_line("local.get $size");
    emitter.push_line(format!("i32.const {HEAP_HEADER_SIZE}"));
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $payload");
    emitter.push_line("global.get $prof_bytes_in_use");
    emitter.push_line("local.get $payload");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.add");
    emitter.push_line("global.get $gc_threshold");
    emitter.push_line("i64.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("call $bd_gc_collect");
    emitter.push_line("drop");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $size");
    emitter.push_line("call $bd_alloc_from_free");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("call $bd_gc_collect");
    emitter.push_line("drop");
    emitter.push_line("local.get $size");
    emitter.push_line("call $bd_alloc_from_free");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("global.get $heap");
    emitter.push_line("local.set $ptr");
    emitter.push_line("global.get $heap");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $new_heap");
    emitter.push_line("local.get $new_heap");
    emitter.push_line("i32.const 65535");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 65536");
    emitter.push_line("i32.div_u");
    emitter.push_line("local.set $pages_needed");
    emitter.push_line("memory.size");
    emitter.push_line("local.set $cur_pages");
    emitter.push_line("local.get $pages_needed");
    emitter.push_line("local.get $cur_pages");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $pages_needed");
    emitter.push_line("local.get $cur_pages");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $grow_by");
    emitter.push_line("local.get $grow_by");
    emitter.push_line("memory.grow");
    emitter.push_line("i32.const -1");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $new_heap");
    emitter.push_line("global.set $heap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("global.get $prof_alloc_count");
    emitter.push_line("i64.const 1");
    emitter.push_line("i64.add");
    emitter.push_line("global.set $prof_alloc_count");
    emitter.push_line("global.get $prof_bytes_allocated");
    emitter.push_line("local.get $payload");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.add");
    emitter.push_line("global.set $prof_bytes_allocated");
    emitter.push_line("global.get $prof_bytes_in_use");
    emitter.push_line("local.get $payload");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.add");
    emitter.push_line("global.set $prof_bytes_in_use");
    emitter.push_line("global.get $prof_bytes_in_use");
    emitter.push_line("global.get $prof_peak_bytes_in_use");
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("global.get $prof_bytes_in_use");
    emitter.push_line("global.set $prof_peak_bytes_in_use");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_trace_push (param $id i32) (local $sp i32)");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("i32.load");
    emitter.push_line("local.set $sp");
    emitter.push_line("local.get $sp");
    emitter.push_line(format!("i32.const {TRACE_STACK_SLOTS}"));
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_TRACE_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {TRACE_STACK_DATA_OFFSET}"));
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $id");
    emitter.push_line("i32.store");
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("i32.store");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_trace_pop (local $sp i32)");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("i32.load");
    emitter.push_line("local.set $sp");
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $sp");
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("local.get $sp");
    emitter.push_line("i32.store");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_throw (param $msg i32)");
    emitter.indent();
    emitter.push_line("global.get $error_kind");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 1");
    emitter.push_line("global.set $error_kind");
    emitter.push_line("local.get $msg");
    emitter.push_line("global.set $error_msg");
    emitter.push_line(format!("i32.const {TRACE_STACK_PTR_OFFSET}"));
    emitter.push_line("i32.load");
    emitter.push_line("global.set $error_trace");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_has_error (result i32)");
    emitter.indent();
    emitter.push_line("global.get $error_kind");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_error_is_throw (result i32)");
    emitter.indent();
    emitter.push_line("global.get $error_kind");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.eq");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_error_message (result i32)");
    emitter.indent();
    emitter.push_line("global.get $error_msg");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_error_trace (result i32)");
    emitter.indent();
    emitter.push_line("global.get $error_trace");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_clear_error");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("global.set $error_kind");
    emitter.push_line("i32.const 0");
    emitter.push_line("global.set $error_msg");
    emitter.push_line("i32.const 0");
    emitter.push_line("global.set $error_trace");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(export \"__bd_has_error\" (func $bd_has_error))");
    emitter.push_line("(export \"__bd_error_message\" (func $bd_error_message))");
    emitter.push_line("(export \"__bd_error_trace\" (func $bd_error_trace))");
}
