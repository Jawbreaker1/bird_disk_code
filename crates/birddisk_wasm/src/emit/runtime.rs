use super::{
    WatEmitter, ARRAY_HEADER_SIZE, ARRAY_KIND_U8, STRING_HEADER_SIZE, TRACE_STACK_DATA_OFFSET,
    TRACE_STACK_PTR_OFFSET, TRACE_STACK_SLOTS, TRAP_ARRAY_OOM, TRAP_STRING_PARSE, TRAP_TRACE_OOM,
    TRAP_UTF8_INVALID,
};

pub(super) fn emit_heap_runtime(
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
        emitter.push_line("(import \"env\" \"bd_read_line_len\" (func $bd_read_line_len (result i32)))");
        emitter.push_line("(import \"env\" \"bd_read_line_fill\" (func $bd_read_line_fill (param i32 i32)))");
    }
    emitter.push_line("(memory $mem 1)");
    if export_memory {
        emitter.push_line("(export \"memory\" (memory $mem))");
    }
    emitter.push_line(format!(
        "(global $heap (mut i32) (i32.const {heap_start}))"
    ));
    emitter.push_line("(func $bd_alloc (param $size i32) (result i32) (local $ptr i32) (local $new_heap i32) (local $pages_needed i32) (local $cur_pages i32) (local $grow_by i32)");
    emitter.indent();
    emitter.push_line("global.get $heap");
    emitter.push_line("local.set $ptr");
    emitter.push_line("global.get $heap");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 7");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const -8");
    emitter.push_line("i32.and");
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
}

pub(super) fn emit_string_runtime(emitter: &mut WatEmitter, allow_from_bytes: bool) {
    let max_len = i32::MAX as i64 - STRING_HEADER_SIZE as i64;
    let max_bytes_len = i32::MAX - ARRAY_HEADER_SIZE;

    emitter.push_line("(func $bd_string_len (param $ptr i32) (result i64)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line("i64.extend_i32_u");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_concat (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $total i64)");
    emitter.push_line("(local $size i64)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $a_data i32)");
    emitter.push_line("(local $b_data i32)");
    emitter.push_line("(local $dst_data i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_b");

    emitter.push_line("local.get $len_a");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.get $len_b");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.add");
    emitter.push_line("local.set $total");

    emitter.push_line("local.get $total");
    emitter.push_line(format!("i64.const {max_len}"));
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $total");
    emitter.push_line(format!("i64.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i64.add");
    emitter.push_line("local.set $size");

    emitter.push_line("local.get $size");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $total");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("i32.store");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $dst_data");

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $a_data");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $b_data");

    emitter.push_line("local.get $dst_data");
    emitter.push_line("local.get $a_data");
    emitter.push_line("local.get $len_a");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $dst_data");
    emitter.push_line("local.get $len_a");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $b_data");
    emitter.push_line("local.get $len_b");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_bytes (param $s i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");

    emitter.push_line("local.get $s");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len");

    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {max_bytes_len}"));
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
    emitter.push_line("i32.store offset=4");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $s");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    if allow_from_bytes {
        emitter.push_line("(func $bd_string_from_bytes (param $arr i32) (result i32)");
        emitter.indent();
        emitter.push_line("(local $len i32)");
        emitter.push_line("(local $ptr i32)");
        emitter.push_line("(local $src i32)");

        emitter.push_line("local.get $arr");
        emitter.push_line("i32.load");
        emitter.push_line("local.set $len");

        emitter.push_line("local.get $arr");
        emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        emitter.push_line("i32.add");
        emitter.push_line("local.set $src");

        emitter.push_line("local.get $src");
        emitter.push_line("local.get $len");
        emitter.push_line("call $bd_validate_utf8");
        emitter.push_line("i32.eqz");
        emitter.push_line("if");
        emitter.indent();
        emitter.push_line(format!("i32.const {TRAP_UTF8_INVALID}"));
        emitter.push_line("call $bd_trap");
        emitter.dedent();
        emitter.push_line("end");

        emitter.push_line("local.get $len");
        emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
        emitter.push_line("i32.add");
        emitter.push_line("call $bd_alloc");
        emitter.push_line("local.set $ptr");

        emitter.push_line("local.get $ptr");
        emitter.push_line("local.get $len");
        emitter.push_line("i32.store");

        emitter.push_line("local.get $ptr");
        emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
        emitter.push_line("i32.add");
        emitter.push_line("local.get $src");
        emitter.push_line("local.get $len");
        emitter.push_line("memory.copy");

        emitter.push_line("local.get $ptr");
        emitter.dedent();
        emitter.push_line(")");
    }

    emitter.push_line("(func $bd_string_to_i64 (param $ptr i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $sign i64)");
    emitter.push_line("(local $value i64)");
    emitter.push_line("(local $digit i64)");
    emitter.push_line("(local $ch i32)");
    emitter.push_line("(local $max_div10 i64)");
    emitter.push_line("(local $max_mod i64)");
    emitter.push_line("(local $data i32)");

    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $data");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");
    emitter.push_line("i64.const 1");
    emitter.push_line("local.set $sign");
    emitter.push_line("i64.const 922337203685477580");
    emitter.push_line("local.set $max_div10");
    emitter.push_line("i64.const 7");
    emitter.push_line("local.set $max_mod");

    emitter.push_line("local.get $data");
    emitter.push_line("i32.load8_u");
    emitter.push_line("local.set $ch");
    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 45");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i64.const -1");
    emitter.push_line("local.set $sign");
    emitter.push_line("i64.const 8");
    emitter.push_line("local.set $max_mod");
    emitter.push_line("i32.const 1");
    emitter.push_line("local.set $idx");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i64.const 0");
    emitter.push_line("local.set $value");

    let exit_label = "str_to_i64_exit";
    let loop_label = "str_to_i64_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");
    emitter.push_line("local.set $ch");

    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.lt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 57");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ch");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.sub");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.set $digit");

    emitter.push_line("local.get $value");
    emitter.push_line("local.get $max_div10");
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("local.get $max_div10");
    emitter.push_line("i64.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $digit");
    emitter.push_line("local.get $max_mod");
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_PARSE}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.mul");
    emitter.push_line("local.get $digit");
    emitter.push_line("i64.add");
    emitter.push_line("local.set $value");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $sign");
    emitter.push_line("i64.const -1");
    emitter.push_line("i64.eq");
    emitter.push_line("if (result i64)");
    emitter.indent();
    emitter.push_line("i64.const 0");
    emitter.push_line("local.get $value");
    emitter.push_line("i64.sub");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $value");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_from_i64 (param $value i64) (result i32)");
    emitter.indent();
    emitter.push_line("(local $tmp i64)");
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $sign i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $digit i64)");
    emitter.push_line("(local $data i32)");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {}", STRING_HEADER_SIZE + 1));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.store8");
    emitter.push_line("local.get $ptr");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 1");
    emitter.push_line("local.set $sign");
    emitter.push_line("i64.const 0");
    emitter.push_line("local.get $value");
    emitter.push_line("i64.sub");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $sign");
    emitter.push_line("local.get $value");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $len");
    let len_exit = "str_i64_len_exit";
    let len_loop = "str_i64_len_loop";
    emitter.push_line(format!("block ${len_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${len_loop}"));
    emitter.indent();
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.eqz");
    emitter.push_line(format!("br_if ${len_exit}"));
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.div_u");
    emitter.push_line("local.set $tmp");
    emitter.push_line(format!("br ${len_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line("local.get $sign");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $len");

    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.store");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $data");

    emitter.push_line("local.get $sign");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $data");
    emitter.push_line("i32.const 45");
    emitter.push_line("i32.store8");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $value");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i64.const 0");
    emitter.push_line("local.get $value");
    emitter.push_line("i64.sub");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $value");
    emitter.push_line("local.set $tmp");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $idx");

    let digit_exit = "str_i64_digit_exit";
    let digit_loop = "str_i64_digit_loop";
    emitter.push_line(format!("block ${digit_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${digit_loop}"));
    emitter.indent();
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.eqz");
    emitter.push_line(format!("br_if ${digit_exit}"));
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.rem_u");
    emitter.push_line("local.set $digit");
    emitter.push_line("local.get $tmp");
    emitter.push_line("i64.const 10");
    emitter.push_line("i64.div_u");
    emitter.push_line("local.set $tmp");
    emitter.push_line("local.get $data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $digit");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("i32.const 48");
    emitter.push_line("i32.add");
    emitter.push_line("i32.store8");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${digit_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_eq (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $idx i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_b");

    emitter.push_line("local.get $len_a");
    emitter.push_line("local.get $len_b");
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let exit_label = "str_eq_exit";
    let loop_label = "str_eq_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_a");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
}

pub(super) fn emit_bytes_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_bytes_len (param $ptr i32) (result i64)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line("i64.extend_i32_u");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_bytes_eq (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $idx i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len_b");

    emitter.push_line("local.get $len_a");
    emitter.push_line("local.get $len_b");
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let exit_label = "bytes_eq_exit";
    let loop_label = "bytes_eq_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_a");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
}

pub(super) fn emit_gc_layout_runtime(
    emitter: &mut WatEmitter,
    book_count: i32,
    offsets_base: i32,
    counts_base: i32,
    fields_base: i32,
) {
    emitter.push_line("(func $bd_object_type (param $ptr i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_array_kind (param $ptr i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load offset=4");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_ref_count (param $type i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $type");
    emitter.push_line(format!("i32.const {book_count}"));
    emitter.push_line("i32.ge_u");
    emitter.push_line("if (result i32)");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line(format!("i32.const {counts_base}"));
    emitter.push_line("local.get $type");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line(
        "(func $bd_ref_field (param $type i32) (param $idx i32) (result i32)",
    );
    emitter.indent();
    emitter.push_line("(local $count i32)");
    emitter.push_line("(local $offset i32)");
    emitter.push_line("local.get $type");
    emitter.push_line(format!("i32.const {book_count}"));
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const -1");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line(format!("i32.const {counts_base}"));
    emitter.push_line("local.get $type");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $count");

    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const -1");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line(format!("i32.const {offsets_base}"));
    emitter.push_line("local.get $type");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $offset");

    emitter.push_line(format!("i32.const {fields_base}"));
    emitter.push_line("local.get $offset");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load");
    emitter.dedent();
    emitter.push_line(")");
}

pub(super) fn emit_io_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_io_print (param $str i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("local.get $str");
    emitter.push_line("i32.load");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $str");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_print");
    emitter.push_line("local.get $len");
    emitter.push_line("i64.extend_i32_u");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_io_read_line (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("call $bd_read_line_len");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_read_line_fill");
    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");
}
