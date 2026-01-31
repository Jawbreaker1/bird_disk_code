use crate::emit::{
    WatEmitter,
    HEAP_AUX_OFFSET,
    HEAP_FLAGS_OFFSET,
    HEAP_KIND_SHIFT,
    HEAP_KIND_STRING,
    HEAP_LEN_OFFSET,
    STRING_HEADER_SIZE,
    TRAP_ARRAY_OOM,
    TRAP_KIND_STRING,
    TRAP_STRING_UTF8,
    TRAP_NULL_DEREF,
};

pub(super) fn emit_string_search(emitter: &mut WatEmitter, max_len: i64) {
    emitter.push_line("(func $bd_string_index_of (param $text i32) (param $needle i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $len_text i32)");
    emitter.push_line("(local $len_need i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $j i32)");
    emitter.push_line("(local $limit i32)");
    emitter.push_line("(local $text_data i32)");
    emitter.push_line("(local $needle_data i32)");

    emitter.push_line("local.get $text");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $text");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $needle");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $needle");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len_text");
    emitter.push_line("local.get $needle");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len_need");

    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i64.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len_need");
    emitter.push_line("local.get $len_text");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i64.const -1");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $text_data");
    emitter.push_line("local.get $needle");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $needle_data");

    emitter.push_line("local.get $len_text");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $limit");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let exit_label = "string_index_exit";
    let loop_label = "string_index_loop";
    let inner_exit = "string_index_inner_exit";
    let inner_loop = "string_index_inner_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $limit");
    emitter.push_line("i32.gt_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $j");
    emitter.push_line(format!("block ${inner_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${inner_loop}"));
    emitter.indent();
    emitter.push_line("local.get $j");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text_data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $j");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $needle_data");
    emitter.push_line("local.get $j");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("br ${inner_exit}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $j");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $j");
    emitter.push_line(format!("br ${inner_loop}"));
    emitter.dedent();
    emitter.push_line("end");
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

    emitter.push_line("i64.const -1");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_contains (param $text i32) (param $needle i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $text");
    emitter.push_line("local.get $needle");
    emitter.push_line("call $bd_string_index_of");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.ge_s");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_replace (param $text i32) (param $needle i32) (param $replacement i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_text i32)");
    emitter.push_line("(local $len_need i32)");
    emitter.push_line("(local $len_rep i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $count i32)");
    emitter.push_line("(local $limit i32)");
    emitter.push_line("(local $j i32)");
    emitter.push_line("(local $new_len i64)");
    emitter.push_line("(local $out_ptr i32)");
    emitter.push_line("(local $out_idx i32)");
    emitter.push_line("(local $text_data i32)");
    emitter.push_line("(local $needle_data i32)");
    emitter.push_line("(local $rep_data i32)");

    emitter.push_line("local.get $text");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $text");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $needle");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $needle");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $replacement");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $replacement");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len_text");
    emitter.push_line("local.get $needle");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len_need");
    emitter.push_line("local.get $replacement");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len_rep");

    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $text");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $len_need");
    emitter.push_line("local.get $len_text");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $text");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $text_data");
    emitter.push_line("local.get $needle");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $needle_data");
    emitter.push_line("local.get $replacement");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $rep_data");

    emitter.push_line("local.get $len_text");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $limit");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $count");

    let count_exit = "string_replace_count_exit";
    let count_loop = "string_replace_count_loop";
    let count_inner_exit = "string_replace_count_inner_exit";
    let count_inner_loop = "string_replace_count_inner_loop";
    emitter.push_line(format!("block ${count_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${count_loop}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $limit");
    emitter.push_line("i32.gt_u");
    emitter.push_line(format!("br_if ${count_exit}"));

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $j");
    emitter.push_line(format!("block ${count_inner_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${count_inner_loop}"));
    emitter.indent();
    emitter.push_line("local.get $j");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $count");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $count");
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${count_loop}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text_data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $j");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $needle_data");
    emitter.push_line("local.get $j");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("br ${count_inner_exit}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $j");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $j");
    emitter.push_line(format!("br ${count_inner_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${count_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len_text");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.get $count");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.get $len_rep");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.sub");
    emitter.push_line("i64.mul");
    emitter.push_line("i64.add");
    emitter.push_line("local.set $new_len");

    emitter.push_line("local.get $new_len");
    emitter.push_line(format!("i64.const {max_len}"));
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $new_len");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $out_ptr");

    emitter.push_line("local.get $out_ptr");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $out_ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $out_ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $out_ptr");
    emitter.push_line("local.get $new_len");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $out_idx");

    let copy_exit = "string_replace_copy_exit";
    let copy_loop = "string_replace_copy_loop";
    let copy_inner_exit = "string_replace_copy_inner_exit";
    let copy_inner_loop = "string_replace_copy_inner_loop";
    emitter.push_line(format!("block ${copy_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${copy_loop}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_text");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${copy_exit}"));

    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $limit");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $out_ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $out_idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $text_data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");
    emitter.push_line("i32.store8");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line("local.get $out_idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $out_idx");
    emitter.push_line(format!("br ${copy_loop}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $j");
    emitter.push_line(format!("block ${copy_inner_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${copy_inner_loop}"));
    emitter.indent();
    emitter.push_line("local.get $j");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.ge_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $out_ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $out_idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $rep_data");
    emitter.push_line("local.get $len_rep");
    emitter.push_line("memory.copy");
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len_need");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line("local.get $out_idx");
    emitter.push_line("local.get $len_rep");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $out_idx");
    emitter.push_line(format!("br ${copy_inner_exit}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $text_data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $j");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $needle_data");
    emitter.push_line("local.get $j");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $out_ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $out_idx");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $text_data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");
    emitter.push_line("i32.store8");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line("local.get $out_idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $out_idx");
    emitter.push_line(format!("br ${copy_inner_exit}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $j");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $j");
    emitter.push_line(format!("br ${copy_inner_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line(format!("br ${copy_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $out_ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $new_len");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("call $bd_validate_utf8");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_UTF8}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $out_ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_string_eq (param $a i32) (param $b i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len_a i32)");
    emitter.push_line("(local $len_b i32)");
    emitter.push_line("(local $idx i32)");

    emitter.push_line("local.get $a");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $a");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $b");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len_a");
    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
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
