use crate::emit::{
    WatEmitter,
    ARRAY_HEADER_SIZE,
    ARRAY_KIND_U8,
    HEAP_AUX_OFFSET,
    HEAP_FLAGS_OFFSET,
    HEAP_KIND_ARRAY,
    HEAP_KIND_SHIFT,
    HEAP_KIND_STRING,
    HEAP_LEN_OFFSET,
    STRING_HEADER_SIZE,
    TRAP_KIND_BYTES,
    TRAP_STRING_PARSE,
    TRAP_UTF8_INVALID,
    TRAP_NULL_DEREF,
};

pub(super) fn emit_string_convert(emitter: &mut WatEmitter, allow_from_bytes: bool) {
    if allow_from_bytes {
        emitter.push_line("(func $bd_string_from_bytes (param $arr i32) (result i32)");
        emitter.indent();
        emitter.push_line("(local $len i32)");
        emitter.push_line("(local $ptr i32)");
        emitter.push_line("(local $src i32)");

        emitter.push_line("local.get $arr");
        emitter.push_line("i32.eqz");
        emitter.push_line("if");
        emitter.indent();
        emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
        emitter.push_line("call $bd_trap");
        emitter.dedent();
        emitter.push_line("end");
        emitter.push_line("local.get $arr");
        emitter.push_line("i32.load");
        emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
        emitter.push_line("i32.shr_u");
        emitter.push_line(format!("i32.const {HEAP_KIND_ARRAY}"));
        emitter.push_line("i32.ne");
        emitter.push_line("if");
        emitter.indent();
        emitter.push_line(format!("i32.const {TRAP_KIND_BYTES}"));
        emitter.push_line("call $bd_trap");
        emitter.dedent();
        emitter.push_line("end");
        emitter.push_line("local.get $arr");
        emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
        emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
        emitter.push_line("i32.ne");
        emitter.push_line("if");
        emitter.indent();
        emitter.push_line(format!("i32.const {TRAP_KIND_BYTES}"));
        emitter.push_line("call $bd_trap");
        emitter.dedent();
        emitter.push_line("end");

        emitter.push_line("local.get $arr");
        emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
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
        emitter.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
        emitter.push_line("i32.store");
        emitter.push_line("local.get $ptr");
        emitter.push_line("i32.const 0");
        emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
        emitter.push_line("local.get $ptr");
        emitter.push_line("i32.const 0");
        emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
        emitter.push_line("local.get $ptr");
        emitter.push_line("local.get $len");
        emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

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
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
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
    emitter.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
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
    emitter.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

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

}
