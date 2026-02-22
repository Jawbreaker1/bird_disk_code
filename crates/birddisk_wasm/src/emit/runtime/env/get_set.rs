use crate::emit::{
    WatEmitter, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET, HEAP_KIND_SHIFT, HEAP_KIND_STRING,
    HEAP_LEN_OFFSET, STRING_HEADER_SIZE, TRAP_ENV, TRAP_KIND_STRING, TRAP_NULL_DEREF,
    TRAP_UTF8_INVALID,
};

pub(super) fn emit_env_get_set(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_env_get (param $name i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $name_len i32)");
    emitter.push_line("(local $name_data i32)");
    emitter.push_line("(local $fill i32)");

    emitter.push_line("local.get $name");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $name");
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

    emitter.push_line("local.get $name");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $name_len");
    emitter.push_line("local.get $name");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $name_data");

    emitter.push_line("local.get $name_data");
    emitter.push_line("local.get $name_len");
    emitter.push_line("call $bd_env_get_len");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ENV}"));
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
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_env_get_fill");
    emitter.push_line("local.set $fill");
    emitter.push_line("local.get $fill");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ENV}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_validate_utf8");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_UTF8_INVALID}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_env_set (param $name i32) (param $value i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $name_len i32)");
    emitter.push_line("(local $value_len i32)");
    emitter.push_line("(local $name_data i32)");
    emitter.push_line("(local $value_data i32)");
    emitter.push_line("(local $result i64)");

    emitter.push_line("local.get $name");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $name");
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

    emitter.push_line("local.get $value");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $value");
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

    emitter.push_line("local.get $name");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $name_len");
    emitter.push_line("local.get $name");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $name_data");

    emitter.push_line("local.get $value");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $value_len");
    emitter.push_line("local.get $value");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $value_data");

    emitter.push_line("local.get $name_data");
    emitter.push_line("local.get $name_len");
    emitter.push_line("local.get $value_data");
    emitter.push_line("local.get $value_len");
    emitter.push_line("call $bd_env_set_raw");
    emitter.push_line("local.set $result");
    emitter.push_line("local.get $result");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ENV}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $result");
    emitter.dedent();
    emitter.push_line(")");
}
