use crate::emit::{
    WatEmitter,
    ARRAY_HEADER_SIZE,
    ARRAY_KIND_REF,
    HEAP_AUX_OFFSET,
    HEAP_FLAGS_OFFSET,
    HEAP_KIND_ARRAY,
    HEAP_KIND_SHIFT,
    HEAP_KIND_STRING,
    HEAP_LEN_OFFSET,
    STRING_HEADER_SIZE,
    TRAP_ENV,
    TRAP_UTF8_INVALID,
};

pub(super) fn emit_env_args(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_env_args (result i32)");
    emitter.indent();
    emitter.push_line("(local $count i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $str i32)");
    emitter.push_line("(local $data i32)");
    emitter.push_line("(local $fill i32)");
    emitter.push_line("(local $base i32)");

    emitter.push_line("call $bd_env_args_count");
    emitter.push_line("local.set $count");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ENV}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $count");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $count");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_KIND_REF}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $data");
    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $base");
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $ptr");
    emitter.push_line("call $bd_root_set");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    emitter.push_line("block $env_args_done");
    emitter.indent();
    emitter.push_line("loop $env_args_loop");
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.ge_u");
    emitter.push_line("br_if $env_args_done");

    emitter.push_line("local.get $idx");
    emitter.push_line("call $bd_env_args_len");
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
    emitter.push_line("local.set $str");
    emitter.push_line("local.get $str");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $str");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $str");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $str");
    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

    emitter.push_line("local.get $str");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_env_args_fill");
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

    emitter.push_line("local.get $str");
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

    emitter.push_line("local.get $data");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $str");
    emitter.push_line("i32.store");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line("br $env_args_loop");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_pop");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

}
