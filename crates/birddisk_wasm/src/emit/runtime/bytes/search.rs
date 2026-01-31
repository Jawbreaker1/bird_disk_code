use crate::emit::{
    WatEmitter,
    ARRAY_HEADER_SIZE,
    ARRAY_KIND_U8,
    HEAP_AUX_OFFSET,
    HEAP_KIND_ARRAY,
    HEAP_KIND_SHIFT,
    HEAP_LEN_OFFSET,
    TRAP_KIND_BYTES,
    TRAP_NULL_DEREF,
};

pub(super) fn emit_bytes_search(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_bytes_index_of (param $ptr i32) (param $needle i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $idx i32)");

    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $ptr");
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
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_BYTES}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let bytes_index_exit = "bytes_index_exit";
    let bytes_index_loop = "bytes_index_loop";
    emitter.push_line(format!("block ${bytes_index_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${bytes_index_loop}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${bytes_index_exit}"));

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $idx");
    emitter.push_line("i32.add");
    emitter.push_line("i32.load8_u");

    emitter.push_line("local.get $needle");
    emitter.push_line("i32.const 255");
    emitter.push_line("i32.and");

    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${bytes_index_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("i64.const -1");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_bytes_contains (param $ptr i32) (param $needle i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $needle");
    emitter.push_line("call $bd_bytes_index_of");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.ge_s");
    emitter.dedent();
    emitter.push_line(")");
}
