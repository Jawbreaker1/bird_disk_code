use crate::emit::{WatEmitter, TRAP_TRACE_OOM};

pub(super) fn emit_gc_roots(
    emitter: &mut WatEmitter,
    root_ptr_offset: i32,
    root_data_offset: i32,
    root_slots: i32,
) {
    emitter.push_line("(func $bd_root_push (param $count i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $base i32)");
    emitter.push_line(format!("i32.const {root_ptr_offset}"));
    emitter.push_line("i32.load");
    emitter.push_line("local.set $base");
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.add");
    emitter.push_line(format!("i32.const {root_slots}"));
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_TRACE_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {root_ptr_offset}"));
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.add");
    emitter.push_line("i32.store");
    emitter.push_line("local.get $base");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_root_set (param $slot i32) (param $ptr i32)");
    emitter.indent();
    emitter.push_line(format!("i32.const {root_data_offset}"));
    emitter.push_line("local.get $slot");
    emitter.push_line("i32.const 4");
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.store");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_root_pop (param $count i32)");
    emitter.indent();
    emitter.push_line("(local $base i32)");
    emitter.push_line(format!("i32.const {root_ptr_offset}"));
    emitter.push_line("i32.load");
    emitter.push_line("local.set $base");
    emitter.push_line("local.get $count");
    emitter.push_line("local.get $base");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $base");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $base");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {root_ptr_offset}"));
    emitter.push_line("local.get $base");
    emitter.push_line("i32.store");
    emitter.dedent();
    emitter.push_line(")");
}
