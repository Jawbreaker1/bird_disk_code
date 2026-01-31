use crate::emit::{
    WatEmitter,
    HEAP_AUX_OFFSET,
    HEAP_TYPE_ID_MASK,
};

pub(super) fn emit_gc_layout(emitter: &mut WatEmitter, book_count: i32, offsets_base: i32, counts_base: i32, fields_base: i32) {
    emitter.push_line("(func $bd_object_type (param $ptr i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_TYPE_ID_MASK}"));
    emitter.push_line("i32.and");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_array_kind (param $ptr i32) (result i32)");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
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

    emitter.push_line("(func $bd_gc_layout_sanity (result i32)");
    emitter.indent();
    emitter.push_line("(local $type i32)");
    emitter.push_line("(local $count i32)");
    emitter.push_line("(local $idx i32)");
    emitter.push_line("(local $sum i32)");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $sum");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $type");

    let exit_label = "gc_layout_exit";
    let loop_label = "gc_layout_loop";
    emitter.push_line(format!("block ${exit_label}"));
    emitter.indent();
    emitter.push_line(format!("loop ${loop_label}"));
    emitter.indent();
    emitter.push_line("local.get $type");
    emitter.push_line(format!("i32.const {book_count}"));
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${exit_label}"));

    emitter.push_line("local.get $type");
    emitter.push_line("call $bd_ref_count");
    emitter.push_line("local.set $count");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $idx");

    let inner_exit = "gc_layout_inner_exit";
    let inner_loop = "gc_layout_inner_loop";
    emitter.push_line(format!("block ${inner_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${inner_loop}"));
    emitter.indent();
    emitter.push_line("local.get $idx");
    emitter.push_line("local.get $count");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${inner_exit}"));

    emitter.push_line("local.get $type");
    emitter.push_line("local.get $idx");
    emitter.push_line("call $bd_ref_field");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.ge_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $sum");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $sum");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $idx");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $idx");
    emitter.push_line(format!("br ${inner_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $type");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $type");
    emitter.push_line(format!("br ${loop_label}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $sum");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(export \"__bd_gc_layout_sanity\" (func $bd_gc_layout_sanity))");

}
