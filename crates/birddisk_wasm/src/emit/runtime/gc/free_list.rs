use crate::emit::{
    WatEmitter,
    ARRAY_HEADER_SIZE,
    ARRAY_KIND_BOOL,
    ARRAY_KIND_F64,
    ARRAY_KIND_I64,
    ARRAY_KIND_REF,
    ARRAY_KIND_U8,
    HEAP_AUX_OFFSET,
    HEAP_FLAGS_OFFSET,
    HEAP_KIND_ARRAY,
    HEAP_KIND_FREE,
    HEAP_KIND_OBJECT,
    HEAP_KIND_SHIFT,
    HEAP_KIND_STRING,
    HEAP_LEN_OFFSET,
    OBJECT_HEADER_SIZE,
    STRING_HEADER_SIZE,
    TRAP_HEAP_HEADER,
};

pub(super) fn emit_gc_free_list(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_free_add (param $ptr i32) (param $size i32)");
    emitter.indent();
    emitter.push_line("(local $prev i32)");
    emitter.push_line("(local $cur i32)");
    emitter.push_line("(local $next i32)");
    emitter.push_line("(local $prev_size i32)");
    emitter.push_line("(local $cur_size i32)");
    emitter.push_line("(local $new_size i32)");

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_FREE << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $size");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

    emitter.push_line("global.get $free_list");
    emitter.push_line("local.set $cur");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $prev");
    let insert_exit = "free_insert_exit";
    let insert_loop = "free_insert_loop";
    emitter.push_line(format!("block ${insert_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${insert_loop}"));
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line("i32.eqz");
    emitter.push_line(format!("br_if ${insert_exit}"));
    emitter.push_line("local.get $cur");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.gt_u");
    emitter.push_line(format!("br_if ${insert_exit}"));
    emitter.push_line("local.get $cur");
    emitter.push_line("local.set $prev");
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $cur");
    emitter.push_line(format!("br ${insert_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $prev");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("global.set $free_list");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $prev");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $cur");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $cur");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $cur_size");
    emitter.push_line("local.get $size");
    emitter.push_line("local.get $cur_size");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $new_size");
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $new_size");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $next");
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $next");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $prev");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $prev");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $prev_size");
    emitter.push_line("local.get $prev");
    emitter.push_line("local.get $prev_size");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $cur_size");
    emitter.push_line("local.get $prev_size");
    emitter.push_line("local.get $cur_size");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $new_size");
    emitter.push_line("local.get $prev");
    emitter.push_line("local.get $new_size");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $next");
    emitter.push_line("local.get $prev");
    emitter.push_line("local.get $next");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_block_size (param $ptr i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $kind i32)");
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $aux i32)");
    emitter.push_line("(local $size i32)");
    emitter.push_line("(local $elem i32)");

    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line("local.set $kind");

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $size");

    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_FREE}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $size");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $size");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_OBJECT}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("i32.const 8");
    emitter.push_line("i32.mul");
    emitter.push_line(format!("i32.const {OBJECT_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $size");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $aux");
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_BOOL}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 4");
    emitter.push_line("local.set $elem");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 1");
    emitter.push_line("local.set $elem");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_REF}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 4");
    emitter.push_line("local.set $elem");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("i32.const 8");
    emitter.push_line("local.set $elem");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $len");
    emitter.push_line("local.get $elem");
    emitter.push_line("i32.mul");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $size");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $size");
    emitter.push_line("i32.const 7");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const -8");
    emitter.push_line("i32.and");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_check_header (param $ptr i32)");
    emitter.indent();
    emitter.push_line("(local $kind i32)");
    emitter.push_line("(local $aux i32)");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.load");
    emitter.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
    emitter.push_line("i32.shr_u");
    emitter.push_line("local.set $kind");

    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $aux");
    emitter.push_line("local.get $aux");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {TRAP_HEAP_HEADER}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_ARRAY}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $aux");
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_I64}"));
    emitter.push_line("i32.eq");
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_BOOL}"));
    emitter.push_line("i32.eq");
    emitter.push_line("i32.or");
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
    emitter.push_line("i32.eq");
    emitter.push_line("i32.or");
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_F64}"));
    emitter.push_line("i32.eq");
    emitter.push_line("i32.or");
    emitter.push_line("local.get $aux");
    emitter.push_line(format!("i32.const {ARRAY_KIND_REF}"));
    emitter.push_line("i32.eq");
    emitter.push_line("i32.or");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_HEAP_HEADER}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_OBJECT}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $kind");
    emitter.push_line(format!("i32.const {HEAP_KIND_FREE}"));
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line(format!("i32.const {TRAP_HEAP_HEADER}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line(")");

}
