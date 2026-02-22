use crate::emit::{
    WatEmitter, ARRAY_HEADER_SIZE, ARRAY_KIND_U8, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET,
    HEAP_KIND_ARRAY, HEAP_KIND_SHIFT, HEAP_KIND_STRING, HEAP_LEN_OFFSET, STRING_HEADER_SIZE,
    TRAP_ARRAY_OOM, TRAP_KIND_STRING, TRAP_NULL_DEREF, TRAP_STRING_OOB, TRAP_STRING_UTF8,
};

pub(super) fn emit_string_basic(emitter: &mut WatEmitter, max_len: i64, max_bytes_len: i32) {
    emitter.push_line("(func $bd_string_len (param $ptr i32) (result i64)");
    emitter.indent();
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
    emitter.push_line(format!("i32.const {HEAP_KIND_STRING}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_STRING}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $ptr");
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
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
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
    emitter.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $total");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

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
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $s");
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

    emitter.push_line("local.get $s");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
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
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

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

    emitter.push_line(
        "(func $bd_string_slice (param $s i32) (param $start i64) (param $len i64) (result i32)",
    );
    emitter.indent();
    emitter.push_line("(local $str_len i32)");
    emitter.push_line("(local $str_len64 i64)");
    emitter.push_line("(local $start_i32 i32)");
    emitter.push_line("(local $end i64)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $src i32)");
    emitter.push_line("(local $dst i32)");

    emitter.push_line("local.get $s");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $s");
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

    emitter.push_line("local.get $start");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_OOB}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $len");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_OOB}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $s");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $str_len");
    emitter.push_line("local.get $str_len");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("local.set $str_len64");

    emitter.push_line("local.get $start");
    emitter.push_line("local.get $len");
    emitter.push_line("i64.add");
    emitter.push_line("local.set $end");
    emitter.push_line("local.get $end");
    emitter.push_line("local.get $str_len64");
    emitter.push_line("i64.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_OOB}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line(format!("i32.const {max_len}"));
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $len");
    emitter.push_line("i32.wrap_i64");
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
    emitter.push_line("i32.wrap_i64");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $dst");

    emitter.push_line("local.get $s");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $start");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $src");

    emitter.push_line("local.get $dst");
    emitter.push_line("local.get $src");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("memory.copy");

    emitter.push_line("local.get $dst");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("call $bd_validate_utf8");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_STRING_UTF8}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");
}
