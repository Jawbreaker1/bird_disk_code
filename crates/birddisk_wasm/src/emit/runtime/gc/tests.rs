use crate::emit::{
    WatEmitter, ARRAY_HEADER_SIZE, ARRAY_KIND_I64, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET,
    HEAP_KIND_ARRAY, HEAP_KIND_OBJECT, HEAP_KIND_SHIFT, HEAP_LEN_OFFSET, OBJECT_HEADER_SIZE,
};

pub(super) fn emit_gc_tests(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_gc_mark_test (result i32)");
    emitter.indent();
    emitter.push_line("(local $a i32)");
    emitter.push_line("(local $b i32)");
    emitter.push_line("(local $base i32)");
    let size = OBJECT_HEADER_SIZE + 8;
    emitter.push_line(format!("i32.const {size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $a");
    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_OBJECT << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $base");
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $a");
    emitter.push_line("call $bd_root_set");

    emitter.push_line(format!("i32.const {size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $b");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_OBJECT << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {OBJECT_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $b");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.store");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {OBJECT_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $a");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.store");

    emitter.push_line("call $bd_gc_mark_objects");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(export \"__bd_gc_mark_test\" (func $bd_gc_mark_test))");

    emitter.push_line("(func $bd_gc_sweep_test (result i32)");
    emitter.indent();
    emitter.push_line("(local $a i32)");
    emitter.push_line("(local $b i32)");
    emitter.push_line("(local $c i32)");
    emitter.push_line("(local $base i32)");
    emitter.push_line("(local $freed i32)");
    let size = OBJECT_HEADER_SIZE + 8;
    emitter.push_line(format!("i32.const {size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $a");
    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_OBJECT << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $base");
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $a");
    emitter.push_line("call $bd_root_set");

    emitter.push_line(format!("i32.const {size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $b");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_OBJECT << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("call $bd_gc_collect");
    emitter.push_line("local.set $freed");

    emitter.push_line(format!("i32.const {size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $c");

    emitter.push_line("local.get $c");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.eq");
    emitter.push_line("local.get $freed");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.eq");
    emitter.push_line("i32.and");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(export \"__bd_gc_sweep_test\" (func $bd_gc_sweep_test))");

    emitter.push_line("(func $bd_gc_last_freed (result i32)");
    emitter.indent();
    emitter.push_line("global.get $gc_last_freed");
    emitter.dedent();
    emitter.push_line(")");
    emitter.push_line("(export \"__bd_gc_last_freed\" (func $bd_gc_last_freed))");

    emitter.push_line("(func $bd_heap_high_water (result i32)");
    emitter.indent();
    emitter.push_line("global.get $heap");
    emitter.dedent();
    emitter.push_line(")");
    emitter.push_line("(export \"__bd_heap_high_water\" (func $bd_heap_high_water))");

    emitter.push_line("(func $bd_gc_split_test (result i32)");
    emitter.indent();
    emitter.push_line("(local $a i32)");
    emitter.push_line("(local $b i32)");
    emitter.push_line("(local $c i32)");
    emitter.push_line("(local $d i32)");
    emitter.push_line("(local $base i32)");
    emitter.push_line("(local $expected i32)");
    let big = ARRAY_HEADER_SIZE + 32;
    let small = ARRAY_HEADER_SIZE + 8;

    emitter.push_line(format!("i32.const {small}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $a");
    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {ARRAY_KIND_I64}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $base");
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $a");
    emitter.push_line("call $bd_root_set");

    emitter.push_line(format!("i32.const {big}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $b");
    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 4");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {ARRAY_KIND_I64}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("call $bd_gc_collect");
    emitter.push_line("drop");

    emitter.push_line(format!("i32.const {small}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $c");

    emitter.push_line(format!("i32.const {small}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $d");

    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {small}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $expected");

    emitter.push_line("local.get $c");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.eq");
    emitter.push_line("local.get $d");
    emitter.push_line("local.get $expected");
    emitter.push_line("i32.eq");
    emitter.push_line("i32.and");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(export \"__bd_gc_split_test\" (func $bd_gc_split_test))");

    emitter.push_line("(func $bd_header_kind_test (result i32)");
    emitter.indent();
    emitter.push_line("(local $ptr i32)");
    emitter.push_line(format!("i32.const {OBJECT_HEADER_SIZE}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {}", 99 << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("call $bd_check_header");
    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
    emitter.push_line("(export \"__bd_header_kind_test\" (func $bd_header_kind_test))");

    emitter.push_line("(func $bd_header_array_aux_test (result i32)");
    emitter.indent();
    emitter.push_line("(local $ptr i32)");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $ptr");
    emitter.push_line("local.get $ptr");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $ptr");
    emitter.push_line("i32.const 99");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $ptr");
    emitter.push_line("call $bd_check_header");
    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
    emitter.push_line("(export \"__bd_header_array_aux_test\" (func $bd_header_array_aux_test))");

    emitter.push_line("(func $bd_free_list_len (result i32)");
    emitter.indent();
    emitter.push_line("(local $cur i32)");
    emitter.push_line("(local $count i32)");
    emitter.push_line("(local $limit i32)");
    emitter.push_line("global.get $free_list");
    emitter.push_line("local.set $cur");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $count");
    emitter.push_line("i32.const 2048");
    emitter.push_line("local.set $limit");
    let free_len_exit = "free_len_exit";
    let free_len_loop = "free_len_loop";
    emitter.push_line(format!("block ${free_len_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${free_len_loop}"));
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line("i32.eqz");
    emitter.push_line(format!("br_if ${free_len_exit}"));
    emitter.push_line("local.get $count");
    emitter.push_line("local.get $limit");
    emitter.push_line("i32.ge_u");
    emitter.push_line(format!("br_if ${free_len_exit}"));
    emitter.push_line("local.get $count");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $count");
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $cur");
    emitter.push_line(format!("br ${free_len_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $count");
    emitter.dedent();
    emitter.push_line(")");
    emitter.push_line("(export \"__bd_free_list_len\" (func $bd_free_list_len))");

    emitter.push_line("(func $bd_free_list_bytes (result i32)");
    emitter.indent();
    emitter.push_line("(local $cur i32)");
    emitter.push_line("(local $sum i32)");
    emitter.push_line("(local $limit i32)");
    emitter.push_line("global.get $free_list");
    emitter.push_line("local.set $cur");
    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $sum");
    emitter.push_line("i32.const 2048");
    emitter.push_line("local.set $limit");
    let free_bytes_exit = "free_bytes_exit";
    let free_bytes_loop = "free_bytes_loop";
    emitter.push_line(format!("block ${free_bytes_exit}"));
    emitter.indent();
    emitter.push_line(format!("loop ${free_bytes_loop}"));
    emitter.indent();
    emitter.push_line("local.get $cur");
    emitter.push_line("i32.eqz");
    emitter.push_line(format!("br_if ${free_bytes_exit}"));
    emitter.push_line("local.get $limit");
    emitter.push_line("i32.eqz");
    emitter.push_line(format!("br_if ${free_bytes_exit}"));
    emitter.push_line("local.get $sum");
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $sum");
    emitter.push_line("local.get $cur");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.set $cur");
    emitter.push_line("local.get $limit");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $limit");
    emitter.push_line(format!("br ${free_bytes_loop}"));
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $sum");
    emitter.dedent();
    emitter.push_line(")");
    emitter.push_line("(export \"__bd_free_list_bytes\" (func $bd_free_list_bytes))");

    emitter.push_line("(func $bd_gc_adjacent_free_test (result i32)");
    emitter.indent();
    emitter.push_line("(local $a i32)");
    emitter.push_line("(local $b i32)");
    emitter.push_line("(local $base i32)");
    let adj_size = OBJECT_HEADER_SIZE + 8;
    emitter.push_line("i32.const 2");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $base");

    emitter.push_line(format!("i32.const {adj_size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $a");
    emitter.push_line("local.get $a");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_OBJECT << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $a");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $base");
    emitter.push_line("local.get $a");
    emitter.push_line("call $bd_root_set");

    emitter.push_line(format!("i32.const {adj_size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $b");
    emitter.push_line("local.get $b");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_OBJECT << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 1");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $b");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("local.get $base");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $b");
    emitter.push_line("call $bd_root_set");

    emitter.push_line("i32.const 2");
    emitter.push_line("call $bd_root_pop");

    emitter.push_line("call $bd_gc_collect");
    emitter.push_line("drop");
    emitter.push_line("call $bd_free_list_len");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(export \"__bd_gc_adjacent_free_test\" (func $bd_gc_adjacent_free_test))");
}
