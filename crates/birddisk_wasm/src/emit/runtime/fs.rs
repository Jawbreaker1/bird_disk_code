use crate::emit::{
    WatEmitter, ARRAY_HEADER_SIZE, ARRAY_KIND_U8, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET,
    HEAP_KIND_ARRAY, HEAP_KIND_SHIFT, HEAP_KIND_STRING, HEAP_LEN_OFFSET, STRING_HEADER_SIZE,
    TRAP_FS_IO, TRAP_KIND_BYTES, TRAP_KIND_STRING, TRAP_NULL_DEREF, TRAP_UTF8_INVALID,
};

pub(in crate::emit) fn emit_fs_imports(emitter: &mut WatEmitter) {
    emitter.push_line(
        "(import \"env\" \"bd_fs_read_len\" (func $bd_fs_read_len (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_fs_read_fill\" (func $bd_fs_read_fill (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_fs_write\" (func $bd_fs_write (param i32 i32 i32 i32) (result i64)))",
    );
}

pub(in crate::emit) fn emit_fs_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_fs_read_text (param $path i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $path_len i32)");
    emitter.push_line("(local $path_data i32)");
    emitter.push_line("(local $fill i32)");

    emitter.push_line("local.get $path");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $path");
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

    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $path_len");
    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $path_data");
    emitter.push_line("local.get $path_data");
    emitter.push_line("local.get $path_len");
    emitter.push_line("call $bd_fs_read_len");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_FS_IO}"));
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
    emitter.push_line("call $bd_fs_read_fill");
    emitter.push_line("local.set $fill");
    emitter.push_line("local.get $fill");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_FS_IO}"));
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

    emitter.push_line("(func $bd_fs_read_bytes (param $path i32) (result i32)");
    emitter.indent();
    emitter.push_line("(local $len i32)");
    emitter.push_line("(local $ptr i32)");
    emitter.push_line("(local $path_len i32)");
    emitter.push_line("(local $path_data i32)");
    emitter.push_line("(local $fill i32)");

    emitter.push_line("local.get $path");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $path");
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

    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $path_len");
    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $path_data");
    emitter.push_line("local.get $path_data");
    emitter.push_line("local.get $path_len");
    emitter.push_line("call $bd_fs_read_len");
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_FS_IO}"));
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
    emitter.push_line("local.get $len");
    emitter.push_line("call $bd_fs_read_fill");
    emitter.push_line("local.set $fill");
    emitter.push_line("local.get $fill");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_FS_IO}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_fs_write_text (param $path i32) (param $text i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $path_len i32)");
    emitter.push_line("(local $text_len i32)");
    emitter.push_line("(local $path_data i32)");
    emitter.push_line("(local $text_data i32)");
    emitter.push_line("(local $result i64)");

    emitter.push_line("local.get $path");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $path");
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

    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $path_len");
    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $path_data");
    emitter.push_line("local.get $text");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $text_len");
    emitter.push_line("local.get $text");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $text_data");

    emitter.push_line("local.get $path_data");
    emitter.push_line("local.get $path_len");
    emitter.push_line("local.get $text_data");
    emitter.push_line("local.get $text_len");
    emitter.push_line("call $bd_fs_write");
    emitter.push_line("local.set $result");
    emitter.push_line("local.get $result");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_FS_IO}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $result");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_fs_write_bytes (param $path i32) (param $bytes i32) (result i64)");
    emitter.indent();
    emitter.push_line("(local $path_len i32)");
    emitter.push_line("(local $bytes_len i32)");
    emitter.push_line("(local $path_data i32)");
    emitter.push_line("(local $bytes_data i32)");
    emitter.push_line("(local $result i64)");

    emitter.push_line("local.get $path");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $path");
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

    emitter.push_line("local.get $bytes");
    emitter.push_line("i32.eqz");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_NULL_DEREF}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $bytes");
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
    emitter.push_line("local.get $bytes");
    emitter.push_line(format!("i32.load offset={HEAP_AUX_OFFSET}"));
    emitter.push_line(format!("i32.const {ARRAY_KIND_U8}"));
    emitter.push_line("i32.ne");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_KIND_BYTES}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $path_len");
    emitter.push_line("local.get $path");
    emitter.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $path_data");
    emitter.push_line("local.get $bytes");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $bytes_len");
    emitter.push_line("local.get $bytes");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.set $bytes_data");

    emitter.push_line("local.get $path_data");
    emitter.push_line("local.get $path_len");
    emitter.push_line("local.get $bytes_data");
    emitter.push_line("local.get $bytes_len");
    emitter.push_line("call $bd_fs_write");
    emitter.push_line("local.set $result");
    emitter.push_line("local.get $result");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.lt_s");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_FS_IO}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $result");
    emitter.dedent();
    emitter.push_line(")");
}
