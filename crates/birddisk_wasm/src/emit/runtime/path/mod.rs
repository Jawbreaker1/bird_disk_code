mod basename;
mod dirname;
mod join;
mod normalize;

use crate::emit::WatEmitter;

pub(in crate::emit) fn emit_path_imports(emitter: &mut WatEmitter) {
    emitter.push_line(
        "(import \"env\" \"bd_path_join_len\" (func $bd_path_join_len (param i32 i32 i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_path_normalize_len\" (func $bd_path_normalize_len (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_path_basename_len\" (func $bd_path_basename_len (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_path_dirname_len\" (func $bd_path_dirname_len (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_path_fill\" (func $bd_path_fill (param i32 i32) (result i32)))",
    );
}

pub(in crate::emit) fn emit_path_runtime(emitter: &mut WatEmitter) {
    join::emit_path_join(emitter);
    normalize::emit_path_normalize(emitter);
    basename::emit_path_basename(emitter);
    dirname::emit_path_dirname(emitter);
}
