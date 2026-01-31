mod args;
mod cwd;
mod get_set;

use crate::emit::WatEmitter;

pub(in crate::emit) fn emit_env_imports(emitter: &mut WatEmitter) {
    emitter.push_line(
        "(import \"env\" \"bd_env_args_count\" (func $bd_env_args_count (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_args_len\" (func $bd_env_args_len (param i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_args_fill\" (func $bd_env_args_fill (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_get_len\" (func $bd_env_get_len (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_get_fill\" (func $bd_env_get_fill (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_cwd_len\" (func $bd_env_cwd_len (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_cwd_fill\" (func $bd_env_cwd_fill (param i32 i32) (result i32)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_set\" (func $bd_env_set_raw (param i32 i32 i32 i32) (result i64)))",
    );
    emitter.push_line(
        "(import \"env\" \"bd_env_set_cwd\" (func $bd_env_set_cwd_raw (param i32 i32) (result i64)))",
    );
}

pub(in crate::emit) fn emit_env_runtime(emitter: &mut WatEmitter) {
    args::emit_env_args(emitter);
    get_set::emit_env_get_set(emitter);
    cwd::emit_env_cwd(emitter);
}
