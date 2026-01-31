use crate::emit::{
    WatEmitter,
};

pub(in crate::emit) fn emit_time_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(import \"env\" \"bd_time_now_ms\" (func $bd_time_now_ms (result i64)))");
    emitter.push_line("(import \"env\" \"bd_time_sleep_ms\" (func $bd_time_sleep_ms (param i64) (result i64)))");
}
