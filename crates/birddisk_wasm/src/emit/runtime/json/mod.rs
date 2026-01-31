mod decode;
mod encode;

use crate::emit::{WatEmitter, STRING_HEADER_SIZE};

pub(in crate::emit) fn emit_json_runtime(emitter: &mut WatEmitter) {
    let max_len = i32::MAX - STRING_HEADER_SIZE;
    let max_input_len = (max_len - 2) / 2;

    encode::emit_json_encode(emitter, max_input_len);
    decode::emit_json_decode(emitter);
}
