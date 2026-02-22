mod basic;
mod convert;
mod search;

use crate::emit::{WatEmitter, ARRAY_HEADER_SIZE, STRING_HEADER_SIZE};

pub(in crate::emit) fn emit_string_runtime(emitter: &mut WatEmitter, allow_from_bytes: bool) {
    let max_len = i32::MAX as i64 - STRING_HEADER_SIZE as i64;
    let max_bytes_len = i32::MAX - ARRAY_HEADER_SIZE;

    basic::emit_string_basic(emitter, max_len, max_bytes_len);
    search::emit_string_search(emitter, max_len);
    convert::emit_string_convert(emitter, allow_from_bytes);
}
