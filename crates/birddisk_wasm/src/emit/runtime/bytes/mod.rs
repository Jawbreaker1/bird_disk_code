mod basic;
mod search;
mod slice;

use crate::emit::{ARRAY_HEADER_SIZE, WatEmitter};

pub(in crate::emit) fn emit_bytes_runtime(emitter: &mut WatEmitter) {
    let max_bytes_len = i32::MAX - ARRAY_HEADER_SIZE;

    basic::emit_bytes_basic(emitter, max_bytes_len);
    slice::emit_bytes_slice(emitter, max_bytes_len);
    search::emit_bytes_search(emitter);
}
