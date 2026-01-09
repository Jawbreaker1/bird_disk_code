mod analysis;
mod emit;
mod runtime;
mod trace;

pub use emit::{emit_wasm, emit_wat, WasmError};
pub use runtime::{run, run_with_io};
