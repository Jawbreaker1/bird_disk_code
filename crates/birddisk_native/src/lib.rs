mod analysis;
mod compiler;
mod driver;
mod error;
mod program;
mod rt;

#[cfg(test)]
mod tests;

pub use birddisk_native_runtime as runtime;
pub use error::NativeError;
pub const NATIVE_MAIN_SYMBOL: &str = "bd_main";

pub use driver::{
    emit_object, layout_for_program, run, run_with_io, run_with_io_options, trace_for_program,
    NativeRunOptions,
};
