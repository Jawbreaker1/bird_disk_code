use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_profiler_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        if !name.starts_with("std::profiler::") {
            return Ok(None);
        }
        if !args.is_empty() {
            return Err(runtime_error(
                "E0400",
                "std::profiler functions expect 0 arguments",
            ));
        }
        let stats = self.heap_stats();
        let value = match name {
            "std::profiler::uptime_ms" | "std::profiler::host_uptime_ms" => self.now_ms(),
            "std::profiler::alloc_count" | "std::profiler::host_alloc_count" => {
                stats.alloc_count as i64
            }
            "std::profiler::bytes_allocated" | "std::profiler::host_bytes_allocated" => {
                stats.bytes_allocated as i64
            }
            "std::profiler::bytes_in_use" | "std::profiler::host_bytes_in_use" => {
                stats.bytes_in_use as i64
            }
            "std::profiler::peak_bytes_in_use" | "std::profiler::host_peak_bytes_in_use" => {
                stats.peak_bytes_in_use as i64
            }
            "std::profiler::gc_runs" | "std::profiler::host_gc_runs" => stats.gc_runs as i64,
            "std::profiler::last_freed" | "std::profiler::host_last_freed" => {
                stats.last_freed as i64
            }
            "std::profiler::last_live" | "std::profiler::host_last_live" => {
                stats.last_live as i64
            }
            "std::profiler::last_freed_bytes" | "std::profiler::host_last_freed_bytes" => {
                stats.last_freed_bytes as i64
            }
            "std::profiler::last_live_bytes" | "std::profiler::host_last_live_bytes" => {
                stats.last_live_bytes as i64
            }
            _ => return Ok(None),
        };
        Ok(Some(Value::I64(value)))
    }
}
