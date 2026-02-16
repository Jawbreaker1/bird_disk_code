use crate::emit::WatEmitter;

pub(in crate::emit) fn emit_profiler_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(func $bd_profiler_uptime_ms (result i64)");
    emitter.indent();
    emitter.push_line("call $bd_time_now_ms");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_alloc_count (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_alloc_count");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_bytes_allocated (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_bytes_allocated");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_bytes_in_use (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_bytes_in_use");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_peak_bytes_in_use (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_peak_bytes_in_use");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_gc_runs (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_gc_runs");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_last_freed (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_last_freed");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_last_live (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_last_live");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_last_freed_bytes (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_last_freed_bytes");
    emitter.dedent();
    emitter.push_line(")");

    emitter.push_line("(func $bd_profiler_last_live_bytes (result i64)");
    emitter.indent();
    emitter.push_line("global.get $prof_last_live_bytes");
    emitter.dedent();
    emitter.push_line(")");
}
