use super::NativeCompiler;
use crate::error::{native_error, NativeError};
use crate::program::{stdlib_signature, type_name};
use birddisk_core::ast::{Expr, ExprKind, Type};
use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_module::Module;

#[derive(Clone, Copy, Debug)]
enum ChannelKind {
    I64,
    Bool,
    F64,
    U8,
    String,
    Bytes,
}

impl ChannelKind {
    fn from_book(book: &str) -> Option<Self> {
        match book {
            "ChannelI64" => Some(ChannelKind::I64),
            "ChannelBool" => Some(ChannelKind::Bool),
            "ChannelF64" => Some(ChannelKind::F64),
            "ChannelU8" => Some(ChannelKind::U8),
            "ChannelString" => Some(ChannelKind::String),
            "ChannelBytes" => Some(ChannelKind::Bytes),
            _ => None,
        }
    }

    fn payload_type(self) -> Type {
        match self {
            ChannelKind::I64 => Type::I64,
            ChannelKind::Bool => Type::Bool,
            ChannelKind::F64 => Type::F64,
            ChannelKind::U8 => Type::U8,
            ChannelKind::String => Type::String,
            ChannelKind::Bytes => Type::Array(Box::new(Type::U8)),
        }
    }

    fn book_name(self) -> &'static str {
        match self {
            ChannelKind::I64 => "ChannelI64",
            ChannelKind::Bool => "ChannelBool",
            ChannelKind::F64 => "ChannelF64",
            ChannelKind::U8 => "ChannelU8",
            ChannelKind::String => "ChannelString",
            ChannelKind::Bytes => "ChannelBytes",
        }
    }

    fn recv_enum(self) -> &'static str {
        match self {
            ChannelKind::I64 => "RecvI64",
            ChannelKind::Bool => "RecvBool",
            ChannelKind::F64 => "RecvF64",
            ChannelKind::U8 => "RecvU8",
            ChannelKind::String => "RecvString",
            ChannelKind::Bytes => "RecvBytes",
        }
    }
}

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    pub(super) fn emit_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        if let Some(sig) = self.functions.get(name) {
            if sig.params.len() != args.len() {
                return Err(native_error(format!(
                    "wrong number of arguments for '{name}': expected {}, got {}.",
                    sig.params.len(),
                    args.len()
                )));
            }
            if let Some(expected) = expected {
                if &sig.return_type != expected {
                    return Err(native_error(format!(
                        "type mismatch: expected {}, got {}.",
                        type_name(expected),
                        type_name(&sig.return_type)
                    )));
                }
            }
            let func_id = self
                .func_ids
                .get(name)
                .copied()
                .ok_or_else(|| native_error(format!("missing function id for '{name}'.")))?;
            let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(self.rt_ptr);
            for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                let value = self.emit_expr(arg, Some(param_ty))?;
                call_args.push(value);
            }
            let call = self.builder.ins().call(func_ref, &call_args);
            self.emit_error_check();
            if matches!(sig.return_type, Type::Void) {
                return Ok(None);
            }
            return Ok(Some(self.builder.inst_results(call)[0]));
        }
        if name.starts_with("std::") {
            return self.emit_std_call(name, args, expected);
        }
        if let Some((base, method)) = name.split_once("::") {
            if let Some(Type::Book(book_name)) = self.lookup_local_type(base) {
                return self.emit_method_call(base, &book_name, method, args, expected);
            }
        }
        if let Some(value) = self.emit_enum_constructor(name, args, expected)? {
            return Ok(Some(value));
        }
        Err(native_error(format!("unknown function '{name}'.")))
    }

    fn emit_method_call(
        &mut self,
        base: &str,
        book: &str,
        method: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        if let Some(kind) = ChannelKind::from_book(book) {
            return self.emit_channel_method(kind, base, method, args, expected);
        }
        let full_name = format!("{book}::{method}");
        let sig = self
            .functions
            .get(&full_name)
            .ok_or_else(|| native_error(format!("unknown method '{full_name}'.")))?;
        if sig.params.is_empty() {
            return Err(native_error(format!(
                "method '{full_name}' must take self."
            )));
        }
        let expected_args = sig.params.len().saturating_sub(1);
        if expected_args != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{full_name}': expected {}, got {}.",
                expected_args,
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
        let func_id = self
            .func_ids
            .get(&full_name)
            .copied()
            .ok_or_else(|| native_error(format!("missing function id for '{full_name}'.")))?;
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let base_info = self
            .vars
            .get(base)
            .cloned()
            .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
        let mut call_args = Vec::with_capacity(args.len() + 2);
        call_args.push(self.rt_ptr);
        call_args.push(self.builder.use_var(base_info.var));
        for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
            call_args.push(self.emit_expr(arg, Some(param_ty))?);
        }
        let call = self.builder.ins().call(func_ref, &call_args);
        self.emit_error_check();
        if matches!(sig.return_type, Type::Void) {
            Ok(None)
        } else {
            Ok(Some(self.builder.inst_results(call)[0]))
        }
    }

    fn emit_channel_method(
        &mut self,
        kind: ChannelKind,
        base: &str,
        method: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        let base_info = self
            .vars
            .get(base)
            .cloned()
            .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
        let base_val = self.builder.use_var(base_info.var);
        match method {
            "send" => {
                if args.len() != 1 {
                    return Err(native_error(format!(
                        "wrong number of arguments for '{}::send': expected 1, got {}.",
                        kind.book_name(),
                        args.len()
                    )));
                }
                if let Some(expected) = expected {
                    if !matches!(expected, Type::Bool) {
                        return Err(native_error(format!(
                            "type mismatch: expected {}, got bool.",
                            type_name(expected)
                        )));
                    }
                }
                let payload_ty = kind.payload_type();
                let value = self.emit_expr(&args[0], Some(&payload_ty))?;
                let result = match kind {
                    ChannelKind::I64 => self.call_runtime_value(
                        self.runtime.channel_send_i64,
                        &[self.rt_ptr, base_val, value],
                    ),
                    ChannelKind::Bool => self.call_runtime_value(
                        self.runtime.channel_send_bool,
                        &[self.rt_ptr, base_val, value],
                    ),
                    ChannelKind::F64 => self.call_runtime_value(
                        self.runtime.channel_send_f64,
                        &[self.rt_ptr, base_val, value],
                    ),
                    ChannelKind::U8 => self.call_runtime_value(
                        self.runtime.channel_send_u8,
                        &[self.rt_ptr, base_val, value],
                    ),
                    ChannelKind::String => self.call_runtime_value(
                        self.runtime.channel_send_string,
                        &[self.rt_ptr, base_val, value],
                    ),
                    ChannelKind::Bytes => self.call_runtime_value(
                        self.runtime.channel_send_bytes,
                        &[self.rt_ptr, base_val, value],
                    ),
                };
                Ok(Some(result))
            }
            "recv" => {
                if !args.is_empty() {
                    return Err(native_error(format!(
                        "wrong number of arguments for '{}::recv': expected 0, got {}.",
                        kind.book_name(),
                        args.len()
                    )));
                }
                let recv_name = kind.recv_enum();
                if let Some(expected) = expected {
                    if expected != &Type::Book(recv_name.to_string()) {
                        return Err(native_error(format!(
                            "type mismatch: expected {}, got {}.",
                            type_name(expected),
                            recv_name
                        )));
                    }
                }
                let enum_info = self
                    .enums
                    .get(recv_name)
                    .ok_or_else(|| native_error(format!("missing enum info for '{recv_name}'.")))?;
                let ok_id = enum_info
                    .variants
                    .get("Ok")
                    .ok_or_else(|| native_error(format!("missing {recv_name}::Ok variant")))?;
                let closed_id = enum_info
                    .variants
                    .get("Closed")
                    .ok_or_else(|| native_error(format!("missing {recv_name}::Closed variant")))?;
                let enum_id_val = self.builder.ins().iconst(types::I64, enum_info.id as i64);
                let ok_val = self.builder.ins().iconst(types::I64, ok_id.id as i64);
                let closed_val = self.builder.ins().iconst(types::I64, closed_id.id as i64);
                let result = match kind {
                    ChannelKind::I64 => self.call_runtime_value(
                        self.runtime.channel_recv_i64,
                        &[self.rt_ptr, base_val, enum_id_val, ok_val, closed_val],
                    ),
                    ChannelKind::Bool => self.call_runtime_value(
                        self.runtime.channel_recv_bool,
                        &[self.rt_ptr, base_val, enum_id_val, ok_val, closed_val],
                    ),
                    ChannelKind::F64 => self.call_runtime_value(
                        self.runtime.channel_recv_f64,
                        &[self.rt_ptr, base_val, enum_id_val, ok_val, closed_val],
                    ),
                    ChannelKind::U8 => self.call_runtime_value(
                        self.runtime.channel_recv_u8,
                        &[self.rt_ptr, base_val, enum_id_val, ok_val, closed_val],
                    ),
                    ChannelKind::String => self.call_runtime_value(
                        self.runtime.channel_recv_string,
                        &[self.rt_ptr, base_val, enum_id_val, ok_val, closed_val],
                    ),
                    ChannelKind::Bytes => self.call_runtime_value(
                        self.runtime.channel_recv_bytes,
                        &[self.rt_ptr, base_val, enum_id_val, ok_val, closed_val],
                    ),
                };
                Ok(Some(result))
            }
            "close" => {
                if !args.is_empty() {
                    return Err(native_error(format!(
                        "wrong number of arguments for '{}::close': expected 0, got {}.",
                        kind.book_name(),
                        args.len()
                    )));
                }
                if let Some(expected) = expected {
                    if !matches!(expected, Type::Void) {
                        return Err(native_error(format!(
                            "type mismatch: expected {}, got void.",
                            type_name(expected)
                        )));
                    }
                }
                match kind {
                    ChannelKind::I64 => self.call_runtime_void(
                        self.runtime.channel_close_i64,
                        &[self.rt_ptr, base_val],
                    ),
                    ChannelKind::Bool => self.call_runtime_void(
                        self.runtime.channel_close_bool,
                        &[self.rt_ptr, base_val],
                    ),
                    ChannelKind::F64 => self.call_runtime_void(
                        self.runtime.channel_close_f64,
                        &[self.rt_ptr, base_val],
                    ),
                    ChannelKind::U8 => self
                        .call_runtime_void(self.runtime.channel_close_u8, &[self.rt_ptr, base_val]),
                    ChannelKind::String => self.call_runtime_void(
                        self.runtime.channel_close_string,
                        &[self.rt_ptr, base_val],
                    ),
                    ChannelKind::Bytes => self.call_runtime_void(
                        self.runtime.channel_close_bytes,
                        &[self.rt_ptr, base_val],
                    ),
                };
                Ok(None)
            }
            _ => Err(native_error(format!(
                "unknown channel method '{}::{}'.",
                kind.book_name(),
                method
            ))),
        }
    }

    fn emit_std_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        if name == "std::thread::spawn" {
            return self.emit_thread_spawn(args, expected);
        }
        let sig = stdlib_signature(name)
            .ok_or_else(|| native_error(format!("unknown function '{name}'.")))?;
        if sig.params.len() != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{name}': expected {}, got {}.",
                sig.params.len(),
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
        let mut arg_vals = Vec::with_capacity(args.len());
        for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
            arg_vals.push(self.emit_expr(arg, Some(param_ty))?);
        }
        let value = match name {
            "std::string::len" => {
                Some(self.call_runtime_value(self.runtime.string_len, &[self.rt_ptr, arg_vals[0]]))
            }
            "std::string::concat" => Some(self.call_runtime_value(
                self.runtime.string_concat,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::eq" => Some(self.call_runtime_value(
                self.runtime.string_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::bytes" => Some(
                self.call_runtime_value(self.runtime.string_bytes, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::string::slice" => Some(self.call_runtime_value(
                self.runtime.string_slice,
                &[self.rt_ptr, arg_vals[0], arg_vals[1], arg_vals[2]],
            )),
            "std::string::index_of" => Some(self.call_runtime_value(
                self.runtime.string_index_of,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::contains" => Some(self.call_runtime_value(
                self.runtime.string_contains,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::replace" => Some(self.call_runtime_value(
                self.runtime.string_replace,
                &[self.rt_ptr, arg_vals[0], arg_vals[1], arg_vals[2]],
            )),
            "std::string::from_bytes" => {
                Some(self.call_runtime_value(
                    self.runtime.string_from_bytes,
                    &[self.rt_ptr, arg_vals[0]],
                ))
            }
            "std::string::to_i64" => Some(
                self.call_runtime_value(self.runtime.string_to_i64, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::string::from_i64" => Some(
                self.call_runtime_value(self.runtime.string_from_i64, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::bytes::len" => {
                Some(self.call_runtime_value(self.runtime.bytes_len, &[self.rt_ptr, arg_vals[0]]))
            }
            "std::bytes::eq" => Some(self.call_runtime_value(
                self.runtime.bytes_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::bytes::slice" => Some(self.call_runtime_value(
                self.runtime.bytes_slice,
                &[self.rt_ptr, arg_vals[0], arg_vals[1], arg_vals[2]],
            )),
            "std::bytes::index_of" => Some(self.call_runtime_value(
                self.runtime.bytes_index_of,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::bytes::contains" => Some(self.call_runtime_value(
                self.runtime.bytes_contains,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::io::print" => {
                self.call_runtime_void(self.runtime.io_print, &[self.rt_ptr, arg_vals[0]]);
                None
            }
            "std::io::read_line" => {
                Some(self.call_runtime_value(self.runtime.io_read_line, &[self.rt_ptr]))
            }
            "std::time::now_ms" => {
                Some(self.call_runtime_value(self.runtime.time_now_ms, &[self.rt_ptr]))
            }
            "std::time::sleep_ms" => Some(
                self.call_runtime_value(self.runtime.time_sleep_ms, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::profiler::uptime_ms" => {
                Some(self.call_runtime_value(self.runtime.profiler_uptime_ms, &[self.rt_ptr]))
            }
            "std::profiler::alloc_count" => {
                Some(self.call_runtime_value(self.runtime.profiler_alloc_count, &[self.rt_ptr]))
            }
            "std::profiler::bytes_allocated" => {
                Some(self.call_runtime_value(self.runtime.profiler_bytes_allocated, &[self.rt_ptr]))
            }
            "std::profiler::bytes_in_use" => {
                Some(self.call_runtime_value(self.runtime.profiler_bytes_in_use, &[self.rt_ptr]))
            }
            "std::profiler::peak_bytes_in_use" => Some(
                self.call_runtime_value(self.runtime.profiler_peak_bytes_in_use, &[self.rt_ptr]),
            ),
            "std::profiler::gc_runs" => {
                Some(self.call_runtime_value(self.runtime.profiler_gc_runs, &[self.rt_ptr]))
            }
            "std::profiler::last_freed" => {
                Some(self.call_runtime_value(self.runtime.profiler_last_freed, &[self.rt_ptr]))
            }
            "std::profiler::last_live" => {
                Some(self.call_runtime_value(self.runtime.profiler_last_live, &[self.rt_ptr]))
            }
            "std::profiler::last_freed_bytes" => Some(
                self.call_runtime_value(self.runtime.profiler_last_freed_bytes, &[self.rt_ptr]),
            ),
            "std::profiler::last_live_bytes" => {
                Some(self.call_runtime_value(self.runtime.profiler_last_live_bytes, &[self.rt_ptr]))
            }
            "std::rand::seed" => {
                self.call_runtime_void(self.runtime.rand_seed, &[self.rt_ptr, arg_vals[0]]);
                None
            }
            "std::rand::range" => Some(self.call_runtime_value(
                self.runtime.rand_range,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::test::assert" => {
                self.call_runtime_void(
                    self.runtime.test_assert,
                    &[self.rt_ptr, arg_vals[0], arg_vals[1]],
                );
                None
            }
            "std::test::assert_eq_i64" => {
                self.call_runtime_void(
                    self.runtime.test_assert_eq_i64,
                    &[self.rt_ptr, arg_vals[0], arg_vals[1], arg_vals[2]],
                );
                None
            }
            "std::test::assert_eq_bool" => {
                self.call_runtime_void(
                    self.runtime.test_assert_eq_bool,
                    &[self.rt_ptr, arg_vals[0], arg_vals[1], arg_vals[2]],
                );
                None
            }
            "std::test::assert_eq_string" => {
                self.call_runtime_void(
                    self.runtime.test_assert_eq_string,
                    &[self.rt_ptr, arg_vals[0], arg_vals[1], arg_vals[2]],
                );
                None
            }
            "std::fs::read_text" => Some(
                self.call_runtime_value(self.runtime.fs_read_text, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::fs::write_text" => Some(self.call_runtime_value(
                self.runtime.fs_write_text,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::fs::read_bytes" => Some(
                self.call_runtime_value(self.runtime.fs_read_bytes, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::fs::write_bytes" => Some(self.call_runtime_value(
                self.runtime.fs_write_bytes,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::path::join" => Some(self.call_runtime_value(
                self.runtime.path_join,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::path::normalize" => Some(
                self.call_runtime_value(self.runtime.path_normalize, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::path::basename" => Some(
                self.call_runtime_value(self.runtime.path_basename, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::path::dirname" => Some(
                self.call_runtime_value(self.runtime.path_dirname, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::env::args" => {
                Some(self.call_runtime_value(self.runtime.env_args, &[self.rt_ptr]))
            }
            "std::env::get" => {
                Some(self.call_runtime_value(self.runtime.env_get, &[self.rt_ptr, arg_vals[0]]))
            }
            "std::env::set_var" => Some(self.call_runtime_value(
                self.runtime.env_set,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::env::cwd" => Some(self.call_runtime_value(self.runtime.env_cwd, &[self.rt_ptr])),
            "std::env::set_cwd" => {
                Some(self.call_runtime_value(self.runtime.env_set_cwd, &[self.rt_ptr, arg_vals[0]]))
            }
            "std::json::encode_i64" => Some(
                self.call_runtime_value(self.runtime.json_encode_i64, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::json::encode_bool" => Some(
                self.call_runtime_value(self.runtime.json_encode_bool, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::json::encode_string" => {
                Some(self.call_runtime_value(
                    self.runtime.json_encode_string,
                    &[self.rt_ptr, arg_vals[0]],
                ))
            }
            "std::json::decode_i64" => Some(
                self.call_runtime_value(self.runtime.json_decode_i64, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::json::decode_bool" => Some(
                self.call_runtime_value(self.runtime.json_decode_bool, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::json::decode_string" => {
                Some(self.call_runtime_value(
                    self.runtime.json_decode_string,
                    &[self.rt_ptr, arg_vals[0]],
                ))
            }
            "std::thread::join" => {
                Some(self.call_runtime_value(self.runtime.thread_join, &[self.rt_ptr, arg_vals[0]]))
            }
            "std::net::connect" => {
                let layout = self
                    .books
                    .get("TcpStream")
                    .ok_or_else(|| native_error("missing layout for TcpStream"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(
                    self.runtime.net_connect,
                    &[self.rt_ptr, arg_vals[0], book_id],
                ))
            }
            "std::net::listen" => {
                let layout = self
                    .books
                    .get("TcpListener")
                    .ok_or_else(|| native_error("missing layout for TcpListener"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(
                    self.runtime.net_listen,
                    &[self.rt_ptr, arg_vals[0], book_id],
                ))
            }
            "std::net::accept" => {
                let layout = self
                    .books
                    .get("TcpStream")
                    .ok_or_else(|| native_error("missing layout for TcpStream"))?;
                let stream_book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(
                    self.runtime.net_accept,
                    &[self.rt_ptr, arg_vals[0], stream_book_id],
                ))
            }
            "std::net::write_text" => Some(self.call_runtime_value(
                self.runtime.net_write_text,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::net::read_line" => Some(
                self.call_runtime_value(self.runtime.net_read_line, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::net::read_exact" => Some(self.call_runtime_value(
                self.runtime.net_read_exact,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::net::read_to_end" => Some(
                self.call_runtime_value(self.runtime.net_read_to_end, &[self.rt_ptr, arg_vals[0]]),
            ),
            "std::net::set_read_timeout_ms" => Some(self.call_runtime_value(
                self.runtime.net_set_read_timeout_ms,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::net::close_stream" => {
                self.call_runtime_void(self.runtime.net_close_stream, &[self.rt_ptr, arg_vals[0]]);
                None
            }
            "std::net::close_listener" => {
                self.call_runtime_void(
                    self.runtime.net_close_listener,
                    &[self.rt_ptr, arg_vals[0]],
                );
                None
            }
            "std::net::pool" => {
                let layout = self
                    .books
                    .get("TcpPool")
                    .ok_or_else(|| native_error("missing layout for TcpPool"))?;
                let pool_book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(
                    self.runtime.net_pool,
                    &[self.rt_ptr, arg_vals[0], arg_vals[1], pool_book_id],
                ))
            }
            "std::net::pool_get" => {
                let layout = self
                    .books
                    .get("TcpStream")
                    .ok_or_else(|| native_error("missing layout for TcpStream"))?;
                let stream_book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(
                    self.runtime.net_pool_get,
                    &[self.rt_ptr, arg_vals[0], stream_book_id],
                ))
            }
            "std::net::pool_put" => Some(self.call_runtime_value(
                self.runtime.net_pool_put,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::net::pool_close" => {
                self.call_runtime_void(self.runtime.net_pool_close, &[self.rt_ptr, arg_vals[0]]);
                None
            }
            "std::channel::i64" => {
                let layout = self
                    .books
                    .get("ChannelI64")
                    .ok_or_else(|| native_error("missing layout for ChannelI64"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(self.runtime.channel_i64, &[self.rt_ptr, book_id]))
            }
            "std::channel::bool" => {
                let layout = self
                    .books
                    .get("ChannelBool")
                    .ok_or_else(|| native_error("missing layout for ChannelBool"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(self.runtime.channel_bool, &[self.rt_ptr, book_id]))
            }
            "std::channel::f64" => {
                let layout = self
                    .books
                    .get("ChannelF64")
                    .ok_or_else(|| native_error("missing layout for ChannelF64"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(self.runtime.channel_f64, &[self.rt_ptr, book_id]))
            }
            "std::channel::u8" => {
                let layout = self
                    .books
                    .get("ChannelU8")
                    .ok_or_else(|| native_error("missing layout for ChannelU8"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(self.runtime.channel_u8, &[self.rt_ptr, book_id]))
            }
            "std::channel::string" => {
                let layout = self
                    .books
                    .get("ChannelString")
                    .ok_or_else(|| native_error("missing layout for ChannelString"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(self.runtime.channel_string, &[self.rt_ptr, book_id]))
            }
            "std::channel::bytes" => {
                let layout = self
                    .books
                    .get("ChannelBytes")
                    .ok_or_else(|| native_error("missing layout for ChannelBytes"))?;
                let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
                Some(self.call_runtime_value(self.runtime.channel_bytes, &[self.rt_ptr, book_id]))
            }
            _ => return Err(native_error(format!("unknown function '{name}'."))),
        };
        Ok(value)
    }

    fn emit_thread_spawn(
        &mut self,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        if args.is_empty() {
            return Err(native_error(
                "std::thread::spawn expects at least 1 argument.".to_string(),
            ));
        }
        if let Some(expected) = expected {
            if expected != &Type::Book("Thread".to_string()) {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got book.",
                    type_name(expected),
                )));
            }
        }
        let entry_name = match &args[0].kind {
            ExprKind::String(value) => value.clone(),
            _ => {
                return Err(native_error(
                    "std::thread::spawn entry must be a string literal.".to_string(),
                ))
            }
        };
        let result = self
            .emit_call(&entry_name, &args[1..], Some(&Type::I64))?
            .ok_or_else(|| native_error("Thread entry rule must return i64.".to_string()))?;

        let layout = self
            .books
            .get("Thread")
            .ok_or_else(|| native_error("missing layout for Thread"))?;
        let book_id = self.builder.ins().iconst(types::I64, layout.id as i64);
        let field_count = self.builder.ins().iconst(types::I64, 0);
        let thread_handle = self.call_runtime_value(
            self.runtime.alloc_object,
            &[self.rt_ptr, book_id, field_count],
        );
        self.call_runtime_void(
            self.runtime.thread_store,
            &[self.rt_ptr, thread_handle, result],
        );
        Ok(Some(thread_handle))
    }
}
