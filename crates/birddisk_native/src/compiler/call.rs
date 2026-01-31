use super::NativeCompiler;
use crate::error::{native_error, NativeError};
use crate::program::{stdlib_signature, type_name};
use birddisk_core::ast::{Expr, Type};
use cranelift_codegen::ir::{InstBuilder, Value};
use cranelift_module::Module;

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
            let func_ref = self
                .module
                .declare_func_in_func(func_id, self.builder.func);
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
        let func_ref = self
            .module
            .declare_func_in_func(func_id, self.builder.func);
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

    fn emit_std_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
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
            "std::string::len" => Some(self.call_runtime_value(
                self.runtime.string_len,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::concat" => Some(self.call_runtime_value(
                self.runtime.string_concat,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::eq" => Some(self.call_runtime_value(
                self.runtime.string_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::bytes" => Some(self.call_runtime_value(
                self.runtime.string_bytes,
                &[self.rt_ptr, arg_vals[0]],
            )),
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
            "std::string::from_bytes" => Some(self.call_runtime_value(
                self.runtime.string_from_bytes,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::to_i64" => Some(self.call_runtime_value(
                self.runtime.string_to_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::from_i64" => Some(self.call_runtime_value(
                self.runtime.string_from_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::bytes::len" => Some(self.call_runtime_value(
                self.runtime.bytes_len,
                &[self.rt_ptr, arg_vals[0]],
            )),
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
            "std::io::read_line" => Some(self.call_runtime_value(
                self.runtime.io_read_line,
                &[self.rt_ptr],
            )),
            "std::time::now_ms" => Some(self.call_runtime_value(
                self.runtime.time_now_ms,
                &[self.rt_ptr],
            )),
            "std::time::sleep_ms" => Some(self.call_runtime_value(
                self.runtime.time_sleep_ms,
                &[self.rt_ptr, arg_vals[0]],
            )),
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
            "std::fs::read_text" => Some(self.call_runtime_value(
                self.runtime.fs_read_text,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::fs::write_text" => Some(self.call_runtime_value(
                self.runtime.fs_write_text,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::fs::read_bytes" => Some(self.call_runtime_value(
                self.runtime.fs_read_bytes,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::fs::write_bytes" => Some(self.call_runtime_value(
                self.runtime.fs_write_bytes,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::path::join" => Some(self.call_runtime_value(
                self.runtime.path_join,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::path::normalize" => Some(self.call_runtime_value(
                self.runtime.path_normalize,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::path::basename" => Some(self.call_runtime_value(
                self.runtime.path_basename,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::path::dirname" => Some(self.call_runtime_value(
                self.runtime.path_dirname,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::env::args" => Some(self.call_runtime_value(
                self.runtime.env_args,
                &[self.rt_ptr],
            )),
            "std::env::get" => Some(self.call_runtime_value(
                self.runtime.env_get,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::env::set_var" => Some(self.call_runtime_value(
                self.runtime.env_set,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::env::cwd" => Some(self.call_runtime_value(
                self.runtime.env_cwd,
                &[self.rt_ptr],
            )),
            "std::env::set_cwd" => Some(self.call_runtime_value(
                self.runtime.env_set_cwd,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::encode_i64" => Some(self.call_runtime_value(
                self.runtime.json_encode_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::encode_bool" => Some(self.call_runtime_value(
                self.runtime.json_encode_bool,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::encode_string" => Some(self.call_runtime_value(
                self.runtime.json_encode_string,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::decode_i64" => Some(self.call_runtime_value(
                self.runtime.json_decode_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::decode_bool" => Some(self.call_runtime_value(
                self.runtime.json_decode_bool,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::decode_string" => Some(self.call_runtime_value(
                self.runtime.json_decode_string,
                &[self.rt_ptr, arg_vals[0]],
            )),
            _ => return Err(native_error(format!("unknown function '{name}'."))),
        };
        Ok(value)
    }
}
