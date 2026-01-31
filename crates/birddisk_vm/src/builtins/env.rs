use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_env_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::env::args" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::args expects 0 arguments",
                    ));
                }
                let values = self.alloc_env_args()?;
                Ok(Some(values))
            }
            "std::env::get" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::get expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let name = self.string_text(*handle)?;
                        let value = match std::env::var_os(&name) {
                            Some(value) => value,
                            None => return Ok(Some(self.alloc_string(""))),
                        };
                        let value = value.into_string().map_err(|_| {
                            runtime_error("E0400", "std::env::get returned invalid UTF-8.")
                        })?;
                        Ok(Some(self.alloc_string(&value)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::env::get expects string argument",
                    )),
                }
            }
            "std::env::set_var" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::set_var expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(name_handle), Value::String(value_handle)) => {
                        let name = self.string_text(*name_handle)?;
                        let value = self.string_text(*value_handle)?;
                        if name.contains('\0') || value.contains('\0') {
                            return Err(runtime_error(
                                "E0400",
                                "std::env::set_var expects strings without NUL.",
                            ));
                        }
                        std::env::set_var(name, value);
                        Ok(Some(Value::I64(1)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::env::set_var expects string arguments",
                    )),
                }
            }
            "std::env::cwd" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::cwd expects 0 arguments",
                    ));
                }
                let cwd = std::env::current_dir().map_err(|_| {
                    runtime_error("E0400", "std::env::cwd failed.")
                })?;
                let cwd = cwd.to_str().ok_or_else(|| {
                    runtime_error("E0400", "std::env::cwd returned invalid UTF-8.")
                })?;
                Ok(Some(self.alloc_string(cwd)))
            }
            "std::env::set_cwd" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::env::set_cwd expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        if path.contains('\0') {
                            return Err(runtime_error(
                                "E0400",
                                "std::env::set_cwd expects string without NUL.",
                            ));
                        }
                        if std::env::set_current_dir(path).is_err() {
                            return Err(runtime_error(
                                "E0400",
                                "std::env::set_cwd failed.",
                            ));
                        }
                        Ok(Some(Value::I64(1)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::env::set_cwd expects string argument",
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}
