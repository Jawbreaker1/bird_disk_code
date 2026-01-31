use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_test_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::test::assert" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::test::assert expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::Bool(cond), Value::String(message)) => {
                        if *cond {
                            Ok(Some(Value::Void))
                        } else {
                            let text = self.string_text(*message)?;
                            Err(runtime_error("E0404", text))
                        }
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::test::assert expects (bool, string)",
                    )),
                }
            }
            "std::test::assert_eq_i64" => {
                if args.len() != 3 {
                    return Err(runtime_error(
                        "E0400",
                        "std::test::assert_eq_i64 expects 3 arguments",
                    ));
                }
                match (&args[0], &args[1], &args[2]) {
                    (Value::I64(left), Value::I64(right), Value::String(message)) => {
                        if left == right {
                            Ok(Some(Value::Void))
                        } else {
                            let text = self.string_text(*message)?;
                            Err(runtime_error("E0404", text))
                        }
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::test::assert_eq_i64 expects (i64, i64, string)",
                    )),
                }
            }
            "std::test::assert_eq_bool" => {
                if args.len() != 3 {
                    return Err(runtime_error(
                        "E0400",
                        "std::test::assert_eq_bool expects 3 arguments",
                    ));
                }
                match (&args[0], &args[1], &args[2]) {
                    (Value::Bool(left), Value::Bool(right), Value::String(message)) => {
                        if left == right {
                            Ok(Some(Value::Void))
                        } else {
                            let text = self.string_text(*message)?;
                            Err(runtime_error("E0404", text))
                        }
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::test::assert_eq_bool expects (bool, bool, string)",
                    )),
                }
            }
            "std::test::assert_eq_string" => {
                if args.len() != 3 {
                    return Err(runtime_error(
                        "E0400",
                        "std::test::assert_eq_string expects 3 arguments",
                    ));
                }
                match (&args[0], &args[1], &args[2]) {
                    (Value::String(left), Value::String(right), Value::String(message)) => {
                        let left_bytes = self.string_bytes(*left)?;
                        let right_bytes = self.string_bytes(*right)?;
                        if left_bytes == right_bytes {
                            Ok(Some(Value::Void))
                        } else {
                            let text = self.string_text(*message)?;
                            Err(runtime_error("E0404", text))
                        }
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::test::assert_eq_string expects (string, string, string)",
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}
