use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_fs_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::fs::read_text" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::read_text expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let text = std::fs::read_to_string(path)
                            .map_err(|_| runtime_error("E0400", "std::fs::read_text failed."))?;
                        Ok(Some(self.alloc_string(&text)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::read_text expects string argument",
                    )),
                }
            }
            "std::fs::write_text" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::write_text expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(path_handle), Value::String(text_handle)) => {
                        let path = self.string_text(*path_handle)?;
                        let text = self.string_text(*text_handle)?;
                        std::fs::write(path, text.as_bytes())
                            .map_err(|_| runtime_error("E0400", "std::fs::write_text failed."))?;
                        Ok(Some(Value::I64(text.as_bytes().len() as i64)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::write_text expects string arguments",
                    )),
                }
            }
            "std::fs::read_bytes" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::read_bytes expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let bytes = std::fs::read(path)
                            .map_err(|_| runtime_error("E0400", "std::fs::read_bytes failed."))?;
                        Ok(Some(self.alloc_u8_array(&bytes)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::read_bytes expects string argument",
                    )),
                }
            }
            "std::fs::write_bytes" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::fs::write_bytes expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(path_handle), Value::Array { handle, elem_type }) => {
                        let path = self.string_text(*path_handle)?;
                        let bytes = self.read_u8_array(*handle, elem_type)?;
                        std::fs::write(path, &bytes)
                            .map_err(|_| runtime_error("E0400", "std::fs::write_bytes failed."))?;
                        Ok(Some(Value::I64(bytes.len() as i64)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::fs::write_bytes expects string and u8[] arguments",
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}
