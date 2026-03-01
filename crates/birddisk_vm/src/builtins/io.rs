use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_io_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::io::print" | "std::io::host_print" => {
                if args.len() != 1 {
                    return Err(runtime_error("E0400", "std::io::print expects 1 argument"));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let text = self.string_text(*handle)?;
                        self.push_output(&text);
                        Ok(Some(Value::Void))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::io::print expects string argument",
                    )),
                }
            }
            "std::io::read_line" | "std::io::host_read_line" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::io::read_line expects 0 arguments",
                    ));
                }
                let line = self.read_input_line();
                Ok(Some(self.alloc_string(&line)))
            }
            _ => Ok(None),
        }
    }
}
