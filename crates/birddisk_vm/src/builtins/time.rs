use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_time_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::time::now_ms" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::time::now_ms expects 0 arguments",
                    ));
                }
                Ok(Some(Value::I64(self.now_ms())))
            }
            "std::time::sleep_ms" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::time::sleep_ms expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::I64(millis) => Ok(Some(Value::I64(self.sleep_ms(*millis)?))),
                    _ => Err(runtime_error(
                        "E0400",
                        "std::time::sleep_ms expects i64 argument",
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}
