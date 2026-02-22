use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(super) fn eval_rand_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::rand::seed" => {
                if args.len() != 1 {
                    return Err(runtime_error("E0400", "std::rand::seed expects 1 argument"));
                }
                match &args[0] {
                    Value::I64(seed) => {
                        self.rand_seed(*seed);
                        Ok(Some(Value::Void))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::rand::seed expects i64 argument",
                    )),
                }
            }
            "std::rand::range" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::rand::range expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::I64(min), Value::I64(max)) => {
                        Ok(Some(Value::I64(self.rand_range(*min, *max)?)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::rand::range expects i64 arguments",
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}
