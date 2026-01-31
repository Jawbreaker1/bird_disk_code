use super::*;
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(super) fn eval_cast(&self, value: Value, ty: &Type) -> Result<Value, RuntimeError> {
        match (value, ty) {
            (Value::I64(value), Type::I64) => Ok(Value::I64(value)),
            (Value::F64(value), Type::F64) => Ok(Value::F64(value)),
            (Value::I64(value), Type::F64) => Ok(Value::F64(value as f64)),
            (Value::F64(value), Type::I64) => {
                if !value.is_finite() {
                    return Err(runtime_error(
                        "E0400",
                        "f64 to i64 conversion requires a finite value.",
                    ));
                }
                let min = i64::MIN as f64;
                let max = i64::MAX as f64;
                if value < min || value > max {
                    return Err(runtime_error(
                        "E0400",
                        "f64 to i64 conversion out of range.",
                    ));
                }
                Ok(Value::I64(value.trunc() as i64))
            }
            _ => Err(runtime_error("E0400", "invalid cast")),
        }
    }
}
