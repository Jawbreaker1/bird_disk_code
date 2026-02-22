use super::*;
use birddisk_core::ast::{BinaryOp, Expr};

impl<'a> Vm<'a> {
    pub(super) fn eval_short_circuit(
        &mut self,
        op: BinaryOp,
        left: Value,
        right: &Expr,
    ) -> Result<Value, RuntimeError> {
        match (op, left) {
            (BinaryOp::AndAnd, Value::Bool(false)) => Ok(Value::Bool(false)),
            (BinaryOp::AndAnd, Value::Bool(true)) => self.eval_expr(right),
            (BinaryOp::OrOr, Value::Bool(true)) => Ok(Value::Bool(true)),
            (BinaryOp::OrOr, Value::Bool(false)) => self.eval_expr(right),
            _ => Err(runtime_error("E0400", "invalid logical operation")),
        }
    }

    pub(super) fn eval_binary(
        &self,
        op: BinaryOp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (op, left, right) {
            (BinaryOp::Add, Value::I64(left), Value::I64(right)) => Ok(Value::I64(left + right)),
            (BinaryOp::Add, Value::F64(left), Value::F64(right)) => Ok(Value::F64(left + right)),
            (BinaryOp::Sub, Value::I64(left), Value::I64(right)) => Ok(Value::I64(left - right)),
            (BinaryOp::Sub, Value::F64(left), Value::F64(right)) => Ok(Value::F64(left - right)),
            (BinaryOp::Mul, Value::I64(left), Value::I64(right)) => Ok(Value::I64(left * right)),
            (BinaryOp::Mul, Value::F64(left), Value::F64(right)) => Ok(Value::F64(left * right)),
            (BinaryOp::Div, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "division by zero"))
            }
            (BinaryOp::Div, Value::F64(_), Value::F64(right)) if right == 0.0 => {
                Err(runtime_error("E0402", "division by zero"))
            }
            (BinaryOp::Mod, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "modulo by zero"))
            }
            (BinaryOp::Mod, Value::F64(_), Value::F64(right)) if right == 0.0 => {
                Err(runtime_error("E0402", "modulo by zero"))
            }
            (BinaryOp::Div, Value::I64(left), Value::I64(right)) => Ok(Value::I64(left / right)),
            (BinaryOp::Div, Value::F64(left), Value::F64(right)) => Ok(Value::F64(left / right)),
            (BinaryOp::Mod, Value::I64(left), Value::I64(right)) => Ok(Value::I64(left % right)),
            (BinaryOp::Mod, Value::F64(left), Value::F64(right)) => Ok(Value::F64(left % right)),
            (BinaryOp::EqEq, Value::I64(left), Value::I64(right)) => Ok(Value::Bool(left == right)),
            (BinaryOp::EqEq, Value::F64(left), Value::F64(right)) => Ok(Value::Bool(left == right)),
            (BinaryOp::NotEq, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left != right))
            }
            (BinaryOp::NotEq, Value::F64(left), Value::F64(right)) => {
                Ok(Value::Bool(left != right))
            }
            (BinaryOp::Lt, Value::I64(left), Value::I64(right)) => Ok(Value::Bool(left < right)),
            (BinaryOp::Lt, Value::F64(left), Value::F64(right)) => Ok(Value::Bool(left < right)),
            (BinaryOp::LtEq, Value::I64(left), Value::I64(right)) => Ok(Value::Bool(left <= right)),
            (BinaryOp::LtEq, Value::F64(left), Value::F64(right)) => Ok(Value::Bool(left <= right)),
            (BinaryOp::Gt, Value::I64(left), Value::I64(right)) => Ok(Value::Bool(left > right)),
            (BinaryOp::Gt, Value::F64(left), Value::F64(right)) => Ok(Value::Bool(left > right)),
            (BinaryOp::GtEq, Value::I64(left), Value::I64(right)) => Ok(Value::Bool(left >= right)),
            (BinaryOp::GtEq, Value::F64(left), Value::F64(right)) => Ok(Value::Bool(left >= right)),
            (BinaryOp::EqEq, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left == right))
            }
            (BinaryOp::NotEq, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left != right))
            }
            (BinaryOp::AndAnd, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left && right))
            }
            (BinaryOp::OrOr, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left || right))
            }
            _ => Err(runtime_error("E0400", "invalid binary operation")),
        }
    }
}
