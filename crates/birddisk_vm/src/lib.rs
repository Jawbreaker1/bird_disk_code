//! BirdDisk VM interpreter (placeholder).

use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, UnaryOp, Value};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub code: &'static str,
    pub message: String,
}

fn runtime_error(code: &'static str, message: impl Into<String>) -> RuntimeError {
    RuntimeError {
        code,
        message: message.into(),
    }
}

pub fn eval(program: &Program) -> Result<i64, RuntimeError> {
    let mut vm = Vm::new(program);
    vm.eval_main()
}

struct Vm<'a> {
    functions: HashMap<String, &'a birddisk_core::ast::Function>,
    scopes: Vec<HashMap<String, Value>>,
}

impl<'a> Vm<'a> {
    fn new(program: &'a Program) -> Self {
        let mut functions = HashMap::new();
        for func in &program.functions {
            functions.insert(func.name.clone(), func);
        }
        Self {
            functions,
            scopes: Vec::new(),
        }
    }

    fn eval_main(&mut self) -> Result<i64, RuntimeError> {
        let main = self
            .functions
            .get("main")
            .ok_or_else(|| runtime_error("E0400", "missing main function"))?;
        let value = self.eval_function(main, &[])?;
        match value {
            Value::I64(value) => Ok(value),
            Value::Bool(_) => Err(runtime_error("E0400", "main must return i64")),
        }
    }

    fn eval_function(
        &mut self,
        function: &birddisk_core::ast::Function,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if args.len() != function.params.len() {
            return Err(runtime_error(
                "E0400",
                format!(
                    "Wrong number of arguments for '{}': expected {}, got {}.",
                    function.name,
                    function.params.len(),
                    args.len()
                ),
            ));
        }
        self.push_scope();
        for (param, arg) in function.params.iter().zip(args.iter()) {
            self.current_scope_mut()
                .insert(param.name.clone(), *arg);
        }
        let result = self.eval_block(&function.body);
        self.pop_scope();
        match result {
            Ok(Some(value)) => Ok(value),
            Ok(None) => Err(runtime_error(
                "E0400",
                format!("function '{}' did not yield", function.name),
            )),
            Err(err) => Err(err),
        }
    }

    fn eval_block(&mut self, stmts: &[Stmt]) -> Result<Option<Value>, RuntimeError> {
        for stmt in stmts {
            if let Some(value) = self.eval_stmt(stmt)? {
                return Ok(Some(value));
            }
        }
        Ok(None)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>, RuntimeError> {
        match stmt {
            Stmt::Set { name, expr, .. } => {
                let value = self.eval_expr(expr)?;
                self.current_scope_mut().insert(name.clone(), value);
                Ok(None)
            }
            Stmt::Put { name, expr, .. } => {
                let value = self.eval_expr(expr)?;
                if let Some(existing) = self.lookup_mut(name) {
                    *existing = value;
                    Ok(None)
                } else {
                    Err(runtime_error(
                        "E0400",
                        format!("Unknown name '{name}' at runtime."),
                    ))
                }
            }
            Stmt::Yield { expr, .. } => Ok(Some(self.eval_expr(expr)?)),
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                let cond_value = self.eval_expr(cond)?;
                match cond_value {
                    Value::Bool(true) => {
                        self.push_scope();
                        let result = self.eval_block(then_body);
                        self.pop_scope();
                        result
                    }
                    Value::Bool(false) => {
                        self.push_scope();
                        let result = self.eval_block(else_body);
                        self.pop_scope();
                        result
                    }
                    _ => Err(runtime_error("E0400", "when condition was not bool")),
                }
            }
            Stmt::Repeat { cond, body, .. } => {
                loop {
                    let cond_value = self.eval_expr(cond)?;
                    match cond_value {
                        Value::Bool(true) => {
                            self.push_scope();
                            let result = self.eval_block(body)?;
                            self.pop_scope();
                            if result.is_some() {
                                return Ok(result);
                            }
                        }
                        Value::Bool(false) => break,
                        _ => return Err(runtime_error("E0400", "repeat condition was not bool")),
                    }
                }
                Ok(None)
            }
        }
    }

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(Value::I64(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::Ident(name) => self.lookup(name).copied().ok_or_else(|| {
                runtime_error("E0400", format!("Unknown name '{name}' at runtime."))
            }),
            ExprKind::Call { name, args } => {
                let function = *self.functions.get(name).ok_or_else(|| {
                    runtime_error("E0400", format!("Unknown function '{name}' at runtime."))
                })?;
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.eval_expr(arg)?);
                }
                self.eval_function(function, &values)
            }
            ExprKind::Unary { op, expr } => {
                let value = self.eval_expr(expr)?;
                match (op, value) {
                    (UnaryOp::Neg, Value::I64(value)) => Ok(Value::I64(-value)),
                    (UnaryOp::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
                    _ => Err(runtime_error("E0400", "invalid unary operand")),
                }
            }
            ExprKind::Binary { left, op, right } => {
                let left_value = self.eval_expr(left)?;
                let right_value = self.eval_expr(right)?;
                self.eval_binary(*op, left_value, right_value)
            }
        }
    }

    fn eval_binary(&self, op: BinaryOp, left: Value, right: Value) -> Result<Value, RuntimeError> {
        match (op, left, right) {
            (BinaryOp::Add, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a + b)),
            (BinaryOp::Sub, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a - b)),
            (BinaryOp::Mul, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a * b)),
            (BinaryOp::Div, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "Division by zero."))
            }
            (BinaryOp::Div, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a / b)),
            (BinaryOp::Mod, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "Modulo by zero."))
            }
            (BinaryOp::Mod, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a % b)),
            (BinaryOp::EqEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a == b)),
            (BinaryOp::NotEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a != b)),
            (BinaryOp::Lt, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a < b)),
            (BinaryOp::LtEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a <= b)),
            (BinaryOp::Gt, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a > b)),
            (BinaryOp::GtEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a >= b)),
            (BinaryOp::AndAnd, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
            (BinaryOp::OrOr, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
            _ => Err(runtime_error("E0400", "invalid binary operands")),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Value> {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
    }

    fn lookup(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn lookup_mut(&mut self, name: &str) -> Option<&mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser};

    fn eval_source(source: &str) -> i64 {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        eval(&program).unwrap()
    }

    #[test]
    fn eval_minimal_main() {
        let result = eval_source("rule main() -> i64:\n  yield 0.\nend\n");
        assert_eq!(result, 0);
    }

    #[test]
    fn eval_when() {
        let result = eval_source(
            "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 2.\n  end\nend\n",
        );
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_repeat() {
        let result = eval_source(
            "rule main() -> i64:\n  set i = 0.\n  set sum: i64 = 0.\n\n  repeat while i < 5:\n    put sum = sum + i.\n    put i = i + 1.\n  end\n\n  yield sum.\nend\n",
        );
        assert_eq!(result, 10);
    }

    #[test]
    fn eval_call() {
        let result = eval_source(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(2, 3).\nend\n",
        );
        assert_eq!(result, 5);
    }

    #[test]
    fn eval_div_by_zero_errors() {
        let tokens = lexer::lex("rule main() -> i64:\n  yield 1 / 0.\nend\n").unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
    }

    #[test]
    fn eval_rejects_wrong_arity() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
        assert!(err.message.contains("expected 2"));
    }
}
