use super::*;
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Type, UnaryOp};

impl<'a> Vm<'a> {
    pub(crate) fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(Value::I64(*value)),
            ExprKind::Float(value) => Ok(Value::F64(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::String(value) => Ok(self.alloc_string(value)),
            ExprKind::Ident(name) => self.lookup(name).cloned().ok_or_else(|| {
                runtime_error("E0400", format!("Unknown name '{name}' at runtime."))
            }),
            ExprKind::Call { name, args } => {
                let (values, arg_count) = self.eval_args_with_roots(args)?;
                let result = (|| -> Result<Value, RuntimeError> {
                    if let Some(value) = self.eval_enum_constructor(name, &values)? {
                        return Ok(value);
                    }
                    if let Some(value) = self.eval_builtin_call(name, &values)? {
                        return Ok(value);
                    }
                    if let Some(function) = self.functions.get(name).copied() {
                        return self.eval_function(function, &values);
                    }
                    if let Some((base, method)) = name.split_once("::") {
                        if base == "std" {
                            return Err(runtime_error(
                                "E0400",
                                format!("Unknown function '{name}' at runtime."),
                            ));
                        }
                        let Some(base_value) = self.lookup(base).cloned() else {
                            return Err(runtime_error(
                                "E0400",
                                format!("Unknown function '{name}' at runtime."),
                            ));
                        };
                        if let Value::Object { ref book, .. } = base_value {
                            if let Some(value) =
                                self.eval_channel_method(&base_value, method, &values)?
                            {
                                return Ok(value);
                            }
                            let full_name = format!("{book}::{method}");
                            let function = *self.functions.get(&full_name).ok_or_else(|| {
                                runtime_error(
                                    "E0400",
                                    format!("Unknown function '{full_name}' at runtime."),
                                )
                            })?;
                            let mut call_values = Vec::with_capacity(values.len() + 1);
                            call_values.push(base_value);
                            call_values.extend(values.iter().cloned());
                            return self.eval_function(function, &call_values);
                        }
                        return Err(runtime_error(
                            "E0400",
                            format!("Unknown function '{name}' at runtime."),
                        ));
                    }
                    let function = *self.functions.get(name).ok_or_else(|| {
                        runtime_error("E0400", format!("Unknown function '{name}' at runtime."))
                    })?;
                    self.eval_function(function, &values)
                })();
                if arg_count > 0 {
                    self.roots.pop_frame(arg_count);
                }
                result
            }
            ExprKind::New { book, args } => {
                let mut instance = self.alloc_object(book)?;
                if let Some(init) = self.functions.get(&format!("{book}::init")).cloned() {
                    let mut values = Vec::new();
                    values.push(instance.clone());
                    for arg in args {
                        values.push(self.eval_expr(arg)?);
                    }
                    let init_value = self.eval_function(init, &values)?;
                    instance = coerce_value(init_value, &Type::Book(book.clone()))?;
                } else if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        format!("Missing constructor '{book}::init'."),
                    ));
                }
                Ok(instance)
            }
            ExprKind::MemberAccess { base, field } => {
                let Some(value) = self.lookup(base).cloned() else {
                    return Err(runtime_error(
                        "E0400",
                        format!("Unknown name '{base}' at runtime."),
                    ));
                };
                match value {
                    Value::Object { book, handle } => {
                        let Some(book_info) = self.books.get(&book) else {
                            return Err(runtime_error("E0400", "Unknown book at runtime."));
                        };
                        let Some(index) = book_info.field_index.get(field) else {
                            return Err(runtime_error(
                                "E0400",
                                format!("Unknown field '{field}' at runtime."),
                            ));
                        };
                        let field_ty = &book_info.field_types[*index];
                        self.read_object_field(handle, field_ty, *index)
                    }
                    _ => Err(runtime_error("E0400", "Field access on non-book.")),
                }
            }
            ExprKind::ArrayLit(elements) => self.eval_array_literal(elements, None),
            ExprKind::ArrayNew { .. } => Err(runtime_error(
                "E0400",
                "array constructor requires explicit array type",
            )),
            ExprKind::Index { base, index } => self.eval_index_expr(base, index),
            ExprKind::Cast { expr, ty } => {
                let value = self.eval_expr(expr)?;
                self.eval_cast(value, ty)
            }
            ExprKind::Unary { op, expr } => {
                let value = self.eval_expr(expr)?;
                match (op, value) {
                    (UnaryOp::Neg, Value::I64(value)) => Ok(Value::I64(-value)),
                    (UnaryOp::Neg, Value::F64(value)) => Ok(Value::F64(-value)),
                    (UnaryOp::Not, Value::Bool(value)) => Ok(Value::Bool(!value)),
                    _ => Err(runtime_error("E0400", "invalid unary operation")),
                }
            }
            ExprKind::Binary { op, left, right } => {
                let left = self.eval_expr(left)?;
                if let BinaryOp::AndAnd | BinaryOp::OrOr = op {
                    return self.eval_short_circuit(*op, left, right);
                }
                let right = self.eval_expr(right)?;
                self.eval_binary(*op, left, right)
            }
        }
    }

    fn eval_args_with_roots(&mut self, args: &[Expr]) -> Result<(Vec<Value>, usize), RuntimeError> {
        if args.is_empty() {
            return Ok((Vec::new(), 0));
        }
        let base = self.roots.push_frame(args.len());
        let mut values = Vec::with_capacity(args.len());
        for (index, arg) in args.iter().enumerate() {
            let value = match self.eval_expr(arg) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(args.len());
                    return Err(err);
                }
            };
            values.push(value.clone());
            self.update_root_slot(base + index, &value);
        }
        Ok((values, args.len()))
    }
}
