use super::*;
use birddisk_core::ast::{ExprKind, Stmt, Type};


impl<'a> Vm<'a> {
    pub(super) fn eval_block(&mut self, stmts: &[Stmt]) -> Result<Option<Value>, RuntimeError> {
        for stmt in stmts {
            if let Some(value) = self.eval_stmt(stmt)? {
                return Ok(Some(value));
            }
        }
        Ok(None)
    }


    pub(super) fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>, RuntimeError> {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                let value = match &expr.kind {
                    ExprKind::ArrayNew { len } => {
                        let elem_ty = ty.as_ref().ok_or_else(|| {
                            runtime_error("E0400", "array constructor requires explicit array type")
                        })?;
                        match elem_ty {
                            Type::Array(inner) => self.eval_array_new(len, inner)?,
                            _ => {
                                return Err(runtime_error(
                                    "E0400",
                                    "array constructor requires array type",
                                ))
                            }
                        }
                    }
                    ExprKind::ArrayLit(elements) => {
                        if let Some(elem_ty) = ty {
                            match elem_ty {
                                Type::Array(inner) => self.eval_array_literal(elements, Some(inner))?,
                                _ => {
                                    return Err(runtime_error(
                                        "E0400",
                                        "array literal requires array type",
                                    ))
                                }
                            }
                        } else {
                            self.eval_array_literal(elements, None)?
                        }
                    }
                    _ => self.eval_expr(expr)?,
                };
                let value = if let Some(expected) = ty {
                    coerce_value(value, expected)?
                } else {
                    value
                };
                self.bind_local(name.clone(), value);
                Ok(None)
            }
            Stmt::Expr { expr, .. } => {
                self.eval_expr(expr)?;
                Ok(None)
            }
            Stmt::Put { name, expr, .. } => {
                let value = match &expr.kind {
                    ExprKind::ArrayNew { len } => {
                        let elem_ty = match self.lookup(name) {
                            Some(Value::Array { elem_type, .. }) => elem_type.clone(),
                            Some(_) => {
                                return Err(runtime_error(
                                    "E0400",
                                    "array constructor requires array target",
                                ))
                            }
                            None => {
                                return Err(runtime_error(
                                    "E0400",
                                    format!("Unknown name '{name}' at runtime."),
                                ))
                            }
                        };
                        self.eval_array_new(len, &elem_ty)?
                    }
                    ExprKind::ArrayLit(elements) => {
                        let elem_ty = match self.lookup(name) {
                            Some(Value::Array { elem_type, .. }) => elem_type.clone(),
                            Some(_) => {
                                return Err(runtime_error(
                                    "E0400",
                                    "array literal requires array target",
                                ))
                            }
                            None => {
                                return Err(runtime_error(
                                    "E0400",
                                    format!("Unknown name '{name}' at runtime."),
                                ))
                            }
                        };
                        self.eval_array_literal(elements, Some(&elem_ty))?
                    }
                    _ => self.eval_expr(expr)?,
                };
                self.assign_var(name, value)?;
                Ok(None)
            }
            Stmt::PutIndex {
                name,
                index,
                expr,
                ..
            } => {
                let idx = self.eval_index_value(index)?;
                let value = self.eval_expr(expr)?;
                let target = self.lookup(name).cloned().ok_or_else(|| {
                    runtime_error("E0400", format!("Unknown name '{name}' at runtime."))
                })?;
                match target {
                    Value::Array { handle, elem_type } => {
                        self.write_array_elem(handle, &elem_type, idx, value)?;
                        Ok(None)
                    }
                    _ => Err(runtime_error("E0400", "Index assignment on non-array.")),
                }
            }
            Stmt::PutField {
                base,
                field,
                expr,
                ..
            } => {
                let value = self.eval_expr(expr)?;
                let (book_name, handle) = match self.lookup(base) {
                    Some(Value::Object { book, handle }) => (book.clone(), *handle),
                    Some(_) => {
                        return Err(runtime_error("E0400", "Field assignment on non-book."))
                    }
                    None => {
                        return Err(runtime_error(
                            "E0400",
                            format!("Unknown name '{base}' at runtime."),
                        ))
                    }
                };
                let book_info = self
                    .books
                    .get(&book_name)
                    .ok_or_else(|| runtime_error("E0400", "Unknown book at runtime."))?;
                let index = *book_info.field_index.get(field).ok_or_else(|| {
                    runtime_error("E0400", format!("Unknown field '{field}' at runtime."))
                })?;
                let field_ty = book_info.field_types[index].clone();
                self.write_object_field(handle, index, &field_ty, value)?;
                Ok(None)
            }
            Stmt::Yield { expr, .. } => Ok(Some(self.eval_expr(expr)?)),
            Stmt::Throw { expr, .. } => {
                let value = self.eval_expr(expr)?;
                let message = match value {
                    Value::String(handle) => self.string_text(handle)?,
                    _ => {
                        return Err(runtime_error(
                            "E0400",
                            "throw expects a string message.",
                        ))
                    }
                };
                Err(runtime_error("E0404", message))
            }
            Stmt::Try {
                try_body,
                catch_name,
                catch_body,
                ..
            } => {
                self.push_scope();
                let result = self.eval_block(try_body);
                self.pop_scope();
                match result {
                    Ok(Some(value)) => Ok(Some(value)),
                    Ok(None) => Ok(None),
                    Err(err) => {
                        if err.code != "E0404" {
                            return Err(err);
                        }
                        self.push_scope();
                        let msg_value = self.alloc_string(&err.message);
                        self.bind_local(catch_name.clone(), msg_value);
                        let result = self.eval_block(catch_body);
                        self.pop_scope();
                        result
                    }
                }
            }
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
                            let result = self.eval_block(body);
                            self.pop_scope();
                            match result {
                                Ok(Some(value)) => return Ok(Some(value)),
                                Ok(None) => {}
                                Err(err) => return Err(err),
                            }
                        }
                        Value::Bool(false) => break,
                        _ => return Err(runtime_error("E0400", "repeat condition was not bool")),
                    }
                }
                Ok(None)
            }
            Stmt::Match {
                expr,
                cases,
                otherwise,
                ..
            } => {
                let value = self.eval_expr(expr)?;
                let Value::Enum { handle, name } = value else {
                    return Err(runtime_error("E0400", "match requires enum value"));
                };
                let enum_info = self
                    .enums
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| runtime_error("E0400", "Unknown enum at runtime"))?;
                let header = self.heap.header(handle);
                if header.kind() != HeapKind::Enum {
                    return Err(runtime_error("E0400", "Expected enum value"));
                }
                if header.type_id() != enum_info.id {
                    return Err(runtime_error("E0400", "Enum type mismatch at runtime"));
                }
                let variant_id = header.len_or_size;
                for case in cases {
                    if case.enum_name != name {
                        continue;
                    }
                    let Some(variant) = enum_info.variants.get(&case.variant_name) else {
                        continue;
                    };
                    if variant.id != variant_id {
                        continue;
                    }
                    self.push_scope();
                    if let (Some(binding), Some(payload_ty)) =
                        (&case.binding, variant.payload.as_ref())
                    {
                        let payload = self.read_enum_payload(handle, payload_ty)?;
                        self.bind_local(binding.clone(), payload);
                    }
                    let result = self.eval_block(&case.body);
                    self.pop_scope();
                    return result;
                }
                self.push_scope();
                let result = self.eval_block(otherwise);
                self.pop_scope();
                result
            }
        }
    }

}
