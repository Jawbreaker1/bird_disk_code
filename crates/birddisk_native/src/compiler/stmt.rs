use super::{NativeCompiler, VarInfo};
use crate::error::{native_error, NativeError};
use birddisk_core::ast::{ExprKind, Function, Stmt, Type};
use cranelift_codegen::ir::{types, InstBuilder};
use cranelift_module::Module;

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    pub(crate) fn bind_params(
        &mut self,
        function: &Function,
        entry: cranelift_codegen::ir::Block,
    ) -> Result<(), NativeError> {
        let params: Vec<cranelift_codegen::ir::Value> = self.builder.block_params(entry).to_vec();
        for (index, param) in function.params.iter().enumerate() {
            let value = params[index + 1];
            let var = self.new_var(&param.ty);
            self.builder.def_var(var, value);
            self.vars.insert(
                param.name.clone(),
                VarInfo {
                    var,
                    ty: param.ty.clone(),
                },
            );
            self.update_root(&param.name, value);
        }
        Ok(())
    }

    pub(crate) fn emit_stmt(&mut self, stmt: &Stmt) -> Result<bool, NativeError> {
        match stmt {
            Stmt::Set { name, expr, .. } => {
                if self.vars.contains_key(name) {
                    return Err(native_error(format!(
                        "native backend does not support shadowing '{name}'."
                    )));
                }
                let var_ty = self
                    .locals
                    .get(name)
                    .ok_or_else(|| native_error(format!("missing type for '{name}'.")))?
                    .clone();
                let var = self.new_var(&var_ty);
                let value = self.emit_expr(expr, Some(&var_ty))?;
                self.builder.def_var(var, value);
                self.vars.insert(
                    name.clone(),
                    VarInfo {
                        var,
                        ty: var_ty,
                    },
                );
                self.update_root(name, value);
                Ok(false)
            }
            Stmt::Expr { expr, .. } => {
                let ExprKind::Call { name, args } = &expr.kind else {
                    return Err(native_error("call statements require function calls."));
                };
                let result = self.emit_call(name, args, Some(&Type::Void))?;
                if result.is_some() {
                    return Err(native_error("call statements require void return type."));
                }
                Ok(false)
            }
            Stmt::Put { name, expr, .. } => {
                let var = self
                    .vars
                    .get(name)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{name}'.")))?;
                let value = self.emit_expr(expr, Some(&var.ty))?;
                self.builder.def_var(var.var, value);
                self.update_root(name, value);
                Ok(false)
            }
            Stmt::PutIndex {
                name,
                index,
                expr,
                ..
            } => {
                let var = self
                    .vars
                    .get(name)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{name}'.")))?;
                let Type::Array(elem_ty) = &var.ty else {
                    return Err(native_error("index assignment requires array type."));
                };
                let handle = self.builder.use_var(var.var);
                let index_val = self.emit_expr(index, Some(&Type::I64))?;
                let value = self.emit_expr(expr, Some(elem_ty.as_ref()))?;
                self.emit_array_set(elem_ty.as_ref(), handle, index_val, value)?;
                Ok(false)
            }
            Stmt::PutField {
                base,
                field,
                expr,
                ..
            } => {
                let base_info = self
                    .vars
                    .get(base)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
                let Type::Book(book_name) = &base_info.ty else {
                    return Err(native_error("field assignment requires book type."));
                };
                let layout = self
                    .books
                    .get(book_name)
                    .ok_or_else(|| native_error(format!("unknown book '{book_name}'.")))?;
                let index = layout
                    .field_index
                    .get(field)
                    .copied()
                    .ok_or_else(|| native_error(format!("unknown field '{field}'.")))?;
                let field_ty = layout
                    .fields
                    .get(index)
                    .ok_or_else(|| native_error(format!("unknown field '{field}'.")))?;
                let handle = self.builder.use_var(base_info.var);
                let index_val = self.builder.ins().iconst(types::I64, index as i64);
                let value = self.emit_expr(expr, Some(field_ty))?;
                self.emit_object_set(field_ty, handle, index_val, value)?;
                Ok(false)
            }
            Stmt::Yield { expr, .. } => {
                let return_type = self.return_type.clone();
                let value = self.emit_expr(expr, Some(&return_type))?;
                self.builder.ins().return_(&[value]);
                Ok(true)
            }
            Stmt::Throw { expr, .. } => {
                let value = self.emit_expr(expr, Some(&Type::String))?;
                self.call_runtime_void(self.runtime.throw_error, &[self.rt_ptr, value]);
                self.builder.ins().jump(self.error_block, &[]);
                Ok(true)
            }
            Stmt::Try {
                try_body,
                catch_name,
                catch_body,
                ..
            } => {
                let catch_block = self.builder.create_block();
                self.error_targets.push(catch_block);
                let terminated = self.emit_block(try_body)?;
                self.error_targets.pop();
                let after_block = if !terminated {
                    Some(self.builder.create_block())
                } else {
                    None
                };
                if let Some(after_block) = after_block {
                    self.builder.ins().jump(after_block, &[]);
                }
                self.builder.switch_to_block(catch_block);
                self.builder.seal_block(catch_block);
                let message = self.call_runtime_value_no_check(self.runtime.error_message, &[self.rt_ptr]);
                self.call_runtime_void_no_check(self.runtime.clear_error, &[self.rt_ptr]);
                self.bind_or_assign_local(catch_name, Type::String, message)?;
                let terminated = self.emit_block(catch_body)?;
                if !terminated {
                    if let Some(after_block) = after_block {
                        self.builder.ins().jump(after_block, &[]);
                    }
                }
                if let Some(after_block) = after_block {
                    self.builder.switch_to_block(after_block);
                    self.builder.seal_block(after_block);
                    Ok(false)
                } else {
                    Ok(true)
                }
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                let cond_val = self.emit_expr(cond, Some(&Type::Bool))?;
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let cont_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(cond_val, then_block, &[], else_block, &[]);
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_term = self.emit_block(then_body)?;
                if !then_term {
                    self.builder.ins().jump(cont_block, &[]);
                }
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_term = self.emit_block(else_body)?;
                if !else_term {
                    self.builder.ins().jump(cont_block, &[]);
                }
                if then_term && else_term {
                    Ok(true)
                } else {
                    self.builder.switch_to_block(cont_block);
                    self.builder.seal_block(cont_block);
                    Ok(false)
                }
            }
            Stmt::Repeat { cond, body, .. } => {
                let loop_block = self.builder.create_block();
                let continue_block = self.builder.create_block();
                self.builder.ins().jump(loop_block, &[]);
                self.builder.switch_to_block(loop_block);
                let _ = self.emit_block(body)?;
                let cond_val = self.emit_expr(cond, Some(&Type::Bool))?;
                self.builder
                    .ins()
                    .brif(cond_val, loop_block, &[], continue_block, &[]);
                self.builder.seal_block(loop_block);
                self.builder.switch_to_block(continue_block);
                self.builder.seal_block(continue_block);
                Ok(false)
            }
            Stmt::Match {
                expr,
                cases,
                otherwise,
                ..
            } => self.emit_match(expr, cases, otherwise),
        }
    }

    pub(super) fn emit_block(&mut self, stmts: &[Stmt]) -> Result<bool, NativeError> {
        for stmt in stmts {
            if self.emit_stmt(stmt)? {
                return Ok(true);
            }
        }
        Ok(false)
    }
}
