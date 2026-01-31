use super::NativeCompiler;
use crate::analysis::{elem_kind_for_type, elem_size_for_kind};
use crate::error::{native_error, NativeError};
use crate::program::type_name;
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, MatchCase, Stmt, Type, UnaryOp};
use birddisk_core::runtime as abi;
use cranelift_codegen::ir::condcodes::{FloatCC, IntCC};
use cranelift_codegen::ir::immediates::Ieee64;
use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_module::{DataDescription, Linkage, Module};

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    pub(super) fn emit_expr(
        &mut self,
        expr: &Expr,
        expected: Option<&Type>,
    ) -> Result<Value, NativeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(self.builder.ins().iconst(types::I64, *value)),
            ExprKind::Float(value) => Ok(self
                .builder
                .ins()
                .f64const(Ieee64::with_float(*value))),
            ExprKind::Bool(value) => {
                let bit = if *value { 1 } else { 0 };
                Ok(self.builder.ins().iconst(types::I64, bit))
            }
            ExprKind::String(value) => self.emit_string_literal(value),
            ExprKind::Ident(name) => {
                let var = self
                    .vars
                    .get(name)
                    .cloned()
                    .ok_or_else(|| native_error(format!("unknown name '{name}'.")))?;
                Ok(self.builder.use_var(var.var))
            }
            ExprKind::Cast { expr, ty } => {
                let value = self.emit_expr(expr, None)?;
                let from_ty = self.infer_expr_type(expr).unwrap_or(Type::I64);
                match (&from_ty, ty) {
                    (Type::I64, Type::F64) => {
                        Ok(self.builder.ins().fcvt_from_sint(types::F64, value))
                    }
                    (Type::F64, Type::I64) => Ok(self.builder.ins().fcvt_to_sint(types::I64, value)),
                    (from, to) if from == to => Ok(value),
                    _ => Err(native_error("invalid cast")),
                }
            }
            ExprKind::Call { name, args } => {
                let value = self.emit_call(name, args, expected)?;
                value.ok_or_else(|| native_error("void call cannot be used as expression."))
            }
            ExprKind::New { book, args } => self.emit_new(book, args),
            ExprKind::MemberAccess { base, field } => self.emit_member_access(base, field),
            ExprKind::Unary { op, expr } => {
                let value = self.emit_expr(expr, None)?;
                match op {
                    UnaryOp::Neg => {
                        let ty = self.infer_expr_type(expr).unwrap_or(Type::I64);
                        if matches!(ty, Type::F64) {
                            Ok(self.builder.ins().fneg(value))
                        } else {
                            Ok(self.builder.ins().ineg(value))
                        }
                    }
                    UnaryOp::Not => {
                        let cond = self.builder.ins().icmp_imm(IntCC::Equal, value, 0);
                        Ok(self.bool_to_i64(cond))
                    }
                }
            }
            ExprKind::Binary { left, op, right } => {
                let left_ty = self.infer_expr_type(left).unwrap_or(Type::I64);
                let right_ty = self.infer_expr_type(right).unwrap_or(Type::I64);
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        let left_val = self.emit_expr(left, None)?;
                        let right_val = self.emit_expr(right, None)?;
                        let value = if matches!((&left_ty, &right_ty), (Type::F64, Type::F64)) {
                            match op {
                                BinaryOp::Add => self.builder.ins().fadd(left_val, right_val),
                                BinaryOp::Sub => self.builder.ins().fsub(left_val, right_val),
                                BinaryOp::Mul => self.builder.ins().fmul(left_val, right_val),
                                BinaryOp::Div => self.builder.ins().fdiv(left_val, right_val),
                                BinaryOp::Mod => {
                                    return Err(native_error("mod is not supported for f64."));
                                }
                                _ => unreachable!(),
                            }
                        } else {
                            match op {
                                BinaryOp::Add => self.builder.ins().iadd(left_val, right_val),
                                BinaryOp::Sub => self.builder.ins().isub(left_val, right_val),
                                BinaryOp::Mul => self.builder.ins().imul(left_val, right_val),
                                BinaryOp::Div => self.builder.ins().sdiv(left_val, right_val),
                                BinaryOp::Mod => self.builder.ins().srem(left_val, right_val),
                                _ => unreachable!(),
                            }
                        };
                        Ok(value)
                    }
                    BinaryOp::EqEq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq => {
                        let left_val = self.emit_expr(left, None)?;
                        let right_val = self.emit_expr(right, None)?;
                        let cond = if matches!((&left_ty, &right_ty), (Type::F64, Type::F64)) {
                            let cc = match op {
                                BinaryOp::EqEq => FloatCC::Equal,
                                BinaryOp::NotEq => FloatCC::NotEqual,
                                BinaryOp::Lt => FloatCC::LessThan,
                                BinaryOp::LtEq => FloatCC::LessThanOrEqual,
                                BinaryOp::Gt => FloatCC::GreaterThan,
                                BinaryOp::GtEq => FloatCC::GreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            self.builder.ins().fcmp(cc, left_val, right_val)
                        } else {
                            let cc = match op {
                                BinaryOp::EqEq => IntCC::Equal,
                                BinaryOp::NotEq => IntCC::NotEqual,
                                BinaryOp::Lt => IntCC::SignedLessThan,
                                BinaryOp::LtEq => IntCC::SignedLessThanOrEqual,
                                BinaryOp::Gt => IntCC::SignedGreaterThan,
                                BinaryOp::GtEq => IntCC::SignedGreaterThanOrEqual,
                                _ => unreachable!(),
                            };
                            self.builder.ins().icmp(cc, left_val, right_val)
                        };
                        Ok(self.bool_to_i64(cond))
                    }
                    BinaryOp::AndAnd => self.emit_and(left, right),
                    BinaryOp::OrOr => self.emit_or(left, right),
                }
            }
            ExprKind::ArrayLit(elements) => self.emit_array_literal(elements, expected),
            ExprKind::ArrayNew { len } => self.emit_array_new(len, expected),
            ExprKind::Index { base, index } => self.emit_index_expr(base, index),
        }
    }

    fn bool_to_i64(&mut self, cond: Value) -> Value {
        let one = self.builder.ins().iconst(types::I64, 1);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().select(cond, one, zero)
    }

    fn i64_to_bool(&mut self, value: Value) -> Value {
        self.builder.ins().icmp_imm(IntCC::NotEqual, value, 0)
    }

    fn emit_and(&mut self, left: &Expr, right: &Expr) -> Result<Value, NativeError> {
        let left_val = self.emit_expr(left, None)?;
        let cond = self.i64_to_bool(left_val);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder
            .ins()
            .brif(cond, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        let right_val = self.emit_expr(right, None)?;
        self.builder.ins().jump(merge_block, &[right_val]);
        self.builder.seal_block(then_block);

        self.builder.switch_to_block(else_block);
        let zero = self.builder.ins().iconst(types::I64, 0);
        self.builder.ins().jump(merge_block, &[zero]);
        self.builder.seal_block(else_block);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(self.builder.block_params(merge_block)[0])
    }

    fn emit_or(&mut self, left: &Expr, right: &Expr) -> Result<Value, NativeError> {
        let left_val = self.emit_expr(left, None)?;
        let cond = self.i64_to_bool(left_val);
        let then_block = self.builder.create_block();
        let else_block = self.builder.create_block();
        let merge_block = self.builder.create_block();
        self.builder.append_block_param(merge_block, types::I64);
        self.builder
            .ins()
            .brif(cond, then_block, &[], else_block, &[]);

        self.builder.switch_to_block(then_block);
        let one = self.builder.ins().iconst(types::I64, 1);
        self.builder.ins().jump(merge_block, &[one]);
        self.builder.seal_block(then_block);

        self.builder.switch_to_block(else_block);
        let right_val = self.emit_expr(right, None)?;
        self.builder.ins().jump(merge_block, &[right_val]);
        self.builder.seal_block(else_block);

        self.builder.switch_to_block(merge_block);
        self.builder.seal_block(merge_block);
        Ok(self.builder.block_params(merge_block)[0])
    }

    pub(super) fn emit_match(
        &mut self,
        expr: &Expr,
        cases: &[MatchCase],
        otherwise: &[Stmt],
    ) -> Result<bool, NativeError> {
        let expr_ty = self
            .infer_expr_type(expr)
            .ok_or_else(|| native_error("match requires enum value."))?;
        let enum_name = match &expr_ty {
            Type::Book(name) if self.enums.contains_key(name) => name.clone(),
            _ => return Err(native_error("match requires enum value.")),
        };
        let enum_info = self
            .enums
            .get(&enum_name)
            .ok_or_else(|| native_error(format!("unknown enum '{enum_name}'.")))?;

        for case in cases {
            if case.enum_name != enum_name {
                return Err(native_error(format!(
                    "case enum '{}' does not match '{}'.",
                    case.enum_name, enum_name
                )));
            }
            let variant = enum_info.variants.get(&case.variant_name).ok_or_else(|| {
                native_error(format!(
                    "unknown enum variant '{}::{}'.",
                    case.enum_name, case.variant_name
                ))
            })?;
            if case.binding.is_some() && variant.payload.is_none() {
                return Err(native_error(format!(
                    "variant '{}::{}' has no payload.",
                    case.enum_name, case.variant_name
                )));
            }
        }

        let value = self.emit_expr(expr, Some(&expr_ty))?;
        let enum_id = self.builder.ins().iconst(types::I64, enum_info.id as i64);
        let variant_val = self.call_runtime_value(
            self.runtime.enum_variant,
            &[self.rt_ptr, value, enum_id],
        );

        let merge_block = self.builder.create_block();
        let otherwise_block = self.builder.create_block();
        let mut needs_merge = false;

        let mut current_check = self.builder.create_block();
        self.builder.ins().jump(current_check, &[]);
        self.builder.switch_to_block(current_check);

        if cases.is_empty() {
            self.builder.ins().jump(otherwise_block, &[]);
            self.builder.seal_block(current_check);
        } else {
            for (idx, case) in cases.iter().enumerate() {
                let variant = enum_info
                    .variants
                    .get(&case.variant_name)
                    .expect("variant already checked");
                let case_block = self.builder.create_block();
                let next_check = if idx + 1 == cases.len() {
                    otherwise_block
                } else {
                    self.builder.create_block()
                };
                let cond = self
                    .builder
                    .ins()
                    .icmp_imm(IntCC::Equal, variant_val, variant.id as i64);
                self.builder
                    .ins()
                    .brif(cond, case_block, &[], next_check, &[]);
                self.builder.seal_block(current_check);

                self.builder.switch_to_block(case_block);
                if let (Some(binding), Some(payload_ty)) =
                    (case.binding.as_ref(), variant.payload.as_ref())
                {
                    let payload_val = self.emit_enum_payload(payload_ty, value)?;
                    self.bind_or_assign_local(binding, payload_ty.clone(), payload_val)?;
                }
                let case_returned = self.emit_block(&case.body)?;
                if !case_returned {
                    self.builder.ins().jump(merge_block, &[]);
                    needs_merge = true;
                }
                self.builder.seal_block(case_block);

                self.builder.switch_to_block(next_check);
                current_check = next_check;
            }
        }

        self.builder.switch_to_block(otherwise_block);
        let otherwise_returned = self.emit_block(otherwise)?;
        if !otherwise_returned {
            self.builder.ins().jump(merge_block, &[]);
            needs_merge = true;
        }
        self.builder.seal_block(otherwise_block);

        if needs_merge {
            self.builder.switch_to_block(merge_block);
            self.builder.seal_block(merge_block);
            Ok(false)
        } else {
            Ok(true)
        }
    }

    fn emit_string_literal(&mut self, value: &str) -> Result<Value, NativeError> {
        let data_id = self.string_data.get(value).cloned().map(Ok).unwrap_or_else(|| {
            let mut data_ctx = DataDescription::new();
            data_ctx.define(value.as_bytes().to_vec().into_boxed_slice());
            let name = format!("bd_str_{}", *self.string_counter);
            let data_id = self
                .module
                .declare_data(&name, Linkage::Local, false, false)
                .map_err(|err| native_error(format!("native declare data failed: {err}")))?;
            self.module
                .define_data(data_id, &data_ctx)
                .map_err(|err| native_error(format!("native define data failed: {err}")))?;
            self.string_data.insert(value.to_string(), data_id);
            *self.string_counter += 1;
            Ok(data_id)
        })?;
        let global = self.module.declare_data_in_func(data_id, self.builder.func);
        let ptr = self.builder.ins().global_value(types::I64, global);
        let len = self
            .builder
            .ins()
            .iconst(types::I64, value.as_bytes().len() as i64);
        Ok(self.call_runtime_value(
            self.runtime.alloc_string,
            &[self.rt_ptr, ptr, len],
        ))
    }

    fn emit_array_new(&mut self, len: &Expr, expected: Option<&Type>) -> Result<Value, NativeError> {
        let Some(Type::Array(elem_ty)) = expected else {
            return Err(native_error(
                "array constructor requires explicit array type.",
            ));
        };
        let len_val = self.emit_expr(len, Some(&Type::I64))?;
        let elem_kind = elem_kind_for_type(elem_ty.as_ref())?;
        let elem_size = elem_size_for_kind(elem_kind)?;
        let kind_val = self.builder.ins().iconst(types::I64, elem_kind as i64);
        let size_val = self.builder.ins().iconst(types::I64, elem_size as i64);
        let handle = self.call_runtime_value(
            self.runtime.alloc_array,
            &[self.rt_ptr, kind_val, size_val, len_val],
        );
        if elem_kind == abi::ARRAY_KIND_REF {
            let idx_var = self.new_var(&Type::I64);
            let zero = self.builder.ins().iconst(types::I64, 0);
            self.builder.def_var(idx_var, zero);
            let loop_block = self.builder.create_block();
            let body_block = self.builder.create_block();
            let exit_block = self.builder.create_block();
            self.builder.ins().jump(loop_block, &[]);

            self.builder.switch_to_block(loop_block);
            let idx_val = self.builder.use_var(idx_var);
            let cond = self
                .builder
                .ins()
                .icmp(IntCC::SignedLessThan, idx_val, len_val);
            self.builder
                .ins()
                .brif(cond, body_block, &[], exit_block, &[]);

            self.builder.switch_to_block(body_block);
            let default_val = self.emit_default_value(elem_ty.as_ref())?;
            self.emit_array_set(elem_ty.as_ref(), handle, idx_val, default_val)?;
            let one = self.builder.ins().iconst(types::I64, 1);
            let next = self.builder.ins().iadd(idx_val, one);
            self.builder.def_var(idx_var, next);
            self.builder.ins().jump(loop_block, &[]);
            self.builder.seal_block(body_block);
            self.builder.seal_block(loop_block);

            self.builder.switch_to_block(exit_block);
            self.builder.seal_block(exit_block);
        }
        Ok(handle)
    }

    fn emit_array_literal(
        &mut self,
        elements: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Value, NativeError> {
        let elem_ty = if let Some(Type::Array(inner)) = expected {
            inner.as_ref().clone()
        } else if elements.is_empty() {
            return Err(native_error(
                "array literal requires explicit array type.",
            ));
        } else {
            let first = self
                .infer_expr_type(&elements[0])
                .ok_or_else(|| native_error("array literal element has unknown type."))?;
            for elem in &elements[1..] {
                let ty = self
                    .infer_expr_type(elem)
                    .ok_or_else(|| native_error("array literal element has unknown type."))?;
                if ty != first {
                    return Err(native_error(
                        "array literal elements must have the same type.",
                    ));
                }
            }
            first
        };
        let len_val = self
            .builder
            .ins()
            .iconst(types::I64, elements.len() as i64);
        let elem_kind = elem_kind_for_type(&elem_ty)?;
        let elem_size = elem_size_for_kind(elem_kind)?;
        let kind_val = self.builder.ins().iconst(types::I64, elem_kind as i64);
        let size_val = self.builder.ins().iconst(types::I64, elem_size as i64);
        let handle = self.call_runtime_value(
            self.runtime.alloc_array,
            &[self.rt_ptr, kind_val, size_val, len_val],
        );
        for (index, expr) in elements.iter().enumerate() {
            let index_val = self.builder.ins().iconst(types::I64, index as i64);
            let value = self.emit_expr(expr, Some(&elem_ty))?;
            self.emit_array_set(&elem_ty, handle, index_val, value)?;
        }
        Ok(handle)
    }

    fn emit_default_value(&mut self, ty: &Type) -> Result<Value, NativeError> {
        match ty {
            Type::I64 => Ok(self.builder.ins().iconst(types::I64, 0)),
            Type::F64 => Ok(self
                .builder
                .ins()
                .f64const(Ieee64::with_float(0.0))),
            Type::Bool => Ok(self.builder.ins().iconst(types::I64, 0)),
            Type::U8 => Ok(self.builder.ins().iconst(types::I64, 0)),
            Type::String => self.emit_string_literal(""),
            Type::Array(elem_ty) => self.emit_empty_array(elem_ty.as_ref()),
            Type::Book(book) => self.emit_default_book(book),
            Type::Void => Err(native_error("void has no default value.")),
        }
    }

    fn emit_default_book(&mut self, book: &str) -> Result<Value, NativeError> {
        let layout = self
            .books
            .get(book)
            .ok_or_else(|| native_error(format!("unknown book '{book}'.")))?;
        let book_id = self
            .builder
            .ins()
            .iconst(types::I64, layout.id as i64);
        let field_count = self
            .builder
            .ins()
            .iconst(types::I64, layout.fields.len() as i64);
        let handle = self.call_runtime_value(
            self.runtime.alloc_object,
            &[self.rt_ptr, book_id, field_count],
        );
        for (index, field_ty) in layout.fields.iter().enumerate() {
            let index_val = self.builder.ins().iconst(types::I64, index as i64);
            let value = self.emit_default_value(field_ty)?;
            self.emit_object_set(field_ty, handle, index_val, value)?;
        }
        Ok(handle)
    }

    fn emit_empty_array(&mut self, elem_ty: &Type) -> Result<Value, NativeError> {
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let elem_size = elem_size_for_kind(elem_kind)?;
        let kind_val = self.builder.ins().iconst(types::I64, elem_kind as i64);
        let size_val = self.builder.ins().iconst(types::I64, elem_size as i64);
        let len_val = self.builder.ins().iconst(types::I64, 0);
        Ok(self.call_runtime_value(
            self.runtime.alloc_array,
            &[self.rt_ptr, kind_val, size_val, len_val],
        ))
    }

    fn emit_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<Value, NativeError> {
        let base_ty = self
            .infer_expr_type(base)
            .ok_or_else(|| native_error("indexing requires array type."))?;
        let Type::Array(elem_ty) = base_ty else {
            return Err(native_error("indexing on non-array."));
        };
        let handle = self.emit_expr(base, None)?;
        let index_val = self.emit_expr(index, Some(&Type::I64))?;
        self.emit_array_get(&elem_ty, handle, index_val)
    }

    fn emit_array_get(
        &mut self,
        elem_ty: &Type,
        handle: Value,
        index: Value,
    ) -> Result<Value, NativeError> {
        match elem_ty {
            Type::I64 => Ok(self.call_runtime_value(
                self.runtime.array_get_i64,
                &[self.rt_ptr, handle, index],
            )),
            Type::F64 => Ok(self.call_runtime_value(
                self.runtime.array_get_f64,
                &[self.rt_ptr, handle, index],
            )),
            Type::Bool => Ok(self.call_runtime_value(
                self.runtime.array_get_bool,
                &[self.rt_ptr, handle, index],
            )),
            Type::U8 => Ok(self.call_runtime_value(
                self.runtime.array_get_u8,
                &[self.rt_ptr, handle, index],
            )),
            Type::String | Type::Array(_) | Type::Book(_) => Ok(self.call_runtime_value(
                self.runtime.array_get_ref,
                &[self.rt_ptr, handle, index],
            )),
            Type::Void => Err(native_error("void is not a valid array element type.")),
        }
    }

    pub(super) fn emit_array_set(
        &mut self,
        elem_ty: &Type,
        handle: Value,
        index: Value,
        value: Value,
    ) -> Result<(), NativeError> {
        match elem_ty {
            Type::I64 => self.call_runtime_void(
                self.runtime.array_set_i64,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::F64 => self.call_runtime_void(
                self.runtime.array_set_f64,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::Bool => self.call_runtime_void(
                self.runtime.array_set_bool,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::U8 => self.call_runtime_void(
                self.runtime.array_set_u8,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::String | Type::Array(_) | Type::Book(_) => self.call_runtime_void(
                self.runtime.array_set_ref,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::Void => return Err(native_error("void is not a valid array element type.")),
        }
        Ok(())
    }

    fn emit_new(&mut self, book: &str, args: &[Expr]) -> Result<Value, NativeError> {
        let layout = self
            .books
            .get(book)
            .ok_or_else(|| native_error(format!("unknown book '{book}'.")))?;
        let book_id = self
            .builder
            .ins()
            .iconst(types::I64, layout.id as i64);
        let field_count = self
            .builder
            .ins()
            .iconst(types::I64, layout.fields.len() as i64);
        let mut handle = self.call_runtime_value(
            self.runtime.alloc_object,
            &[self.rt_ptr, book_id, field_count],
        );
        for (index, field_ty) in layout.fields.iter().enumerate() {
            let index_val = self.builder.ins().iconst(types::I64, index as i64);
            let value = self.emit_default_value(field_ty)?;
            self.emit_object_set(field_ty, handle, index_val, value)?;
        }
        let init_name = format!("{book}::init");
        if let Some(sig) = self.functions.get(&init_name) {
            if sig.params.is_empty() {
                return Err(native_error(format!(
                    "method '{init_name}' must take self."
                )));
            }
            let expected_args = sig.params.len().saturating_sub(1);
            if expected_args != args.len() {
                return Err(native_error(format!(
                    "wrong number of arguments for '{init_name}': expected {}, got {}.",
                    expected_args,
                    args.len()
                )));
            }
            let func_id = self
                .func_ids
                .get(&init_name)
                .copied()
                .ok_or_else(|| native_error(format!("missing function id for '{init_name}'.")))?;
            let func_ref = self
                .module
                .declare_func_in_func(func_id, self.builder.func);
            let mut call_args = Vec::with_capacity(args.len() + 2);
            call_args.push(self.rt_ptr);
            call_args.push(handle);
            for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
                call_args.push(self.emit_expr(arg, Some(param_ty))?);
            }
            let call = self.builder.ins().call(func_ref, &call_args);
            handle = self.builder.inst_results(call)[0];
        } else if !args.is_empty() {
            return Err(native_error(format!(
                "missing constructor '{init_name}'."
            )));
        }
        Ok(handle)
    }

    pub(super) fn emit_enum_constructor(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        let Some((enum_name, variant_name)) = name.split_once("::") else {
            return Ok(None);
        };
        if enum_name == "std" || variant_name.contains("::") {
            return Ok(None);
        }
        if self.functions.contains_key(name) {
            return Ok(None);
        }
        if self.lookup_local_type(enum_name).is_some() {
            return Ok(None);
        }
        let Some(enum_info) = self.enums.get(enum_name) else {
            return Ok(None);
        };
        let variant = enum_info
            .variants
            .get(variant_name)
            .ok_or_else(|| native_error(format!("unknown enum variant '{name}'.")))?;
        let expected_args = if variant.payload.is_some() { 1 } else { 0 };
        if args.len() != expected_args {
            return Err(native_error(format!(
                "wrong number of arguments for '{name}': expected {}, got {}.",
                expected_args,
                args.len()
            )));
        }
        if let Some(expected) = expected {
            let enum_ty = Type::Book(enum_name.to_string());
            if &enum_ty != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&enum_ty)
                )));
            }
        }

        let payload_ty = variant.payload.as_ref();
        let payload_val = if let Some(payload_ty) = payload_ty {
            let Some(arg) = args.first() else {
                return Err(native_error("missing enum payload value."));
            };
            Some(self.emit_expr(arg, Some(payload_ty))?)
        } else {
            None
        };

        let enum_id = self.builder.ins().iconst(types::I64, enum_info.id as i64);
        let variant_id = self.builder.ins().iconst(types::I64, variant.id as i64);
        let (kind_val, len_val) = if let Some(payload_ty) = payload_ty {
            let kind = elem_kind_for_type(payload_ty)?;
            (
                self.builder.ins().iconst(types::I64, kind as i64),
                self.builder
                    .ins()
                    .iconst(types::I64, abi::OBJECT_FIELD_SIZE as i64),
            )
        } else {
            (
                self.builder.ins().iconst(types::I64, 0),
                self.builder.ins().iconst(types::I64, 0),
            )
        };
        let handle = self.call_runtime_value(
            self.runtime.alloc_enum,
            &[self.rt_ptr, enum_id, variant_id, kind_val, len_val],
        );

        if let (Some(payload_ty), Some(payload_val)) = (payload_ty, payload_val) {
            self.emit_enum_set_payload(payload_ty, handle, payload_val)?;
        }
        Ok(Some(handle))
    }

    fn emit_member_access(&mut self, base: &str, field: &str) -> Result<Value, NativeError> {
        let base_info = self
            .vars
            .get(base)
            .cloned()
            .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
        let Type::Book(book_name) = &base_info.ty else {
            return Err(native_error("field access requires book type."));
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
        self.emit_object_get(field_ty, handle, index_val)
    }

    fn emit_enum_payload(&mut self, payload_ty: &Type, handle: Value) -> Result<Value, NativeError> {
        let value = match payload_ty {
            Type::I64 => self.call_runtime_value(
                self.runtime.enum_payload_i64,
                &[self.rt_ptr, handle],
            ),
            Type::F64 => self.call_runtime_value(
                self.runtime.enum_payload_f64,
                &[self.rt_ptr, handle],
            ),
            Type::Bool => self.call_runtime_value(
                self.runtime.enum_payload_bool,
                &[self.rt_ptr, handle],
            ),
            Type::U8 => self.call_runtime_value(
                self.runtime.enum_payload_u8,
                &[self.rt_ptr, handle],
            ),
            Type::String | Type::Array(_) | Type::Book(_) => self.call_runtime_value(
                self.runtime.enum_payload_ref,
                &[self.rt_ptr, handle],
            ),
            Type::Void => return Err(native_error("enum payload cannot be void.")),
        };
        Ok(value)
    }

    fn emit_enum_set_payload(
        &mut self,
        payload_ty: &Type,
        handle: Value,
        value: Value,
    ) -> Result<(), NativeError> {
        match payload_ty {
            Type::I64 => self.call_runtime_void(
                self.runtime.enum_set_payload_i64,
                &[self.rt_ptr, handle, value],
            ),
            Type::F64 => self.call_runtime_void(
                self.runtime.enum_set_payload_f64,
                &[self.rt_ptr, handle, value],
            ),
            Type::Bool => self.call_runtime_void(
                self.runtime.enum_set_payload_bool,
                &[self.rt_ptr, handle, value],
            ),
            Type::U8 => self.call_runtime_void(
                self.runtime.enum_set_payload_u8,
                &[self.rt_ptr, handle, value],
            ),
            Type::String | Type::Array(_) | Type::Book(_) => self.call_runtime_void(
                self.runtime.enum_set_payload_ref,
                &[self.rt_ptr, handle, value],
            ),
            Type::Void => return Err(native_error("enum payload cannot be void.")),
        }
        Ok(())
    }

    fn emit_object_get(
        &mut self,
        field_ty: &Type,
        handle: Value,
        index: Value,
    ) -> Result<Value, NativeError> {
        match field_ty {
            Type::I64 => Ok(self.call_runtime_value(
                self.runtime.object_get_i64,
                &[self.rt_ptr, handle, index],
            )),
            Type::F64 => Ok(self.call_runtime_value(
                self.runtime.object_get_f64,
                &[self.rt_ptr, handle, index],
            )),
            Type::Bool => Ok(self.call_runtime_value(
                self.runtime.object_get_bool,
                &[self.rt_ptr, handle, index],
            )),
            Type::U8 => Ok(self.call_runtime_value(
                self.runtime.object_get_u8,
                &[self.rt_ptr, handle, index],
            )),
            Type::String | Type::Array(_) | Type::Book(_) => Ok(self.call_runtime_value(
                self.runtime.object_get_ref,
                &[self.rt_ptr, handle, index],
            )),
            Type::Void => Err(native_error("void is not a valid field type.")),
        }
    }

    pub(super) fn emit_object_set(
        &mut self,
        field_ty: &Type,
        handle: Value,
        index: Value,
        value: Value,
    ) -> Result<(), NativeError> {
        match field_ty {
            Type::I64 => self.call_runtime_void(
                self.runtime.object_set_i64,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::F64 => self.call_runtime_void(
                self.runtime.object_set_f64,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::Bool => self.call_runtime_void(
                self.runtime.object_set_bool,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::U8 => self.call_runtime_void(
                self.runtime.object_set_u8,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::String | Type::Array(_) | Type::Book(_) => self.call_runtime_void(
                self.runtime.object_set_ref,
                &[self.rt_ptr, handle, index, value],
            ),
            Type::Void => return Err(native_error("void is not a valid field type.")),
        }
        Ok(())
    }
}
