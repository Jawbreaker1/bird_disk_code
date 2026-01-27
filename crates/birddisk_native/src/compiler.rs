use crate::analysis::{elem_kind_for_type, elem_size_for_kind};
use crate::error::{native_error, NativeError};
use crate::program::{type_name, BookLayout, EnumInfo, FunctionSig, stdlib_signature};
use crate::rt::RuntimeFuncs;
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, MatchCase, Stmt, Type, UnaryOp};
use birddisk_core::runtime as abi;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{DataDescription, DataId, FuncId, Linkage, Module};
use std::collections::HashMap;

#[derive(Debug, Clone)]
struct VarInfo {
    var: Variable,
    ty: Type,
}

pub(crate) struct NativeCompiler<'a, 'b, M: Module> {
    pub(crate) builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut M,
    runtime: RuntimeFuncs,
    rt_ptr: Value,
    error_block: cranelift_codegen::ir::Block,
    error_targets: Vec<cranelift_codegen::ir::Block>,
    return_type: Type,
    root_base: Option<Value>,
    root_slots: HashMap<String, u32>,
    locals: HashMap<String, Type>,
    books: &'a HashMap<String, BookLayout>,
    enums: &'a HashMap<String, EnumInfo>,
    functions: &'a HashMap<String, FunctionSig>,
    func_ids: &'a HashMap<String, FuncId>,
    vars: HashMap<String, VarInfo>,
    string_data: &'a mut HashMap<String, DataId>,
    string_counter: &'a mut usize,
    next_var: u32,
}

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    pub(crate) fn new(
        builder: &'a mut FunctionBuilder<'b>,
        module: &'a mut M,
        runtime: RuntimeFuncs,
        rt_ptr: Value,
        error_block: cranelift_codegen::ir::Block,
        return_type: Type,
        locals: HashMap<String, Type>,
        root_slots: HashMap<String, u32>,
        books: &'a HashMap<String, BookLayout>,
        enums: &'a HashMap<String, EnumInfo>,
        functions: &'a HashMap<String, FunctionSig>,
        func_ids: &'a HashMap<String, FuncId>,
        string_data: &'a mut HashMap<String, DataId>,
        string_counter: &'a mut usize,
    ) -> Self {
        Self {
            builder,
            module,
            runtime,
            rt_ptr,
            error_block,
            error_targets: vec![error_block],
            return_type,
            root_base: None,
            root_slots,
            locals,
            books,
            enums,
            functions,
            func_ids,
            vars: HashMap::new(),
            string_data,
            string_counter,
            next_var: 0,
        }
    }

    pub(crate) fn emit_root_push(&mut self) {
        let slot_count = self.root_slots.len() as i64;
        if slot_count == 0 {
            return;
        }
        let slots = self.builder.ins().iconst(types::I64, slot_count);
        let base = self.call_runtime_value(self.runtime.root_push, &[self.rt_ptr, slots]);
        self.root_base = Some(base);
    }

    pub(crate) fn emit_trace_push(&mut self, trace_id: i64) {
        let id = self.builder.ins().iconst(types::I64, trace_id);
        self.call_runtime_void(self.runtime.trace_push, &[self.rt_ptr, id]);
    }

    pub(crate) fn emit_root_pop(&mut self) {
        let slot_count = self.root_slots.len() as i64;
        if slot_count == 0 {
            return;
        }
        let slots = self.builder.ins().iconst(types::I64, slot_count);
        self.call_runtime_void(self.runtime.root_pop, &[self.rt_ptr, slots]);
    }

    pub(crate) fn emit_trace_pop(&mut self) {
        self.call_runtime_void(self.runtime.trace_pop, &[self.rt_ptr]);
    }

    pub(crate) fn emit_root_pop_no_check(&mut self) {
        let slot_count = self.root_slots.len() as i64;
        if slot_count == 0 {
            return;
        }
        let slots = self.builder.ins().iconst(types::I64, slot_count);
        self.call_runtime_void_no_check(self.runtime.root_pop, &[self.rt_ptr, slots]);
    }

    pub(crate) fn emit_trace_pop_no_check(&mut self) {
        self.call_runtime_void_no_check(self.runtime.trace_pop, &[self.rt_ptr]);
    }

    pub(crate) fn bind_params(&mut self, function: &birddisk_core::ast::Function, entry: cranelift_codegen::ir::Block) -> Result<(), NativeError> {
        let params: Vec<Value> = self.builder.block_params(entry).to_vec();
        for (index, param) in function.params.iter().enumerate() {
            let value = params[index + 1];
            let var = self.new_var();
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
                let var = self.new_var();
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
                if matches!(self.return_type, Type::Void) {
                    return Err(native_error("void functions cannot yield a value."));
                }
                let return_type = self.return_type.clone();
                let value = self.emit_expr(expr, Some(&return_type))?;
                self.emit_root_pop();
                self.emit_trace_pop();
                self.builder.ins().return_(&[value]);
                Ok(true)
            }
            Stmt::Throw { expr, .. } => {
                let value = self.emit_expr(expr, Some(&Type::String))?;
                self.call_runtime_void_no_check(self.runtime.throw_error, &[self.rt_ptr, value]);
                let target = self
                    .error_targets
                    .last()
                    .copied()
                    .unwrap_or(self.error_block);
                self.builder.ins().jump(target, &[]);
                Ok(true)
            }
            Stmt::Try {
                try_body,
                catch_name,
                catch_body,
                ..
            } => {
                let catch_block = self.builder.create_block();
                let catch_body_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                let outer_error_block = self
                    .error_targets
                    .last()
                    .copied()
                    .unwrap_or(self.error_block);

                self.error_targets.push(catch_block);
                let try_returned = self.emit_block(try_body)?;
                self.error_targets.pop();

                if !try_returned {
                    self.builder.ins().jump(merge_block, &[]);
                }

                self.builder.switch_to_block(catch_block);
                let is_throw = self.call_runtime_value_no_check(
                    self.runtime.error_is_throw,
                    &[self.rt_ptr],
                );
                let cond = self.builder.ins().icmp_imm(IntCC::NotEqual, is_throw, 0);
                self.builder
                    .ins()
                    .brif(cond, catch_body_block, &[], outer_error_block, &[]);
                self.builder.seal_block(catch_block);

                self.builder.switch_to_block(catch_body_block);
                let msg = self.call_runtime_value_no_check(
                    self.runtime.error_message,
                    &[self.rt_ptr],
                );
                self.call_runtime_void_no_check(self.runtime.clear_error, &[self.rt_ptr]);
                self.bind_or_assign_local(catch_name, Type::String, msg)?;
                let catch_returned = self.emit_block(catch_body)?;
                if !catch_returned {
                    self.builder.ins().jump(merge_block, &[]);
                }
                self.builder.seal_block(catch_body_block);

                if try_returned && catch_returned {
                    return Ok(true);
                }
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(false)
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                let cond_val = self.emit_expr(cond, Some(&Type::Bool))?;
                let cond = self.i64_to_bool(cond_val);
                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();
                self.builder
                    .ins()
                    .brif(cond, then_block, &[], else_block, &[]);

                self.builder.switch_to_block(then_block);
                let then_returned = self.emit_block(then_body)?;
                if !then_returned {
                    self.builder.ins().jump(merge_block, &[]);
                }
                self.builder.seal_block(then_block);

                self.builder.switch_to_block(else_block);
                let else_returned = self.emit_block(else_body)?;
                if !else_returned {
                    self.builder.ins().jump(merge_block, &[]);
                }
                self.builder.seal_block(else_block);

                if then_returned && else_returned {
                    return Ok(true);
                }

                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);
                Ok(false)
            }
            Stmt::Repeat { cond, body, .. } => {
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();
                self.builder.ins().jump(header_block, &[]);

                self.builder.switch_to_block(header_block);
                let cond_val = self.emit_expr(cond, Some(&Type::Bool))?;
                let cond = self.i64_to_bool(cond_val);
                self.builder
                    .ins()
                    .brif(cond, body_block, &[], exit_block, &[]);

                self.builder.switch_to_block(body_block);
                let body_returned = self.emit_block(body)?;
                if !body_returned {
                    self.builder.ins().jump(header_block, &[]);
                }
                self.builder.seal_block(body_block);
                self.builder.seal_block(header_block);

                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(exit_block);
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

    fn emit_block(&mut self, stmts: &[Stmt]) -> Result<bool, NativeError> {
        for stmt in stmts {
            if self.emit_stmt(stmt)? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn emit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<Value, NativeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(self.builder.ins().iconst(types::I64, *value)),
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
            ExprKind::Call { name, args } => {
                let value = self.emit_call(name, args, expected)?;
                value.ok_or_else(|| native_error("void call cannot be used as expression."))
            }
            ExprKind::New { book, args } => self.emit_new(book, args),
            ExprKind::MemberAccess { base, field } => self.emit_member_access(base, field),
            ExprKind::Unary { op, expr } => {
                let value = self.emit_expr(expr, None)?;
                match op {
                    UnaryOp::Neg => Ok(self.builder.ins().ineg(value)),
                    UnaryOp::Not => {
                        let cond = self.builder.ins().icmp_imm(IntCC::Equal, value, 0);
                        Ok(self.bool_to_i64(cond))
                    }
                }
            }
            ExprKind::Binary { left, op, right } => {
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        let left = self.emit_expr(left, None)?;
                        let right = self.emit_expr(right, None)?;
                        let value = match op {
                            BinaryOp::Add => self.builder.ins().iadd(left, right),
                            BinaryOp::Sub => self.builder.ins().isub(left, right),
                            BinaryOp::Mul => self.builder.ins().imul(left, right),
                            BinaryOp::Div => self.builder.ins().sdiv(left, right),
                            BinaryOp::Mod => self.builder.ins().srem(left, right),
                            _ => unreachable!(),
                        };
                        Ok(value)
                    }
                    BinaryOp::EqEq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq => {
                        let left = self.emit_expr(left, None)?;
                        let right = self.emit_expr(right, None)?;
                        let cond = match op {
                            BinaryOp::EqEq => self.builder.ins().icmp(IntCC::Equal, left, right),
                            BinaryOp::NotEq => {
                                self.builder.ins().icmp(IntCC::NotEqual, left, right)
                            }
                            BinaryOp::Lt => self.builder.ins().icmp(IntCC::SignedLessThan, left, right),
                            BinaryOp::LtEq => {
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedLessThanOrEqual, left, right)
                            }
                            BinaryOp::Gt => self.builder.ins().icmp(IntCC::SignedGreaterThan, left, right),
                            BinaryOp::GtEq => {
                                self.builder
                                    .ins()
                                    .icmp(IntCC::SignedGreaterThanOrEqual, left, right)
                            }
                            _ => unreachable!(),
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

    fn emit_match(
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

    fn emit_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        if name.starts_with("std::") {
            return self.emit_std_call(name, args, expected);
        }
        if let Some(sig) = self.functions.get(name) {
            if sig.params.len() != args.len() {
                return Err(native_error(format!(
                    "wrong number of arguments for '{name}': expected {}, got {}.",
                    sig.params.len(),
                    args.len()
                )));
            }
            if let Some(expected) = expected {
                if &sig.return_type != expected {
                    return Err(native_error(format!(
                        "type mismatch: expected {}, got {}.",
                        type_name(expected),
                        type_name(&sig.return_type)
                    )));
                }
            }
            let func_id = self
                .func_ids
                .get(name)
                .copied()
                .ok_or_else(|| native_error(format!("missing function id for '{name}'.")))?;
            let func_ref = self
                .module
                .declare_func_in_func(func_id, self.builder.func);
            let mut call_args = Vec::with_capacity(args.len() + 1);
            call_args.push(self.rt_ptr);
            for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                let value = self.emit_expr(arg, Some(param_ty))?;
                call_args.push(value);
            }
            let call = self.builder.ins().call(func_ref, &call_args);
            self.emit_error_check();
            if matches!(sig.return_type, Type::Void) {
                return Ok(None);
            }
            return Ok(Some(self.builder.inst_results(call)[0]));
        }
        if let Some((base, method)) = name.split_once("::") {
            if let Some(Type::Book(book_name)) = self.lookup_local_type(base) {
                return self.emit_method_call(base, &book_name, method, args, expected);
            }
        }
        if let Some(value) = self.emit_enum_constructor(name, args, expected)? {
            return Ok(Some(value));
        }
        Err(native_error(format!("unknown function '{name}'.")))
    }

    fn emit_method_call(
        &mut self,
        base: &str,
        book: &str,
        method: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        let full_name = format!("{book}::{method}");
        let sig = self
            .functions
            .get(&full_name)
            .ok_or_else(|| native_error(format!("unknown method '{full_name}'.")))?;
        if sig.params.is_empty() {
            return Err(native_error(format!(
                "method '{full_name}' must take self."
            )));
        }
        let expected_args = sig.params.len().saturating_sub(1);
        if expected_args != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{full_name}': expected {}, got {}.",
                expected_args,
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
        let func_id = self
            .func_ids
            .get(&full_name)
            .copied()
            .ok_or_else(|| native_error(format!("missing function id for '{full_name}'.")))?;
        let func_ref = self
            .module
            .declare_func_in_func(func_id, self.builder.func);
        let base_info = self
            .vars
            .get(base)
            .cloned()
            .ok_or_else(|| native_error(format!("unknown name '{base}'.")))?;
        let mut call_args = Vec::with_capacity(args.len() + 2);
        call_args.push(self.rt_ptr);
        call_args.push(self.builder.use_var(base_info.var));
        for (arg, param_ty) in args.iter().zip(sig.params.iter().skip(1)) {
            call_args.push(self.emit_expr(arg, Some(param_ty))?);
        }
        let call = self.builder.ins().call(func_ref, &call_args);
        self.emit_error_check();
        if matches!(sig.return_type, Type::Void) {
            Ok(None)
        } else {
            Ok(Some(self.builder.inst_results(call)[0]))
        }
    }

    fn emit_std_call(
        &mut self,
        name: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<Option<Value>, NativeError> {
        let sig = stdlib_signature(name)
            .ok_or_else(|| native_error(format!("unknown function '{name}'.")))?;
        if sig.params.len() != args.len() {
            return Err(native_error(format!(
                "wrong number of arguments for '{name}': expected {}, got {}.",
                sig.params.len(),
                args.len()
            )));
        }
        if let Some(expected) = expected {
            if &sig.return_type != expected {
                return Err(native_error(format!(
                    "type mismatch: expected {}, got {}.",
                    type_name(expected),
                    type_name(&sig.return_type)
                )));
            }
        }
        let mut arg_vals = Vec::with_capacity(args.len());
        for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
            arg_vals.push(self.emit_expr(arg, Some(param_ty))?);
        }
        let value = match name {
            "std::string::len" => Some(self.call_runtime_value(
                self.runtime.string_len,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::concat" => Some(self.call_runtime_value(
                self.runtime.string_concat,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::eq" => Some(self.call_runtime_value(
                self.runtime.string_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::string::bytes" => Some(self.call_runtime_value(
                self.runtime.string_bytes,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::from_bytes" => Some(self.call_runtime_value(
                self.runtime.string_from_bytes,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::to_i64" => Some(self.call_runtime_value(
                self.runtime.string_to_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::string::from_i64" => Some(self.call_runtime_value(
                self.runtime.string_from_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::bytes::len" => Some(self.call_runtime_value(
                self.runtime.bytes_len,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::bytes::eq" => Some(self.call_runtime_value(
                self.runtime.bytes_eq,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::io::print" => {
                self.call_runtime_void(self.runtime.io_print, &[self.rt_ptr, arg_vals[0]]);
                None
            }
            "std::io::read_line" => Some(self.call_runtime_value(
                self.runtime.io_read_line,
                &[self.rt_ptr],
            )),
            "std::time::now_ms" => Some(self.call_runtime_value(
                self.runtime.time_now_ms,
                &[self.rt_ptr],
            )),
            "std::time::sleep_ms" => Some(self.call_runtime_value(
                self.runtime.time_sleep_ms,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::fs::read_text" => Some(self.call_runtime_value(
                self.runtime.fs_read_text,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::fs::write_text" => Some(self.call_runtime_value(
                self.runtime.fs_write_text,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::fs::read_bytes" => Some(self.call_runtime_value(
                self.runtime.fs_read_bytes,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::fs::write_bytes" => Some(self.call_runtime_value(
                self.runtime.fs_write_bytes,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::path::join" => Some(self.call_runtime_value(
                self.runtime.path_join,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::path::normalize" => Some(self.call_runtime_value(
                self.runtime.path_normalize,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::path::basename" => Some(self.call_runtime_value(
                self.runtime.path_basename,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::path::dirname" => Some(self.call_runtime_value(
                self.runtime.path_dirname,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::env::args" => Some(self.call_runtime_value(
                self.runtime.env_args,
                &[self.rt_ptr],
            )),
            "std::env::get" => Some(self.call_runtime_value(
                self.runtime.env_get,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::env::set_var" => Some(self.call_runtime_value(
                self.runtime.env_set,
                &[self.rt_ptr, arg_vals[0], arg_vals[1]],
            )),
            "std::env::cwd" => Some(self.call_runtime_value(
                self.runtime.env_cwd,
                &[self.rt_ptr],
            )),
            "std::env::set_cwd" => Some(self.call_runtime_value(
                self.runtime.env_set_cwd,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::encode_i64" => Some(self.call_runtime_value(
                self.runtime.json_encode_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::encode_bool" => Some(self.call_runtime_value(
                self.runtime.json_encode_bool,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::encode_string" => Some(self.call_runtime_value(
                self.runtime.json_encode_string,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::decode_i64" => Some(self.call_runtime_value(
                self.runtime.json_decode_i64,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::decode_bool" => Some(self.call_runtime_value(
                self.runtime.json_decode_bool,
                &[self.rt_ptr, arg_vals[0]],
            )),
            "std::json::decode_string" => Some(self.call_runtime_value(
                self.runtime.json_decode_string,
                &[self.rt_ptr, arg_vals[0]],
            )),
            _ => return Err(native_error(format!("unknown function '{name}'."))),
        };
        Ok(value)
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
            let idx_var = self.new_var();
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

    fn emit_array_set(
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

    fn emit_enum_constructor(
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
        let variant = enum_info.variants.get(variant_name).ok_or_else(|| {
            native_error(format!("unknown enum variant '{name}'."))
        })?;
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

    fn emit_object_set(
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

    fn update_root(&mut self, name: &str, value: Value) {
        let Some(slot) = self.root_slots.get(name) else {
            return;
        };
        let base = match self.root_base {
            Some(base) => base,
            None => return,
        };
        let slot_val = self.builder.ins().iconst(types::I64, *slot as i64);
        let absolute = self.builder.ins().iadd(base, slot_val);
        self.call_runtime_void(
            self.runtime.root_set,
            &[self.rt_ptr, absolute, value],
        );
    }

    pub(crate) fn emit_error_block(&mut self) {
        self.builder.switch_to_block(self.error_block);
        self.emit_root_pop_no_check();
        self.emit_trace_pop_no_check();
        if matches!(self.return_type, Type::Void) {
            self.builder.ins().return_(&[]);
        } else {
            let zero = self.builder.ins().iconst(types::I64, 0);
            self.builder.ins().return_(&[zero]);
        }
        self.builder.seal_block(self.error_block);
    }

    fn emit_error_check(&mut self) {
        let func_ref = self
            .module
            .declare_func_in_func(self.runtime.has_error, self.builder.func);
        let call = self.builder.ins().call(func_ref, &[self.rt_ptr]);
        let flag = self.builder.inst_results(call)[0];
        let cond = self.builder.ins().icmp_imm(IntCC::NotEqual, flag, 0);
        let ok_block = self.builder.create_block();
        let target = self
            .error_targets
            .last()
            .copied()
            .unwrap_or(self.error_block);
        self.builder
            .ins()
            .brif(cond, target, &[], ok_block, &[]);
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
    }

    fn call_runtime_value(&mut self, func_id: FuncId, args: &[Value]) -> Value {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        let result = self.builder.inst_results(call)[0];
        self.emit_error_check();
        result
    }

    fn call_runtime_void(&mut self, func_id: FuncId, args: &[Value]) {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
        self.emit_error_check();
    }

    fn call_runtime_value_no_check(&mut self, func_id: FuncId, args: &[Value]) -> Value {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        self.builder.inst_results(call)[0]
    }

    fn call_runtime_void_no_check(&mut self, func_id: FuncId, args: &[Value]) {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
    }

    fn lookup_local_type(&self, name: &str) -> Option<Type> {
        self.vars
            .get(name)
            .map(|info| info.ty.clone())
            .or_else(|| self.locals.get(name).cloned())
    }

    fn bind_or_assign_local(
        &mut self,
        name: &str,
        ty: Type,
        value: Value,
    ) -> Result<(), NativeError> {
        if let Some(info) = self.vars.get(name).cloned() {
            if info.ty != ty {
                return Err(native_error(format!(
                    "type mismatch for '{name}': expected {}, got {}.",
                    type_name(&info.ty),
                    type_name(&ty)
                )));
            }
            self.builder.def_var(info.var, value);
            self.update_root(name, value);
            return Ok(());
        }
        if let Some(expected) = self.locals.get(name) {
            if expected != &ty {
                return Err(native_error(format!(
                    "type mismatch for '{name}': expected {}, got {}.",
                    type_name(expected),
                    type_name(&ty)
                )));
            }
        }
        let var = self.new_var();
        self.builder.def_var(var, value);
        self.vars.insert(
            name.to_string(),
            VarInfo {
                var,
                ty: ty.clone(),
            },
        );
        self.update_root(name, value);
        Ok(())
    }

    fn infer_expr_type(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Int(_) => Some(Type::I64),
            ExprKind::Bool(_) => Some(Type::Bool),
            ExprKind::String(_) => Some(Type::String),
            ExprKind::Ident(name) => self
                .vars
                .get(name)
                .map(|info| info.ty.clone())
                .or_else(|| self.locals.get(name).cloned()),
            ExprKind::Call { name, .. } => {
                if let Some(return_type) = stdlib_signature(name)
                    .map(|sig| sig.return_type)
                    .or_else(|| self.functions.get(name).map(|sig| sig.return_type.clone()))
                {
                    return Some(return_type);
                }
                if let Some((enum_name, variant_name)) = name.split_once("::") {
                    if enum_name != "std"
                        && !variant_name.contains("::")
                        && self.lookup_local_type(enum_name).is_none()
                    {
                        if let Some(enum_info) = self.enums.get(enum_name) {
                            if enum_info.variants.contains_key(variant_name) {
                                return Some(Type::Book(enum_name.to_string()));
                            }
                            return None;
                        }
                    }
                }
                if let Some((base, method)) = name.split_once("::") {
                    if base != "std" {
                        if let Some(Type::Book(book)) = self.lookup_local_type(base) {
                            let full_name = format!("{book}::{method}");
                            return self
                                .functions
                                .get(&full_name)
                                .map(|sig| sig.return_type.clone());
                        }
                    }
                }
                None
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => self.infer_expr_type(expr),
                UnaryOp::Not => Some(Type::Bool),
            },
            ExprKind::Binary { op, .. } => match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod => Some(Type::I64),
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::AndAnd
                | BinaryOp::OrOr => Some(Type::Bool),
            },
            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    None
                } else {
                    let first = self.infer_expr_type(&elements[0])?;
                    for elem in &elements[1..] {
                        let ty = self.infer_expr_type(elem)?;
                        if ty != first {
                            return None;
                        }
                    }
                    Some(Type::Array(Box::new(first)))
                }
            }
            ExprKind::Index { base, .. } => {
                let base_ty = self.infer_expr_type(base)?;
                match base_ty {
                    Type::Array(inner) => Some(*inner),
                    _ => None,
                }
            }
            ExprKind::New { book, .. } => Some(Type::Book(book.clone())),
            ExprKind::MemberAccess { base, field } => {
                let Type::Book(book) = self.lookup_local_type(base)? else {
                    return None;
                };
                let layout = self.books.get(&book)?;
                let index = layout.field_index.get(field)?;
                layout.fields.get(*index).cloned()
            }
            _ => None,
        }
    }

    fn new_var(&mut self) -> Variable {
        let var = Variable::from_u32(self.next_var);
        self.next_var += 1;
        self.builder.declare_var(var, types::I64);
        var
    }
}
