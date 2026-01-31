mod call;
mod expr;
mod helpers;
mod stmt;
mod types;

use crate::program::{BookLayout, EnumInfo, FunctionSig};
use crate::rt::RuntimeFuncs;
use birddisk_core::ast::Type;
use cranelift_codegen::ir::{types as clif_types, Block, InstBuilder, Value};
use cranelift_frontend::{FunctionBuilder, Variable};
use cranelift_module::{DataId, FuncId, Module};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(super) struct VarInfo {
    pub(super) var: Variable,
    pub(super) ty: Type,
}

pub(crate) struct NativeCompiler<'a, 'b, M: Module> {
    pub(crate) builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut M,
    runtime: RuntimeFuncs,
    rt_ptr: Value,
    error_block: Block,
    error_targets: Vec<Block>,
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
        error_block: Block,
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
        let slots = self.builder.ins().iconst(clif_types::I64, slot_count);
        let base = self.call_runtime_value(self.runtime.root_push, &[self.rt_ptr, slots]);
        self.root_base = Some(base);
    }

    pub(crate) fn emit_trace_push(&mut self, trace_id: i64) {
        let id = self.builder.ins().iconst(clif_types::I64, trace_id);
        self.call_runtime_void(self.runtime.trace_push, &[self.rt_ptr, id]);
    }

    pub(crate) fn emit_root_pop(&mut self) {
        let slot_count = self.root_slots.len() as i64;
        if slot_count == 0 {
            return;
        }
        let slots = self.builder.ins().iconst(clif_types::I64, slot_count);
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
        let slots = self.builder.ins().iconst(clif_types::I64, slot_count);
        self.call_runtime_void_no_check(self.runtime.root_pop, &[self.rt_ptr, slots]);
    }

    pub(crate) fn emit_trace_pop_no_check(&mut self) {
        self.call_runtime_void_no_check(self.runtime.trace_pop, &[self.rt_ptr]);
    }
}

fn clif_type(ty: &Type) -> cranelift_codegen::ir::types::Type {
    match ty {
        Type::F64 => cranelift_codegen::ir::types::F64,
        _ => cranelift_codegen::ir::types::I64,
    }
}
