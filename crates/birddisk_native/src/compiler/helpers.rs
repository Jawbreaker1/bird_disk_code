use super::NativeCompiler;
use birddisk_core::ast::Type;
use cranelift_codegen::ir::condcodes::IntCC;
use cranelift_codegen::ir::immediates::Ieee64;
use cranelift_codegen::ir::{types, InstBuilder, Value};
use cranelift_module::{FuncId, Module};

impl<'a, 'b, M: Module> NativeCompiler<'a, 'b, M> {
    pub(super) fn update_root(&mut self, name: &str, value: Value) {
        let Some(slot) = self.root_slots.get(name) else {
            return;
        };
        let base = match self.root_base {
            Some(base) => base,
            None => return,
        };
        let slot_val = self.builder.ins().iconst(types::I64, *slot as i64);
        let absolute = self.builder.ins().iadd(base, slot_val);
        self.call_runtime_void(self.runtime.root_set, &[self.rt_ptr, absolute, value]);
    }

    pub(crate) fn emit_error_block(&mut self) {
        self.builder.switch_to_block(self.error_block);
        self.emit_root_pop_no_check();
        self.emit_trace_pop_no_check();
        if matches!(self.return_type, Type::Void) {
            self.builder.ins().return_(&[]);
        } else {
            let zero = match self.return_type {
                Type::F64 => self.builder.ins().f64const(Ieee64::with_float(0.0)),
                _ => self.builder.ins().iconst(types::I64, 0),
            };
            self.builder.ins().return_(&[zero]);
        }
        self.builder.seal_block(self.error_block);
    }

    pub(super) fn emit_error_check(&mut self) {
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
        self.builder.ins().brif(cond, target, &[], ok_block, &[]);
        self.builder.switch_to_block(ok_block);
        self.builder.seal_block(ok_block);
    }

    pub(super) fn call_runtime_value(&mut self, func_id: FuncId, args: &[Value]) -> Value {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        let result = self.builder.inst_results(call)[0];
        self.emit_error_check();
        result
    }

    pub(super) fn call_runtime_void(&mut self, func_id: FuncId, args: &[Value]) {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
        self.emit_error_check();
    }

    pub(super) fn call_runtime_value_no_check(&mut self, func_id: FuncId, args: &[Value]) -> Value {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(func_ref, args);
        self.builder.inst_results(call)[0]
    }

    pub(super) fn call_runtime_void_no_check(&mut self, func_id: FuncId, args: &[Value]) {
        let func_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        self.builder.ins().call(func_ref, args);
    }
}
