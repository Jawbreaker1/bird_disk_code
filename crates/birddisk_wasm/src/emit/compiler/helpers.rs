use super::FuncCompiler;
use super::super::{
    WasmError, ARRAY_HEADER_SIZE, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET, HEAP_KIND_ARRAY,
    HEAP_KIND_OBJECT, HEAP_KIND_SHIFT, HEAP_KIND_STRING, HEAP_LEN_OFFSET, OBJECT_FIELD_SIZE,
    OBJECT_HEADER_SIZE, STRING_HEADER_SIZE, TRAP_ARRAY_LEN_NEG, TRAP_ARRAY_OOB, TRAP_ARRAY_OOM,
    TRAP_KIND_ARRAY, TRAP_KIND_OBJECT, TRAP_NULL_DEREF,
};
use birddisk_core::ast::Type;
use super::super::types::array_elem_kind;

impl<'a> FuncCompiler<'a> {
    pub(super) fn emit_len_non_negative_check(&mut self, len_local: u32) -> Result<(), WasmError> {
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i64.const 0");
        self.push_line("i64.lt_s");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_LEN_NEG);
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    pub(super) fn emit_len_max_check(&mut self, len_local: u32, elem_size: i32) -> Result<(), WasmError> {
        let max_len = (i32::MAX as i64 - ARRAY_HEADER_SIZE as i64) / elem_size as i64;
        self.push_line(format!("local.get {len_local}"));
        self.push_line(format!("i64.const {max_len}"));
        self.push_line("i64.gt_u");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_OOM);
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    pub(super) fn emit_bounds_check(&mut self, base_local: u32, idx_local: u32) -> Result<(), WasmError> {
        self.emit_null_check(base_local);
        self.emit_kind_check(base_local, HEAP_KIND_ARRAY, TRAP_KIND_ARRAY);
        let len_local = self.temp_local(Type::I64);
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
        self.push_line("i64.extend_i32_u");
        self.push_line(format!("local.set {len_local}"));

        self.push_line(format!("local.get {idx_local}"));
        self.push_line("i64.const 0");
        self.push_line("i64.lt_s");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_OOB);
        self.indent -= 1;
        self.push_line("end");

        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i64.ge_u");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_ARRAY_OOB);
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    pub(super) fn emit_array_address_const(&mut self, base_local: u32, index: i64, elem_size: i32) {
        let offset = (index * elem_size as i64) as i32;
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("i32.const {offset}"));
        self.push_line("i32.add");
    }

    pub(super) fn emit_array_address_index(&mut self, base_local: u32, idx_local: u32, elem_size: i32) {
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("i64.const {elem_size}"));
        self.push_line("i64.mul");
        self.push_line("i32.wrap_i64");
        self.push_line("i32.add");
    }

    pub(super) fn emit_field_address(&mut self, base_local: u32, index: usize) {
        self.emit_null_check(base_local);
        self.emit_kind_check(base_local, HEAP_KIND_OBJECT, TRAP_KIND_OBJECT);
        let offset = (index as i32).saturating_mul(OBJECT_FIELD_SIZE);
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {OBJECT_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("i32.const {offset}"));
        self.push_line("i32.add");
    }

    pub(super) fn emit_array_init(
        &mut self,
        base_local: u32,
        len_local: u32,
        elem_ty: &Type,
        elem_size: i32,
    ) -> Result<(), WasmError> {
        let idx_local = self.temp_local(Type::I64);
        self.push_line(format!("i64.const 0"));
        self.push_line(format!("local.set {idx_local}"));
        let exit_label = self.fresh_label("arr_init_exit");
        let loop_label = self.fresh_label("arr_init_loop");
        self.push_line(format!("block ${exit_label}"));
        self.indent += 1;
        self.push_line(format!("loop ${loop_label}"));
        self.indent += 1;
        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i64.ge_u");
        self.push_line(format!("br_if ${exit_label}"));
        self.emit_array_address_index(base_local, idx_local, elem_size);
        self.emit_default_value(elem_ty)?;
        self.emit_store(elem_ty);
        self.push_line(format!("local.get {idx_local}"));
        self.push_line("i64.const 1");
        self.push_line("i64.add");
        self.push_line(format!("local.set {idx_local}"));
        self.push_line(format!("br ${loop_label}"));
        self.indent -= 1;
        self.push_line("end");
        self.indent -= 1;
        self.push_line("end");
        Ok(())
    }

    pub(super) fn emit_store(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.store"),
            Type::U8 => self.push_line("i32.store8"),
            Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => self.push_line("i32.store"),
        }
    }

    pub(super) fn emit_field_store(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.store"),
            Type::U8 | Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => {
                self.push_line("i64.extend_i32_u");
                self.push_line("i64.store");
            }
        }
    }

    pub(super) fn emit_load(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.load"),
            Type::U8 => self.push_line("i32.load8_u"),
            Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => self.push_line("i32.load"),
        }
    }

    pub(super) fn emit_field_load(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.load"),
            Type::U8 | Type::Bool | Type::String | Type::Array(_) | Type::Book(_) => {
                self.push_line("i64.load");
                self.push_line("i32.wrap_i64");
            }
        }
    }

    pub(super) fn emit_trap(&mut self, code: i32) {
        self.push_line(format!("i32.const {code}"));
        self.push_line("call $bd_trap");
    }

    pub(super) fn emit_null_check(&mut self, base_local: u32) {
        self.push_line(format!("local.get {base_local}"));
        self.push_line("i32.eqz");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(TRAP_NULL_DEREF);
        self.indent -= 1;
        self.push_line("end");
    }

    pub(super) fn emit_kind_check(&mut self, base_local: u32, expected_kind: i32, trap: i32) {
        self.push_line(format!("local.get {base_local}"));
        self.push_line("i32.load");
        self.push_line(format!("i32.const {HEAP_KIND_SHIFT}"));
        self.push_line("i32.shr_u");
        self.push_line(format!("i32.const {expected_kind}"));
        self.push_line("i32.ne");
        self.push_line("if");
        self.indent += 1;
        self.emit_trap(trap);
        self.indent -= 1;
        self.push_line("end");
    }

    pub(super) fn emit_default_value(&mut self, ty: &Type) -> Result<(), WasmError> {
        match ty {
            Type::I64 => self.push_line("i64.const 0"),
            Type::Bool => self.push_line("i32.const 0"),
            Type::String => {
                self.emit_empty_string()?;
            }
            Type::U8 => {
                self.push_line("i32.const 0");
            }
            Type::Array(elem) => {
                self.emit_empty_array(elem.as_ref())?;
            }
            Type::Book(_) => {
                self.push_line("i32.const 0");
            }
        }
        Ok(())
    }

    pub(super) fn emit_empty_array(&mut self, elem_ty: &Type) -> Result<(), WasmError> {
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("call $bd_alloc");
        self.emit_local_set(ptr_local, &Type::Array(Box::new(elem_ty.clone())));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", array_elem_kind(elem_ty)));
        self.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    pub(super) fn emit_empty_string(&mut self) -> Result<(), WasmError> {
        let ptr_local = self.temp_local(Type::String);
        self.push_line(format!("i32.const {STRING_HEADER_SIZE}"));
        self.push_line("call $bd_alloc");
        self.emit_local_set(ptr_local, &Type::String);
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", HEAP_KIND_STRING << HEAP_KIND_SHIFT));
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }
}
