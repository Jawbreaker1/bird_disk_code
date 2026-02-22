use super::super::types::array_elem_kind;
use super::super::types::array_elem_size;
use super::super::{
    wasm_error, WasmError, ARRAY_HEADER_SIZE, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET, HEAP_HEADER_SIZE,
    HEAP_KIND_ARRAY, HEAP_KIND_ENUM, HEAP_KIND_OBJECT, HEAP_KIND_SHIFT, HEAP_KIND_STRING,
    HEAP_LEN_OFFSET, OBJECT_FIELD_SIZE, OBJECT_HEADER_SIZE, STRING_HEADER_SIZE, TRAP_KIND_OBJECT,
};
use super::FuncCompiler;
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Type, UnaryOp};

impl<'a> FuncCompiler<'a> {
    pub(super) fn emit_expr(
        &mut self,
        expr: &Expr,
        expected: Option<&Type>,
    ) -> Result<(), WasmError> {
        match &expr.kind {
            ExprKind::Int(value) => {
                if matches!(expected, Some(Type::U8)) {
                    if !(0..=u8::MAX as i64).contains(value) {
                        return Err(wasm_error("E0400", "u8 literal out of range."));
                    }
                    self.push_line(format!("i32.const {value}"));
                } else {
                    self.push_line(format!("i64.const {value}"));
                }
            }
            ExprKind::Float(value) => {
                self.push_line(format!("f64.const {}", format_f64(*value)));
            }
            ExprKind::Bool(value) => {
                let bit = if *value { 1 } else { 0 };
                self.push_line(format!("i32.const {bit}"));
            }
            ExprKind::String(value) => {
                self.emit_string_literal(value)?;
            }
            ExprKind::Ident(name) => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.push_line(format!("local.get {}", info.idx));
            }
            ExprKind::Call { name, args } => {
                if expected.is_some() {
                    let inferred = self.infer_expr_type(expr)?;
                    if matches!(inferred, Type::Void) {
                        return Err(wasm_error(
                            "E0400",
                            "Void call cannot be used as an expression.",
                        ));
                    }
                }
                if self.emit_string_call(name, args)? {
                    return Ok(());
                }
                if self.emit_json_call(name, args)? {
                    return Ok(());
                }
                if self.emit_bytes_call(name, args)? {
                    return Ok(());
                }
                if self.emit_io_call(name, args)? {
                    return Ok(());
                }
                if self.emit_time_call(name, args)? {
                    return Ok(());
                }
                if self.emit_profiler_call(name, args)? {
                    return Ok(());
                }
                if self.emit_rand_call(name, args)? {
                    return Ok(());
                }
                if self.emit_test_call(name, args)? {
                    return Ok(());
                }
                if self.emit_fs_call(name, args)? {
                    return Ok(());
                }
                if self.emit_env_call(name, args)? {
                    return Ok(());
                }
                if self.emit_path_call(name, args)? {
                    return Ok(());
                }
                if self.emit_channel_call(name, args)? {
                    return Ok(());
                }
                if self.emit_enum_constructor(name, args)? {
                    return Ok(());
                }
                if let Some(sig) = self.functions.get(name) {
                    if sig.params.len() != args.len() {
                        return Err(wasm_error(
                            "E0400",
                            format!(
                                "Wrong number of arguments for '{name}': expected {}, got {}.",
                                sig.params.len(),
                                args.len()
                            ),
                        ));
                    }
                    let arg_locals = self.emit_call_args(args, &sig.params)?;
                    for local in arg_locals {
                        self.push_line(format!("local.get {local}"));
                    }
                    self.push_line(format!("call ${name}"));
                    if matches!(sig.return_type, Type::Void) {
                        self.emit_error_check();
                        return Ok(());
                    }
                    let ret_local = self.temp_local(sig.return_type.clone());
                    self.emit_local_set(ret_local, &sig.return_type);
                    self.emit_error_check();
                    self.push_line(format!("local.get {ret_local}"));
                    return Ok(());
                }
                if let Some((base, method)) = name.split_once("::") {
                    if base != "std" {
                        if let Some(info) = self.lookup(base) {
                            if let Type::Book(book_name) = &info.ty {
                                if self.emit_channel_method_call(
                                    book_name, info.idx, method, args, expected,
                                )? {
                                    return Ok(());
                                }
                                let full_name = format!("{book_name}::{method}");
                                let sig = self.functions.get(&full_name).ok_or_else(|| {
                                    wasm_error("E0400", format!("Unknown function '{full_name}'"))
                                })?;
                                if sig.params.len() < 1 {
                                    return Err(wasm_error(
                                        "E0400",
                                        format!("Method '{full_name}' must take self."),
                                    ));
                                }
                                if sig.params.len() - 1 != args.len() {
                                    return Err(wasm_error(
                                        "E0400",
                                        format!(
                                            "Wrong number of arguments for '{full_name}': expected {}, got {}.",
                                            sig.params.len() - 1,
                                            args.len()
                                        ),
                                    ));
                                }
                                self.emit_null_check(info.idx);
                                self.emit_kind_check(info.idx, HEAP_KIND_OBJECT, TRAP_KIND_OBJECT);
                                let arg_locals = self.emit_call_args(args, &sig.params[1..])?;
                                self.push_line(format!("local.get {}", info.idx));
                                for local in arg_locals {
                                    self.push_line(format!("local.get {local}"));
                                }
                                self.push_line(format!("call ${full_name}"));
                                if matches!(sig.return_type, Type::Void) {
                                    self.emit_error_check();
                                    return Ok(());
                                }
                                let ret_local = self.temp_local(sig.return_type.clone());
                                self.emit_local_set(ret_local, &sig.return_type);
                                self.emit_error_check();
                                self.push_line(format!("local.get {ret_local}"));
                                return Ok(());
                            }
                        }
                    }
                }
                return Err(wasm_error("E0400", format!("Unknown function '{name}'")));
            }
            ExprKind::New { book, args } => {
                let layout = self
                    .books
                    .get(book)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown book '{book}'")))?;
                let ptr_local = self.temp_local(Type::Book(book.clone()));
                let size = OBJECT_HEADER_SIZE
                    + (layout.fields.len() as i32).saturating_mul(OBJECT_FIELD_SIZE);
                self.push_line(format!("i32.const {size}"));
                self.push_line("call $bd_alloc");
                self.emit_local_set(ptr_local, &Type::Book(book.clone()));
                let tag = (HEAP_KIND_OBJECT << HEAP_KIND_SHIFT) | layout.id as i32;
                self.push_line(format!("local.get {ptr_local}"));
                self.push_line(format!("i32.const {tag}"));
                self.push_line("i32.store");
                self.push_line(format!("local.get {ptr_local}"));
                self.push_line("i32.const 0");
                self.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
                self.push_line(format!("local.get {ptr_local}"));
                self.push_line(format!("i32.const {}", layout.fields.len()));
                self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
                self.push_line(format!("local.get {ptr_local}"));
                self.push_line("i32.const 0");
                self.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
                for (index, _field_ty) in layout.fields.iter().enumerate() {
                    self.emit_field_address(ptr_local, index);
                    self.push_line("i64.const 0");
                    self.push_line("i64.store");
                }
                for (index, field_ty) in layout.fields.iter().enumerate() {
                    self.emit_field_address(ptr_local, index);
                    self.emit_default_value(field_ty)?;
                    self.emit_field_store(field_ty);
                }
                if let Some(sig) = self.functions.get(&format!("{book}::init")) {
                    if sig.params.len() < 1 {
                        return Err(wasm_error(
                            "E0400",
                            format!("Method '{book}::init' must take self."),
                        ));
                    }
                    if sig.params.len() - 1 != args.len() {
                        return Err(wasm_error(
                            "E0400",
                            format!(
                                "Wrong number of arguments for '{book}::init': expected {}, got {}.",
                                sig.params.len() - 1,
                                args.len()
                            ),
                        ));
                    }
                    let arg_locals = self.emit_call_args(args, &sig.params[1..])?;
                    self.push_line(format!("local.get {ptr_local}"));
                    for local in arg_locals {
                        self.push_line(format!("local.get {local}"));
                    }
                    self.push_line(format!("call ${book}::init"));
                    self.emit_local_set(ptr_local, &Type::Book(book.clone()));
                } else if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        format!("Missing constructor '{book}::init'."),
                    ));
                }
                self.push_line(format!("local.get {ptr_local}"));
            }
            ExprKind::MemberAccess { base, field } => {
                let info = self
                    .lookup(base)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{base}'")))?;
                let Type::Book(book_name) = &info.ty else {
                    return Err(wasm_error("E0400", "Field access requires book type."));
                };
                let layout = self
                    .books
                    .get(book_name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown book '{book_name}'")))?;
                let Some(index) = layout.field_index.get(field) else {
                    return Err(wasm_error(
                        "E0400",
                        format!("Unknown field '{field}' on '{book_name}'"),
                    ));
                };
                let field_ty = layout.fields.get(*index).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown field '{field}' on '{book_name}'"))
                })?;
                self.emit_field_address(info.idx, *index);
                self.emit_field_load(field_ty);
            }
            ExprKind::ArrayLit(elements) => self.emit_array_literal(elements, expected)?,
            ExprKind::ArrayNew { len } => self.emit_array_new(len, expected)?,
            ExprKind::Index { base, index } => self.emit_index_expr(base, index)?,
            ExprKind::Cast { expr, ty } => {
                let from_ty = self.infer_expr_type(expr)?;
                let to_ty = ty.clone();
                self.emit_expr(expr, None)?;
                match (from_ty, &to_ty) {
                    (Type::I64, Type::F64) => self.push_line("f64.convert_i64_s"),
                    (Type::F64, Type::I64) => self.push_line("i64.trunc_f64_s"),
                    (from, to) if from == *to => {}
                    (from, to) => {
                        return Err(wasm_error(
                            "E0400",
                            format!("Cannot cast from {from:?} to {to:?}."),
                        ));
                    }
                }
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    let expr_ty = self.infer_expr_type(expr)?;
                    match expr_ty {
                        Type::F64 => {
                            self.emit_expr(expr, None)?;
                            self.push_line("f64.neg");
                        }
                        _ => {
                            self.push_line("i64.const 0");
                            self.emit_expr(expr, None)?;
                            self.push_line("i64.sub");
                        }
                    }
                }
                UnaryOp::Not => {
                    self.emit_expr(expr, None)?;
                    self.push_line("i32.eqz");
                }
            },
            ExprKind::Binary { left, op, right } => {
                let left_ty = self.infer_expr_type(left)?;
                let right_ty = self.infer_expr_type(right)?;
                self.emit_expr(left, None)?;
                self.emit_expr(right, None)?;
                let instr = match (op, left_ty, right_ty) {
                    (BinaryOp::Add, Type::F64, Type::F64) => "f64.add",
                    (BinaryOp::Sub, Type::F64, Type::F64) => "f64.sub",
                    (BinaryOp::Mul, Type::F64, Type::F64) => "f64.mul",
                    (BinaryOp::Div, Type::F64, Type::F64) => "f64.div",
                    (BinaryOp::Mod, Type::F64, Type::F64) => "f64.rem",
                    (BinaryOp::EqEq, Type::F64, Type::F64) => "f64.eq",
                    (BinaryOp::NotEq, Type::F64, Type::F64) => "f64.ne",
                    (BinaryOp::EqEq, Type::Bool, Type::Bool) => "i32.eq",
                    (BinaryOp::NotEq, Type::Bool, Type::Bool) => "i32.ne",
                    (BinaryOp::Lt, Type::F64, Type::F64) => "f64.lt",
                    (BinaryOp::LtEq, Type::F64, Type::F64) => "f64.le",
                    (BinaryOp::Gt, Type::F64, Type::F64) => "f64.gt",
                    (BinaryOp::GtEq, Type::F64, Type::F64) => "f64.ge",
                    (BinaryOp::Add, _, _) => "i64.add",
                    (BinaryOp::Sub, _, _) => "i64.sub",
                    (BinaryOp::Mul, _, _) => "i64.mul",
                    (BinaryOp::Div, _, _) => "i64.div_s",
                    (BinaryOp::Mod, _, _) => "i64.rem_s",
                    (BinaryOp::EqEq, _, _) => "i64.eq",
                    (BinaryOp::NotEq, _, _) => "i64.ne",
                    (BinaryOp::Lt, _, _) => "i64.lt_s",
                    (BinaryOp::LtEq, _, _) => "i64.le_s",
                    (BinaryOp::Gt, _, _) => "i64.gt_s",
                    (BinaryOp::GtEq, _, _) => "i64.ge_s",
                    (BinaryOp::AndAnd, _, _) => "i32.and",
                    (BinaryOp::OrOr, _, _) => "i32.or",
                };
                self.push_line(instr);
            }
        }
        Ok(())
    }

    fn emit_array_literal(
        &mut self,
        elements: &[Expr],
        expected: Option<&Type>,
    ) -> Result<(), WasmError> {
        let expected_elem = match expected {
            Some(Type::Array(elem)) => Some(elem.as_ref().clone()),
            _ => None,
        };
        let elem_ty = if elements.is_empty() {
            match expected_elem {
                Some(elem) => elem,
                None => {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal requires explicit array type.",
                    ))
                }
            }
        } else if let Some(elem) = expected_elem.clone() {
            for element in elements {
                let actual = match (&element.kind, &elem) {
                    (ExprKind::Int(_), Type::U8) => Type::U8,
                    _ => self.infer_expr_type(element)?,
                };
                if actual != elem {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal elements must have the same type.",
                    ));
                }
            }
            elem
        } else {
            let first_ty = self.infer_expr_type(&elements[0])?;
            for elem in elements.iter().skip(1) {
                let ty = self.infer_expr_type(elem)?;
                if ty != first_ty {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal elements must have the same type.",
                    ));
                }
            }
            first_ty
        };

        let elem_size = array_elem_size(&elem_ty)?;
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        let byte_size = ARRAY_HEADER_SIZE + (elements.len() as i32 * elem_size);

        self.push_line(format!("i32.const {byte_size}"));
        self.push_line("call $bd_alloc");
        self.emit_local_set(ptr_local, &Type::Array(Box::new(elem_ty.clone())));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", elements.len()));
        self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", array_elem_kind(&elem_ty)));
        self.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
        if super::is_ref_type(&elem_ty) {
            for idx in 0..elements.len() {
                self.emit_array_address_const(ptr_local, idx as i64, elem_size);
                self.push_line("i32.const 0");
                self.emit_store(&elem_ty);
            }
        }

        for (idx, elem) in elements.iter().enumerate() {
            self.emit_array_address_const(ptr_local, idx as i64, elem_size);
            self.emit_expr(elem, Some(&elem_ty))?;
            self.emit_store(&elem_ty);
        }

        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn emit_array_new(&mut self, len: &Expr, expected: Option<&Type>) -> Result<(), WasmError> {
        let elem_ty = match expected {
            Some(Type::Array(elem)) => elem.as_ref().clone(),
            _ => {
                return Err(wasm_error(
                    "E0400",
                    "Array constructor requires explicit array type.",
                ))
            }
        };
        let elem_size = array_elem_size(&elem_ty)?;
        let len_local = self.temp_local(Type::I64);
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));

        self.emit_expr(len, None)?;
        self.push_line(format!("local.set {len_local}"));
        self.emit_len_non_negative_check(len_local)?;
        self.emit_len_max_check(len_local, elem_size)?;

        self.push_line(format!("local.get {len_local}"));
        self.push_line(format!("i64.const {elem_size}"));
        self.push_line("i64.mul");
        self.push_line(format!("i64.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i64.add");
        self.push_line("i32.wrap_i64");
        self.push_line("call $bd_alloc");
        self.emit_local_set(ptr_local, &Type::Array(Box::new(elem_ty.clone())));

        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i32.wrap_i64");
        self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", array_elem_kind(&elem_ty)));
        self.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

        self.emit_array_init(ptr_local, len_local, &elem_ty, elem_size)?;
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn emit_enum_constructor(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        let Some((enum_name, variant_name)) = name.split_once("::") else {
            return Ok(false);
        };
        if enum_name == "std" || variant_name.contains("::") {
            return Ok(false);
        }
        if self.functions.contains_key(name) {
            return Ok(false);
        }
        if self.lookup(enum_name).is_some() {
            return Ok(false);
        }
        let Some(enum_info) = self.enums.get(enum_name) else {
            return Ok(false);
        };
        let variant = enum_info
            .variants
            .get(variant_name)
            .ok_or_else(|| wasm_error("E0400", format!("Unknown enum variant '{name}'.")))?;
        let expected_args = if variant.payload.is_some() { 1 } else { 0 };
        if args.len() != expected_args {
            return Err(wasm_error(
                "E0400",
                format!(
                    "Wrong number of arguments for '{name}': expected {}, got {}.",
                    expected_args,
                    args.len()
                ),
            ));
        }

        let payload_ty = variant.payload.as_ref();
        let payload_local = if let Some(payload_ty) = payload_ty {
            let local = self.temp_local(payload_ty.clone());
            let Some(arg) = args.first() else {
                return Err(wasm_error("E0400", "Missing enum payload value."));
            };
            self.emit_expr(arg, Some(payload_ty))?;
            self.emit_local_set(local, payload_ty);
            Some(local)
        } else {
            None
        };

        let ptr_local = self.temp_local(Type::Book(enum_name.to_string()));
        let size = HEAP_HEADER_SIZE + if payload_ty.is_some() { 8 } else { 0 };
        self.push_line(format!("i32.const {size}"));
        self.push_line("call $bd_alloc");
        self.emit_local_set(ptr_local, &Type::Book(enum_name.to_string()));
        let tag = (HEAP_KIND_ENUM << HEAP_KIND_SHIFT) | enum_info.id as i32;
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {tag}"));
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", variant.id));
        self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
        self.push_line(format!("local.get {ptr_local}"));
        if let Some(payload_ty) = payload_ty {
            let kind = array_elem_kind(payload_ty);
            self.push_line(format!("i32.const {kind}"));
        } else {
            self.push_line("i32.const 0");
        }
        self.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

        if let (Some(payload_ty), Some(payload_local)) = (payload_ty, payload_local) {
            self.push_line(format!("local.get {ptr_local}"));
            self.push_line(format!("i32.const {HEAP_HEADER_SIZE}"));
            self.push_line("i32.add");
            self.push_line(format!("local.get {payload_local}"));
            match payload_ty {
                Type::I64 => {
                    self.push_line("i64.store");
                }
                Type::F64 => {
                    self.push_line("f64.store");
                }
                Type::U8 | Type::Bool => {
                    self.push_line("i32.store8");
                }
                Type::String | Type::Array(_) | Type::Book(_) => {
                    self.push_line("i64.extend_i32_u");
                    self.push_line("i64.store");
                }
                Type::Void => {
                    return Err(wasm_error("E0400", "Enum payload cannot be void."));
                }
            }
        }

        self.push_line(format!("local.get {ptr_local}"));
        Ok(true)
    }

    fn emit_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<(), WasmError> {
        let base_ty = self.infer_expr_type(base)?;
        let elem_ty = match base_ty {
            Type::Array(elem) => elem.as_ref().clone(),
            _ => return Err(wasm_error("E0400", "Indexing requires array type.")),
        };
        let idx_local = self.temp_local(Type::I64);
        self.emit_expr(index, None)?;
        self.push_line(format!("local.set {idx_local}"));
        let base_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        self.emit_expr(base, None)?;
        self.emit_local_set(base_local, &Type::Array(Box::new(elem_ty.clone())));
        self.emit_bounds_check(base_local, idx_local)?;
        self.emit_array_address_index(base_local, idx_local, array_elem_size(&elem_ty)?);
        self.emit_load(&elem_ty);
        Ok(())
    }

    fn emit_call_args(
        &mut self,
        args: &[Expr],
        param_types: &[Type],
    ) -> Result<Vec<u32>, WasmError> {
        let mut locals = Vec::with_capacity(args.len());
        for (arg, param_ty) in args.iter().zip(param_types.iter()) {
            let local = self.temp_local(param_ty.clone());
            self.emit_expr(arg, Some(param_ty))?;
            self.emit_local_set(local, param_ty);
            locals.push(local);
        }
        Ok(locals)
    }

    fn emit_string_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::string::len" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::string::len expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_string_len");
                Ok(true)
            }
            "std::string::concat" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::concat expects 2 arguments",
                    ));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_string_concat");
                Ok(true)
            }
            "std::string::eq" => {
                if args.len() != 2 {
                    return Err(wasm_error("E0400", "std::string::eq expects 2 arguments"));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_string_eq");
                Ok(true)
            }
            "std::string::bytes" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::string::bytes expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_string_bytes");
                Ok(true)
            }
            "std::string::slice" => {
                if args.len() != 3 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::slice expects 3 arguments",
                    ));
                }
                let param_types = [Type::String, Type::I64, Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line(format!("local.get {}", arg_locals[2]));
                self.push_line("call $bd_string_slice");
                Ok(true)
            }
            "std::string::index_of" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::index_of expects 2 arguments",
                    ));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_string_index_of");
                Ok(true)
            }
            "std::string::contains" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::contains expects 2 arguments",
                    ));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_string_contains");
                Ok(true)
            }
            "std::string::replace" => {
                if args.len() != 3 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::replace expects 3 arguments",
                    ));
                }
                let param_types = [Type::String, Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line(format!("local.get {}", arg_locals[2]));
                self.push_line("call $bd_string_replace");
                Ok(true)
            }
            "std::string::from_bytes" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::from_bytes expects 1 argument",
                    ));
                }
                let param_types = [Type::Array(Box::new(Type::U8))];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_string_from_bytes");
                Ok(true)
            }
            "std::string::to_i64" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::to_i64 expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_string_to_i64");
                Ok(true)
            }
            "std::string::from_i64" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::string::from_i64 expects 1 argument",
                    ));
                }
                let param_types = [Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_string_from_i64");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_json_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::json::encode_i64" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::json::encode_i64 expects 1 argument",
                    ));
                }
                let param_types = [Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_json_encode_i64");
                Ok(true)
            }
            "std::json::encode_bool" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::json::encode_bool expects 1 argument",
                    ));
                }
                let param_types = [Type::Bool];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_json_encode_bool");
                Ok(true)
            }
            "std::json::encode_string" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::json::encode_string expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_json_encode_string");
                Ok(true)
            }
            "std::json::decode_i64" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::json::decode_i64 expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_json_decode_i64");
                Ok(true)
            }
            "std::json::decode_bool" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::json::decode_bool expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_json_decode_bool");
                Ok(true)
            }
            "std::json::decode_string" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::json::decode_string expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_json_decode_string");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_bytes_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::bytes::len" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::bytes::len expects 1 argument"));
                }
                let param_types = [Type::Array(Box::new(Type::U8))];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_bytes_len");
                Ok(true)
            }
            "std::bytes::eq" => {
                if args.len() != 2 {
                    return Err(wasm_error("E0400", "std::bytes::eq expects 2 arguments"));
                }
                let param_types = [
                    Type::Array(Box::new(Type::U8)),
                    Type::Array(Box::new(Type::U8)),
                ];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_bytes_eq");
                Ok(true)
            }
            "std::bytes::slice" => {
                if args.len() != 3 {
                    return Err(wasm_error("E0400", "std::bytes::slice expects 3 arguments"));
                }
                let param_types = [Type::Array(Box::new(Type::U8)), Type::I64, Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line(format!("local.get {}", arg_locals[2]));
                self.push_line("call $bd_bytes_slice");
                Ok(true)
            }
            "std::bytes::index_of" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::bytes::index_of expects 2 arguments",
                    ));
                }
                let param_types = [Type::Array(Box::new(Type::U8)), Type::U8];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_bytes_index_of");
                Ok(true)
            }
            "std::bytes::contains" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::bytes::contains expects 2 arguments",
                    ));
                }
                let param_types = [Type::Array(Box::new(Type::U8)), Type::U8];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_bytes_contains");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_io_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::io::print" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::io::print expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_io_print");
                Ok(true)
            }
            "std::io::read_line" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "std::io::read_line expects 0 arguments",
                    ));
                }
                self.push_line("call $bd_io_read_line");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_time_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::time::now_ms" => {
                if !args.is_empty() {
                    return Err(wasm_error("E0400", "std::time::now_ms expects 0 arguments"));
                }
                self.push_line("call $bd_time_now_ms");
                Ok(true)
            }
            "std::time::sleep_ms" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::time::sleep_ms expects 1 argument",
                    ));
                }
                let param_types = [Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_time_sleep_ms");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_profiler_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        if !name.starts_with("std::profiler::") {
            return Ok(false);
        }
        if !args.is_empty() {
            return Err(wasm_error(
                "E0400",
                "std::profiler functions expect 0 arguments",
            ));
        }
        match name {
            "std::profiler::uptime_ms" => self.push_line("call $bd_profiler_uptime_ms"),
            "std::profiler::alloc_count" => self.push_line("call $bd_profiler_alloc_count"),
            "std::profiler::bytes_allocated" => self.push_line("call $bd_profiler_bytes_allocated"),
            "std::profiler::bytes_in_use" => self.push_line("call $bd_profiler_bytes_in_use"),
            "std::profiler::peak_bytes_in_use" => {
                self.push_line("call $bd_profiler_peak_bytes_in_use")
            }
            "std::profiler::gc_runs" => self.push_line("call $bd_profiler_gc_runs"),
            "std::profiler::last_freed" => self.push_line("call $bd_profiler_last_freed"),
            "std::profiler::last_live" => self.push_line("call $bd_profiler_last_live"),
            "std::profiler::last_freed_bytes" => {
                self.push_line("call $bd_profiler_last_freed_bytes")
            }
            "std::profiler::last_live_bytes" => self.push_line("call $bd_profiler_last_live_bytes"),
            _ => return Ok(false),
        }
        Ok(true)
    }

    fn emit_rand_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::rand::seed" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::rand::seed expects 1 argument"));
                }
                let param_types = [Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_rand_seed");
                Ok(true)
            }
            "std::rand::range" => {
                if args.len() != 2 {
                    return Err(wasm_error("E0400", "std::rand::range expects 2 arguments"));
                }
                let param_types = [Type::I64, Type::I64];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_rand_range");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_test_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::test::assert" => {
                if args.len() != 2 {
                    return Err(wasm_error("E0400", "std::test::assert expects 2 arguments"));
                }
                let param_types = [Type::Bool, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("i32.eqz");
                self.push_line("if");
                self.indent += 1;
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_throw");
                self.emit_error_check();
                self.indent -= 1;
                self.push_line("end");
                Ok(true)
            }
            "std::test::assert_eq_i64" => {
                if args.len() != 3 {
                    return Err(wasm_error(
                        "E0400",
                        "std::test::assert_eq_i64 expects 3 arguments",
                    ));
                }
                let param_types = [Type::I64, Type::I64, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("i64.ne");
                self.push_line("if");
                self.indent += 1;
                self.push_line(format!("local.get {}", arg_locals[2]));
                self.push_line("call $bd_throw");
                self.emit_error_check();
                self.indent -= 1;
                self.push_line("end");
                Ok(true)
            }
            "std::test::assert_eq_bool" => {
                if args.len() != 3 {
                    return Err(wasm_error(
                        "E0400",
                        "std::test::assert_eq_bool expects 3 arguments",
                    ));
                }
                let param_types = [Type::Bool, Type::Bool, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("i32.ne");
                self.push_line("if");
                self.indent += 1;
                self.push_line(format!("local.get {}", arg_locals[2]));
                self.push_line("call $bd_throw");
                self.emit_error_check();
                self.indent -= 1;
                self.push_line("end");
                Ok(true)
            }
            "std::test::assert_eq_string" => {
                if args.len() != 3 {
                    return Err(wasm_error(
                        "E0400",
                        "std::test::assert_eq_string expects 3 arguments",
                    ));
                }
                let param_types = [Type::String, Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_string_eq");
                self.push_line("i32.eqz");
                self.push_line("if");
                self.indent += 1;
                self.push_line(format!("local.get {}", arg_locals[2]));
                self.push_line("call $bd_throw");
                self.emit_error_check();
                self.indent -= 1;
                self.push_line("end");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_fs_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::fs::read_text" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::fs::read_text expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_fs_read_text");
                Ok(true)
            }
            "std::fs::write_text" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::fs::write_text expects 2 arguments",
                    ));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_fs_write_text");
                Ok(true)
            }
            "std::fs::read_bytes" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::fs::read_bytes expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_fs_read_bytes");
                Ok(true)
            }
            "std::fs::write_bytes" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::fs::write_bytes expects 2 arguments",
                    ));
                }
                let param_types = [Type::String, Type::Array(Box::new(Type::U8))];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_fs_write_bytes");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_path_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::path::join" => {
                if args.len() != 2 {
                    return Err(wasm_error("E0400", "std::path::join expects 2 arguments"));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_path_join");
                Ok(true)
            }
            "std::path::normalize" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::path::normalize expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_path_normalize");
                Ok(true)
            }
            "std::path::basename" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::path::basename expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_path_basename");
                Ok(true)
            }
            "std::path::dirname" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::path::dirname expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_path_dirname");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_channel_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::channel::i64" => {
                if !args.is_empty() {
                    return Err(wasm_error("E0400", "std::channel::i64 expects 0 arguments"));
                }
                self.push_line("call $bd_channel_i64");
                Ok(true)
            }
            "std::channel::bool" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "std::channel::bool expects 0 arguments",
                    ));
                }
                self.push_line("call $bd_channel_bool");
                Ok(true)
            }
            "std::channel::f64" => {
                if !args.is_empty() {
                    return Err(wasm_error("E0400", "std::channel::f64 expects 0 arguments"));
                }
                self.push_line("call $bd_channel_f64");
                Ok(true)
            }
            "std::channel::u8" => {
                if !args.is_empty() {
                    return Err(wasm_error("E0400", "std::channel::u8 expects 0 arguments"));
                }
                self.push_line("call $bd_channel_u8");
                Ok(true)
            }
            "std::channel::string" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "std::channel::string expects 0 arguments",
                    ));
                }
                self.push_line("call $bd_channel_string");
                Ok(true)
            }
            "std::channel::bytes" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "std::channel::bytes expects 0 arguments",
                    ));
                }
                self.push_line("call $bd_channel_bytes");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_channel_method_call(
        &mut self,
        book: &str,
        base_local: u32,
        method: &str,
        args: &[Expr],
        expected: Option<&Type>,
    ) -> Result<bool, WasmError> {
        let (suffix, payload_ty, recv_ty) = match book {
            "ChannelI64" => ("i64", Type::I64, Type::Book("RecvI64".to_string())),
            "ChannelBool" => ("bool", Type::Bool, Type::Book("RecvBool".to_string())),
            "ChannelF64" => ("f64", Type::F64, Type::Book("RecvF64".to_string())),
            "ChannelU8" => ("u8", Type::U8, Type::Book("RecvU8".to_string())),
            "ChannelString" => ("string", Type::String, Type::Book("RecvString".to_string())),
            "ChannelBytes" => (
                "bytes",
                Type::Array(Box::new(Type::U8)),
                Type::Book("RecvBytes".to_string()),
            ),
            _ => return Ok(false),
        };
        match method {
            "send" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        format!("{book}::send expects 1 argument"),
                    ));
                }
                if let Some(expected) = expected {
                    if !matches!(expected, Type::Bool) {
                        return Err(wasm_error("E0400", "Type mismatch in channel send."));
                    }
                }
                self.push_line(format!("local.get {base_local}"));
                self.emit_expr(&args[0], Some(&payload_ty))?;
                self.push_line(format!("call $bd_channel_send_{suffix}"));
                Ok(true)
            }
            "recv" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        format!("{book}::recv expects 0 arguments"),
                    ));
                }
                if let Some(expected) = expected {
                    if expected != &recv_ty {
                        return Err(wasm_error("E0400", "Type mismatch in channel recv."));
                    }
                }
                self.push_line(format!("local.get {base_local}"));
                self.push_line(format!("call $bd_channel_recv_{suffix}"));
                Ok(true)
            }
            "close" => {
                if !args.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        format!("{book}::close expects 0 arguments"),
                    ));
                }
                if let Some(expected) = expected {
                    if !matches!(expected, Type::Void) {
                        return Err(wasm_error("E0400", "Type mismatch in channel close."));
                    }
                }
                self.push_line(format!("local.get {base_local}"));
                self.push_line(format!("call $bd_channel_close_{suffix}"));
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_env_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::env::args" => {
                if !args.is_empty() {
                    return Err(wasm_error("E0400", "std::env::args expects 0 arguments"));
                }
                self.push_line("call $bd_env_args");
                Ok(true)
            }
            "std::env::get" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::env::get expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_env_get");
                Ok(true)
            }
            "std::env::set_var" => {
                if args.len() != 2 {
                    return Err(wasm_error("E0400", "std::env::set_var expects 2 arguments"));
                }
                let param_types = [Type::String, Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_env_set");
                Ok(true)
            }
            "std::env::cwd" => {
                if !args.is_empty() {
                    return Err(wasm_error("E0400", "std::env::cwd expects 0 arguments"));
                }
                self.push_line("call $bd_env_cwd");
                Ok(true)
            }
            "std::env::set_cwd" => {
                if args.len() != 1 {
                    return Err(wasm_error("E0400", "std::env::set_cwd expects 1 argument"));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_env_set_cwd");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_string_literal(&mut self, value: &str) -> Result<(), WasmError> {
        let bytes = value.as_bytes();
        let len = i32::try_from(bytes.len())
            .map_err(|_| wasm_error("E0400", "String literal is too large to encode."))?;
        let byte_size = STRING_HEADER_SIZE + len;
        let ptr_local = self.temp_local(Type::String);

        self.push_line(format!("i32.const {byte_size}"));
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
        self.push_line(format!("i32.const {len}"));
        self.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));

        for (idx, byte) in bytes.iter().enumerate() {
            let offset = STRING_HEADER_SIZE + idx as i32;
            self.push_line(format!("local.get {ptr_local}"));
            self.push_line(format!("i32.const {offset}"));
            self.push_line("i32.add");
            self.push_line(format!("i32.const {byte}"));
            self.push_line("i32.store8");
        }

        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }
    pub(super) fn infer_expr_type(&self, expr: &Expr) -> Result<Type, WasmError> {
        match &expr.kind {
            ExprKind::Int(_) => Ok(Type::I64),
            ExprKind::Float(_) => Ok(Type::F64),
            ExprKind::Bool(_) => Ok(Type::Bool),
            ExprKind::String(_) => Ok(Type::String),
            ExprKind::Ident(name) => self
                .lookup(name)
                .map(|info| info.ty)
                .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'"))),
            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal requires explicit array type.",
                    ));
                }
                let first_ty = self.infer_expr_type(&elements[0])?;
                for element in elements.iter().skip(1) {
                    let ty = self.infer_expr_type(element)?;
                    if ty != first_ty {
                        return Err(wasm_error(
                            "E0400",
                            "Array literal elements must have the same type.",
                        ));
                    }
                }
                Ok(Type::Array(Box::new(first_ty)))
            }
            ExprKind::ArrayNew { .. } => Err(wasm_error(
                "E0400",
                "Array constructor requires explicit array type.",
            )),
            ExprKind::Index { base, .. } => {
                let base_ty = self.infer_expr_type(base)?;
                match base_ty {
                    Type::Array(elem) => Ok(elem.as_ref().clone()),
                    _ => Err(wasm_error("E0400", "Indexing requires array type.")),
                }
            }
            ExprKind::Call { name, .. } => {
                if let Some(return_ty) = self.infer_call_type(name) {
                    return Ok(return_ty);
                }
                if let Some((enum_name, variant_name)) = name.split_once("::") {
                    if enum_name != "std"
                        && !variant_name.contains("::")
                        && self.lookup(enum_name).is_none()
                    {
                        if let Some(enum_info) = self.enums.get(enum_name) {
                            if enum_info.variants.contains_key(variant_name) {
                                return Ok(Type::Book(enum_name.to_string()));
                            }
                            return Err(wasm_error(
                                "E0400",
                                format!("Unknown enum variant '{name}'."),
                            ));
                        }
                    }
                }
                if let Some((base, method)) = name.split_once("::") {
                    if base != "std" {
                        if let Some(info) = self.lookup(base) {
                            if let Type::Book(book_name) = &info.ty {
                                let full_name = format!("{book_name}::{method}");
                                return self.infer_call_type(&full_name).ok_or_else(|| {
                                    wasm_error("E0400", format!("Unknown function '{full_name}'"))
                                });
                            }
                        }
                    }
                }
                Err(wasm_error("E0400", format!("Unknown function '{name}'")))
            }
            ExprKind::New { book, .. } => Ok(Type::Book(book.clone())),
            ExprKind::MemberAccess { base, field } => {
                let base_ty = self
                    .lookup(base)
                    .map(|info| info.ty)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{base}'")))?;
                let Type::Book(book_name) = base_ty else {
                    return Err(wasm_error("E0400", "Field access requires book type."));
                };
                let layout = self
                    .books
                    .get(&book_name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown book '{book_name}'")))?;
                let Some(index) = layout.field_index.get(field) else {
                    return Err(wasm_error(
                        "E0400",
                        format!("Unknown field '{field}' on '{book_name}'"),
                    ));
                };
                let field_ty = layout.fields.get(*index).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown field '{field}' on '{book_name}'"))
                })?;
                Ok(field_ty.clone())
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    let inner = self.infer_expr_type(expr)?;
                    if matches!(inner, Type::F64) {
                        Ok(Type::F64)
                    } else {
                        Ok(Type::I64)
                    }
                }
                UnaryOp::Not => Ok(Type::Bool),
            },
            ExprKind::Cast { ty, .. } => Ok(ty.clone()),
            ExprKind::Binary { op, left, right } => {
                let left_ty = self.infer_expr_type(left)?;
                let right_ty = self.infer_expr_type(right)?;
                match op {
                    BinaryOp::Add
                    | BinaryOp::Sub
                    | BinaryOp::Mul
                    | BinaryOp::Div
                    | BinaryOp::Mod => {
                        if matches!((&left_ty, &right_ty), (Type::F64, Type::F64)) {
                            Ok(Type::F64)
                        } else {
                            Ok(Type::I64)
                        }
                    }
                    BinaryOp::EqEq
                    | BinaryOp::NotEq
                    | BinaryOp::Lt
                    | BinaryOp::LtEq
                    | BinaryOp::Gt
                    | BinaryOp::GtEq
                    | BinaryOp::AndAnd
                    | BinaryOp::OrOr => Ok(Type::Bool),
                }
            }
        }
    }

    fn infer_call_type(&self, name: &str) -> Option<Type> {
        match name {
            "std::string::len" => Some(Type::I64),
            "std::string::concat" => Some(Type::String),
            "std::string::eq" => Some(Type::Bool),
            "std::string::bytes" => Some(Type::Array(Box::new(Type::U8))),
            "std::string::slice" => Some(Type::String),
            "std::string::index_of" => Some(Type::I64),
            "std::string::contains" => Some(Type::Bool),
            "std::string::replace" => Some(Type::String),
            "std::string::from_bytes" => Some(Type::String),
            "std::string::to_i64" => Some(Type::I64),
            "std::string::from_i64" => Some(Type::String),
            "std::bytes::len" => Some(Type::I64),
            "std::bytes::eq" => Some(Type::Bool),
            "std::bytes::slice" => Some(Type::Array(Box::new(Type::U8))),
            "std::bytes::index_of" => Some(Type::I64),
            "std::bytes::contains" => Some(Type::Bool),
            "std::io::print" => Some(Type::Void),
            "std::io::read_line" => Some(Type::String),
            "std::time::now_ms" => Some(Type::I64),
            "std::time::sleep_ms" => Some(Type::I64),
            "std::profiler::uptime_ms" => Some(Type::I64),
            "std::profiler::alloc_count" => Some(Type::I64),
            "std::profiler::bytes_allocated" => Some(Type::I64),
            "std::profiler::bytes_in_use" => Some(Type::I64),
            "std::profiler::peak_bytes_in_use" => Some(Type::I64),
            "std::profiler::gc_runs" => Some(Type::I64),
            "std::profiler::last_freed" => Some(Type::I64),
            "std::profiler::last_live" => Some(Type::I64),
            "std::profiler::last_freed_bytes" => Some(Type::I64),
            "std::profiler::last_live_bytes" => Some(Type::I64),
            "std::rand::seed" => Some(Type::Void),
            "std::rand::range" => Some(Type::I64),
            "std::test::assert" => Some(Type::Void),
            "std::test::assert_eq_i64" => Some(Type::Void),
            "std::test::assert_eq_bool" => Some(Type::Void),
            "std::test::assert_eq_string" => Some(Type::Void),
            "std::fs::read_text" => Some(Type::String),
            "std::fs::write_text" => Some(Type::I64),
            "std::fs::read_bytes" => Some(Type::Array(Box::new(Type::U8))),
            "std::fs::write_bytes" => Some(Type::I64),
            "std::env::args" => Some(Type::Array(Box::new(Type::String))),
            "std::env::get" => Some(Type::String),
            "std::env::set_var" => Some(Type::I64),
            "std::env::cwd" => Some(Type::String),
            "std::env::set_cwd" => Some(Type::I64),
            "std::path::join" => Some(Type::String),
            "std::path::normalize" => Some(Type::String),
            "std::path::basename" => Some(Type::String),
            "std::path::dirname" => Some(Type::String),
            "std::json::encode_i64" => Some(Type::String),
            "std::json::encode_bool" => Some(Type::String),
            "std::json::encode_string" => Some(Type::String),
            "std::json::decode_i64" => Some(Type::I64),
            "std::json::decode_bool" => Some(Type::Bool),
            "std::json::decode_string" => Some(Type::String),
            "std::channel::i64" => Some(Type::Book("ChannelI64".to_string())),
            "std::channel::bool" => Some(Type::Book("ChannelBool".to_string())),
            "std::channel::f64" => Some(Type::Book("ChannelF64".to_string())),
            "std::channel::u8" => Some(Type::Book("ChannelU8".to_string())),
            "std::channel::string" => Some(Type::Book("ChannelString".to_string())),
            "std::channel::bytes" => Some(Type::Book("ChannelBytes".to_string())),
            "ChannelI64::send" => Some(Type::Bool),
            "ChannelI64::recv" => Some(Type::Book("RecvI64".to_string())),
            "ChannelI64::close" => Some(Type::Void),
            "ChannelBool::send" => Some(Type::Bool),
            "ChannelBool::recv" => Some(Type::Book("RecvBool".to_string())),
            "ChannelBool::close" => Some(Type::Void),
            "ChannelF64::send" => Some(Type::Bool),
            "ChannelF64::recv" => Some(Type::Book("RecvF64".to_string())),
            "ChannelF64::close" => Some(Type::Void),
            "ChannelU8::send" => Some(Type::Bool),
            "ChannelU8::recv" => Some(Type::Book("RecvU8".to_string())),
            "ChannelU8::close" => Some(Type::Void),
            "ChannelString::send" => Some(Type::Bool),
            "ChannelString::recv" => Some(Type::Book("RecvString".to_string())),
            "ChannelString::close" => Some(Type::Void),
            "ChannelBytes::send" => Some(Type::Bool),
            "ChannelBytes::recv" => Some(Type::Book("RecvBytes".to_string())),
            "ChannelBytes::close" => Some(Type::Void),
            _ => self.functions.get(name).map(|sig| sig.return_type.clone()),
        }
    }
}

fn format_f64(value: f64) -> String {
    let mut text = value.to_string();
    if !text.contains('.') && !text.contains('e') && !text.contains('E') {
        text.push_str(".0");
    }
    text
}
