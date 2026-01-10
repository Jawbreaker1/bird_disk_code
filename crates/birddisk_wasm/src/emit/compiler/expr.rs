use super::FuncCompiler;
use super::super::types::array_elem_size;
use super::super::{
    wasm_error, WasmError, ARRAY_HEADER_SIZE, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET, HEAP_KIND_ARRAY,
    HEAP_KIND_OBJECT, HEAP_KIND_SHIFT, HEAP_KIND_STRING, HEAP_LEN_OFFSET, OBJECT_FIELD_SIZE,
    OBJECT_HEADER_SIZE, STRING_HEADER_SIZE, TRAP_KIND_OBJECT,
};
use super::super::types::array_elem_kind;
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Type, UnaryOp};

impl<'a> FuncCompiler<'a> {
    pub(super) fn emit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<(), WasmError> {
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
                if let Some((base, method)) = name.split_once("::") {
                    if let Some(info) = self.lookup(base) {
                        if let Type::Book(book_name) = &info.ty {
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
                            return Ok(());
                        }
                    }
                }
                if self.emit_string_call(name, args)? {
                    return Ok(());
                }
                if self.emit_bytes_call(name, args)? {
                    return Ok(());
                }
                if self.emit_io_call(name, args)? {
                    return Ok(());
                }
                let sig = self.functions.get(name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown function '{name}'"))
                })?;
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
            }
            ExprKind::New { book, args } => {
                let layout = self.books.get(book).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book}'"))
                })?;
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
                let layout = self.books.get(book_name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book_name}'"))
                })?;
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
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    self.push_line("i64.const 0");
                    self.emit_expr(expr, None)?;
                    self.push_line("i64.sub");
                }
                UnaryOp::Not => {
                    self.emit_expr(expr, None)?;
                    self.push_line("i32.eqz");
                }
            },
            ExprKind::Binary { left, op, right } => {
                self.emit_expr(left, None)?;
                self.emit_expr(right, None)?;
                let instr = match op {
                    BinaryOp::Add => "i64.add",
                    BinaryOp::Sub => "i64.sub",
                    BinaryOp::Mul => "i64.mul",
                    BinaryOp::Div => "i64.div_s",
                    BinaryOp::Mod => "i64.rem_s",
                    BinaryOp::EqEq => "i64.eq",
                    BinaryOp::NotEq => "i64.ne",
                    BinaryOp::Lt => "i64.lt_s",
                    BinaryOp::LtEq => "i64.le_s",
                    BinaryOp::Gt => "i64.gt_s",
                    BinaryOp::GtEq => "i64.ge_s",
                    BinaryOp::AndAnd => "i32.and",
                    BinaryOp::OrOr => "i32.or",
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
                    return Err(wasm_error(
                        "E0400",
                        "std::string::len expects 1 argument",
                    ));
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
                    return Err(wasm_error(
                        "E0400",
                        "std::string::eq expects 2 arguments",
                    ));
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
                    return Err(wasm_error(
                        "E0400",
                        "std::string::bytes expects 1 argument",
                    ));
                }
                let param_types = [Type::String];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_string_bytes");
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

    fn emit_bytes_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::bytes::len" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::bytes::len expects 1 argument",
                    ));
                }
                let param_types = [Type::Array(Box::new(Type::U8))];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line("call $bd_bytes_len");
                Ok(true)
            }
            "std::bytes::eq" => {
                if args.len() != 2 {
                    return Err(wasm_error(
                        "E0400",
                        "std::bytes::eq expects 2 arguments",
                    ));
                }
                let param_types = [Type::Array(Box::new(Type::U8)), Type::Array(Box::new(Type::U8))];
                let arg_locals = self.emit_call_args(args, &param_types)?;
                self.push_line(format!("local.get {}", arg_locals[0]));
                self.push_line(format!("local.get {}", arg_locals[1]));
                self.push_line("call $bd_bytes_eq");
                Ok(true)
            }
            _ => Ok(false),
        }
    }

    fn emit_io_call(&mut self, name: &str, args: &[Expr]) -> Result<bool, WasmError> {
        match name {
            "std::io::print" => {
                if args.len() != 1 {
                    return Err(wasm_error(
                        "E0400",
                        "std::io::print expects 1 argument",
                    ));
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

    fn emit_string_literal(&mut self, value: &str) -> Result<(), WasmError> {
        let bytes = value.as_bytes();
        let len = i32::try_from(bytes.len()).map_err(|_| {
            wasm_error("E0400", "String literal is too large to encode.")
        })?;
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
                if let Some((base, method)) = name.split_once("::") {
                    if let Some(info) = self.lookup(base) {
                        if let Type::Book(book_name) = &info.ty {
                            let full_name = format!("{book_name}::{method}");
                            return self.infer_call_type(&full_name).ok_or_else(|| {
                                wasm_error("E0400", format!("Unknown function '{full_name}'"))
                            });
                        }
                    }
                }
                self.infer_call_type(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown function '{name}'")))
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
                let layout = self.books.get(&book_name).ok_or_else(|| {
                    wasm_error("E0400", format!("Unknown book '{book_name}'"))
                })?;
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
            ExprKind::Unary { op, .. } => match op {
                UnaryOp::Neg => Ok(Type::I64),
                UnaryOp::Not => Ok(Type::Bool),
            },
            ExprKind::Binary { op, .. } => match op {
                BinaryOp::Add
                | BinaryOp::Sub
                | BinaryOp::Mul
                | BinaryOp::Div
                | BinaryOp::Mod => Ok(Type::I64),
                BinaryOp::EqEq
                | BinaryOp::NotEq
                | BinaryOp::Lt
                | BinaryOp::LtEq
                | BinaryOp::Gt
                | BinaryOp::GtEq
                | BinaryOp::AndAnd
                | BinaryOp::OrOr => Ok(Type::Bool),
            },
        }
    }

    fn infer_call_type(&self, name: &str) -> Option<Type> {
        match name {
            "std::string::len" => Some(Type::I64),
            "std::string::concat" => Some(Type::String),
            "std::string::eq" => Some(Type::Bool),
            "std::string::bytes" => Some(Type::Array(Box::new(Type::U8))),
            "std::string::from_bytes" => Some(Type::String),
            "std::string::to_i64" => Some(Type::I64),
            "std::string::from_i64" => Some(Type::String),
            "std::bytes::len" => Some(Type::I64),
            "std::bytes::eq" => Some(Type::Bool),
            "std::io::print" => Some(Type::I64),
            "std::io::read_line" => Some(Type::String),
            _ => self.functions.get(name).map(|sig| sig.return_type.clone()),
        }
    }
}
