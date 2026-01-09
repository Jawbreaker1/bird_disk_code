use crate::heap::{ElemKind, Heap, HeapHandle, HeapKind, HeapLayout, RootStack, RootValue};
use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::{coerce_value, value_type, Value};
use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, Type, UnaryOp};
use birddisk_core::TraceFrame;
use std::collections::{HashMap, VecDeque};

pub fn eval(program: &Program) -> Result<i64, RuntimeError> {
    let (result, _) = eval_with_io(program, "")?;
    Ok(result)
}

pub fn eval_with_io(program: &Program, input: &str) -> Result<(i64, String), RuntimeError> {
    let mut vm = Vm::new(program, input);
    let result = vm.eval_main()?;
    Ok((result, vm.output))
}

pub(crate) struct Vm<'a> {
    functions: HashMap<String, &'a birddisk_core::ast::Function>,
    books: HashMap<String, BookInfo>,
    scopes: Vec<Scope>,
    input: VecDeque<String>,
    output: String,
    trace: Vec<TraceFrame>,
    heap: Heap,
    roots: RootStack,
    gc_layout: GcLayout,
    gc_threshold: usize,
}

pub(crate) struct BookInfo {
    id: u32,
    field_types: Vec<Type>,
    field_index: HashMap<String, usize>,
}

#[derive(Debug)]
struct GcLayout {
    ref_fields: Vec<Vec<usize>>,
}

impl HeapLayout for GcLayout {
    fn object_ref_fields(&self, type_id: u32) -> &[usize] {
        self.ref_fields
            .get(type_id as usize)
            .map(|fields| fields.as_slice())
            .unwrap_or(&[])
    }
}

#[derive(Debug)]
struct Scope {
    values: HashMap<String, Value>,
    roots: HashMap<String, usize>,
}

impl Scope {
    fn new() -> Self {
        Self {
            values: HashMap::new(),
            roots: HashMap::new(),
        }
    }
}

impl<'a> Vm<'a> {
    fn new(program: &'a Program, input: &str) -> Self {
        let mut functions = HashMap::new();
        for func in &program.functions {
            functions.insert(func.name.clone(), func);
        }
        for book in &program.books {
            for method in &book.methods {
                let name = format!("{}::{}", book.name, method.name);
                functions.insert(name, method);
            }
        }
        let mut books = HashMap::new();
        let mut ref_fields = Vec::new();
        for (book_id, book) in program.books.iter().enumerate() {
            let mut field_types = Vec::new();
            let mut field_index = HashMap::new();
            let mut book_ref_fields = Vec::new();
            for (idx, field) in book.fields.iter().enumerate() {
                field_types.push(field.ty.clone());
                field_index.insert(field.name.clone(), idx);
                if is_ref_type(&field.ty) {
                    book_ref_fields.push(idx);
                }
            }
            books.insert(
                book.name.clone(),
                BookInfo {
                    id: book_id as u32,
                    field_types,
                    field_index,
                },
            );
            ref_fields.push(book_ref_fields);
        }
        Self {
            functions,
            books,
            scopes: Vec::new(),
            input: split_lines(input),
            output: String::new(),
            trace: Vec::new(),
            heap: Heap::new(),
            roots: RootStack::new(),
            gc_layout: GcLayout { ref_fields },
            gc_threshold: GC_MIN_THRESHOLD,
        }
    }

    fn eval_main(&mut self) -> Result<i64, RuntimeError> {
        let main = self
            .functions
            .get("main")
            .ok_or_else(|| runtime_error("E0400", "missing main function"))?;
        let value = match self.eval_function(main, &[]) {
            Ok(value) => value,
            Err(err) => return Err(self.with_trace(err)),
        };
        match value {
            Value::I64(value) => Ok(value),
            Value::Bool(_)
            | Value::String(_)
            | Value::U8(_)
            | Value::Array { .. }
            | Value::Object { .. } => Err(runtime_error("E0400", "main must return i64")),
        }
    }

    fn eval_function(
        &mut self,
        function: &birddisk_core::ast::Function,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        self.push_trace(function);
        if args.len() != function.params.len() {
            return Err(runtime_error(
                "E0400",
                format!(
                    "Wrong number of arguments for '{}': expected {}, got {}.",
                    function.name,
                    function.params.len(),
                    args.len()
                ),
            ));
        }
        self.push_scope();
        for (param, arg) in function.params.iter().zip(args.iter()) {
            let coerced = coerce_value(arg.clone(), &param.ty)?;
            self.bind_local(param.name.clone(), coerced);
        }
        let result = self.eval_block(&function.body);
        self.pop_scope();
        match result {
            Ok(Some(value)) => {
                self.pop_trace();
                let expected = &function.return_type;
                let value = coerce_value(value, expected)?;
                Ok(value)
            }
            Ok(None) => Err(runtime_error(
                "E0400",
                format!("function '{}' did not yield", function.name),
            )),
            Err(err) => Err(err),
        }
    }

    fn push_trace(&mut self, function: &birddisk_core::ast::Function) {
        self.trace.push(TraceFrame {
            function: function.name.clone(),
            span: function.span,
        });
    }

    fn pop_trace(&mut self) {
        self.trace.pop();
    }

    fn with_trace(&self, mut err: RuntimeError) -> RuntimeError {
        if err.trace.is_empty() {
            err.trace = self.trace.iter().cloned().rev().collect();
        }
        err
    }

    fn eval_block(&mut self, stmts: &[Stmt]) -> Result<Option<Value>, RuntimeError> {
        for stmt in stmts {
            if let Some(value) = self.eval_stmt(stmt)? {
                return Ok(Some(value));
            }
        }
        Ok(None)
    }

    fn eval_stmt(&mut self, stmt: &Stmt) -> Result<Option<Value>, RuntimeError> {
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
                                Type::Array(inner) => {
                                    self.eval_array_literal(elements, Some(inner))?
                                }
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
                            let result = self.eval_block(body)?;
                            self.pop_scope();
                            if result.is_some() {
                                return Ok(result);
                            }
                        }
                        Value::Bool(false) => break,
                        _ => return Err(runtime_error("E0400", "repeat condition was not bool")),
                    }
                }
                Ok(None)
            }
        }
    }

    pub(crate) fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(Value::I64(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::String(value) => Ok(self.alloc_string(value)),
            ExprKind::Ident(name) => self.lookup(name).cloned().ok_or_else(|| {
                runtime_error("E0400", format!("Unknown name '{name}' at runtime."))
            }),
            ExprKind::Call { name, args } => {
                let (values, arg_count) = self.eval_args_with_roots(args)?;
                let result = if let Some(value) = self.eval_builtin_call(name, &values)? {
                    Ok(value)
                } else if let Some((base, method)) = name.split_once("::") {
                    if let Some(base_value) = self.lookup(base).cloned() {
                        if let Value::Object { ref book, .. } = base_value {
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
                            self.eval_function(function, &call_values)
                        } else {
                            Err(runtime_error("E0400", "Method call on non-book."))
                        }
                    } else {
                        Err(runtime_error(
                            "E0400",
                            format!("Unknown name '{base}' at runtime."),
                        ))
                    }
                } else {
                    let function = *self.functions.get(name).ok_or_else(|| {
                        runtime_error("E0400", format!("Unknown function '{name}' at runtime."))
                    })?;
                    self.eval_function(function, &values)
                };
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
            ExprKind::Unary { op, expr } => {
                let value = self.eval_expr(expr)?;
                match (op, value) {
                    (UnaryOp::Neg, Value::I64(value)) => Ok(Value::I64(-value)),
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

    fn eval_args_with_roots(
        &mut self,
        args: &[Expr],
    ) -> Result<(Vec<Value>, usize), RuntimeError> {
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

    fn eval_array_literal(
        &mut self,
        elements: &[Expr],
        elem_ty: Option<&Type>,
    ) -> Result<Value, RuntimeError> {
        if elements.is_empty() {
            let Some(elem_ty) = elem_ty else {
                return Err(runtime_error(
                    "E0400",
                    "array literal requires explicit array type",
                ));
            };
            return self.alloc_array(elem_ty, Vec::new());
        }
        let frame_len = elements.len();
        let base = self.roots.push_frame(frame_len);
        let mut values = Vec::with_capacity(frame_len);
        if let Some(elem_ty) = elem_ty {
            for (index, element) in elements.iter().enumerate() {
                let value = match self.eval_expr(element) {
                    Ok(value) => value,
                    Err(err) => {
                        self.roots.pop_frame(frame_len);
                        return Err(err);
                    }
                };
                let value = match coerce_value(value, elem_ty) {
                    Ok(value) => value,
                    Err(err) => {
                        self.roots.pop_frame(frame_len);
                        return Err(err);
                    }
                };
                self.update_root_slot(base + index, &value);
                values.push(value);
            }
            let result = self.alloc_array(elem_ty, values);
            self.roots.pop_frame(frame_len);
            return result;
        }
        for (index, element) in elements.iter().enumerate() {
            let value = match self.eval_expr(element) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(frame_len);
                    return Err(err);
                }
            };
            self.update_root_slot(base + index, &value);
            values.push(value);
        }
        let elem_type = value_type(&values[0])?;
        for value in values.iter().skip(1) {
            let ty = value_type(value)?;
            if ty != elem_type {
                self.roots.pop_frame(frame_len);
                return Err(runtime_error(
                    "E0400",
                    "array literal elements must have the same type",
                ));
            }
        }
        let result = self.alloc_array(&elem_type, values);
        self.roots.pop_frame(frame_len);
        result
    }

    fn eval_short_circuit(
        &mut self,
        op: BinaryOp,
        left: Value,
        right: &Expr,
    ) -> Result<Value, RuntimeError> {
        match (op, left) {
            (BinaryOp::AndAnd, Value::Bool(false)) => Ok(Value::Bool(false)),
            (BinaryOp::AndAnd, Value::Bool(true)) => self.eval_expr(right),
            (BinaryOp::OrOr, Value::Bool(true)) => Ok(Value::Bool(true)),
            (BinaryOp::OrOr, Value::Bool(false)) => self.eval_expr(right),
            _ => Err(runtime_error("E0400", "invalid logical operation")),
        }
    }

    fn eval_binary(
        &self,
        op: BinaryOp,
        left: Value,
        right: Value,
    ) -> Result<Value, RuntimeError> {
        match (op, left, right) {
            (BinaryOp::Add, Value::I64(left), Value::I64(right)) => {
                Ok(Value::I64(left + right))
            }
            (BinaryOp::Sub, Value::I64(left), Value::I64(right)) => {
                Ok(Value::I64(left - right))
            }
            (BinaryOp::Mul, Value::I64(left), Value::I64(right)) => {
                Ok(Value::I64(left * right))
            }
            (BinaryOp::Div, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "division by zero"))
            }
            (BinaryOp::Mod, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "modulo by zero"))
            }
            (BinaryOp::Div, Value::I64(left), Value::I64(right)) => {
                Ok(Value::I64(left / right))
            }
            (BinaryOp::Mod, Value::I64(left), Value::I64(right)) => {
                Ok(Value::I64(left % right))
            }
            (BinaryOp::EqEq, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left == right))
            }
            (BinaryOp::NotEq, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left != right))
            }
            (BinaryOp::Lt, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left < right))
            }
            (BinaryOp::LtEq, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left <= right))
            }
            (BinaryOp::Gt, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left > right))
            }
            (BinaryOp::GtEq, Value::I64(left), Value::I64(right)) => {
                Ok(Value::Bool(left >= right))
            }
            (BinaryOp::EqEq, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left == right))
            }
            (BinaryOp::NotEq, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left != right))
            }
            (BinaryOp::AndAnd, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left && right))
            }
            (BinaryOp::OrOr, Value::Bool(left), Value::Bool(right)) => {
                Ok(Value::Bool(left || right))
            }
            _ => Err(runtime_error("E0400", "invalid binary operation")),
        }
    }

    fn eval_index_value(&mut self, index: &Expr) -> Result<i64, RuntimeError> {
        match self.eval_expr(index)? {
            Value::I64(value) => Ok(value),
            _ => Err(runtime_error("E0400", "index must be i64")),
        }
    }

    fn eval_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<Value, RuntimeError> {
        let index = self.eval_index_value(index)?;
        let value = self.eval_expr(base)?;
        match value {
            Value::Array { handle, elem_type } => self.read_array_elem(handle, &elem_type, index),
            _ => Err(runtime_error("E0400", "Indexing on non-array.")),
        }
    }

    fn eval_array_new(&mut self, len: &Expr, elem_ty: &Type) -> Result<Value, RuntimeError> {
        let len = self.eval_index_value(len)?;
        if len < 0 {
            return Err(runtime_error("E0400", "array length must be >= 0"));
        }
        let len = len as usize;
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let elem_size = elem_size(elem_kind);
        self.maybe_collect();
        let handle = self.heap.alloc_array(elem_kind, len, elem_size);
        let root_base = self.roots.push_frame(1);
        self.roots.set_slot(root_base, RootValue::Ptr(handle));
        for index in 0..len {
            let value = match self.default_value(elem_ty) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(1);
                    return Err(err);
                }
            };
            if let Err(err) = self.write_array_elem(handle, elem_ty, index as i64, value) {
                self.roots.pop_frame(1);
                return Err(err);
            }
        }
        self.roots.pop_frame(1);
        Ok(Value::Array {
            handle,
            elem_type: elem_ty.clone(),
        })
    }

    fn alloc_object(&mut self, book: &str) -> Result<Value, RuntimeError> {
        let Some(info) = self.books.get(book) else {
            return Err(runtime_error(
                "E0400",
                format!("Unknown book '{book}' at runtime."),
            ));
        };
        let book_id = info.id;
        let field_types = info.field_types.clone();
        self.maybe_collect();
        let handle = self.heap.alloc_object(book_id, field_types.len());
        let root_base = self.roots.push_frame(1);
        self.roots.set_slot(root_base, RootValue::Ptr(handle));
        for (index, field_ty) in field_types.iter().enumerate() {
            let value = match self.default_value(field_ty) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(1);
                    return Err(err);
                }
            };
            if let Err(err) = self.write_object_field(handle, index, field_ty, value) {
                self.roots.pop_frame(1);
                return Err(err);
            }
        }
        self.roots.pop_frame(1);
        Ok(Value::Object {
            handle,
            book: book.to_string(),
        })
    }

    fn default_value(&mut self, ty: &Type) -> Result<Value, RuntimeError> {
        match ty {
            Type::I64 => Ok(Value::I64(0)),
            Type::Bool => Ok(Value::Bool(false)),
            Type::String => Ok(self.alloc_string("")),
            Type::U8 => Ok(Value::U8(0)),
            Type::Array(inner) => self.alloc_array(&*inner.clone(), Vec::new()),
            Type::Book(name) => self.alloc_object(name),
        }
    }

    pub(crate) fn alloc_string(&mut self, text: &str) -> Value {
        let bytes = text.as_bytes();
        self.alloc_string_from_bytes(bytes)
    }

    pub(crate) fn alloc_string_from_bytes(&mut self, bytes: &[u8]) -> Value {
        self.maybe_collect();
        let handle = self.heap.alloc_string(bytes.len());
        let payload = self.heap.payload_mut(handle);
        if let Some(target) = payload.get_mut(..bytes.len()) {
            target.copy_from_slice(bytes);
        }
        Value::String(handle)
    }

    pub(crate) fn string_len(&self, handle: HeapHandle) -> Result<usize, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::String {
            return Err(runtime_error("E0400", "Expected string handle."));
        }
        Ok(header.len_or_size as usize)
    }

    pub(crate) fn string_bytes(&self, handle: HeapHandle) -> Result<Vec<u8>, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::String {
            return Err(runtime_error("E0400", "Expected string handle."));
        }
        let len = header.len_or_size as usize;
        let payload = self.heap.payload(handle);
        let bytes = payload.get(..len).ok_or_else(|| {
            runtime_error("E0400", "String payload out of bounds.")
        })?;
        Ok(bytes.to_vec())
    }

    pub(crate) fn string_text(&self, handle: HeapHandle) -> Result<String, RuntimeError> {
        let bytes = self.string_bytes(handle)?;
        String::from_utf8(bytes)
            .map_err(|_| runtime_error("E0400", "Invalid UTF-8 in string value."))
    }

    pub(crate) fn alloc_u8_array(&mut self, bytes: &[u8]) -> Value {
        self.maybe_collect();
        let handle = self
            .heap
            .alloc_array(ElemKind::U8, bytes.len(), 1);
        let payload = self.heap.payload_mut(handle);
        if let Some(target) = payload.get_mut(..bytes.len()) {
            target.copy_from_slice(bytes);
        }
        Value::Array {
            handle,
            elem_type: Type::U8,
        }
    }

    pub(crate) fn read_u8_array(
        &self,
        handle: HeapHandle,
        elem_type: &Type,
    ) -> Result<Vec<u8>, RuntimeError> {
        if *elem_type != Type::U8 {
            return Err(runtime_error("E0400", "std::bytes expects u8 array."));
        }
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Array || header.aux != ElemKind::U8 as u32 {
            return Err(runtime_error("E0400", "std::bytes expects u8 array."));
        }
        let len = header.len_or_size as usize;
        let payload = self.heap.payload(handle);
        let bytes = payload.get(..len).ok_or_else(|| {
            runtime_error("E0400", "Array payload out of bounds.")
        })?;
        Ok(bytes.to_vec())
    }

    fn alloc_array(&mut self, elem_ty: &Type, elements: Vec<Value>) -> Result<Value, RuntimeError> {
        self.maybe_collect();
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let elem_size = elem_size(elem_kind);
        let handle = self.heap.alloc_array(elem_kind, elements.len(), elem_size);
        let root_base = self.roots.push_frame(1);
        self.roots.set_slot(root_base, RootValue::Ptr(handle));
        for (index, value) in elements.into_iter().enumerate() {
            if let Err(err) = self.write_array_elem(handle, elem_ty, index as i64, value) {
                self.roots.pop_frame(1);
                return Err(err);
            }
        }
        self.roots.pop_frame(1);
        Ok(Value::Array {
            handle,
            elem_type: elem_ty.clone(),
        })
    }

    pub(crate) fn array_len(&self, handle: HeapHandle) -> Result<usize, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Array {
            return Err(runtime_error("E0400", "Expected array handle."));
        }
        Ok(header.len_or_size as usize)
    }

    fn read_array_elem(
        &self,
        handle: HeapHandle,
        elem_ty: &Type,
        index: i64,
    ) -> Result<Value, RuntimeError> {
        if index < 0 {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let index = index as usize;
        let len = self.array_len(handle)?;
        if index >= len {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let header = self.heap.header(handle);
        if header.aux != elem_kind as u32 {
            return Err(runtime_error("E0400", "Array element type mismatch."));
        }
        let payload = self.heap.payload(handle);
        let offset = index * elem_size(elem_kind);
        let value = match elem_kind {
            ElemKind::I64 => {
                let bytes = payload.get(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                Value::I64(i64::from_le_bytes(bytes.try_into().unwrap()))
            }
            ElemKind::Bool => {
                let byte = *payload.get(offset).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                Value::Bool(byte != 0)
            }
            ElemKind::U8 => {
                let byte = *payload.get(offset).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                Value::U8(byte)
            }
            ElemKind::Ref => {
                let bytes = payload.get(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                let raw = u64::from_le_bytes(bytes.try_into().unwrap());
                let handle = HeapHandle::from_u32(raw as u32);
                self.value_from_handle(handle, elem_ty)?
            }
        };
        Ok(value)
    }

    fn write_array_elem(
        &mut self,
        handle: HeapHandle,
        elem_ty: &Type,
        index: i64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        if index < 0 {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let index = index as usize;
        let len = self.array_len(handle)?;
        if index >= len {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let header = self.heap.header(handle);
        if header.aux != elem_kind as u32 {
            return Err(runtime_error("E0400", "Array element type mismatch."));
        }
        let value = coerce_value(value, elem_ty)?;
        let payload = self.heap.payload_mut(handle);
        let offset = index * elem_size(elem_kind);
        match (elem_kind, value) {
            (ElemKind::I64, Value::I64(value)) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                target.copy_from_slice(&value.to_le_bytes());
            }
            (ElemKind::Bool, Value::Bool(value)) => {
                let slot = payload.get_mut(offset).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                *slot = if value { 1 } else { 0 };
            }
            (ElemKind::U8, Value::U8(value)) => {
                let slot = payload.get_mut(offset).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                *slot = value;
            }
            (ElemKind::Ref, value) => {
                let handle = value
                    .heap_handle()
                    .ok_or_else(|| runtime_error("E0400", "Expected reference value."))?;
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Array payload out of bounds.")
                })?;
                target.copy_from_slice(&(handle.as_u32() as u64).to_le_bytes());
            }
            _ => {
                return Err(runtime_error("E0400", "Array element type mismatch."));
            }
        }
        Ok(())
    }

    fn read_object_field(
        &self,
        handle: HeapHandle,
        field_ty: &Type,
        index: usize,
    ) -> Result<Value, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Object {
            return Err(runtime_error("E0400", "Expected book handle."));
        }
        let field_count = header.len_or_size as usize;
        if index >= field_count {
            return Err(runtime_error("E0400", "Field index out of bounds."));
        }
        let payload = self.heap.payload(handle);
        let offset = index * 8;
        let bytes = payload.get(offset..offset + 8).ok_or_else(|| {
            runtime_error("E0400", "Object payload out of bounds.")
        })?;
        let raw = u64::from_le_bytes(bytes.try_into().unwrap());
        match field_ty {
            Type::I64 => Ok(Value::I64(i64::from_le_bytes(bytes.try_into().unwrap()))),
            Type::Bool => Ok(Value::Bool(raw != 0)),
            Type::U8 => Ok(Value::U8(raw as u8)),
            Type::String | Type::Array(_) | Type::Book(_) => {
                self.value_from_handle(HeapHandle::from_u32(raw as u32), field_ty)
            }
        }
    }

    fn write_object_field(
        &mut self,
        handle: HeapHandle,
        index: usize,
        field_ty: &Type,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Object {
            return Err(runtime_error("E0400", "Expected book handle."));
        }
        let field_count = header.len_or_size as usize;
        if index >= field_count {
            return Err(runtime_error("E0400", "Field index out of bounds."));
        }
        let value = coerce_value(value, field_ty)?;
        let payload = self.heap.payload_mut(handle);
        let offset = index * 8;
        match value {
            Value::I64(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&value.to_le_bytes());
            }
            Value::Bool(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(value as u64).to_le_bytes());
            }
            Value::U8(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(value as u64).to_le_bytes());
            }
            Value::String(handle) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(handle.as_u32() as u64).to_le_bytes());
            }
            Value::Array { handle, .. } | Value::Object { handle, .. } => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(handle.as_u32() as u64).to_le_bytes());
            }
        }
        Ok(())
    }

    fn value_from_handle(&self, handle: HeapHandle, ty: &Type) -> Result<Value, RuntimeError> {
        match ty {
            Type::String => Ok(Value::String(handle)),
            Type::Array(inner) => Ok(Value::Array {
                handle,
                elem_type: (*inner.clone()),
            }),
            Type::Book(book) => Ok(Value::Object {
                handle,
                book: book.clone(),
            }),
            _ => Err(runtime_error("E0400", "Expected reference type.")),
        }
    }

    fn bind_local(&mut self, name: String, value: Value) {
        let existing_slot = self
            .scopes
            .last()
            .and_then(|scope| scope.roots.get(&name).cloned());
        let slot = if let Some(slot) = existing_slot {
            slot
        } else {
            let slot = self.roots.extend_frame(1);
            if let Some(scope) = self.scopes.last_mut() {
                scope.roots.insert(name.clone(), slot);
            }
            slot
        };
        if let Some(scope) = self.scopes.last_mut() {
            scope.values.insert(name, value.clone());
        }
        self.update_root_slot(slot, &value);
    }

    fn assign_var(&mut self, name: &str, value: Value) -> Result<(), RuntimeError> {
        let mut updated: Option<(usize, Value)> = None;
        let mut found = false;
        for scope in self.scopes.iter_mut().rev() {
            if let Some(existing) = scope.values.get_mut(name) {
                let expected = value_type(existing)?;
                let coerced = coerce_value(value.clone(), &expected)?;
                *existing = coerced.clone();
                found = true;
                if let Some(slot) = scope.roots.get(name).cloned() {
                    updated = Some((slot, coerced));
                }
                break;
            }
        }
        if let Some((slot, value)) = updated {
            self.update_root_slot(slot, &value);
        }
        if found {
            return Ok(());
        }
        Err(runtime_error(
            "E0400",
            format!("Unknown name '{name}' at runtime."),
        ))
    }

    fn update_root_slot(&mut self, slot: usize, value: &Value) {
        let root_value = match value.heap_handle() {
            Some(handle) => RootValue::Ptr(handle),
            None => RootValue::Null,
        };
        self.roots.set_slot(slot, root_value);
    }

    fn maybe_collect(&mut self) {
        let stats = self.heap.stats();
        if stats.bytes_in_use < self.gc_threshold {
            return;
        }
        let report = self.heap.gc_with_layout(&self.roots, &self.gc_layout);
        let next = report
            .live_bytes
            .saturating_mul(2)
            .max(GC_MIN_THRESHOLD);
        self.gc_threshold = next;
    }

    fn push_scope(&mut self) {
        self.scopes.push(Scope::new());
        self.roots.push_frame(0);
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
        self.roots.pop_frame_auto();
    }

    pub(crate) fn push_output(&mut self, value: &str) {
        self.output.push_str(value);
    }

    pub(crate) fn read_input_line(&mut self) -> String {
        self.input.pop_front().unwrap_or_default()
    }

    fn lookup(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.values.get(name) {
                return Some(value);
            }
        }
        None
    }
}

fn split_lines(input: &str) -> VecDeque<String> {
    if input.is_empty() {
        return VecDeque::new();
    }
    input.split('\n').map(|line| line.to_string()).collect()
}

const GC_MIN_THRESHOLD: usize = 1024 * 64;

fn is_ref_type(ty: &Type) -> bool {
    matches!(ty, Type::String | Type::Array(_) | Type::Book(_))
}

fn elem_kind_for_type(ty: &Type) -> Result<ElemKind, RuntimeError> {
    match ty {
        Type::I64 => Ok(ElemKind::I64),
        Type::Bool => Ok(ElemKind::Bool),
        Type::U8 => Ok(ElemKind::U8),
        Type::String | Type::Array(_) | Type::Book(_) => Ok(ElemKind::Ref),
    }
}

fn elem_size(kind: ElemKind) -> usize {
    match kind {
        ElemKind::I64 => 8,
        ElemKind::Bool => 1,
        ElemKind::U8 => 1,
        ElemKind::Ref => 8,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser};

    fn run_with_gc(source: &str, threshold: usize) -> (i64, usize) {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let mut vm = Vm::new(&program, "");
        vm.gc_threshold = threshold;
        let result = vm.eval_main().unwrap();
        let gc_runs = vm.heap.stats().gc_runs;
        (result, gc_runs)
    }

    #[test]
    fn gc_preserves_object_cycles() {
        let source = "book Node:\n  field link: Node[].\n  field value: i64.\n\n  rule init(self: Node, value: i64) -> Node:\n    put self::value = value.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set a: Node = new Node(1).\n  set once: i64 = 0.\n  repeat while once < 1:\n    set b: Node = new Node(2).\n    set al: Node[] = [b].\n    set bl: Node[] = [a].\n    put a::link = al.\n    put b::link = bl.\n    put once = once + 1.\n  end\n\n  set i: i64 = 0.\n  repeat while i < 6:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n\n  set nexts: Node[] = a::link.\n  set first: Node = nexts[0].\n  yield first::value.\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 2);
        assert!(gc_runs > 0);
    }

    #[test]
    fn gc_preserves_nested_arrays_in_objects() {
        let source = "book Holder:\n  field grid: i64[][].\n\n  rule init(self: Holder) -> Holder:\n    set row1: i64[] = [1, 2].\n    set row2: i64[] = [3, 4].\n    set grid: i64[][] = [row1, row2].\n    put self::grid = grid.\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  set holder: Holder = new Holder().\n  set i: i64 = 0.\n  repeat while i < 6:\n    set junk: i64[] = array(2048).\n    put junk[0] = i.\n    put i = i + 1.\n  end\n  set grid: i64[][] = holder::grid.\n  yield grid[1][0].\nend\n";
        let (result, gc_runs) = run_with_gc(source, 1024);
        assert_eq!(result, 3);
        assert!(gc_runs > 0);
    }
}
