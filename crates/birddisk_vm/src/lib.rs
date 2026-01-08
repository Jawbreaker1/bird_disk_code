//! BirdDisk VM interpreter (placeholder).

use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Program, Stmt, Type, UnaryOp, Value};
use birddisk_core::TraceFrame;
use std::collections::{HashMap, VecDeque};

#[derive(Debug, Clone)]
pub struct RuntimeError {
    pub code: &'static str,
    pub message: String,
    pub trace: Vec<TraceFrame>,
}

fn runtime_error(code: &'static str, message: impl Into<String>) -> RuntimeError {
    RuntimeError {
        code,
        message: message.into(),
        trace: Vec::new(),
    }
}

pub fn eval(program: &Program) -> Result<i64, RuntimeError> {
    let (result, _) = eval_with_io(program, "")?;
    Ok(result)
}

pub fn eval_with_io(program: &Program, input: &str) -> Result<(i64, String), RuntimeError> {
    let mut vm = Vm::new(program, input);
    let result = vm.eval_main()?;
    Ok((result, vm.output))
}

struct Vm<'a> {
    functions: HashMap<String, &'a birddisk_core::ast::Function>,
    books: HashMap<String, BookInfo>,
    scopes: Vec<HashMap<String, Value>>,
    input: VecDeque<String>,
    output: String,
    trace: Vec<TraceFrame>,
}

struct BookInfo {
    field_types: Vec<Type>,
    field_index: HashMap<String, usize>,
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
        for book in &program.books {
            let mut field_types = Vec::new();
            let mut field_index = HashMap::new();
            for (idx, field) in book.fields.iter().enumerate() {
                field_types.push(field.ty.clone());
                field_index.insert(field.name.clone(), idx);
            }
            books.insert(
                book.name.clone(),
                BookInfo {
                    field_types,
                    field_index,
                },
            );
        }
        Self {
            functions,
            books,
            scopes: Vec::new(),
            input: split_lines(input),
            output: String::new(),
            trace: Vec::new(),
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
            self.current_scope_mut()
                .insert(param.name.clone(), coerced);
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
                    ExprKind::ArrayLit(elements) if elements.is_empty() => {
                        let elem_ty = ty.as_ref().ok_or_else(|| {
                            runtime_error("E0400", "array literal requires explicit array type")
                        })?;
                        match elem_ty {
                            Type::Array(inner) => Value::Array {
                                elements: Vec::new(),
                                elem_type: (*inner.clone()),
                            },
                            _ => {
                                return Err(runtime_error(
                                    "E0400",
                                    "array literal requires array type",
                                ))
                            }
                        }
                    }
                    _ => self.eval_expr(expr)?,
                };
                let value = if let Some(expected) = ty {
                    coerce_value(value, expected)?
                } else {
                    value
                };
                self.current_scope_mut().insert(name.clone(), value);
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
                    ExprKind::ArrayLit(elements) if elements.is_empty() => {
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
                        Value::Array {
                            elements: Vec::new(),
                            elem_type: elem_ty,
                        }
                    }
                    _ => self.eval_expr(expr)?,
                };
                if let Some(existing) = self.lookup_mut(name) {
                    let expected = value_type(existing)?;
                    let value = coerce_value(value, &expected)?;
                    *existing = value;
                    Ok(None)
                } else {
                    Err(runtime_error(
                        "E0400",
                        format!("Unknown name '{name}' at runtime."),
                    ))
                }
            }
            Stmt::PutIndex {
                name,
                index,
                expr,
                ..
            } => {
                let idx = self.eval_index_value(index)?;
                let value = self.eval_expr(expr)?;
                if let Some(existing) = self.lookup_mut(name) {
                    match existing {
                        Value::Array { elements, elem_type } => {
                            if idx < 0 {
                                return Err(runtime_error("E0403", "Array index out of bounds."));
                            }
                            let idx = idx as usize;
                            if idx >= elements.len() {
                                return Err(runtime_error("E0403", "Array index out of bounds."));
                            }
                            let value = coerce_value(value, elem_type)?;
                            elements[idx] = value;
                            Ok(None)
                        }
                        _ => Err(runtime_error("E0400", "Index assignment on non-array.")),
                    }
                } else {
                    Err(runtime_error(
                        "E0400",
                        format!("Unknown name '{name}' at runtime."),
                    ))
                }
            }
            Stmt::PutField {
                base,
                field,
                expr,
                ..
            } => {
                let value = self.eval_expr(expr)?;
                let book_name = match self.lookup(base) {
                    Some(Value::Object { book, .. }) => book.clone(),
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
                let Some(target) = self.lookup_mut(base) else {
                    return Err(runtime_error(
                        "E0400",
                        format!("Unknown name '{base}' at runtime."),
                    ));
                };
                match target {
                    Value::Object { fields, .. } => {
                        fields[index] = coerce_value(value, &field_ty)?;
                        Ok(None)
                    }
                    _ => Err(runtime_error("E0400", "Field assignment on non-book.")),
                }
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

    fn eval_expr(&mut self, expr: &Expr) -> Result<Value, RuntimeError> {
        match &expr.kind {
            ExprKind::Int(value) => Ok(Value::I64(*value)),
            ExprKind::Bool(value) => Ok(Value::Bool(*value)),
            ExprKind::String(value) => Ok(Value::String(value.clone())),
            ExprKind::Ident(name) => self.lookup(name).cloned().ok_or_else(|| {
                runtime_error("E0400", format!("Unknown name '{name}' at runtime."))
            }),
            ExprKind::Call { name, args } => {
                if let Some(value) = self.eval_builtin_call(name, args)? {
                    return Ok(value);
                }
                if let Some((base, method)) = name.split_once("::") {
                    if let Some(base_value) = self.lookup(base).cloned() {
                        if let Value::Object { ref book, .. } = base_value {
                            let full_name = format!("{book}::{method}");
                            let function = *self.functions.get(&full_name).ok_or_else(|| {
                                runtime_error(
                                    "E0400",
                                    format!("Unknown function '{full_name}' at runtime."),
                                )
                            })?;
                            let mut values = Vec::new();
                            values.push(base_value);
                            for arg in args {
                                values.push(self.eval_expr(arg)?);
                            }
                            return self.eval_function(function, &values);
                        }
                    }
                }
                let function = *self.functions.get(name).ok_or_else(|| {
                    runtime_error("E0400", format!("Unknown function '{name}' at runtime."))
                })?;
                let mut values = Vec::new();
                for arg in args {
                    values.push(self.eval_expr(arg)?);
                }
                self.eval_function(function, &values)
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
                    Value::Object { book, fields } => {
                        let Some(book_info) = self.books.get(&book) else {
                            return Err(runtime_error("E0400", "Unknown book at runtime."));
                        };
                        let Some(index) = book_info.field_index.get(field) else {
                            return Err(runtime_error(
                                "E0400",
                                format!("Unknown field '{field}' at runtime."),
                            ));
                        };
                        Ok(fields[*index].clone())
                    }
                    _ => Err(runtime_error("E0400", "Field access on non-book.")),
                }
            }
            ExprKind::ArrayLit(elements) => {
                if elements.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "array literal requires explicit array type",
                    ));
                }
                let mut values = Vec::new();
                for element in elements {
                    values.push(self.eval_expr(element)?);
                }
                let elem_type = value_type(&values[0])?;
                for value in values.iter().skip(1) {
                    let ty = value_type(value)?;
                    if ty != elem_type {
                        return Err(runtime_error(
                            "E0400",
                            "array literal elements must have the same type",
                        ));
                    }
                }
                Ok(Value::Array {
                    elements: values,
                    elem_type,
                })
            }
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
                    _ => Err(runtime_error("E0400", "invalid unary operand")),
                }
            }
            ExprKind::Binary { left, op, right } => {
                let left_value = self.eval_expr(left)?;
                let right_value = self.eval_expr(right)?;
                self.eval_binary(*op, left_value, right_value)
            }
        }
    }

    fn eval_binary(&self, op: BinaryOp, left: Value, right: Value) -> Result<Value, RuntimeError> {
        match (op, left, right) {
            (BinaryOp::Add, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a + b)),
            (BinaryOp::Sub, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a - b)),
            (BinaryOp::Mul, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a * b)),
            (BinaryOp::Div, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "Division by zero."))
            }
            (BinaryOp::Div, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a / b)),
            (BinaryOp::Mod, Value::I64(_), Value::I64(0)) => {
                Err(runtime_error("E0402", "Modulo by zero."))
            }
            (BinaryOp::Mod, Value::I64(a), Value::I64(b)) => Ok(Value::I64(a % b)),
            (BinaryOp::EqEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a == b)),
            (BinaryOp::NotEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a != b)),
            (BinaryOp::Lt, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a < b)),
            (BinaryOp::LtEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a <= b)),
            (BinaryOp::Gt, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a > b)),
            (BinaryOp::GtEq, Value::I64(a), Value::I64(b)) => Ok(Value::Bool(a >= b)),
            (BinaryOp::AndAnd, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a && b)),
            (BinaryOp::OrOr, Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a || b)),
            _ => Err(runtime_error("E0400", "invalid binary operands")),
        }
    }

    fn eval_index_value(&mut self, index: &Expr) -> Result<i64, RuntimeError> {
        match self.eval_expr(index)? {
            Value::I64(value) => Ok(value),
            _ => Err(runtime_error("E0400", "array index must be i64")),
        }
    }

    fn eval_index_expr(&mut self, base: &Expr, index: &Expr) -> Result<Value, RuntimeError> {
        let idx = self.eval_index_value(index)?;
        let base_value = self.eval_expr(base)?;
        match base_value {
            Value::Array { elements, .. } => {
                if idx < 0 {
                    return Err(runtime_error("E0403", "Array index out of bounds."));
                }
                let idx = idx as usize;
                if idx >= elements.len() {
                    return Err(runtime_error("E0403", "Array index out of bounds."));
                }
                Ok(elements[idx].clone())
            }
            _ => Err(runtime_error("E0400", "indexing requires array")),
        }
    }

    fn eval_array_new(&mut self, len: &Expr, elem_ty: &Type) -> Result<Value, RuntimeError> {
        let len_value = self.eval_expr(len)?;
        let len = match len_value {
            Value::I64(value) => value,
            _ => return Err(runtime_error("E0400", "array length must be i64")),
        };
        if len < 0 {
            return Err(runtime_error("E0400", "array length must be >= 0"));
        }
        let mut elements = Vec::with_capacity(len as usize);
        let default = self.default_value(elem_ty)?;
        for _ in 0..len {
            elements.push(default.clone());
        }
        Ok(Value::Array {
            elements,
            elem_type: elem_ty.clone(),
        })
    }

    fn alloc_object(&self, book: &str) -> Result<Value, RuntimeError> {
        let Some(info) = self.books.get(book) else {
            return Err(runtime_error("E0400", format!("Unknown book '{book}'.")));
        };
        let mut fields = Vec::with_capacity(info.field_types.len());
        for field_ty in &info.field_types {
            fields.push(self.default_value(field_ty)?);
        }
        Ok(Value::Object {
            book: book.to_string(),
            fields,
        })
    }

    fn default_value(&self, ty: &Type) -> Result<Value, RuntimeError> {
        match ty {
            Type::I64 => Ok(Value::I64(0)),
            Type::Bool => Ok(Value::Bool(false)),
            Type::String => Ok(Value::String(String::new())),
            Type::U8 => Ok(Value::U8(0)),
            Type::Array(inner) => Ok(Value::Array {
                elements: Vec::new(),
                elem_type: (*inner.clone()),
            }),
            Type::Book(name) => self.alloc_object(name),
        }
    }

    fn eval_builtin_call(
        &mut self,
        name: &str,
        args: &[Expr],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::string::len" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::len expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                match value {
                    Value::String(value) => Ok(Some(Value::I64(value.len() as i64))),
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::len expects string argument",
                    )),
                }
            }
            "std::string::concat" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::concat expects 2 arguments",
                    ));
                }
                let left = self.eval_expr(&args[0])?;
                let right = self.eval_expr(&args[1])?;
                match (left, right) {
                    (Value::String(mut left), Value::String(right)) => {
                        left.push_str(&right);
                        Ok(Some(Value::String(left)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::concat expects string arguments",
                    )),
                }
            }
            "std::string::eq" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::eq expects 2 arguments",
                    ));
                }
                let left = self.eval_expr(&args[0])?;
                let right = self.eval_expr(&args[1])?;
                match (left, right) {
                    (Value::String(left), Value::String(right)) => {
                        Ok(Some(Value::Bool(left == right)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::eq expects string arguments",
                    )),
                }
            }
            "std::string::bytes" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::bytes expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                match value {
                    Value::String(value) => {
                        let elements = value
                            .into_bytes()
                            .into_iter()
                            .map(Value::U8)
                            .collect();
                        Ok(Some(Value::Array {
                            elements,
                            elem_type: Type::U8,
                        }))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::bytes expects string argument",
                    )),
                }
            }
            "std::bytes::len" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::len expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                let bytes = coerce_array_to_u8(value)?;
                Ok(Some(Value::I64(bytes.len() as i64)))
            }
            "std::bytes::eq" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::bytes::eq expects 2 arguments",
                    ));
                }
                let left = self.eval_expr(&args[0])?;
                let right = self.eval_expr(&args[1])?;
                let left = coerce_array_to_u8(left)?;
                let right = coerce_array_to_u8(right)?;
                Ok(Some(Value::Bool(left == right)))
            }
            "std::string::from_bytes" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::from_bytes expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                let bytes = coerce_array_to_u8(value)?;
                let text = String::from_utf8(bytes).map_err(|_| {
                    runtime_error(
                        "E0400",
                        "Invalid UTF-8 in std::string::from_bytes.",
                    )
                })?;
                Ok(Some(Value::String(text)))
            }
            "std::string::to_i64" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::to_i64 expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                match value {
                    Value::String(text) => {
                        let parsed = parse_string_i64(&text).ok_or_else(|| {
                            runtime_error("E0400", "Invalid integer in std::string::to_i64.")
                        })?;
                        Ok(Some(Value::I64(parsed)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::to_i64 expects string argument",
                    )),
                }
            }
            "std::string::from_i64" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::string::from_i64 expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                match value {
                    Value::I64(number) => Ok(Some(Value::String(number.to_string()))),
                    _ => Err(runtime_error(
                        "E0400",
                        "std::string::from_i64 expects i64 argument",
                    )),
                }
            }
            "std::io::print" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::io::print expects 1 argument",
                    ));
                }
                let value = self.eval_expr(&args[0])?;
                match value {
                    Value::String(value) => {
                        self.output.push_str(&value);
                        Ok(Some(Value::I64(value.len() as i64)))
                    }
                    _ => Err(runtime_error("E0400", "std::io::print expects string argument")),
                }
            }
            "std::io::read_line" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        "std::io::read_line expects 0 arguments",
                    ));
                }
                let line = self.input.pop_front().unwrap_or_default();
                Ok(Some(Value::String(line)))
            }
            _ => Ok(None),
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Value> {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
    }

    fn lookup(&self, name: &str) -> Option<&Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }

    fn lookup_mut(&mut self, name: &str) -> Option<&mut Value> {
        for scope in self.scopes.iter_mut().rev() {
            if let Some(value) = scope.get_mut(name) {
                return Some(value);
            }
        }
        None
    }
}

fn value_type(value: &Value) -> Result<Type, RuntimeError> {
    match value {
        Value::I64(_) => Ok(Type::I64),
        Value::Bool(_) => Ok(Type::Bool),
        Value::String(_) => Ok(Type::String),
        Value::U8(_) => Ok(Type::U8),
        Value::Array { elem_type, .. } => Ok(Type::Array(Box::new(elem_type.clone()))),
        Value::Object { book, .. } => Ok(Type::Book(book.clone())),
    }
}

fn coerce_value(value: Value, expected: &Type) -> Result<Value, RuntimeError> {
    match expected {
        Type::I64 => match value {
            Value::I64(value) => Ok(Value::I64(value)),
            _ => Err(runtime_error("E0400", "Expected i64 value.")),
        },
        Type::Bool => match value {
            Value::Bool(value) => Ok(Value::Bool(value)),
            _ => Err(runtime_error("E0400", "Expected bool value.")),
        },
        Type::String => match value {
            Value::String(value) => Ok(Value::String(value)),
            _ => Err(runtime_error("E0400", "Expected string value.")),
        },
        Type::U8 => match value {
            Value::U8(value) => Ok(Value::U8(value)),
            Value::I64(value) => {
                if (0..=u8::MAX as i64).contains(&value) {
                    Ok(Value::U8(value as u8))
                } else {
                    Err(runtime_error("E0400", "u8 value out of range."))
                }
            }
            _ => Err(runtime_error("E0400", "Expected u8 value.")),
        },
        Type::Array(expected_elem) => match value {
            Value::Array { elements, .. } => {
                let mut coerced = Vec::with_capacity(elements.len());
                for element in elements {
                    coerced.push(coerce_value(element, expected_elem.as_ref())?);
                }
                Ok(Value::Array {
                    elements: coerced,
                    elem_type: (*expected_elem.clone()),
                })
            }
            _ => Err(runtime_error("E0400", "Expected array value.")),
        },
        Type::Book(expected) => match value {
            Value::Object { book, fields } => {
                if &book == expected {
                    Ok(Value::Object { book, fields })
                } else {
                    Err(runtime_error("E0400", "Expected book value."))
                }
            }
            _ => Err(runtime_error("E0400", "Expected book value.")),
        },
    }
}

fn coerce_array_to_u8(value: Value) -> Result<Vec<u8>, RuntimeError> {
    match value {
        Value::Array { elements, .. } => {
            let mut bytes = Vec::with_capacity(elements.len());
            for element in elements {
                let byte = match element {
                    Value::U8(value) => value,
                    Value::I64(value) => {
                        if (0..=u8::MAX as i64).contains(&value) {
                            value as u8
                        } else {
                            return Err(runtime_error("E0400", "u8 value out of range."));
                        }
                    }
                    _ => {
                        return Err(runtime_error(
                            "E0400",
                            "std::bytes expects u8 array.",
                        ))
                    }
                };
                bytes.push(byte);
            }
            Ok(bytes)
        }
        _ => Err(runtime_error("E0400", "std::bytes expects u8 array.")),
    }
}

fn parse_string_i64(text: &str) -> Option<i64> {
    if text.is_empty() {
        return None;
    }
    let bytes = text.as_bytes();
    let mut idx = 0;
    let mut sign: i128 = 1;
    if bytes[0] == b'-' {
        sign = -1;
        idx = 1;
        if idx == bytes.len() {
            return None;
        }
    }
    let mut value: i128 = 0;
    while idx < bytes.len() {
        let ch = bytes[idx];
        if !(b'0'..=b'9').contains(&ch) {
            return None;
        }
        value = value * 10 + (ch - b'0') as i128;
        idx += 1;
    }
    value *= sign;
    if value < i64::MIN as i128 || value > i64::MAX as i128 {
        return None;
    }
    Some(value as i64)
}

fn split_lines(input: &str) -> VecDeque<String> {
    if input.is_empty() {
        return VecDeque::new();
    }
    input.split('\n').map(|line| line.to_string()).collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser, parse_and_typecheck};
    use std::path::PathBuf;

    fn eval_source(source: &str) -> i64 {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        eval(&program).unwrap()
    }

    fn fixture_path(rel: &str) -> PathBuf {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.pop();
        root.pop();
        root.push(rel);
        root
    }

    #[test]
    fn eval_minimal_main() {
        let result = eval_source("rule main() -> i64:\n  yield 0.\nend\n");
        assert_eq!(result, 0);
    }

    #[test]
    fn eval_when() {
        let result = eval_source(
            "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 2.\n  end\nend\n",
        );
        assert_eq!(result, 1);
    }

    #[test]
    fn eval_repeat() {
        let result = eval_source(
            "rule main() -> i64:\n  set i = 0.\n  set sum: i64 = 0.\n\n  repeat while i < 5:\n    put sum = sum + i.\n    put i = i + 1.\n  end\n\n  yield sum.\nend\n",
        );
        assert_eq!(result, 10);
    }

    #[test]
    fn eval_call() {
        let result = eval_source(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(2, 3).\nend\n",
        );
        assert_eq!(result, 5);
    }

    #[test]
    fn eval_div_by_zero_errors() {
        let tokens = lexer::lex("rule main() -> i64:\n  yield 1 / 0.\nend\n").unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
    }

    #[test]
    fn eval_rejects_wrong_arity() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
        assert!(err.message.contains("expected 2"));
    }

    #[test]
    fn eval_trace_includes_call_stack() {
        let source = "rule boom() -> i64:\n  yield 1 / 0.\nend\n\nrule main() -> i64:\n  yield boom().\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0402");
        assert!(err.trace.len() >= 2);
        assert_eq!(err.trace[0].function, "boom");
        assert_eq!(err.trace[1].function, "main");
    }

    #[test]
    fn eval_array_literal_index() {
        let result = eval_source(
            "rule main() -> i64:\n  set xs: i64[] = [1, 2, 3].\n  yield xs[1].\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_array_new_and_put_index() {
        let result = eval_source(
            "rule main() -> i64:\n  set xs: i64[] = array(3).\n  put xs[1] = 7.\n  yield xs[1].\nend\n",
        );
        assert_eq!(result, 7);
    }

    #[test]
    fn eval_array_index_out_of_bounds() {
        let tokens = lexer::lex("rule main() -> i64:\n  set xs: i64[] = [1].\n  yield xs[2].\nend\n").unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0403");
    }

    #[test]
    fn eval_array_out_of_bounds_fixture() {
        let path = fixture_path("vm_error_tests/arrays/array_index_out_of_bounds.bd");
        let program = parse_and_typecheck(path.to_str().unwrap()).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0403");
    }

    #[test]
    fn eval_string_len_concat() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  set base: string = \"hi\".\n  set joined: string = std::string::concat(base, \"!\").\n  yield std::string::len(joined).\nend\n",
        );
        assert_eq!(result, 3);
    }

    #[test]
    fn eval_bytes_len_and_eq() {
        let result = eval_source(
            "import std::string.\nimport std::bytes.\nrule main() -> i64:\n  set left: u8[] = std::string::bytes(\"hi\").\n  set right: u8[] = std::string::bytes(\"hi\").\n  when std::bytes::eq(left, right):\n    yield std::bytes::len(left).\n  otherwise:\n    yield 0.\n  end\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_u8_array_literal() {
        let result = eval_source(
            "import std::bytes.\nrule main() -> i64:\n  set data: u8[] = [65, 66, 67].\n  yield std::bytes::len(data).\nend\n",
        );
        assert_eq!(result, 3);
    }

    #[test]
    fn eval_string_from_bytes_roundtrip() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  set bytes: u8[] = std::string::bytes(\"hi\").\n  set text: string = std::string::from_bytes(bytes).\n  yield std::string::len(text).\nend\n",
        );
        assert_eq!(result, 2);
    }

    #[test]
    fn eval_string_from_bytes_rejects_invalid_utf8() {
        let tokens = lexer::lex(
            "import std::string.\nrule main() -> i64:\n  set bytes: u8[] = [195, 40].\n  set text: string = std::string::from_bytes(bytes).\n  yield std::string::len(text).\nend\n",
        )
        .unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
    }

    #[test]
    fn eval_string_to_i64() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  yield std::string::to_i64(\"123\").\nend\n",
        );
        assert_eq!(result, 123);
    }

    #[test]
    fn eval_string_from_i64() {
        let result = eval_source(
            "import std::string.\nrule main() -> i64:\n  set text: string = std::string::from_i64(-42).\n  yield std::string::len(text).\nend\n",
        );
        assert_eq!(result, 3);
    }

    #[test]
    fn eval_string_to_i64_rejects_invalid() {
        let tokens = lexer::lex(
            "import std::string.\nrule main() -> i64:\n  yield std::string::to_i64(\"12x\").\nend\n",
        )
        .unwrap();
        let program = parser::parse(&tokens).unwrap();
        let err = eval(&program).unwrap_err();
        assert_eq!(err.code, "E0400");
    }

    #[test]
    fn eval_io_print_and_read_line() {
        let source = "import std::io.\nimport std::string.\nrule main() -> i64:\n  set line: string = std::io::read_line().\n  set out: string = std::string::concat(line, \"!\").\n  yield std::io::print(out).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        let (result, output) = eval_with_io(&program, "BirdDisk").unwrap();
        assert_eq!(result, 9);
        assert_eq!(output, "BirdDisk!");
    }
}
