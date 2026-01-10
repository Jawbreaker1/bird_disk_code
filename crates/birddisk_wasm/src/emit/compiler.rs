mod expr;
mod helpers;

use super::types::{array_elem_size, wat_type};
use super::{wasm_error, BookLayout, FunctionSig, WasmError};
use birddisk_core::ast::{Function, Stmt, Type};
use std::collections::HashMap;

pub(super) fn emit_function(
    emitter: &mut WatEmitter,
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
    books: &HashMap<String, BookLayout>,
    frame_id: i32,
    name_override: Option<&str>,
) -> Result<(), WasmError> {
    let func_name = name_override.unwrap_or(&func.name);
    let mut signature = String::new();
    for param in &func.params {
        signature.push_str(&format!(" (param {})", wat_type(&param.ty)));
    }
    signature.push_str(&format!(" (result {})", wat_type(&func.return_type)));

    emitter.push_line(format!("(func ${}{}", func_name, signature));
    emitter.indent();

    let mut compiler = FuncCompiler::new(func, functions, books, frame_id);
    compiler.emit_body()?;
    compiler.insert_root_prologue();

    for local in &compiler.locals {
        emitter.push_line(format!("(local {})", wat_type(local)));
    }

    for line in &compiler.code {
        emitter.push_line(line);
    }

    emitter.dedent();
    emitter.push_line(")");
    Ok(())
}

pub(crate) struct WatEmitter {
    lines: Vec<String>,
    indent: usize,
}

impl WatEmitter {
    pub(crate) fn new() -> Self {
        Self {
            lines: Vec::new(),
            indent: 0,
        }
    }

    pub(crate) fn push_line(&mut self, line: impl AsRef<str>) {
        let prefix = "  ".repeat(self.indent);
        self.lines.push(format!("{prefix}{}", line.as_ref()));
    }

    pub(crate) fn indent(&mut self) {
        self.indent += 1;
    }

    pub(crate) fn dedent(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }

    pub(crate) fn finish(self) -> String {
        self.lines.join("\n")
    }
}

#[derive(Clone)]
struct LocalInfo {
    idx: u32,
    ty: Type,
}

struct FuncCompiler<'a> {
    func: &'a Function,
    functions: &'a HashMap<String, FunctionSig>,
    books: &'a HashMap<String, BookLayout>,
    frame_id: i32,
    root_base_local: u32,
    root_count_local: u32,
    locals: Vec<Type>,
    scopes: Vec<HashMap<String, LocalInfo>>,
    code: Vec<String>,
    indent: usize,
    label_counter: usize,
}

impl<'a> FuncCompiler<'a> {
    fn new(
        func: &'a Function,
        functions: &'a HashMap<String, FunctionSig>,
        books: &'a HashMap<String, BookLayout>,
        frame_id: i32,
    ) -> Self {
        let mut compiler = Self {
            func,
            functions,
            books,
            frame_id,
            root_base_local: 0,
            root_count_local: 0,
            locals: Vec::new(),
            scopes: Vec::new(),
            code: Vec::new(),
            indent: 0,
            label_counter: 0,
        };
        compiler.push_scope();
        for (idx, param) in func.params.iter().enumerate() {
            compiler.current_scope_mut().insert(
                param.name.clone(),
                LocalInfo {
                    idx: idx as u32,
                    ty: param.ty.clone(),
                },
            );
        }
        compiler.root_base_local = compiler.reserve_local(Type::Bool);
        compiler.root_count_local = compiler.reserve_local(Type::Bool);
        compiler
    }

    fn emit_body(&mut self) -> Result<(), WasmError> {
        self.emit_trace_push();
        for stmt in &self.func.body {
            self.emit_stmt(stmt)?;
        }
        self.emit_default_return();
        Ok(())
    }

    fn emit_trace_push(&mut self) {
        self.push_line(format!("i32.const {}", self.frame_id));
        self.push_line("call $bd_trace_push");
    }

    fn emit_trace_pop(&mut self) {
        self.push_line("call $bd_trace_pop");
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<(), WasmError> {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                let inferred = match ty {
                    Some(ty) => ty.clone(),
                    None => self.infer_expr_type(expr)?,
                };
                let idx = self.reserve_local(inferred.clone());
                self.emit_expr(expr, Some(&inferred))?;
                self.emit_local_set(idx, &inferred);
                self.bind_local(name, idx, inferred);
            }
            Stmt::Put { name, expr, .. } => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.emit_expr(expr, Some(&info.ty))?;
                self.emit_local_set(info.idx, &info.ty);
            }
            Stmt::PutIndex {
                name,
                index,
                expr,
                ..
            } => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                let elem_ty = match &info.ty {
                    Type::Array(elem) => elem.as_ref().clone(),
                    _ => {
                        return Err(wasm_error(
                            "E0400",
                            "Index assignment requires array type.",
                        ))
                    }
                };
                let idx_local = self.temp_local(Type::I64);
                self.emit_expr(index, None)?;
                self.push_line(format!("local.set {idx_local}"));
                let val_local = self.temp_local(elem_ty.clone());
                self.emit_expr(expr, Some(&elem_ty))?;
                self.emit_local_set(val_local, &elem_ty);
                self.emit_bounds_check(info.idx, idx_local)?;
                self.emit_array_address_index(info.idx, idx_local, array_elem_size(&elem_ty)?);
                self.push_line(format!("local.get {val_local}"));
                self.emit_store(&elem_ty);
            }
            Stmt::PutField {
                base,
                field,
                expr,
                ..
            } => {
                let info = self
                    .lookup(base)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{base}'")))?;
                let Type::Book(book_name) = &info.ty else {
                    return Err(wasm_error("E0400", "Field assignment requires book type."));
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
                let val_local = self.temp_local(field_ty.clone());
                self.emit_expr(expr, Some(field_ty))?;
                self.emit_local_set(val_local, field_ty);
                self.emit_field_address(info.idx, *index);
                self.push_line(format!("local.get {val_local}"));
                self.emit_field_store(field_ty);
            }
            Stmt::Yield { expr, .. } => {
                let ret_local = self.temp_local(self.func.return_type.clone());
                self.emit_expr(expr, Some(&self.func.return_type))?;
                self.emit_local_set(ret_local, &self.func.return_type);
                self.emit_root_pop();
                self.emit_trace_pop();
                self.push_line(format!("local.get {ret_local}"));
                self.push_line("return");
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                self.emit_expr(cond, Some(&Type::Bool))?;
                self.push_line("if");
                self.indent += 1;
                self.push_scope();
                for stmt in then_body {
                    self.emit_stmt(stmt)?;
                }
                self.pop_scope();
                self.indent -= 1;
                self.push_line("else");
                self.indent += 1;
                self.push_scope();
                for stmt in else_body {
                    self.emit_stmt(stmt)?;
                }
                self.pop_scope();
                self.indent -= 1;
                self.push_line("end");
            }
            Stmt::Repeat { cond, body, .. } => {
                let exit_label = self.fresh_label("exit");
                let loop_label = self.fresh_label("loop");
                self.push_line(format!("block ${exit_label}"));
                self.indent += 1;
                self.push_line(format!("loop ${loop_label}"));
                self.indent += 1;
                self.emit_expr(cond, Some(&Type::Bool))?;
                self.push_line("i32.eqz");
                self.push_line(format!("br_if ${exit_label}"));
                self.push_scope();
                for stmt in body {
                    self.emit_stmt(stmt)?;
                }
                self.pop_scope();
                self.push_line(format!("br ${loop_label}"));
                self.indent -= 1;
                self.push_line("end");
                self.indent -= 1;
                self.push_line("end");
            }
        }
        Ok(())
    }

    fn emit_default_return(&mut self) {
        let ret_local = self.temp_local(self.func.return_type.clone());
        match &self.func.return_type {
            Type::I64 => self.push_line("i64.const 0"),
            Type::Bool | Type::String | Type::U8 | Type::Array(_) | Type::Book(_) => {
                self.push_line("i32.const 0")
            }
        }
        self.emit_local_set(ret_local, &self.func.return_type);
        self.emit_root_pop();
        self.emit_trace_pop();
        self.push_line(format!("local.get {ret_local}"));
        self.push_line("return");
    }

    fn temp_local(&mut self, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn reserve_local(&mut self, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn emit_local_set(&mut self, idx: u32, ty: &Type) {
        self.push_line(format!("local.set {idx}"));
        if is_ref_type(ty) {
            self.emit_root_set(idx);
        }
    }

    fn emit_root_set(&mut self, idx: u32) {
        self.push_line(format!("local.get {}", self.root_base_local));
        self.push_line(format!("i32.const {idx}"));
        self.push_line("i32.add");
        self.push_line(format!("local.get {idx}"));
        self.push_line("call $bd_root_set");
    }

    fn emit_root_pop(&mut self) {
        self.push_line(format!("local.get {}", self.root_count_local));
        self.push_line("call $bd_root_pop");
    }

    fn bind_local(&mut self, name: &str, idx: u32, ty: Type) {
        self.current_scope_mut().insert(
            name.to_string(),
            LocalInfo {
                idx,
                ty,
            },
        );
    }

    fn lookup(&self, name: &str) -> Option<LocalInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(info.clone());
            }
        }
        None
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope_mut(&mut self) -> &mut HashMap<String, LocalInfo> {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
    }

    fn push_line(&mut self, line: impl AsRef<str>) {
        let prefix = "  ".repeat(self.indent);
        self.code.push(format!("{prefix}{}", line.as_ref()));
    }

    fn insert_root_prologue(&mut self) {
        let total_slots = (self.func.params.len() + self.locals.len()) as u32;
        let mut prologue = Vec::new();
        prologue.push(format!("i32.const {total_slots}"));
        prologue.push(format!("local.set {}", self.root_count_local));
        prologue.push(format!("i32.const {total_slots}"));
        prologue.push("call $bd_root_push".to_string());
        prologue.push(format!("local.set {}", self.root_base_local));
        for slot in 0..total_slots {
            prologue.push(format!("local.get {}", self.root_base_local));
            prologue.push(format!("i32.const {slot}"));
            prologue.push("i32.add".to_string());
            prologue.push("i32.const 0".to_string());
            prologue.push("call $bd_root_set".to_string());
        }
        for (idx, param) in self.func.params.iter().enumerate() {
            if is_ref_type(&param.ty) {
                prologue.push(format!("local.get {}", self.root_base_local));
                prologue.push(format!("i32.const {idx}"));
                prologue.push("i32.add".to_string());
                prologue.push(format!("local.get {idx}"));
                prologue.push("call $bd_root_set".to_string());
            }
        }
        self.code.splice(0..0, prologue);
    }

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{prefix}{}", self.label_counter);
        self.label_counter += 1;
        label
    }
}

fn is_ref_type(ty: &Type) -> bool {
    matches!(ty, Type::String | Type::Array(_) | Type::Book(_))
}
