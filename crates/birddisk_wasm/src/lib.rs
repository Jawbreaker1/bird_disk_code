use birddisk_core::ast::{BinaryOp, Expr, ExprKind, Function, Program, Stmt, Type, UnaryOp};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub struct WasmError {
    pub code: &'static str,
    pub message: String,
}

fn wasm_error(code: &'static str, message: impl Into<String>) -> WasmError {
    WasmError {
        code,
        message: message.into(),
    }
}

const ARRAY_HEADER_SIZE: i32 = 8;
const TRAP_ARRAY_OOB: i32 = 403;
const TRAP_ARRAY_LEN_NEG: i32 = 400;
const TRAP_ARRAY_OOM: i32 = 405;

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
}

pub fn emit_wat(program: &Program) -> Result<String, WasmError> {
    let uses_arrays = program_has_arrays(program);
    let mut functions = HashMap::new();
    for func in &program.functions {
        functions.insert(
            func.name.clone(),
            FunctionSig {
                params: func.params.iter().map(|p| p.ty.clone()).collect(),
                return_type: func.return_type.clone(),
            },
        );
    }

    let mut emitter = WatEmitter::new();
    emitter.push_line("(module");
    emitter.indent();

    if uses_arrays {
        emit_array_runtime(&mut emitter);
    }

    for func in &program.functions {
        emit_function(&mut emitter, func, &functions)?;
    }

    if functions.contains_key("main") {
        emitter.push_line("(export \"main\" (func $main))");
    }

    emitter.dedent();
    emitter.push_line(")");

    Ok(emitter.finish())
}

pub fn emit_wasm(program: &Program) -> Result<Vec<u8>, WasmError> {
    let wat = emit_wat(program)?;
    wat::parse_str(&wat).map_err(|err| wasm_error("E0400", format!("WASM encode error: {err}")))
}

pub fn run(program: &Program) -> Result<i64, WasmError> {
    use wasmtime::{Engine, Linker, Module, Store};

    let uses_arrays = program_has_arrays(program);
    let main = program
        .functions
        .iter()
        .find(|func| func.name == "main")
        .ok_or_else(|| wasm_error("E0400", "missing main function"))?;
    if &main.return_type != &Type::I64 {
        return Err(wasm_error("E0400", "main must return i64"));
    }

    let wat = emit_wat(program)?;
    let engine = Engine::default();
    let module = Module::new(&engine, wat)
        .map_err(|err| wasm_error("E0400", format!("WASM compile error: {err}")))?;
    let mut store = Store::new(&engine, ());
    let mut linker = Linker::new(&engine);
    if uses_arrays {
        linker
            .func_wrap("env", "bd_trap", |code: i32| -> anyhow::Result<()> {
                Err(anyhow::anyhow!(format!("bd_trap:{code}")))
            })
            .map_err(|err| wasm_error("E0400", format!("WASM link error: {err}")))?;
    }
    let instance = linker
        .instantiate(&mut store, &module)
        .map_err(|err| map_trap(err, "WASM instantiation error"))?;
    let func = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .map_err(|err| map_trap(err, "WASM missing main export"))?;
    func.call(&mut store, ())
        .map_err(|err| map_trap(err, "WASM runtime error"))
}

fn map_trap(err: anyhow::Error, default_message: &str) -> WasmError {
    if let Some(code) = trap_code_from_message(&err.to_string()) {
        return match code {
            TRAP_ARRAY_OOB => wasm_error("E0403", "Array index out of bounds."),
            TRAP_ARRAY_LEN_NEG => wasm_error("E0400", "Array length must be >= 0."),
            TRAP_ARRAY_OOM => wasm_error("E0400", "Out of memory during array allocation."),
            _ => wasm_error("E0400", format!("{default_message}: {err}")),
        };
    }
    if let Some(trap) = err.downcast_ref::<wasmtime::Trap>() {
        if *trap == wasmtime::Trap::IntegerDivisionByZero {
            return wasm_error("E0402", "Division or modulo by zero.");
        }
        return wasm_error("E0400", format!("{default_message}: {trap}"));
    }
    wasm_error("E0400", format!("{default_message}: {err}"))
}

fn trap_code_from_message(message: &str) -> Option<i32> {
    let marker = "bd_trap:";
    let idx = message.find(marker)?;
    let code = message[idx + marker.len()..]
        .chars()
        .take_while(|ch| ch.is_ascii_digit())
        .collect::<String>();
    if code.is_empty() {
        None
    } else {
        code.parse().ok()
    }
}

fn program_has_arrays(program: &Program) -> bool {
    for func in &program.functions {
        if type_has_array(&func.return_type) {
            return true;
        }
        for param in &func.params {
            if type_has_array(&param.ty) {
                return true;
            }
        }
        for stmt in &func.body {
            if stmt_has_array(stmt) {
                return true;
            }
        }
    }
    false
}

fn type_has_array(ty: &Type) -> bool {
    match ty {
        Type::Array(_) => true,
        _ => false,
    }
}

fn stmt_has_array(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Set { ty, expr, .. } => {
            ty.as_ref().map_or(false, type_has_array) || expr_has_array(expr)
        }
        Stmt::Put { expr, .. } => expr_has_array(expr),
        Stmt::PutIndex { .. } => true,
        Stmt::Yield { expr, .. } => expr_has_array(expr),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            expr_has_array(cond)
                || then_body.iter().any(stmt_has_array)
                || else_body.iter().any(stmt_has_array)
        }
        Stmt::Repeat { cond, body, .. } => {
            expr_has_array(cond) || body.iter().any(stmt_has_array)
        }
    }
}

fn expr_has_array(expr: &Expr) -> bool {
    match &expr.kind {
        ExprKind::ArrayLit(_) => true,
        ExprKind::ArrayNew { .. } => true,
        ExprKind::Index { base, index } => expr_has_array(base) || expr_has_array(index),
        ExprKind::Call { args, .. } => args.iter().any(expr_has_array),
        ExprKind::Unary { expr, .. } => expr_has_array(expr),
        ExprKind::Binary { left, right, .. } => {
            expr_has_array(left) || expr_has_array(right)
        }
        _ => false,
    }
}

fn emit_array_runtime(emitter: &mut WatEmitter) {
    emitter.push_line("(import \"env\" \"bd_trap\" (func $bd_trap (param i32)))");
    emitter.push_line("(memory $mem 1)");
    emitter.push_line(format!(
        "(global $heap (mut i32) (i32.const {ARRAY_HEADER_SIZE}))"
    ));
    emitter.push_line("(func $bd_alloc (param $size i32) (result i32) (local $ptr i32) (local $new_heap i32) (local $pages_needed i32) (local $cur_pages i32) (local $grow_by i32)");
    emitter.indent();
    emitter.push_line("global.get $heap");
    emitter.push_line("local.set $ptr");
    emitter.push_line("global.get $heap");
    emitter.push_line("local.get $size");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 7");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const -8");
    emitter.push_line("i32.and");
    emitter.push_line("local.set $new_heap");
    emitter.push_line("local.get $new_heap");
    emitter.push_line("i32.const 65535");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 65536");
    emitter.push_line("i32.div_u");
    emitter.push_line("local.set $pages_needed");
    emitter.push_line("memory.size");
    emitter.push_line("local.set $cur_pages");
    emitter.push_line("local.get $pages_needed");
    emitter.push_line("local.get $cur_pages");
    emitter.push_line("i32.gt_u");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("local.get $pages_needed");
    emitter.push_line("local.get $cur_pages");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $grow_by");
    emitter.push_line("local.get $grow_by");
    emitter.push_line("memory.grow");
    emitter.push_line("i32.const -1");
    emitter.push_line("i32.eq");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {TRAP_ARRAY_OOM}"));
    emitter.push_line("call $bd_trap");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line("local.get $new_heap");
    emitter.push_line("global.set $heap");
    emitter.push_line("local.get $ptr");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_function(
    emitter: &mut WatEmitter,
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
) -> Result<(), WasmError> {
    let mut signature = String::new();
    for param in &func.params {
        signature.push_str(&format!(" (param {})", wat_type(&param.ty)));
    }
    signature.push_str(&format!(" (result {})", wat_type(&func.return_type)));

    emitter.push_line(format!("(func ${}{}", func.name, signature));
    emitter.indent();

    let mut compiler = FuncCompiler::new(func, functions);
    compiler.emit_body()?;

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

fn wat_type(ty: &Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::Bool => "i32",
        Type::Array(_) => "i32",
    }
}

fn array_elem_size(ty: &Type) -> Result<i32, WasmError> {
    match ty {
        Type::I64 => Ok(8),
        Type::Bool => Ok(4),
        Type::Array(_) => Ok(4),
    }
}

struct WatEmitter {
    lines: Vec<String>,
    indent: usize,
}

impl WatEmitter {
    fn new() -> Self {
        Self {
            lines: Vec::new(),
            indent: 0,
        }
    }

    fn push_line(&mut self, line: impl AsRef<str>) {
        let prefix = "  ".repeat(self.indent);
        self.lines.push(format!("{prefix}{}", line.as_ref()));
    }

    fn indent(&mut self) {
        self.indent += 1;
    }

    fn dedent(&mut self) {
        self.indent = self.indent.saturating_sub(1);
    }

    fn finish(self) -> String {
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
    locals: Vec<Type>,
    scopes: Vec<HashMap<String, LocalInfo>>,
    code: Vec<String>,
    indent: usize,
    label_counter: usize,
}

impl<'a> FuncCompiler<'a> {
    fn new(func: &'a Function, functions: &'a HashMap<String, FunctionSig>) -> Self {
        let mut compiler = Self {
            func,
            functions,
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
        compiler
    }

    fn emit_body(&mut self) -> Result<(), WasmError> {
        for stmt in &self.func.body {
            self.emit_stmt(stmt)?;
        }
        self.emit_default_return();
        Ok(())
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
                self.push_line(format!("local.set {idx}"));
                self.bind_local(name, idx, inferred);
            }
            Stmt::Put { name, expr, .. } => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.emit_expr(expr, Some(&info.ty))?;
                self.push_line(format!("local.set {}", info.idx));
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
                self.push_line(format!("local.set {val_local}"));
                self.emit_bounds_check(info.idx, idx_local)?;
                self.emit_array_address_index(info.idx, idx_local, array_elem_size(&elem_ty)?);
                self.push_line(format!("local.get {val_local}"));
                self.emit_store(&elem_ty);
            }
            Stmt::Yield { expr, .. } => {
                self.emit_expr(expr, Some(&self.func.return_type))?;
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

    fn emit_expr(&mut self, expr: &Expr, expected: Option<&Type>) -> Result<(), WasmError> {
        match &expr.kind {
            ExprKind::Int(value) => {
                self.push_line(format!("i64.const {value}"));
            }
            ExprKind::Bool(value) => {
                let bit = if *value { 1 } else { 0 };
                self.push_line(format!("i32.const {bit}"));
            }
            ExprKind::Ident(name) => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.push_line(format!("local.get {}", info.idx));
            }
            ExprKind::Call { name, args } => {
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
                for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
                    self.emit_expr(arg, Some(param_ty))?;
                }
                self.push_line(format!("call ${name}"));
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
        let elem_ty = if elements.is_empty() {
            match expected {
                Some(Type::Array(elem)) => elem.as_ref().clone(),
                _ => {
                    return Err(wasm_error(
                        "E0400",
                        "Array literal requires explicit array type.",
                    ))
                }
            }
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
        self.push_line(format!("local.set {ptr_local}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("i32.const {}", elements.len()));
        self.push_line("i32.store");

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
        self.push_line(format!("local.set {ptr_local}"));

        self.push_line(format!("local.get {ptr_local}"));
        self.push_line(format!("local.get {len_local}"));
        self.push_line("i32.wrap_i64");
        self.push_line("i32.store");

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
        self.push_line(format!("local.set {base_local}"));
        self.emit_bounds_check(base_local, idx_local)?;
        self.emit_array_address_index(base_local, idx_local, array_elem_size(&elem_ty)?);
        self.emit_load(&elem_ty);
        Ok(())
    }

    fn emit_default_return(&mut self) {
        match &self.func.return_type {
            Type::I64 => self.push_line("i64.const 0"),
            Type::Bool | Type::Array(_) => self.push_line("i32.const 0"),
        }
        self.push_line("return");
    }

    fn emit_len_non_negative_check(&mut self, len_local: u32) -> Result<(), WasmError> {
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

    fn emit_len_max_check(&mut self, len_local: u32, elem_size: i32) -> Result<(), WasmError> {
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

    fn emit_bounds_check(&mut self, base_local: u32, idx_local: u32) -> Result<(), WasmError> {
        let len_local = self.temp_local(Type::I64);
        self.push_line(format!("local.get {base_local}"));
        self.push_line("i32.load");
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

    fn emit_array_address_const(&mut self, base_local: u32, index: i64, elem_size: i32) {
        let offset = (index * elem_size as i64) as i32;
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("i32.const {offset}"));
        self.push_line("i32.add");
    }

    fn emit_array_address_index(&mut self, base_local: u32, idx_local: u32, elem_size: i32) {
        self.push_line(format!("local.get {base_local}"));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("i32.add");
        self.push_line(format!("local.get {idx_local}"));
        self.push_line(format!("i64.const {elem_size}"));
        self.push_line("i64.mul");
        self.push_line("i32.wrap_i64");
        self.push_line("i32.add");
    }

    fn emit_array_init(
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

    fn emit_store(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.store"),
            Type::Bool | Type::Array(_) => self.push_line("i32.store"),
        }
    }

    fn emit_load(&mut self, ty: &Type) {
        match ty {
            Type::I64 => self.push_line("i64.load"),
            Type::Bool | Type::Array(_) => self.push_line("i32.load"),
        }
    }

    fn emit_trap(&mut self, code: i32) {
        self.push_line(format!("i32.const {code}"));
        self.push_line("call $bd_trap");
    }

    fn emit_default_value(&mut self, ty: &Type) -> Result<(), WasmError> {
        match ty {
            Type::I64 => self.push_line("i64.const 0"),
            Type::Bool => self.push_line("i32.const 0"),
            Type::Array(elem) => {
                self.emit_empty_array(elem.as_ref())?;
            }
        }
        Ok(())
    }

    fn emit_empty_array(&mut self, elem_ty: &Type) -> Result<(), WasmError> {
        let ptr_local = self.temp_local(Type::Array(Box::new(elem_ty.clone())));
        self.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
        self.push_line("call $bd_alloc");
        self.push_line(format!("local.set {ptr_local}"));
        self.push_line(format!("local.get {ptr_local}"));
        self.push_line("i32.const 0");
        self.push_line("i32.store");
        self.push_line(format!("local.get {ptr_local}"));
        Ok(())
    }

    fn temp_local(&mut self, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        idx
    }

    fn infer_expr_type(&self, expr: &Expr) -> Result<Type, WasmError> {
        match &expr.kind {
            ExprKind::Int(_) => Ok(Type::I64),
            ExprKind::Bool(_) => Ok(Type::Bool),
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
            ExprKind::Call { name, .. } => self
                .functions
                .get(name)
                .map(|sig| sig.return_type.clone())
                .ok_or_else(|| wasm_error("E0400", format!("Unknown function '{name}'"))),
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

    fn reserve_local(&mut self, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        idx
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

    fn fresh_label(&mut self, prefix: &str) -> String {
        let label = format!("{prefix}{}", self.label_counter);
        self.label_counter += 1;
        label
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use birddisk_core::{lexer, parser};

    fn compile_and_run(source: &str) -> Result<i64, WasmError> {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        run(&program)
    }

    #[test]
    fn wasm_runs_minimal_main() {
        let result = compile_and_run("rule main() -> i64:\n  yield 0.\nend\n").unwrap();
        assert_eq!(result, 0);
    }

    #[test]
    fn wasm_emits_binary() {
        let tokens = lexer::lex("rule main() -> i64:\n  yield 0.\nend\n").unwrap();
        let program = parser::parse(&tokens).unwrap();
        let bytes = emit_wasm(&program).unwrap();
        assert!(!bytes.is_empty());
    }

    #[test]
    fn wasm_runs_loop_and_call() {
        let result = compile_and_run(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  set i: i64 = 0.\n  set sum: i64 = 0.\n  repeat while i < 4:\n    put sum = add(sum, i).\n    put i = i + 1.\n  end\n  yield sum.\nend\n",
        )
        .unwrap();
        assert_eq!(result, 6);
    }

    #[test]
    fn wasm_runs_when_otherwise() {
        let result = compile_and_run(
            "rule main() -> i64:\n  when true:\n    yield 2.\n  otherwise:\n    yield 3.\n  end\nend\n",
        )
        .unwrap();
        assert_eq!(result, 2);
    }

    #[test]
    fn wasm_reports_div_by_zero() {
        let err = compile_and_run("rule main() -> i64:\n  yield 1 / 0.\nend\n").unwrap_err();
        assert_eq!(err.code, "E0402");
    }
}
