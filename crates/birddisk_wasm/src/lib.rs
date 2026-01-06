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

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Type>,
    return_type: Type,
}

pub fn emit_wat(program: &Program) -> Result<String, WasmError> {
    let mut functions = HashMap::new();
    for func in &program.functions {
        functions.insert(
            func.name.clone(),
            FunctionSig {
                params: func.params.iter().map(|p| p.ty).collect(),
                return_type: func.return_type,
            },
        );
    }

    let mut emitter = WatEmitter::new();
    emitter.push_line("(module");
    emitter.indent();

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

pub fn run(program: &Program) -> Result<i64, WasmError> {
    use wasmtime::{Engine, Instance, Module, Store};

    let main = program
        .functions
        .iter()
        .find(|func| func.name == "main")
        .ok_or_else(|| wasm_error("E0400", "missing main function"))?;
    if main.return_type != Type::I64 {
        return Err(wasm_error("E0400", "main must return i64"));
    }

    let wat = emit_wat(program)?;
    let engine = Engine::default();
    let module = Module::new(&engine, wat)
        .map_err(|err| wasm_error("E0400", format!("WASM compile error: {err}")))?;
    let mut store = Store::new(&engine, ());
    let instance = Instance::new(&mut store, &module, &[])
        .map_err(|err| map_trap(err, "WASM instantiation error"))?;
    let func = instance
        .get_typed_func::<(), i64>(&mut store, "main")
        .map_err(|err| map_trap(err, "WASM missing main export"))?;
    func.call(&mut store, ())
        .map_err(|err| map_trap(err, "WASM runtime error"))
}

fn map_trap(err: anyhow::Error, default_message: &str) -> WasmError {
    if let Some(trap) = err.downcast_ref::<wasmtime::Trap>() {
        if *trap == wasmtime::Trap::IntegerDivisionByZero {
            return wasm_error("E0402", "Division or modulo by zero.");
        }
        return wasm_error("E0400", format!("{default_message}: {trap}"));
    }
    wasm_error("E0400", format!("{default_message}: {err}"))
}

fn emit_function(
    emitter: &mut WatEmitter,
    func: &Function,
    functions: &HashMap<String, FunctionSig>,
) -> Result<(), WasmError> {
    let mut signature = String::new();
    for param in &func.params {
        signature.push_str(&format!(" (param {})", wat_type(param.ty)));
    }
    signature.push_str(&format!(" (result {})", wat_type(func.return_type)));

    emitter.push_line(format!("(func ${}{}", func.name, signature));
    emitter.indent();

    let mut compiler = FuncCompiler::new(func, functions);
    compiler.emit_body()?;

    for local in &compiler.locals {
        emitter.push_line(format!("(local {})", wat_type(*local)));
    }

    for line in &compiler.code {
        emitter.push_line(line);
    }

    emitter.dedent();
    emitter.push_line(")");
    Ok(())
}

fn wat_type(ty: Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::Bool => "i32",
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

#[derive(Clone, Copy)]
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
                    ty: param.ty,
                },
            );
        }
        compiler
    }

    fn emit_body(&mut self) -> Result<(), WasmError> {
        for stmt in &self.func.body {
            self.emit_stmt(stmt)?;
        }
        self.push_line("unreachable");
        Ok(())
    }

    fn emit_stmt(&mut self, stmt: &Stmt) -> Result<(), WasmError> {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                let inferred = match ty {
                    Some(ty) => *ty,
                    None => self.infer_expr_type(expr)?,
                };
                let idx = self.declare_local(name, inferred);
                self.emit_expr(expr)?;
                self.push_line(format!("local.set {idx}"));
            }
            Stmt::Put { name, expr, .. } => {
                let info = self
                    .lookup(name)
                    .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'")))?;
                self.emit_expr(expr)?;
                self.push_line(format!("local.set {}", info.idx));
            }
            Stmt::Yield { expr, .. } => {
                self.emit_expr(expr)?;
                self.push_line("return");
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                self.emit_expr(cond)?;
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
                self.emit_expr(cond)?;
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

    fn emit_expr(&mut self, expr: &Expr) -> Result<(), WasmError> {
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
                for arg in args {
                    self.emit_expr(arg)?;
                }
                self.push_line(format!("call ${name}"));
            }
            ExprKind::Unary { op, expr } => match op {
                UnaryOp::Neg => {
                    self.push_line("i64.const 0");
                    self.emit_expr(expr)?;
                    self.push_line("i64.sub");
                }
                UnaryOp::Not => {
                    self.emit_expr(expr)?;
                    self.push_line("i32.eqz");
                }
            },
            ExprKind::Binary { left, op, right } => {
                self.emit_expr(left)?;
                self.emit_expr(right)?;
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

    fn infer_expr_type(&self, expr: &Expr) -> Result<Type, WasmError> {
        match &expr.kind {
            ExprKind::Int(_) => Ok(Type::I64),
            ExprKind::Bool(_) => Ok(Type::Bool),
            ExprKind::Ident(name) => self
                .lookup(name)
                .map(|info| info.ty)
                .ok_or_else(|| wasm_error("E0400", format!("Unknown name '{name}'"))),
            ExprKind::Call { name, .. } => self
                .functions
                .get(name)
                .map(|sig| sig.return_type)
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

    fn declare_local(&mut self, name: &str, ty: Type) -> u32 {
        let idx = self.func.params.len() as u32 + self.locals.len() as u32;
        self.locals.push(ty);
        self.current_scope_mut().insert(
            name.to_string(),
            LocalInfo {
                idx,
                ty,
            },
        );
        idx
    }

    fn lookup(&self, name: &str) -> Option<LocalInfo> {
        for scope in self.scopes.iter().rev() {
            if let Some(info) = scope.get(name) {
                return Some(*info);
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
