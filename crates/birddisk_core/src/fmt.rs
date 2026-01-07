use crate::ast::{BinaryOp, Expr, ExprKind, Function, Program, Stmt, Type, UnaryOp};
use crate::{lexer, parser};
use std::fs;
use std::path::Path;

pub fn format_source(source: &str) -> Result<String, String> {
    let tokens = lexer::lex(source).map_err(|err| err.message)?;
    let program = parser::parse(&tokens).map_err(|err| err.message)?;
    Ok(format_program(&program))
}

pub fn format_path(path: &str) -> Result<(), String> {
    let path = Path::new(path);
    if path.is_dir() {
        format_dir(path)?;
    } else {
        format_file(path)?;
    }
    Ok(())
}

fn format_dir(dir: &Path) -> Result<(), String> {
    let entries = fs::read_dir(dir).map_err(|err| err.to_string())?;
    for entry in entries {
        let entry = entry.map_err(|err| err.to_string())?;
        let path = entry.path();
        if path.is_dir() {
            format_dir(&path)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("bd") {
            format_file(&path)?;
        }
    }
    Ok(())
}

fn format_file(path: &Path) -> Result<(), String> {
    let source = fs::read_to_string(path).map_err(|err| err.to_string())?;
    let formatted = format_source(&source)?;
    if source != formatted {
        fs::write(path, formatted).map_err(|err| err.to_string())?;
    }
    Ok(())
}

fn format_program(program: &Program) -> String {
    let mut fmt = Formatter::new();
    for (idx, func) in program.functions.iter().enumerate() {
        if idx > 0 {
            fmt.push_line("");
        }
        fmt.format_function(func);
    }
    fmt.finish()
}

struct Formatter {
    out: String,
    indent: usize,
}

impl Formatter {
    fn new() -> Self {
        Self {
            out: String::new(),
            indent: 0,
        }
    }

    fn finish(mut self) -> String {
        if !self.out.ends_with('\n') {
            self.out.push('\n');
        }
        self.out
    }

    fn push_line(&mut self, line: &str) {
        let prefix = "  ".repeat(self.indent);
        self.out.push_str(&prefix);
        self.out.push_str(line);
        self.out.push('\n');
    }

    fn format_function(&mut self, func: &Function) {
        let mut params = String::new();
        for (idx, param) in func.params.iter().enumerate() {
            if idx > 0 {
                params.push_str(", ");
            }
            params.push_str(&format!("{}: {}", param.name, format_type(&param.ty)));
        }
        self.push_line(&format!(
            "rule {}({}) -> {}:",
            func.name,
            params,
            format_type(&func.return_type)
        ));
        self.indent += 1;
        for stmt in &func.body {
            self.format_stmt(stmt);
        }
        self.indent -= 1;
        self.push_line("end");
    }

    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Set { name, ty, expr, .. } => {
                let mut line = format!("set {name}");
                if let Some(ty) = ty {
                    line.push_str(&format!(": {}", format_type(ty)));
                }
                line.push_str(" = ");
                line.push_str(&format_expr(expr, 0));
                line.push('.');
                self.push_line(&line);
            }
            Stmt::Put { name, expr, .. } => {
                let line = format!("put {name} = {}.", format_expr(expr, 0));
                self.push_line(&line);
            }
            Stmt::PutIndex {
                name, index, expr, ..
            } => {
                let index_expr = format_expr(index, 0);
                let value_expr = format_expr(expr, 0);
                let line = format!("put {name}[{index_expr}] = {value_expr}.");
                self.push_line(&line);
            }
            Stmt::Yield { expr, .. } => {
                let line = format!("yield {}.", format_expr(expr, 0));
                self.push_line(&line);
            }
            Stmt::When {
                cond,
                then_body,
                else_body,
                ..
            } => {
                self.push_line(&format!("when {}:", format_expr(cond, 0)));
                self.indent += 1;
                for stmt in then_body {
                    self.format_stmt(stmt);
                }
                self.indent -= 1;
                self.push_line("otherwise:");
                self.indent += 1;
                for stmt in else_body {
                    self.format_stmt(stmt);
                }
                self.indent -= 1;
                self.push_line("end");
            }
            Stmt::Repeat { cond, body, .. } => {
                self.push_line(&format!("repeat while {}:", format_expr(cond, 0)));
                self.indent += 1;
                for stmt in body {
                    self.format_stmt(stmt);
                }
                self.indent -= 1;
                self.push_line("end");
            }
        }
    }
}

fn format_type(ty: &Type) -> String {
    match ty {
        Type::I64 => "i64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Array(inner) => format!("{}[]", format_type(inner)),
    }
}

fn format_expr(expr: &Expr, parent_prec: u8) -> String {
    match &expr.kind {
        ExprKind::Int(value) => value.to_string(),
        ExprKind::Bool(value) => value.to_string(),
        ExprKind::Ident(name) => name.clone(),
        ExprKind::Call { name, args } => {
            let mut rendered = String::new();
            rendered.push_str(name);
            rendered.push('(');
            for (idx, arg) in args.iter().enumerate() {
                if idx > 0 {
                    rendered.push_str(", ");
                }
                rendered.push_str(&format_expr(arg, 0));
            }
            rendered.push(')');
            rendered
        }
        ExprKind::ArrayLit(elements) => {
            let mut rendered = String::new();
            rendered.push('[');
            for (idx, elem) in elements.iter().enumerate() {
                if idx > 0 {
                    rendered.push_str(", ");
                }
                rendered.push_str(&format_expr(elem, 0));
            }
            rendered.push(']');
            rendered
        }
        ExprKind::ArrayNew { len } => format!("array({})", format_expr(len, 0)),
        ExprKind::Index { base, index } => {
            let base_expr = format_expr(base, precedence_primary());
            let index_expr = format_expr(index, 0);
            format!("{base_expr}[{index_expr}]")
        }
        ExprKind::Unary { op, expr } => {
            let op_str = match op {
                UnaryOp::Neg => "-",
                UnaryOp::Not => "!",
            };
            let inner = format_expr(expr, precedence_unary());
            if precedence(expr) < precedence_unary() {
                format!("{op_str}({inner})")
            } else {
                format!("{op_str}{inner}")
            }
        }
        ExprKind::Binary { left, op, right } => {
            let prec = precedence_op(*op);
            let left_s = format_expr(left, prec);
            let right_s = format_expr(right, prec);
            let rendered = format!("{left_s} {} {right_s}", format_binary_op(*op));
            if prec < parent_prec {
                format!("({rendered})")
            } else {
                rendered
            }
        }
    }
}

fn format_binary_op(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Sub => "-",
        BinaryOp::Mul => "*",
        BinaryOp::Div => "/",
        BinaryOp::Mod => "%",
        BinaryOp::EqEq => "==",
        BinaryOp::NotEq => "!=",
        BinaryOp::Lt => "<",
        BinaryOp::LtEq => "<=",
        BinaryOp::Gt => ">",
        BinaryOp::GtEq => ">=",
        BinaryOp::AndAnd => "&&",
        BinaryOp::OrOr => "||",
    }
}

fn precedence(expr: &Expr) -> u8 {
    match &expr.kind {
        ExprKind::Binary { op, .. } => precedence_op(*op),
        ExprKind::Unary { .. } => precedence_unary(),
        _ => precedence_primary(),
    }
}

fn precedence_op(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::OrOr => 1,
        BinaryOp::AndAnd => 2,
        BinaryOp::EqEq | BinaryOp::NotEq => 3,
        BinaryOp::Lt | BinaryOp::LtEq | BinaryOp::Gt | BinaryOp::GtEq => 4,
        BinaryOp::Add | BinaryOp::Sub => 5,
        BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => 6,
    }
}

fn precedence_unary() -> u8 {
    7
}

fn precedence_primary() -> u8 {
    8
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn format_minimal_main() {
        let source = "rule main() -> i64:\n  yield 0.\nend\n";
        let formatted = format_source(source).unwrap();
        assert_eq!(formatted, source);
    }

    #[test]
    fn format_normalizes_spacing() {
        let source = "rule  main(  ) ->i64:\n set  x=10.\nwhen x>5:\n  yield 1.\notherwise:\n yield 2.\nend\nend\n";
        let expected = "rule main() -> i64:\n  set x = 10.\n  when x > 5:\n    yield 1.\n  otherwise:\n    yield 2.\n  end\nend\n";
        let formatted = format_source(source).unwrap();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn format_parentheses_for_precedence() {
        let source = "rule main() -> i64:\n  yield (1 + 2) * 3.\nend\n";
        let formatted = format_source(source).unwrap();
        assert_eq!(formatted, source);
    }

    #[test]
    fn format_messy_repeat_and_when() {
        let source = "rule main() -> i64:\n set i=0.\n set sum: i64=0.\nrepeat while i<3:\n put sum=sum+i.\n put i=i+1.\nend\nwhen sum>2:\n yield sum.\notherwise:\n yield 0.\nend\nend\n";
        let expected = "rule main() -> i64:\n  set i = 0.\n  set sum: i64 = 0.\n  repeat while i < 3:\n    put sum = sum + i.\n    put i = i + 1.\n  end\n  when sum > 2:\n    yield sum.\n  otherwise:\n    yield 0.\n  end\nend\n";
        let formatted = format_source(source).unwrap();
        assert_eq!(formatted, expected);
    }

    #[test]
    fn format_messy_calls_and_unary() {
        let source = "rule main() -> i64:\n  yield add(1,2)+-3.\nend\n\nrule add(a:i64,b:i64)->i64:\n  yield a+b.\nend\n";
        let expected = "rule main() -> i64:\n  yield add(1, 2) + -3.\nend\n\nrule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n";
        let formatted = format_source(source).unwrap();
        assert_eq!(formatted, expected);
    }
}
