mod books;
mod enums;
mod expr;
mod stdlib;
mod stmt;

use crate::ast::{Program, Stmt, Type};
use crate::diagnostics::{diagnostic, Diagnostic, Edit, FixIt, Position, Span};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Ty {
    I64,
    F64,
    Bool,
    String,
    U8,
    Void,
    Array(Box<Ty>),
    Book(String),
    Enum(String),
    Unknown,
}

impl Ty {
    fn from_ast(ty: Type) -> Self {
        match ty {
            Type::I64 => Ty::I64,
            Type::F64 => Ty::F64,
            Type::Bool => Ty::Bool,
            Type::String => Ty::String,
            Type::U8 => Ty::U8,
            Type::Void => Ty::Void,
            Type::Array(inner) => Ty::Array(Box::new(Ty::from_ast(*inner))),
            Type::Book(name) => Ty::Book(name),
        }
    }

    fn name(&self) -> String {
        match self {
            Ty::I64 => "i64".to_string(),
            Ty::F64 => "f64".to_string(),
            Ty::Bool => "bool".to_string(),
            Ty::String => "string".to_string(),
            Ty::U8 => "u8".to_string(),
            Ty::Void => "void".to_string(),
            Ty::Array(inner) => format!("{}[]", inner.name()),
            Ty::Book(name) => name.clone(),
            Ty::Enum(name) => name.clone(),
            Ty::Unknown => "unknown".to_string(),
        }
    }
}

#[derive(Clone)]
struct FunctionSig {
    params: Vec<Ty>,
    return_type: Ty,
}

#[derive(Clone)]
struct BookInfo {
    fields: HashMap<String, Ty>,
}

#[derive(Clone)]
struct EnumVariantInfo {
    payload: Option<Ty>,
}

#[derive(Clone)]
struct EnumInfo {
    variants: HashMap<String, EnumVariantInfo>,
}

pub fn typecheck(program: &Program, file: &str) -> Vec<Diagnostic> {
    let mut checker = Checker::new(file);
    checker.collect_signatures(program);
    checker.check_program(program);
    checker.diagnostics
}

struct Checker<'a> {
    file: &'a str,
    diagnostics: Vec<Diagnostic>,
    functions: HashMap<String, FunctionSig>,
    method_functions: HashSet<String>,
    books: HashMap<String, BookInfo>,
    enums: HashMap<String, EnumInfo>,
    scopes: Vec<HashMap<String, Ty>>,
    current_return: Ty,
}

impl<'a> Checker<'a> {
    fn new(file: &'a str) -> Self {
        Self {
            file,
            diagnostics: Vec::new(),
            functions: HashMap::new(),
            method_functions: HashSet::new(),
            books: HashMap::new(),
            enums: HashMap::new(),
            scopes: Vec::new(),
            current_return: Ty::Unknown,
        }
    }

    fn collect_signatures(&mut self, program: &Program) {
        self.collect_enums(program);
        self.collect_books(program);
        self.register_stdlib(program);
        for function in &program.functions {
            if self.functions.contains_key(&function.name) {
                self.diagnostics.push(diagnostic(
                    "E0307",
                    "error",
                    format!("Duplicate function '{}'.", function.name),
                    self.file,
                    function.span,
                    vec!["Function names must be unique.".to_string()],
                    vec!["SPEC.md#4-functions".to_string()],
                    Vec::new(),
                    None,
                ));
                continue;
            }
            let params = function
                .params
                .iter()
                .map(|p| {
                    let ty = self.type_from_ast(p.ty.clone());
                    self.validate_value_type(&ty, p.span);
                    ty
                })
                .collect();
            let sig = FunctionSig {
                params,
                return_type: {
                    let ty = self.type_from_ast(function.return_type.clone());
                    self.validate_return_type(&ty, function.span);
                    ty
                },
            };
            self.functions.insert(function.name.clone(), sig);
        }
    }

    fn check_program(&mut self, program: &Program) {
        if !self.functions.contains_key("main") {
            let default_pos = Position::new(1, 1);
            let span = program
                .functions
                .first()
                .map(|func| func.span)
                .unwrap_or_else(|| Span::new(default_pos, default_pos));
            let fixits = vec![FixIt {
                title: "Add a default main function".to_string(),
                edits: vec![Edit {
                    file: self.file.to_string(),
                    span: Span::new(default_pos, default_pos),
                    replacement: "rule main() -> i64:\n  yield 0.\nend\n\n".to_string(),
                }],
            }];
            self.diagnostics.push(diagnostic(
                "E0309",
                "error",
                "Missing 'main' entry point in entry file.".to_string(),
                self.file,
                span,
                vec![
                    "Add `rule main() -> i64:` to define the entry point.".to_string(),
                    "Imported modules may omit `main`.".to_string(),
                ],
                vec!["SPEC.md#1-program-structure".to_string()],
                fixits,
                None,
            ));
        }
        for function in &program.functions {
            self.check_function(function);
        }
        for book in &program.books {
            for method in &book.methods {
                self.check_function(method);
            }
        }
    }

    fn check_function(&mut self, function: &crate::ast::Function) {
        self.scopes.clear();
        self.push_scope();
        self.current_return = self.type_from_ast(function.return_type.clone());

        for param in &function.params {
            if self.scopes[0].contains_key(&param.name) {
                self.diagnostics.push(diagnostic(
                    "E0308",
                    "error",
                    format!("Duplicate parameter '{}'.", param.name),
                    self.file,
                    param.span,
                    vec!["Parameter names must be unique.".to_string()],
                    vec!["SPEC.md#4-functions".to_string()],
                    Vec::new(),
                    None,
                ));
            } else {
                let ty = self.type_from_ast(param.ty.clone());
                self.scopes[0].insert(param.name.clone(), ty);
            }
        }

        for stmt in &function.body {
            self.check_stmt(stmt);
        }

        if self.current_return != Ty::Void && !block_always_yields(&function.body) {
            self.diagnostics.push(diagnostic(
                "E0306",
                "error",
                "Not all control-flow paths yield a value.".to_string(),
                self.file,
                function.span,
                vec!["Ensure every path ends with `yield`.".to_string()],
                vec!["SPEC.md#4-functions".to_string()],
                Vec::new(),
                None,
            ));
        }

        self.pop_scope();
    }

    fn lookup(&self, name: &str) -> Option<Ty> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
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

    fn current_scope_mut(&mut self) -> &mut HashMap<String, Ty> {
        self.scopes
            .last_mut()
            .expect("scope stack should not be empty")
    }

    fn suggest_name(&self, name: &str) -> Option<String> {
        let mut candidates = HashSet::new();
        for scope in &self.scopes {
            for key in scope.keys() {
                candidates.insert(key.as_str());
            }
        }
        best_suggestion(name, candidates.into_iter())
    }

    fn suggest_function(&self, name: &str) -> Option<String> {
        best_suggestion(name, self.functions.keys().map(|key| key.as_str()))
    }

    fn suggest_book(&self, name: &str) -> Option<String> {
        best_suggestion(name, self.books.keys().map(|key| key.as_str()))
    }

    fn suggest_enum(&self, name: &str) -> Option<String> {
        best_suggestion(name, self.enums.keys().map(|key| key.as_str()))
    }

    fn type_from_ast(&self, ty: Type) -> Ty {
        match ty {
            Type::Array(inner) => Ty::Array(Box::new(self.type_from_ast(*inner))),
            Type::Book(name) => {
                if self.enums.contains_key(&name) {
                    Ty::Enum(name)
                } else {
                    Ty::Book(name)
                }
            }
            _ => Ty::from_ast(ty),
        }
    }

    fn validate_value_type(&mut self, ty: &Ty, span: Span) {
        match ty {
            Ty::Void => {
                self.diagnostics.push(diagnostic(
                    "E0312",
                    "error",
                    "Void type is only allowed as a function return type.".to_string(),
                    self.file,
                    span,
                    vec!["Use a concrete value type for bindings and fields.".to_string()],
                    vec!["SPEC.md#2-types".to_string()],
                    Vec::new(),
                    None,
                ));
            }
            Ty::Array(inner) => self.validate_value_type(inner, span),
            Ty::Book(name) => {
                if self.books.contains_key(name) {
                    return;
                }
                let suggestion = self.suggest_book(name);
                let notes = notes_with_suggestion(
                    vec!["Define the book before using it as a type.".to_string()],
                    suggestion,
                );
                self.diagnostics.push(diagnostic(
                    "E0301",
                    "error",
                    format!("Unknown book type '{name}'."),
                    self.file,
                    span,
                    notes,
                    vec!["SPEC.md#15-objects".to_string()],
                    Vec::new(),
                    None,
                ));
            }
            Ty::Enum(name) => {
                if self.enums.contains_key(name) {
                    return;
                }
                let suggestion = self.suggest_enum(name);
                let notes = notes_with_suggestion(
                    vec!["Define the enum before using it as a type.".to_string()],
                    suggestion,
                );
                self.diagnostics.push(diagnostic(
                    "E0313",
                    "error",
                    format!("Unknown enum type '{name}'."),
                    self.file,
                    span,
                    notes,
                    vec!["SPEC.md#2-1-enums".to_string()],
                    Vec::new(),
                    None,
                ));
            }
            _ => {}
        }
    }

    fn validate_return_type(&mut self, ty: &Ty, span: Span) {
        if matches!(ty, Ty::Void) {
            return;
        }
        self.validate_value_type(ty, span);
    }
}

fn type_mismatch(file: &str, span: Span, expected: Ty, actual: Ty) -> Diagnostic {
    diagnostic(
        "E0300",
        "error",
        format!(
            "Type mismatch: expected {}, got {}.",
            expected.name(),
            actual.name()
        ),
        file,
        span,
        vec!["Expression type must match the expected type.".to_string()],
        vec!["SPEC.md#2-types".to_string()],
        Vec::new(),
        None,
    )
}

fn notes_with_suggestion(mut notes: Vec<String>, suggestion: Option<String>) -> Vec<String> {
    if let Some(suggestion) = suggestion {
        notes.push(format!("Did you mean `{suggestion}`?"));
    }
    notes
}

fn best_suggestion<'a>(name: &str, candidates: impl Iterator<Item = &'a str>) -> Option<String> {
    let mut best: Option<(&str, usize)> = None;
    for candidate in candidates {
        if candidate == name {
            return Some(candidate.to_string());
        }
        let distance = edit_distance(name, candidate);
        match best {
            Some((_, best_distance)) if distance >= best_distance => {}
            _ => best = Some((candidate, distance)),
        }
    }

    let (candidate, distance) = best?;
    if distance <= max_suggestion_distance(name.len()) {
        Some(candidate.to_string())
    } else {
        None
    }
}

fn max_suggestion_distance(len: usize) -> usize {
    if len <= 4 {
        1
    } else if len <= 8 {
        2
    } else {
        3
    }
}

fn edit_distance(a: &str, b: &str) -> usize {
    let mut prev: Vec<usize> = (0..=b.len()).collect();
    let mut curr = vec![0; b.len() + 1];

    for (i, ca) in a.chars().enumerate() {
        curr[0] = i + 1;
        for (j, cb) in b.chars().enumerate() {
            let cost = if ca == cb { 0 } else { 1 };
            curr[j + 1] = (prev[j + 1] + 1).min(curr[j] + 1).min(prev[j] + cost);
        }
        prev.clone_from_slice(&curr);
    }

    prev[b.len()]
}

fn block_always_yields(stmts: &[Stmt]) -> bool {
    for stmt in stmts {
        if stmt_always_yields(stmt) {
            return true;
        }
    }
    false
}

fn stmt_always_yields(stmt: &Stmt) -> bool {
    match stmt {
        Stmt::Yield { .. } => true,
        Stmt::Throw { .. } => true,
        Stmt::When {
            then_body,
            else_body,
            ..
        } => block_always_yields(then_body) && block_always_yields(else_body),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => block_always_yields(try_body) && block_always_yields(catch_body),
        Stmt::Match {
            cases, otherwise, ..
        } => {
            cases.iter().all(|case| block_always_yields(&case.body))
                && block_always_yields(otherwise)
        }
        Stmt::Repeat { .. } => false,
        Stmt::Set { .. }
        | Stmt::Expr { .. }
        | Stmt::Put { .. }
        | Stmt::PutIndex { .. }
        | Stmt::PutField { .. } => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{lexer, parse_and_typecheck, parser};
    use std::path::PathBuf;

    fn check(source: &str) -> Vec<Diagnostic> {
        let tokens = lexer::lex(source).unwrap();
        let program = parser::parse(&tokens).unwrap();
        typecheck(&program, "test.bd")
    }

    fn fixture_path(rel: &str) -> PathBuf {
        let mut root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
        root.pop();
        root.pop();
        root.push(rel);
        root
    }

    #[test]
    fn typecheck_simple_ok() {
        let diags = check("rule main() -> i64:\n  set x = 1.\n  yield x.\nend\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_non_bool_condition() {
        let diags = check("rule main() -> i64:\n  when 1:\n    yield 0.\n  otherwise:\n    yield 1.\n  end\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0304"));
    }

    #[test]
    fn typecheck_throw_requires_string() {
        let diags = check("rule main() -> i64:\n  throw 1.\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_try_catch_binds_string() {
        let diags = check(
            "rule main() -> i64:\n  try:\n    throw \"boom\".\n  catch err:\n    set msg: string = err.\n    yield 0.\n  end\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_wrong_arity() {
        let diags = check(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1).\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0302"));
    }

    #[test]
    fn typecheck_suggests_unknown_name() {
        let diags = check("rule main() -> i64:\n  set count = 1.\n  yield coun.\nend\n");
        let diag = diags.iter().find(|d| d.code == "E0301").unwrap();
        assert!(diag.notes.iter().any(|note| note.contains("count")));
    }

    #[test]
    fn typecheck_suggests_unknown_function() {
        let diags = check(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield ad(1, 2).\nend\n",
        );
        let diag = diags.iter().find(|d| d.code == "E0303").unwrap();
        assert!(diag.notes.iter().any(|note| note.contains("add")));
    }

    #[test]
    fn typecheck_rejects_missing_main() {
        let diags = check("rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n");
        let diag = diags.iter().find(|d| d.code == "E0309");
        assert!(diag.is_some());
        assert!(!diag.unwrap().fixits.is_empty());
    }

    #[test]
    fn typecheck_rejects_missing_yield() {
        let diags = check("rule main() -> i64:\n  set x = 1.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0306"));
    }

    #[test]
    fn typecheck_accepts_yield_after_when() {
        let diags = check(
            "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    set x = 2.\n  end\n  yield 3.\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_reports_rhs_mismatch_in_bool_op() {
        let diags = check("rule main() -> i64:\n  yield true && 1.\nend\n");
        assert!(diags.iter().any(|d| {
            d.code == "E0300"
                && d.message.contains("expected bool")
                && d.message.contains("got i64")
        }));
    }

    #[test]
    fn typecheck_reports_rhs_mismatch_in_arithmetic_op() {
        let diags = check("rule main() -> i64:\n  yield 1 + true.\nend\n");
        assert!(diags.iter().any(|d| {
            d.code == "E0300"
                && d.message.contains("expected i64")
                && d.message.contains("got bool")
        }));
    }

    #[test]
    fn typecheck_infers_array_literal() {
        let diags = check("rule main() -> i64:\n  set xs = [1, 2].\n  yield xs[1].\nend\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_empty_array_without_annotation() {
        let diags = check("rule main() -> i64:\n  set xs = [].\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0305"));
    }

    #[test]
    fn typecheck_rejects_array_constructor_without_annotation() {
        let diags = check("rule main() -> i64:\n  set xs = array(3).\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0310"));
    }

    #[test]
    fn typecheck_rejects_array_constructor_non_i64_len() {
        let diags = check("rule main() -> i64:\n  set xs: i64[] = array(true).\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_std_string_without_import() {
        let diags = check(
            "rule main() -> i64:\n  set s: string = \"hi\".\n  yield std::string::len(s).\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0303"));
    }

    #[test]
    fn typecheck_accepts_std_string_import() {
        let diags = check(
            "import std::string.\nrule main() -> i64:\n  set s: string = \"hi\".\n  yield std::string::len(s).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_accepts_u8_literal_in_context() {
        let diags = check("rule main() -> i64:\n  set b: u8 = 255.\n  set xs: u8[] = [0, 1, 2].\n  yield 0.\nend\n");
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_u8_out_of_range() {
        let diags = check("rule main() -> i64:\n  set b: u8 = 256.\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0311"));
    }

    #[test]
    fn typecheck_accepts_bytes_module() {
        let diags = check(
            "import std::string.\nimport std::bytes.\nrule main() -> i64:\n  set bs: u8[] = std::string::bytes(\"hi\").\n  yield std::bytes::len(bs).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_accepts_string_from_bytes() {
        let diags = check(
            "import std::string.\nrule main() -> i64:\n  set bs: u8[] = [104, 105].\n  set text: string = std::string::from_bytes(bs).\n  yield std::string::len(text).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_std_io_without_import() {
        let diags = check("rule main() -> i64:\n  std::io::print(\"hi\").\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0303"));
    }

    #[test]
    fn typecheck_accepts_std_io_import() {
        let diags = check(
            "import std::io.\nrule main() -> i64:\n  std::io::print(\"hi\").\n  yield 0.\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_accepts_std_thread_spawn_join() {
        let diags = check(
            "import std::thread.\nrule worker(value: i64) -> i64:\n  yield value + 1.\nend\n\nrule main() -> i64:\n  set t: Thread = std::thread::spawn(\"worker\", 2).\n  yield std::thread::join(t).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_accepts_std_thread_spawn_with_tcpstream_arg() {
        let diags = check(
            "import std::net.\nimport std::thread.\nrule worker(stream: TcpStream, route: i64) -> i64:\n  std::net::close_stream(stream).\n  yield route.\nend\n\nrule main() -> i64:\n  set listener: TcpListener = std::net::listen(\"127.0.0.1:0\").\n  set addr: string = std::net::listener_addr(listener).\n  set client: TcpStream = std::net::connect(addr).\n  set server: TcpStream = std::net::accept(listener).\n  set t: Thread = std::thread::spawn(\"worker\", server, 1).\n  std::net::close_stream(client).\n  std::net::close_listener(listener).\n  yield std::thread::join(t).\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_std_net_without_import() {
        let diags = check(
            "rule main() -> i64:\n  set s: TcpStream = std::net::connect(\"127.0.0.1:1\").\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0301" || d.code == "E0303"));
    }

    #[test]
    fn typecheck_accepts_std_net_import() {
        let diags = check(
            "import std::net.\nrule main() -> i64:\n  set p: TcpPool = std::net::pool(\"127.0.0.1:1\", 1).\n  std::net::pool_close(p).\n  yield 0.\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_std_thread_non_literal_entry() {
        let diags = check(
            "import std::thread.\nrule worker() -> i64:\n  yield 1.\nend\n\nrule main() -> i64:\n  set name: string = \"worker\".\n  set t: Thread = std::thread::spawn(name).\n  yield std::thread::join(t).\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_accepts_void_function_without_yield() {
        let diags = check(
            "import std::io.\nrule log() -> void:\n  std::io::print(\"hi\").\nend\n\nrule main() -> i64:\n  log().\n  yield 0.\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_void_yield() {
        let diags =
            check("rule log() -> void:\n  yield 1.\nend\n\nrule main() -> i64:\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0312"));
    }

    #[test]
    fn typecheck_rejects_void_binding() {
        let diags = check(
            "rule log() -> void:\nend\n\nrule main() -> i64:\n  set x = log().\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0312"));
    }

    #[test]
    fn typecheck_rejects_call_stmt_non_void() {
        let diags = check(
            "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  add(1, 2).\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0312"));
    }

    #[test]
    fn typecheck_rejects_index_type_fixture() {
        let path = fixture_path("vm_error_tests/arrays/array_index_type_error.bd");
        let diags = parse_and_typecheck(path.to_str().unwrap()).unwrap_err();
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_unknown_book_type() {
        let diags = check("rule main(b: Ghost) -> i64:\n  yield 0.\nend\n");
        assert!(diags.iter().any(|d| d.code == "E0301"));
    }

    #[test]
    fn typecheck_rejects_method_without_self() {
        let diags = check(
            "book Thing:\n  rule inc(x: i64) -> i64:\n    yield x.\n  end\nend\n\nrule main() -> i64:\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_method_wrong_self_type() {
        let diags = check(
            "book Thing:\n  rule inc(self: i64) -> i64:\n    yield self.\n  end\nend\n\nrule main() -> i64:\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_unknown_field_access() {
        let diags = check(
            "book Thing:\n  field x: i64.\nend\n\nrule main() -> i64:\n  set t: Thing = new Thing().\n  yield t::y.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0301"));
    }

    #[test]
    fn typecheck_rejects_missing_constructor_args() {
        let diags = check(
            "book Thing:\n  field x: i64.\nend\n\nrule main() -> i64:\n  set t: Thing = new Thing(1).\n  yield 0.\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0303"));
    }

    #[test]
    fn typecheck_accepts_enum_match() {
        let diags = check(
            "enum Result:\n  case Ok(value: i64).\n  case Err(message: string).\nend\n\nrule main() -> i64:\n  set r: Result = Result::Ok(1).\n  match r:\n    case Result::Ok(value):\n      yield value.\n    case Result::Err(message):\n      yield 0.\n    otherwise:\n      yield 0.\n  end\nend\n",
        );
        assert!(diags.is_empty());
    }

    #[test]
    fn typecheck_rejects_match_non_enum() {
        let diags = check(
            "rule main() -> i64:\n  match 1:\n    case Foo::Bar:\n      yield 0.\n    otherwise:\n      yield 0.\n  end\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0300"));
    }

    #[test]
    fn typecheck_rejects_unknown_enum_variant() {
        let diags = check(
            "enum Result:\n  case Ok(value: i64).\nend\n\nrule main() -> i64:\n  set r: Result = Result::Ok(1).\n  match r:\n    case Result::Err(value):\n      yield 0.\n    otherwise:\n      yield 0.\n  end\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0314"));
    }

    #[test]
    fn typecheck_rejects_payload_binding_on_unit_variant() {
        let diags = check(
            "enum Flag:\n  case On.\nend\n\nrule main() -> i64:\n  set f: Flag = Flag::On().\n  match f:\n    case Flag::On(value):\n      yield 0.\n    otherwise:\n      yield 0.\n  end\nend\n",
        );
        assert!(diags.iter().any(|d| d.code == "E0315"));
    }
}
