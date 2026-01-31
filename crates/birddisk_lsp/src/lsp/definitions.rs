use super::imports::{find_stdlib_root, parse_program_from_path, resolve_user_module_path};
use super::server::{
    extract_uri, extract_uri_and_position, path_to_uri, span_to_range, token_at_position,
    token_before_position, uri_to_path, Server,
};
use super::stdlib::stdlib_module_path;
use birddisk_core::ast::{Book, Function, Program, Type};
use birddisk_core::lexer::{self, Token, TokenKind};
use birddisk_core::{Position, Span};
use serde_json::{json, Value};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

impl Server {
    pub(crate) fn handle_definition(&self, params: Value) -> Value {
        let Some((uri, pos)) = extract_uri_and_position(&params) else {
            return Value::Null;
        };
        let Some(text) = self.document_text(&uri) else {
            return Value::Null;
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Value::Null,
        };
        let program = match birddisk_core::parser::parse(&tokens) {
            Ok(program) => program,
            Err(_) => return Value::Null,
        };
        let index = build_symbol_index(&uri, &program);
        match definition_location(&tokens, &index, pos) {
            Some(location) => json!([{"uri": location.uri, "range": span_to_range(location.span)}]),
            None => Value::Null,
        }
    }

    pub(crate) fn handle_type_definition(&self, params: Value) -> Value {
        let Some((uri, pos)) = extract_uri_and_position(&params) else {
            return Value::Null;
        };
        let Some(text) = self.document_text(&uri) else {
            return Value::Null;
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Value::Null,
        };
        let program = match birddisk_core::parser::parse(&tokens) {
            Ok(program) => program,
            Err(_) => return Value::Null,
        };
        let index = build_symbol_index(&uri, &program);
        match type_definition_location(&tokens, &index, pos) {
            Some(location) => json!([{"uri": location.uri, "range": span_to_range(location.span)}]),
            None => Value::Null,
        }
    }

    pub(crate) fn handle_references(&self, params: Value) -> Value {
        let Some((uri, pos)) = extract_uri_and_position(&params) else {
            return json!([]);
        };
        let Some(text) = self.document_text(&uri) else {
            return json!([]);
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return json!([]),
        };
        let target = if let Some(path) = qualified_path_at_position(&tokens, pos) {
            path
        } else if let Some((_, token)) = token_at_position(&tokens, pos) {
            match &token.kind {
                TokenKind::Ident(name) => vec![name.clone()],
                _ => return json!([]),
            }
        } else {
            return json!([]);
        };
        let mut sources = Vec::new();
        sources.push((uri.clone(), tokens));
        if target.len() > 1 {
            let mut seen_uris = HashSet::new();
            seen_uris.insert(uri.clone());
            for (doc_uri, doc) in &self.docs {
                if seen_uris.insert(doc_uri.clone()) {
                    if let Ok(tokens) = lexer::lex(&doc.text) {
                        sources.push((doc_uri.clone(), tokens));
                    }
                }
            }
            if let Ok(program) = birddisk_core::parser::parse(&sources[0].1) {
                let (_index, paths) = build_symbol_index_with_paths(&uri, &program);
                let current_path = uri_to_path(&uri)
                    .and_then(|path| path.canonicalize().ok())
                    .unwrap_or_else(|| PathBuf::from(&uri));
                for path in paths {
                    let canonical = path.canonicalize().unwrap_or_else(|_| path.clone());
                    if canonical == current_path {
                        continue;
                    }
                    let uri = path_to_uri(&canonical);
                    if seen_uris.insert(uri.clone()) {
                        if let Ok(source) = std::fs::read_to_string(&canonical) {
                            if let Ok(tokens) = lexer::lex(&source) {
                                sources.push((uri, tokens));
                            }
                        }
                    }
                }
            }
        }
        let locations: Vec<Value> = collect_reference_locations(&target, &sources)
            .into_iter()
            .map(|location| json!({"uri": location.uri, "range": span_to_range(location.span)}))
            .collect();
        json!(locations)
    }

    pub(crate) fn handle_document_symbols(&self, params: Value) -> Value {
        let Some(uri) = extract_uri(&params) else {
            return json!([]);
        };
        let Some(text) = self.document_text(&uri) else {
            return json!([]);
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return json!([]),
        };
        let program = match birddisk_core::parser::parse(&tokens) {
            Ok(program) => program,
            Err(_) => return json!([]),
        };
        json!(document_symbols(&program))
    }

    pub(crate) fn handle_workspace_symbols(&self, params: Value) -> Value {
        let query = params
            .get("query")
            .and_then(|value| value.as_str())
            .unwrap_or("")
            .to_lowercase();
        let mut symbols = Vec::new();
        for (uri, doc) in &self.docs {
            let tokens = match lexer::lex(&doc.text) {
                Ok(tokens) => tokens,
                Err(_) => continue,
            };
            let program = match birddisk_core::parser::parse(&tokens) {
                Ok(program) => program,
                Err(_) => continue,
            };
            symbols.extend(workspace_symbols_for_program(uri, &program, &query));
        }
        json!(symbols)
    }

    pub(crate) fn handle_rename(&self, params: Value) -> Value {
        let Some((uri, pos)) = extract_uri_and_position(&params) else {
            return Value::Null;
        };
        let new_name = params
            .get("newName")
            .and_then(|value| value.as_str())
            .unwrap_or("");
        if new_name.is_empty() {
            return Value::Null;
        }
        let Some(text) = self.document_text(&uri) else {
            return Value::Null;
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return Value::Null,
        };
        let Some((_, token)) = token_at_position(&tokens, pos) else {
            return Value::Null;
        };
        let TokenKind::Ident(name) = &token.kind else {
            return Value::Null;
        };
        let edits: Vec<Value> = tokens
            .iter()
            .filter_map(|tok| match &tok.kind {
                TokenKind::Ident(ident) if ident == name => Some(json!({
                    "range": span_to_range(tok.span),
                    "newText": new_name
                })),
                _ => None,
            })
            .collect();
        if edits.is_empty() {
            return Value::Null;
        }
        json!({"changes": {uri: edits}})
    }
}

#[derive(Clone)]
pub(crate) struct FunctionInfo {
    pub(crate) name: String,
    pub(crate) params: Vec<(String, Type)>,
    pub(crate) return_type: Type,
    pub(crate) span: Span,
    pub(crate) uri: String,
}

#[derive(Clone)]
pub(crate) struct FieldInfo {
    pub(crate) name: String,
    pub(crate) ty: Type,
    pub(crate) span: Span,
    pub(crate) uri: String,
}

#[derive(Clone)]
pub(crate) struct BookInfo {
    pub(crate) name: String,
    pub(crate) span: Span,
    pub(crate) uri: String,
    pub(crate) fields: HashMap<String, FieldInfo>,
    pub(crate) methods: HashMap<String, FunctionInfo>,
}

pub(crate) struct SymbolIndex {
    pub(crate) functions: HashMap<String, FunctionInfo>,
    pub(crate) books: HashMap<String, BookInfo>,
}

impl SymbolIndex {
    pub(crate) fn new(program: &Program, uri: &str) -> Self {
        let mut index = Self {
            functions: HashMap::new(),
            books: HashMap::new(),
        };
        add_program_symbols(&mut index, program, uri, None);
        index
    }

    fn insert_function(&mut self, name: String, info: FunctionInfo) {
        self.functions.entry(name).or_insert(info);
    }

    fn insert_book(&mut self, info: BookInfo) {
        self.books.entry(info.name.clone()).or_insert(info);
    }
}

#[derive(Clone)]
pub(crate) struct DefinitionLocation {
    pub(crate) uri: String,
    pub(crate) span: Span,
}

#[derive(Clone)]
struct ReferenceLocation {
    uri: String,
    span: Span,
}

pub(crate) fn definition_location(
    tokens: &[Token],
    index: &SymbolIndex,
    pos: Position,
) -> Option<DefinitionLocation> {
    if let Some(path) = qualified_path_at_position(tokens, pos) {
        if path.len() >= 2 {
            let qualified = path.join("::");
            if let Some(info) = index.functions.get(&qualified) {
                return Some(DefinitionLocation {
                    uri: info.uri.clone(),
                    span: info.span,
                });
            }
        }
    }
    let (idx, token) = token_at_position(tokens, pos)?;
    let TokenKind::Ident(name) = &token.kind else {
        return None;
    };
    if let Some((book, member)) = member_context(tokens, pos) {
        if let Some(book_info) = index.books.get(&book) {
            if let Some(field) = book_info.fields.get(&member) {
                return Some(DefinitionLocation {
                    uri: field.uri.clone(),
                    span: field.span,
                });
            }
            if let Some(method) = book_info.methods.get(&member) {
                return Some(DefinitionLocation {
                    uri: method.uri.clone(),
                    span: method.span,
                });
            }
        }
    }
    if let Some(info) = index.functions.get(name) {
        return Some(DefinitionLocation {
            uri: info.uri.clone(),
            span: info.span,
        });
    }
    if let Some(book_info) = index.books.get(name) {
        return Some(DefinitionLocation {
            uri: book_info.uri.clone(),
            span: book_info.span,
        });
    }
    if idx > 0 {
        if let Some(TokenKind::Ident(base)) = tokens.get(idx - 1).map(|tok| &tok.kind) {
            if let Some(book_info) = index.books.get(base) {
                if let Some(field) = book_info.fields.get(name) {
                    return Some(DefinitionLocation {
                        uri: field.uri.clone(),
                        span: field.span,
                    });
                }
            }
        }
    }
    None
}

pub(crate) fn type_definition_location(
    tokens: &[Token],
    index: &SymbolIndex,
    pos: Position,
) -> Option<DefinitionLocation> {
    let name = type_name_at_position(tokens, pos)?;
    let book = index.books.get(&name)?;
    Some(DefinitionLocation {
        uri: book.uri.clone(),
        span: book.span,
    })
}

fn collect_reference_locations(
    target: &[String],
    sources: &[(String, Vec<Token>)],
) -> Vec<ReferenceLocation> {
    let mut locations = Vec::new();
    for (uri, tokens) in sources {
        for span in reference_spans(tokens, target) {
            locations.push(ReferenceLocation {
                uri: uri.clone(),
                span,
            });
        }
    }
    locations
}

fn reference_spans(tokens: &[Token], target: &[String]) -> Vec<Span> {
    let target = target.join("::");
    collect_qualified_paths(tokens)
        .into_iter()
        .filter_map(|(path, span)| {
            if path.join("::") == target {
                Some(span)
            } else {
                None
            }
        })
        .collect()
}

pub(crate) fn qualified_path_at_position(tokens: &[Token], pos: Position) -> Option<Vec<String>> {
    let (idx, token) = token_at_position(tokens, pos)?;
    let TokenKind::Ident(_name) = &token.kind else {
        return None;
    };
    if idx + 1 < tokens.len() && matches!(tokens[idx + 1].kind, TokenKind::DoubleColon) {
        return None;
    }
    let mut start = idx;
    while start >= 2 {
        if matches!(tokens[start - 1].kind, TokenKind::DoubleColon) {
            if let TokenKind::Ident(_) = &tokens[start - 2].kind {
                start = start.saturating_sub(2);
                continue;
            }
        }
        break;
    }
    let mut parts = Vec::new();
    let mut cursor = start;
    loop {
        let TokenKind::Ident(part) = &tokens[cursor].kind else {
            break;
        };
        parts.push(part.clone());
        if cursor + 2 < tokens.len() && matches!(tokens[cursor + 1].kind, TokenKind::DoubleColon) {
            if let TokenKind::Ident(_) = &tokens[cursor + 2].kind {
                cursor += 2;
                continue;
            }
        }
        break;
    }
    if parts.len() >= 2 { Some(parts) } else { None }
}

pub(crate) fn member_context(tokens: &[Token], pos: Position) -> Option<(String, String)> {
    let (idx, token) = token_at_position(tokens, pos)?;
    if let TokenKind::Ident(name) = &token.kind {
        if idx >= 2 {
            if matches!(tokens[idx - 1].kind, TokenKind::DoubleColon) {
                if let TokenKind::Ident(base) = &tokens[idx - 2].kind {
                    return Some((base.clone(), name.clone()));
                }
            }
        }
    }
    let (idx, token) = token_before_position(tokens, pos)?;
    if matches!(token.kind, TokenKind::DoubleColon) {
        if idx >= 1 {
            if let TokenKind::Ident(base) = &tokens[idx - 1].kind {
                return Some((base.clone(), String::new()));
            }
        }
    }
    None
}

fn type_name_at_position(tokens: &[Token], pos: Position) -> Option<String> {
    let (idx, token) = token_at_position(tokens, pos)?;
    let TokenKind::Ident(name) = &token.kind else {
        return None;
    };
    if is_type_context(tokens, idx) { Some(name.clone()) } else { None }
}

fn is_type_context(tokens: &[Token], idx: usize) -> bool {
    if idx == 0 {
        return false;
    }
    matches!(
        tokens[idx - 1].kind,
        TokenKind::Colon | TokenKind::Arrow | TokenKind::New | TokenKind::Book | TokenKind::As
    )
}

fn collect_qualified_paths(tokens: &[Token]) -> Vec<(Vec<String>, Span)> {
    let mut results = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        let TokenKind::Ident(name) = &tokens[idx].kind else {
            idx += 1;
            continue;
        };
        let mut parts = vec![name.clone()];
        let mut last_span = tokens[idx].span;
        let mut cursor = idx;
        while cursor + 2 < tokens.len() && matches!(tokens[cursor + 1].kind, TokenKind::DoubleColon)
        {
            if let TokenKind::Ident(next) = &tokens[cursor + 2].kind {
                parts.push(next.clone());
                cursor += 2;
                last_span = tokens[cursor].span;
                continue;
            }
            break;
        }
        results.push((parts, last_span));
        idx = cursor + 1;
    }
    results
}

pub(crate) fn document_symbols(program: &Program) -> Vec<Value> {
    const SYMBOL_CLASS: i32 = 5;
    const SYMBOL_METHOD: i32 = 6;
    const SYMBOL_FIELD: i32 = 8;
    const SYMBOL_FUNCTION: i32 = 12;
    let mut symbols = Vec::new();
    for book in &program.books {
        let mut children = Vec::new();
        for field in &book.fields {
            children.push(json!({
                "name": field.name.clone(),
                "kind": SYMBOL_FIELD,
                "range": span_to_range(field.span),
                "selectionRange": span_to_range(field.span)
            }));
        }
        for method in &book.methods {
            children.push(json!({
                "name": method.name.clone(),
                "kind": SYMBOL_METHOD,
                "range": span_to_range(method.span),
                "selectionRange": span_to_range(method.span)
            }));
        }
        symbols.push(json!({
            "name": book.name.clone(),
            "kind": SYMBOL_CLASS,
            "range": span_to_range(book.span),
            "selectionRange": span_to_range(book.span),
            "children": children
        }));
    }
    for func in &program.functions {
        symbols.push(json!({
            "name": func.name.clone(),
            "kind": SYMBOL_FUNCTION,
            "range": span_to_range(func.span),
            "selectionRange": span_to_range(func.span)
        }));
    }
    symbols
}

fn workspace_symbols_for_program(uri: &str, program: &Program, query: &str) -> Vec<Value> {
    const SYMBOL_CLASS: i32 = 5;
    const SYMBOL_METHOD: i32 = 6;
    const SYMBOL_FIELD: i32 = 8;
    const SYMBOL_FUNCTION: i32 = 12;
    let mut symbols = Vec::new();
    for book in &program.books {
        if query.is_empty() || book.name.to_lowercase().contains(query) {
            symbols.push(json!({
                "name": book.name.clone(),
                "kind": SYMBOL_CLASS,
                "location": {"uri": uri, "range": span_to_range(book.span)}
            }));
        }
        for field in &book.fields {
            if query.is_empty() || field.name.to_lowercase().contains(query) {
                symbols.push(json!({
                    "name": field.name.clone(),
                    "kind": SYMBOL_FIELD,
                    "location": {"uri": uri, "range": span_to_range(field.span)},
                    "containerName": book.name.clone()
                }));
            }
        }
        for method in &book.methods {
            if query.is_empty() || method.name.to_lowercase().contains(query) {
                symbols.push(json!({
                    "name": method.name.clone(),
                    "kind": SYMBOL_METHOD,
                    "location": {"uri": uri, "range": span_to_range(method.span)},
                    "containerName": book.name.clone()
                }));
            }
        }
    }
    for func in &program.functions {
        if query.is_empty() || func.name.to_lowercase().contains(query) {
            symbols.push(json!({
                "name": func.name.clone(),
                "kind": SYMBOL_FUNCTION,
                "location": {"uri": uri, "range": span_to_range(func.span)}
            }));
        }
    }
    symbols
}

fn add_program_symbols(index: &mut SymbolIndex, program: &Program, uri: &str, prefix: Option<&str>) {
    for func in &program.functions {
        let name = match prefix {
            Some(prefix) => format!("{prefix}::{}", func.name),
            None => func.name.clone(),
        };
        let info = function_info(func, name.clone(), uri);
        index.insert_function(name, info);
    }
    for book in &program.books {
        index.insert_book(book_info(book, uri));
    }
}

fn function_info(func: &Function, name: String, uri: &str) -> FunctionInfo {
    FunctionInfo {
        name,
        params: func
            .params
            .iter()
            .map(|param| (param.name.clone(), param.ty.clone()))
            .collect(),
        return_type: func.return_type.clone(),
        span: func.span,
        uri: uri.to_string(),
    }
}

fn book_info(book: &Book, uri: &str) -> BookInfo {
    let mut fields = HashMap::new();
    for field in &book.fields {
        fields.insert(
            field.name.clone(),
            FieldInfo {
                name: field.name.clone(),
                ty: field.ty.clone(),
                span: field.span,
                uri: uri.to_string(),
            },
        );
    }
    let mut methods = HashMap::new();
    for method in &book.methods {
        methods.insert(
            method.name.clone(),
            function_info(method, method.name.clone(), uri),
        );
    }
    BookInfo {
        name: book.name.clone(),
        span: book.span,
        uri: uri.to_string(),
        fields,
        methods,
    }
}

fn load_module_symbols(
    path: &Path,
    module_path: &[String],
    index: &mut SymbolIndex,
    visited: &mut HashSet<PathBuf>,
) {
    let path = path.canonicalize().unwrap_or_else(|_| path.to_path_buf());
    if !visited.insert(path.clone()) {
        return;
    }
    let Some(program) = parse_program_from_path(&path) else {
        return;
    };
    let module_uri = path_to_uri(&path);
    let prefix = module_path.join("::");
    add_program_symbols(index, &program, &module_uri, Some(&prefix));
    add_imports_to_index(&path, &program, index, visited);
}

fn add_imports_to_index(
    entry_path: &Path,
    program: &Program,
    index: &mut SymbolIndex,
    visited: &mut HashSet<PathBuf>,
) {
    for import in &program.imports {
        if import.path.is_empty() {
            continue;
        }
        if import.path[0] == "std" {
            let Some(root) = find_stdlib_root(entry_path) else {
                continue;
            };
            let Some(path) = stdlib_module_path(&root, &import.path) else {
                continue;
            };
            if path.exists() {
                load_module_symbols(&path, &import.path, index, visited);
            }
            continue;
        }
        if let Some(path) = resolve_user_module_path(entry_path, &import.path) {
            load_module_symbols(&path, &import.path, index, visited);
        }
    }
}

pub(crate) fn build_symbol_index_with_paths(uri: &str, program: &Program) -> (SymbolIndex, Vec<PathBuf>) {
    let mut index = SymbolIndex::new(program, uri);
    let Some(path) = uri_to_path(uri) else {
        return (index, Vec::new());
    };
    let mut visited = HashSet::new();
    visited.insert(path.clone());
    add_imports_to_index(&path, program, &mut index, &mut visited);
    let mut paths: Vec<PathBuf> = visited.into_iter().collect();
    paths.sort();
    (index, paths)
}

pub(crate) fn build_symbol_index(uri: &str, program: &Program) -> SymbolIndex {
    build_symbol_index_with_paths(uri, program).0
}
