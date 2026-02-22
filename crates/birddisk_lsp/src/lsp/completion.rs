use super::definitions::{build_symbol_index, member_context};
use super::imports::{import_modules, stdlib_module_functions, user_module_functions};
use super::server::{extract_uri_and_position, line_at_position, uri_to_path, Server};
use super::signature::env_for_position;
use super::stdlib::stdlib_signatures;
use birddisk_core::ast::Program;
use birddisk_core::lexer::{self, Token, TokenKind};
use birddisk_core::Position;
use serde_json::{json, Value};
use std::collections::HashSet;

impl Server {
    pub(crate) fn handle_completion(&self, params: Value) -> Value {
        let Some((uri, pos)) = extract_uri_and_position(&params) else {
            return Value::Null;
        };
        let Some(text) = self.document_text(&uri) else {
            return Value::Null;
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => Vec::new(),
        };
        let program = birddisk_core::parser::parse(&tokens).ok();
        let items = completion_items(&uri, &text, &tokens, program.as_ref(), pos);
        json!({"isIncomplete": false, "items": items})
    }
}

pub(crate) fn completion_items(
    uri: &str,
    text: &str,
    tokens: &[Token],
    program: Option<&Program>,
    pos: Position,
) -> Vec<Value> {
    let mut items = Vec::new();
    let keywords = [
        "rule",
        "set",
        "put",
        "yield",
        "when",
        "otherwise",
        "repeat",
        "while",
        "end",
        "import",
        "book",
        "field",
        "new",
        "as",
    ];
    if let Some(line) = line_at_position(text, pos.line) {
        if line.trim_start().starts_with("import ") {
            for module in import_modules(uri) {
                items.push(completion_item(&module, 9));
            }
            return items;
        }
    }

    if let Some(path) = completion_path(tokens, pos) {
        if path.after_colon && !path.segments.is_empty() {
            if path.segments[0] == "std" {
                if path.segments.len() == 1 {
                    for module in import_modules(uri) {
                        let tail = module.strip_prefix("std::").unwrap_or(&module);
                        if !tail.is_empty() {
                            items.push(completion_item(tail, 9));
                        }
                    }
                    return items;
                }
                let module_name = path.segments.join("::");
                let stdlib_root =
                    uri_to_path(uri).and_then(|path| super::imports::find_stdlib_root(&path));
                let functions = stdlib_module_functions(&module_name, stdlib_root.as_deref());
                for func in functions {
                    items.push(completion_item(&func, 3));
                }
                if !items.is_empty() {
                    return items;
                }
            } else if let Some(program) = program {
                let mut next_segments = HashSet::new();
                let mut exact_modules: Vec<Vec<String>> = Vec::new();
                for import in &program.imports {
                    if import.path.is_empty() || import.path[0] == "std" {
                        continue;
                    }
                    if import.path.len() > path.segments.len()
                        && import.path[..path.segments.len()] == path.segments[..]
                    {
                        next_segments.insert(import.path[path.segments.len()].clone());
                    } else if import.path == path.segments {
                        exact_modules.push(import.path.clone());
                    }
                }
                if !next_segments.is_empty() {
                    let mut segments: Vec<String> = next_segments.into_iter().collect();
                    segments.sort();
                    for segment in segments {
                        items.push(completion_item(&segment, 9));
                    }
                    return items;
                }
                if !exact_modules.is_empty() {
                    if let Some(entry_path) = uri_to_path(uri) {
                        for module in exact_modules {
                            let funcs = user_module_functions(&entry_path, &module);
                            for func in funcs {
                                items.push(completion_item(&func, 3));
                            }
                        }
                    }
                    if !items.is_empty() {
                        return items;
                    }
                }
            }
        }
    }

    if let Some((base, member)) = member_context(tokens, pos) {
        if !base.is_empty() {
            if base == "std" {
                for module in import_modules(uri) {
                    let tail = module.strip_prefix("std::").unwrap_or(&module);
                    if !tail.is_empty() {
                        items.push(completion_item(tail, 9));
                    }
                }
                return items;
            }
            if let Some(program) = program {
                let index = build_symbol_index(uri, program);
                let stdlib_root =
                    uri_to_path(uri).and_then(|path| super::imports::find_stdlib_root(&path));
                let stdlib = stdlib_signatures(program, stdlib_root.as_deref());
                let env = env_for_position(program, pos, &index, &stdlib);
                let book_name = match env.get(&base) {
                    Some(birddisk_core::ast::Type::Book(name)) => Some(name),
                    _ => index.books.get(&base).map(|book| book.name.clone()),
                };
                if let Some(book_name) = book_name {
                    if let Some(book) = index.books.get(&book_name) {
                        for field in book.fields.values() {
                            items.push(completion_item(&field.name, 5));
                        }
                        for method in book.methods.values() {
                            items.push(completion_item(&method.name, 3));
                        }
                        if !items.is_empty() {
                            return items;
                        }
                    }
                }
            }
        }
        if !member.is_empty() {
            return items;
        }
    }

    for keyword in &keywords {
        items.push(completion_item(keyword, 14));
    }
    if let Some(program) = program {
        let index = build_symbol_index(uri, program);
        for name in index.functions.keys() {
            items.push(completion_item(name, 3));
        }
        for name in index.books.keys() {
            items.push(completion_item(name, 7));
        }
    }
    items
}

#[derive(Clone)]
struct CompletionPath {
    segments: Vec<String>,
    after_colon: bool,
}

fn completion_path(tokens: &[Token], pos: Position) -> Option<CompletionPath> {
    let (mut idx, token) = super::server::token_before_position(tokens, pos)?;
    let after_colon = matches!(token.kind, TokenKind::DoubleColon);
    if matches!(token.kind, TokenKind::DoubleColon) {
        if idx == 0 {
            return None;
        }
        idx = idx.saturating_sub(1);
    }
    if path_segment(tokens.get(idx)?).is_none() {
        return None;
    }
    let mut segments = Vec::new();
    let mut expect_ident = true;
    loop {
        if expect_ident {
            let tok = tokens.get(idx)?;
            if let Some(segment) = path_segment(tok) {
                segments.push(segment);
            } else {
                break;
            }
            if idx == 0 {
                break;
            }
            idx = idx.saturating_sub(1);
            expect_ident = false;
        } else {
            let tok = tokens.get(idx)?;
            if !matches!(tok.kind, TokenKind::DoubleColon) {
                break;
            }
            if idx == 0 {
                break;
            }
            idx = idx.saturating_sub(1);
            expect_ident = true;
        }
    }
    if segments.is_empty() {
        return None;
    }
    segments.reverse();
    Some(CompletionPath {
        segments,
        after_colon,
    })
}

fn path_segment(token: &Token) -> Option<String> {
    match &token.kind {
        TokenKind::Ident(name) => Some(name.clone()),
        TokenKind::TypeI64 => Some("i64".to_string()),
        TokenKind::TypeF64 => Some("f64".to_string()),
        TokenKind::TypeBool => Some("bool".to_string()),
        TokenKind::TypeString => Some("string".to_string()),
        TokenKind::TypeU8 => Some("u8".to_string()),
        TokenKind::TypeVoid => Some("void".to_string()),
        _ => None,
    }
}

fn completion_item(label: &str, kind: i32) -> Value {
    json!({"label": label, "kind": kind})
}
