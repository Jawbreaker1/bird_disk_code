use super::definitions::{member_context, SymbolIndex};
use super::server::{
    extract_uri_and_position, format_ast_params, format_params, span_contains, token_at_position,
    type_name, Server,
};
use birddisk_core::ast::Program;
use birddisk_core::lexer::{self, Token, TokenKind};
use birddisk_core::Position;
use serde_json::{json, Value};

impl Server {
    pub(crate) fn handle_hover(&self, params: Value) -> Value {
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
        let index = SymbolIndex::new(&program, &uri);
        let info = hover_info(&program, &tokens, &index, pos);
        match info {
            Some(contents) => json!({"contents": {"kind": "plaintext", "value": contents}}),
            None => Value::Null,
        }
    }
}

pub(crate) fn hover_info(
    program: &Program,
    tokens: &[Token],
    index: &SymbolIndex,
    pos: Position,
) -> Option<String> {
    if let Some((_, token)) = token_at_position(tokens, pos) {
        if let TokenKind::Ident(name) = &token.kind {
            if let Some((book, member)) = member_context(tokens, pos) {
                if let Some(book_info) = index.books.get(&book) {
                    if let Some(field) = book_info.fields.get(&member) {
                        return Some(format!("field {}: {}", field.name, type_name(&field.ty)));
                    }
                    if let Some(method) = book_info.methods.get(&member) {
                        return Some(format!(
                            "rule {}::{}({}) -> {}",
                            book_info.name,
                            method.name,
                            format_params(&method.params),
                            type_name(&method.return_type)
                        ));
                    }
                }
            }
            if let Some(info) = index.functions.get(name) {
                return Some(format!(
                    "rule {}({}) -> {}",
                    info.name,
                    format_params(&info.params),
                    type_name(&info.return_type)
                ));
            }
            if let Some(book_info) = index.books.get(name) {
                return Some(format!("book {}", book_info.name));
            }
        }
    }

    for book in &program.books {
        for field in &book.fields {
            if span_contains(field.span, pos) {
                return Some(format!("field {}: {}", field.name, type_name(&field.ty)));
            }
        }
        for method in &book.methods {
            for param in &method.params {
                if span_contains(param.span, pos) {
                    return Some(format!("param {}: {}", param.name, type_name(&param.ty)));
                }
            }
            if span_contains(method.span, pos) {
                return Some(format!(
                    "rule {}::{}({}) -> {}",
                    book.name,
                    method.name,
                    format_ast_params(&method.params),
                    type_name(&method.return_type)
                ));
            }
        }
        if span_contains(book.span, pos) {
            return Some(format!("book {}", book.name));
        }
    }
    for func in &program.functions {
        for param in &func.params {
            if span_contains(param.span, pos) {
                return Some(format!("param {}: {}", param.name, type_name(&param.ty)));
            }
        }
        if span_contains(func.span, pos) {
            return Some(format!(
                "rule {}({}) -> {}",
                func.name,
                format_ast_params(&func.params),
                type_name(&func.return_type)
            ));
        }
    }
    None
}
