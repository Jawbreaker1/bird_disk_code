use birddisk_core::lexer::{self, Token, TokenKind};
use birddisk_core::{Diagnostic, Position, Span};
use serde_json::{json, Value};
use std::cmp::Ordering;
use std::collections::HashMap;
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

pub(crate) const SEMANTIC_TOKEN_TYPES: [&str; 11] = [
    "keyword",
    "type",
    "function",
    "method",
    "class",
    "property",
    "parameter",
    "variable",
    "string",
    "number",
    "operator",
];

pub(crate) const SEMANTIC_TOKEN_MODIFIERS: [&str; 0] = [];

#[derive(Clone)]
pub(crate) struct Document {
    pub(crate) text: String,
}

pub(crate) struct Server {
    pub(crate) docs: HashMap<String, Document>,
    shutdown: bool,
}

impl Server {
    pub(crate) fn new() -> Self {
        Self {
            docs: HashMap::new(),
            shutdown: false,
        }
    }

    pub(crate) fn run<R: BufRead, W: Write>(
        &mut self,
        reader: &mut R,
        writer: &mut W,
    ) -> io::Result<()> {
        while let Some(message) = read_message(reader)? {
            if let Some(method) = message.get("method").and_then(|value| value.as_str()) {
                let id = message.get("id").cloned();
                let params = message.get("params").cloned().unwrap_or(Value::Null);
                match method {
                    "initialize" => {
                        let result = json!({
                            "capabilities": {
                                "textDocumentSync": 1,
                                "hoverProvider": true,
                                "definitionProvider": true,
                                "typeDefinitionProvider": true,
                                "renameProvider": true,
                                "completionProvider": {"triggerCharacters": [":"]},
                                "signatureHelpProvider": {"triggerCharacters": ["(", ","]},
                                "referencesProvider": true,
                                "documentSymbolProvider": true,
                                "workspaceSymbolProvider": true,
                                "semanticTokensProvider": {
                                    "legend": {
                                        "tokenTypes": SEMANTIC_TOKEN_TYPES,
                                        "tokenModifiers": SEMANTIC_TOKEN_MODIFIERS
                                    },
                                    "full": true
                                },
                                "inlayHintProvider": true
                            },
                            "serverInfo": {
                                "name": "birddisk-lsp",
                                "version": birddisk_core::VERSION
                            }
                        });
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "shutdown" => {
                        self.shutdown = true;
                        if let Some(id) = id {
                            send_response(writer, id, Value::Null)?;
                        }
                    }
                    "exit" => {
                        if self.shutdown {
                            break;
                        }
                    }
                    "textDocument/didOpen" => {
                        self.handle_did_open(&params);
                        if let Some(uri) = extract_uri(&params) {
                            self.publish_diagnostics(writer, &uri)?;
                        }
                    }
                    "textDocument/didChange" => {
                        self.handle_did_change(&params);
                    }
                    "textDocument/didSave" => {
                        if let Some(uri) = extract_uri(&params) {
                            self.publish_diagnostics(writer, &uri)?;
                        }
                    }
                    "textDocument/didClose" => {
                        if let Some(uri) = extract_uri(&params) {
                            self.docs.remove(&uri);
                            send_notification(
                                writer,
                                "textDocument/publishDiagnostics",
                                json!({"uri": uri, "diagnostics": []}),
                            )?;
                        }
                    }
                    "textDocument/hover" => {
                        let result = self.handle_hover(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/definition" => {
                        let result = self.handle_definition(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/typeDefinition" => {
                        let result = self.handle_type_definition(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/references" => {
                        let result = self.handle_references(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/completion" => {
                        let result = self.handle_completion(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/signatureHelp" => {
                        let result = self.handle_signature_help(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/documentSymbol" => {
                        let result = self.handle_document_symbols(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "workspace/symbol" => {
                        let result = self.handle_workspace_symbols(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/rename" => {
                        let result = self.handle_rename(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/semanticTokens/full" => {
                        let result = self.handle_semantic_tokens(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    "textDocument/inlayHint" => {
                        let result = self.handle_inlay_hints(params);
                        if let Some(id) = id {
                            send_response(writer, id, result)?;
                        }
                    }
                    _ => {
                        if let Some(id) = id {
                            send_response(writer, id, Value::Null)?;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub(crate) fn handle_did_open(&mut self, params: &Value) {
        let Some(text_doc) = params.get("textDocument") else {
            return;
        };
        let Some(uri) = text_doc.get("uri").and_then(|value| value.as_str()) else {
            return;
        };
        let text = text_doc
            .get("text")
            .and_then(|value| value.as_str())
            .unwrap_or("")
            .to_string();
        self.docs.insert(uri.to_string(), Document { text });
    }

    pub(crate) fn handle_did_change(&mut self, params: &Value) {
        let Some(text_doc) = params.get("textDocument") else {
            return;
        };
        let Some(uri) = text_doc.get("uri").and_then(|value| value.as_str()) else {
            return;
        };
        let Some(changes) = params.get("contentChanges").and_then(|value| value.as_array()) else {
            return;
        };
        let Some(change) = changes.first() else {
            return;
        };
        let Some(text) = change.get("text").and_then(|value| value.as_str()) else {
            return;
        };
        self.docs.insert(
            uri.to_string(),
            Document {
                text: text.to_string(),
            },
        );
    }

    pub(crate) fn handle_semantic_tokens(&self, params: Value) -> Value {
        let Some(uri) = extract_uri(&params) else {
            return json!({"data": []});
        };
        let Some(text) = self.document_text(&uri) else {
            return json!({"data": []});
        };
        let tokens = match lexer::lex(&text) {
            Ok(tokens) => tokens,
            Err(_) => return json!({"data": []}),
        };
        let data = semantic_tokens(&tokens);
        json!({"data": data})
    }

    pub(crate) fn publish_diagnostics<W: Write>(&self, writer: &mut W, uri: &str) -> io::Result<()> {
        let Some(path) = uri_to_path(uri) else {
            return Ok(());
        };
        let diagnostics = match birddisk_core::parse_and_typecheck(path.to_string_lossy().as_ref()) {
            Ok(_) => Vec::new(),
            Err(diags) => diags,
        };
        let lsp_diags: Vec<Value> = diagnostics
            .into_iter()
            .map(to_lsp_diagnostic)
            .collect();
        send_notification(
            writer,
            "textDocument/publishDiagnostics",
            json!({"uri": uri, "diagnostics": lsp_diags}),
        )
    }

    pub(crate) fn document_text(&self, uri: &str) -> Option<String> {
        if let Some(doc) = self.docs.get(uri) {
            return Some(doc.text.clone());
        }
        let path = uri_to_path(uri)?;
        std::fs::read_to_string(path).ok()
    }
}

pub(crate) fn read_message<R: BufRead>(reader: &mut R) -> io::Result<Option<Value>> {
    let mut content_length = None;
    loop {
        let mut line = String::new();
        let bytes = reader.read_line(&mut line)?;
        if bytes == 0 {
            return Ok(None);
        }
        let line = line.trim_end_matches(&['\r', '\n'][..]);
        if line.is_empty() {
            break;
        }
        if let Some(rest) = line.strip_prefix("Content-Length:") {
            let len = rest.trim().parse::<usize>().ok();
            content_length = len;
        }
    }
    let Some(length) = content_length else {
        return Ok(None);
    };
    let mut body = vec![0; length];
    reader.read_exact(&mut body)?;
    let value = serde_json::from_slice(&body).unwrap_or(Value::Null);
    Ok(Some(value))
}

pub(crate) fn send_response<W: Write>(writer: &mut W, id: Value, result: Value) -> io::Result<()> {
    let response = json!({"jsonrpc": "2.0", "id": id, "result": result});
    send_message(writer, &response)
}

pub(crate) fn send_notification<W: Write>(
    writer: &mut W,
    method: &str,
    params: Value,
) -> io::Result<()> {
    let message = json!({"jsonrpc": "2.0", "method": method, "params": params});
    send_message(writer, &message)
}

pub(crate) fn send_message<W: Write>(writer: &mut W, value: &Value) -> io::Result<()> {
    let payload = serde_json::to_vec(value).unwrap_or_default();
    write!(writer, "Content-Length: {}\r\n\r\n", payload.len())?;
    writer.write_all(&payload)?;
    writer.flush()
}

pub(crate) fn extract_uri(params: &Value) -> Option<String> {
    params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|value| value.as_str())
        .map(|value| value.to_string())
}

pub(crate) fn extract_uri_and_position(params: &Value) -> Option<(String, Position)> {
    let uri = params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|value| value.as_str())
        .map(|value| value.to_string())?;
    let pos = params.get("position")?;
    let line = pos.get("line")?.as_u64()? as u32 + 1;
    let col = pos.get("character")?.as_u64()? as u32 + 1;
    Some((uri, Position::new(line, col)))
}

pub(crate) fn extract_range(params: &Value) -> Option<Span> {
    let range = params.get("range")?;
    let start = range.get("start")?;
    let end = range.get("end")?;
    let start_pos = lsp_position(start)?;
    let end_pos = lsp_position(end)?;
    Some(Span::new(start_pos, end_pos))
}

pub(crate) fn lsp_position(value: &Value) -> Option<Position> {
    let line = value.get("line")?.as_u64()? as u32 + 1;
    let col = value.get("character")?.as_u64()? as u32 + 1;
    Some(Position::new(line, col))
}

pub(crate) fn uri_to_path(uri: &str) -> Option<PathBuf> {
    let uri = uri.strip_prefix("file://")?;
    let mut bytes = Vec::new();
    let mut chars = uri.as_bytes().iter().copied().peekable();
    while let Some(ch) = chars.next() {
        if ch == b'%' {
            let hi = chars.next()?;
            let lo = chars.next()?;
            let hex = [hi, lo];
            let value = u8::from_str_radix(std::str::from_utf8(&hex).ok()?, 16).ok()?;
            bytes.push(value);
        } else {
            bytes.push(ch);
        }
    }
    let path_str = String::from_utf8_lossy(&bytes).to_string();
    Some(PathBuf::from(path_str))
}

pub(crate) fn path_to_uri(path: &Path) -> String {
    let mut uri = String::from("file://");
    for &byte in path.to_string_lossy().as_bytes() {
        let keep = matches!(byte, b'a'..=b'z'
            | b'A'..=b'Z'
            | b'0'..=b'9'
            | b'-'
            | b'_'
            | b'.'
            | b'~'
            | b'/'
            | b':');
        if keep {
            uri.push(byte as char);
        } else {
            uri.push_str(&format!("%{:02X}", byte));
        }
    }
    uri
}

pub(crate) fn to_lsp_diagnostic(diag: Diagnostic) -> Value {
    let severity = if diag.severity == "warning" { 2 } else { 1 };
    json!({
        "range": span_to_range(diag.span),
        "severity": severity,
        "code": diag.code,
        "source": "birddisk",
        "message": diag.message
    })
}

pub(crate) fn span_to_range(span: Span) -> Value {
    json!({
        "start": {
            "line": span.start.line.saturating_sub(1),
            "character": span.start.col.saturating_sub(1)
        },
        "end": {
            "line": span.end.line.saturating_sub(1),
            "character": span.end.col.saturating_sub(1)
        }
    })
}

pub(crate) fn token_at_position(tokens: &[Token], pos: Position) -> Option<(usize, &Token)> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, token)| span_contains(token.span, pos))
}

pub(crate) fn token_before_position(tokens: &[Token], pos: Position) -> Option<(usize, &Token)> {
    let mut best: Option<(usize, &Token)> = None;
    for (idx, token) in tokens.iter().enumerate() {
        if position_leq(token.span.end, pos) {
            best = Some((idx, token));
        }
    }
    best
}

pub(crate) fn span_contains(span: Span, pos: Position) -> bool {
    position_leq(span.start, pos) && position_lt(pos, span.end)
}

pub(crate) fn position_leq(left: Position, right: Position) -> bool {
    match left.line.cmp(&right.line) {
        Ordering::Less => true,
        Ordering::Greater => false,
        Ordering::Equal => left.col <= right.col,
    }
}

pub(crate) fn position_lt(left: Position, right: Position) -> bool {
    match left.line.cmp(&right.line) {
        Ordering::Less => true,
        Ordering::Greater => false,
        Ordering::Equal => left.col < right.col,
    }
}

pub(crate) fn line_at_position(text: &str, line: u32) -> Option<&str> {
    let target = line.saturating_sub(1) as usize;
    text.lines().nth(target)
}

pub(crate) fn format_params(params: &[(String, birddisk_core::ast::Type)]) -> String {
    params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, type_name(ty)))
        .collect::<Vec<_>>()
        .join(", ")
}

pub(crate) fn format_ast_params(params: &[birddisk_core::ast::Param]) -> String {
    params
        .iter()
        .map(|param| format!("{}: {}", param.name, type_name(&param.ty)))
        .collect::<Vec<_>>()
        .join(", ")
}

pub(crate) fn type_name(ty: &birddisk_core::ast::Type) -> String {
    match ty {
        birddisk_core::ast::Type::I64 => "i64".to_string(),
        birddisk_core::ast::Type::F64 => "f64".to_string(),
        birddisk_core::ast::Type::Bool => "bool".to_string(),
        birddisk_core::ast::Type::String => "string".to_string(),
        birddisk_core::ast::Type::U8 => "u8".to_string(),
        birddisk_core::ast::Type::Void => "void".to_string(),
        birddisk_core::ast::Type::Array(inner) => format!("{}[]", type_name(inner)),
        birddisk_core::ast::Type::Book(name) => name.clone(),
    }
}

pub(crate) fn position_to_lsp(position: Position) -> Value {
    json!({
        "line": position.line.saturating_sub(1),
        "character": position.col.saturating_sub(1)
    })
}

pub(crate) fn semantic_tokens(tokens: &[Token]) -> Vec<u32> {
    let mut results = Vec::new();
    let mut prev_line = 0;
    let mut prev_col = 0;
    for (idx, token) in tokens.iter().enumerate() {
        let kind = match semantic_kind(tokens, idx) {
            Some(kind) => kind,
            None => continue,
        };
        if token.span.start.line != token.span.end.line {
            continue;
        }
        let line = token.span.start.line.saturating_sub(1);
        let col = token.span.start.col.saturating_sub(1);
        let len = token
            .span
            .end
            .col
            .saturating_sub(token.span.start.col);
        if len == 0 {
            continue;
        }
        let token_type = semantic_token_type_index(kind);
        let delta_line = line.saturating_sub(prev_line);
        let delta_start = if delta_line == 0 { col.saturating_sub(prev_col) } else { col };
        results.push(delta_line);
        results.push(delta_start);
        results.push(len);
        results.push(token_type);
        results.push(0);
        prev_line = line;
        prev_col = col;
    }
    results
}

#[derive(Clone, Copy)]
enum SemanticTokenKind {
    Keyword,
    Type,
    Function,
    Method,
    Class,
    Property,
    Parameter,
    Variable,
    String,
    Number,
    Operator,
}

fn semantic_kind(tokens: &[Token], idx: usize) -> Option<SemanticTokenKind> {
    let token = &tokens[idx];
    match &token.kind {
        TokenKind::Import
        | TokenKind::Rule
        | TokenKind::Set
        | TokenKind::Put
        | TokenKind::Yield
        | TokenKind::When
        | TokenKind::Otherwise
        | TokenKind::Repeat
        | TokenKind::While
        | TokenKind::Book
        | TokenKind::Field
        | TokenKind::New
        | TokenKind::As
        | TokenKind::End
        | TokenKind::Array => Some(SemanticTokenKind::Keyword),
        TokenKind::TypeI64
        | TokenKind::TypeF64
        | TokenKind::TypeBool
        | TokenKind::TypeString
        | TokenKind::TypeU8
        | TokenKind::TypeVoid => Some(SemanticTokenKind::Type),
        TokenKind::BoolLit(_) => Some(SemanticTokenKind::Keyword),
        TokenKind::IntLit(_) | TokenKind::FloatLit(_) => Some(SemanticTokenKind::Number),
        TokenKind::StringLit(_) => Some(SemanticTokenKind::String),
        TokenKind::Plus
        | TokenKind::Minus
        | TokenKind::Star
        | TokenKind::Slash
        | TokenKind::Percent
        | TokenKind::EqEq
        | TokenKind::NotEq
        | TokenKind::Lt
        | TokenKind::LtEq
        | TokenKind::Gt
        | TokenKind::GtEq
        | TokenKind::AndAnd
        | TokenKind::OrOr
        | TokenKind::Bang
        | TokenKind::Assign => Some(SemanticTokenKind::Operator),
        TokenKind::Ident(_) => semantic_ident_kind(tokens, idx),
        _ => None,
    }
}

fn semantic_ident_kind(tokens: &[Token], idx: usize) -> Option<SemanticTokenKind> {
    let prev = tokens.get(idx.saturating_sub(1)).map(|tok| &tok.kind);
    let next = tokens.get(idx + 1).map(|tok| &tok.kind);

    if matches!(prev, Some(TokenKind::Rule)) {
        return Some(SemanticTokenKind::Function);
    }
    if matches!(next, Some(TokenKind::Colon))
        && matches!(prev, Some(TokenKind::LParen) | Some(TokenKind::Comma))
    {
        return Some(SemanticTokenKind::Parameter);
    }
    if matches!(prev, Some(TokenKind::Book)) {
        return Some(SemanticTokenKind::Class);
    }
    if matches!(prev, Some(TokenKind::Field)) {
        return Some(SemanticTokenKind::Property);
    }
    if matches!(prev, Some(TokenKind::New)) {
        return Some(SemanticTokenKind::Class);
    }
    if matches!(prev, Some(TokenKind::Colon) | Some(TokenKind::Arrow)) {
        return Some(SemanticTokenKind::Type);
    }
    if matches!(prev, Some(TokenKind::DoubleColon)) && matches!(next, Some(TokenKind::LParen)) {
        return Some(SemanticTokenKind::Method);
    }
    if matches!(next, Some(TokenKind::LParen)) {
        return Some(SemanticTokenKind::Function);
    }
    if matches!(prev, Some(TokenKind::DoubleColon)) {
        return Some(SemanticTokenKind::Property);
    }
    Some(SemanticTokenKind::Variable)
}

fn semantic_token_type_index(kind: SemanticTokenKind) -> u32 {
    match kind {
        SemanticTokenKind::Keyword => 0,
        SemanticTokenKind::Type => 1,
        SemanticTokenKind::Function => 2,
        SemanticTokenKind::Method => 3,
        SemanticTokenKind::Class => 4,
        SemanticTokenKind::Property => 5,
        SemanticTokenKind::Parameter => 6,
        SemanticTokenKind::Variable => 7,
        SemanticTokenKind::String => 8,
        SemanticTokenKind::Number => 9,
        SemanticTokenKind::Operator => 10,
    }
}
