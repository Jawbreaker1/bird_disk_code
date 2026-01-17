use birddisk_core::ast::{Book, Expr, ExprKind, Function, Program, Stmt, Type};
use birddisk_core::lexer::{self, Token, TokenKind};
use birddisk_core::{Diagnostic, Position, Span};
use serde_json::{json, Value};
use std::cmp::Ordering;
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead, Write};
use std::path::{Path, PathBuf};

fn main() -> io::Result<()> {
    let stdin = io::stdin();
    let mut reader = io::BufReader::new(stdin.lock());
    let stdout = io::stdout();
    let mut writer = io::BufWriter::new(stdout.lock());
    let mut server = Server::new();
    server.run(&mut reader, &mut writer)
}

const SEMANTIC_TOKEN_TYPES: [&str; 11] = [
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

const SEMANTIC_TOKEN_MODIFIERS: [&str; 0] = [];

struct Document {
    text: String,
}

struct Server {
    docs: HashMap<String, Document>,
    shutdown: bool,
}

impl Server {
    fn new() -> Self {
        Self {
            docs: HashMap::new(),
            shutdown: false,
        }
    }

    fn run<R: BufRead, W: Write>(&mut self, reader: &mut R, writer: &mut W) -> io::Result<()> {
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

    fn handle_did_open(&mut self, params: &Value) {
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

    fn handle_did_change(&mut self, params: &Value) {
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

    fn handle_hover(&self, params: Value) -> Value {
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

    fn handle_definition(&self, params: Value) -> Value {
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

    fn handle_type_definition(&self, params: Value) -> Value {
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

    fn handle_references(&self, params: Value) -> Value {
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

    fn handle_completion(&self, params: Value) -> Value {
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

    fn handle_signature_help(&self, params: Value) -> Value {
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
        let stdlib_root = uri_to_path(&uri).and_then(|path| find_stdlib_root(&path));
        let stdlib = stdlib_signatures(&program, stdlib_root.as_deref());
        let index = build_symbol_index(&uri, &program);
        signature_help_at_position(&program, pos, &index, &stdlib).unwrap_or(Value::Null)
    }

    fn handle_document_symbols(&self, params: Value) -> Value {
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

    fn handle_workspace_symbols(&self, params: Value) -> Value {
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

    fn handle_rename(&self, params: Value) -> Value {
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

    fn handle_semantic_tokens(&self, params: Value) -> Value {
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

    fn handle_inlay_hints(&self, params: Value) -> Value {
        let Some(uri) = extract_uri(&params) else {
            return Value::Null;
        };
        let Some(range) = extract_range(&params) else {
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
        let hints = inlay_hints(&program, range, &uri);
        Value::Array(hints)
    }

    fn publish_diagnostics<W: Write>(&self, writer: &mut W, uri: &str) -> io::Result<()> {
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

    fn document_text(&self, uri: &str) -> Option<String> {
        if let Some(doc) = self.docs.get(uri) {
            return Some(doc.text.clone());
        }
        let path = uri_to_path(uri)?;
        std::fs::read_to_string(path).ok()
    }
}

fn read_message<R: BufRead>(reader: &mut R) -> io::Result<Option<Value>> {
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

fn send_response<W: Write>(writer: &mut W, id: Value, result: Value) -> io::Result<()> {
    let response = json!({"jsonrpc": "2.0", "id": id, "result": result});
    send_message(writer, &response)
}

fn send_notification<W: Write>(writer: &mut W, method: &str, params: Value) -> io::Result<()> {
    let message = json!({"jsonrpc": "2.0", "method": method, "params": params});
    send_message(writer, &message)
}

fn send_message<W: Write>(writer: &mut W, value: &Value) -> io::Result<()> {
    let payload = serde_json::to_vec(value).unwrap_or_default();
    write!(writer, "Content-Length: {}\r\n\r\n", payload.len())?;
    writer.write_all(&payload)?;
    writer.flush()
}

fn extract_uri(params: &Value) -> Option<String> {
    params
        .get("textDocument")
        .and_then(|doc| doc.get("uri"))
        .and_then(|value| value.as_str())
        .map(|value| value.to_string())
}

fn extract_uri_and_position(params: &Value) -> Option<(String, Position)> {
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

struct CompletionPath {
    segments: Vec<String>,
    after_colon: bool,
}

fn extract_range(params: &Value) -> Option<Span> {
    let range = params.get("range")?;
    let start = range.get("start")?;
    let end = range.get("end")?;
    let start_pos = lsp_position(start)?;
    let end_pos = lsp_position(end)?;
    Some(Span::new(start_pos, end_pos))
}

fn lsp_position(value: &Value) -> Option<Position> {
    let line = value.get("line")?.as_u64()? as u32 + 1;
    let col = value.get("character")?.as_u64()? as u32 + 1;
    Some(Position::new(line, col))
}

fn uri_to_path(uri: &str) -> Option<PathBuf> {
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

fn path_to_uri(path: &Path) -> String {
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

fn to_lsp_diagnostic(diag: Diagnostic) -> Value {
    let severity = if diag.severity == "warning" { 2 } else { 1 };
    json!({
        "range": span_to_range(diag.span),
        "severity": severity,
        "code": diag.code,
        "source": "birddisk",
        "message": diag.message
    })
}

fn span_to_range(span: Span) -> Value {
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

fn token_at_position(tokens: &[Token], pos: Position) -> Option<(usize, &Token)> {
    tokens
        .iter()
        .enumerate()
        .find(|(_, token)| span_contains(token.span, pos))
}

fn token_before_position(tokens: &[Token], pos: Position) -> Option<(usize, &Token)> {
    let mut best: Option<(usize, &Token)> = None;
    for (idx, token) in tokens.iter().enumerate() {
        if position_leq(token.span.end, pos) {
            best = Some((idx, token));
        }
    }
    best
}

fn completion_path(tokens: &[Token], pos: Position) -> Option<CompletionPath> {
    let (mut idx, token) = token_before_position(tokens, pos)?;
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
        TokenKind::TypeBool => Some("bool".to_string()),
        TokenKind::TypeString => Some("string".to_string()),
        TokenKind::TypeU8 => Some("u8".to_string()),
        TokenKind::TypeVoid => Some("void".to_string()),
        _ => None,
    }
}

fn span_contains(span: Span, pos: Position) -> bool {
    position_leq(span.start, pos) && position_lt(pos, span.end)
}

fn position_leq(left: Position, right: Position) -> bool {
    match left.line.cmp(&right.line) {
        Ordering::Less => true,
        Ordering::Greater => false,
        Ordering::Equal => left.col <= right.col,
    }
}

fn position_lt(left: Position, right: Position) -> bool {
    match left.line.cmp(&right.line) {
        Ordering::Less => true,
        Ordering::Greater => false,
        Ordering::Equal => left.col < right.col,
    }
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

struct SymbolIndex {
    functions: HashMap<String, FunctionInfo>,
    books: HashMap<String, BookInfo>,
}

#[derive(Clone)]
struct FunctionInfo {
    name: String,
    params: Vec<(String, Type)>,
    return_type: Type,
    span: Span,
    uri: String,
}

#[derive(Clone)]
struct FieldInfo {
    name: String,
    ty: Type,
    span: Span,
    uri: String,
}

#[derive(Clone)]
struct BookInfo {
    name: String,
    span: Span,
    uri: String,
    fields: HashMap<String, FieldInfo>,
    methods: HashMap<String, FunctionInfo>,
}

impl SymbolIndex {
    fn new(program: &Program, uri: &str) -> Self {
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
struct CallSignature {
    params: Vec<String>,
    return_type: Type,
}

#[derive(Clone)]
struct TypeEnv {
    scopes: Vec<HashMap<String, Type>>,
}

impl TypeEnv {
    fn new() -> Self {
        Self {
            scopes: vec![HashMap::new()],
        }
    }

    fn insert(&mut self, name: String, ty: Type) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, ty);
        }
    }

    fn get(&self, name: &str) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(ty) = scope.get(name) {
                return Some(ty.clone());
            }
        }
        None
    }

    fn push(&mut self) {
        self.scopes.push(HashMap::new());
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

fn semantic_tokens(tokens: &[Token]) -> Vec<u32> {
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
        let delta_start = if delta_line == 0 {
            col.saturating_sub(prev_col)
        } else {
            col
        };
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
        | TokenKind::End
        | TokenKind::Array => Some(SemanticTokenKind::Keyword),
        TokenKind::TypeI64
        | TokenKind::TypeBool
        | TokenKind::TypeString
        | TokenKind::TypeU8
        | TokenKind::TypeVoid => Some(SemanticTokenKind::Type),
        TokenKind::BoolLit(_) => Some(SemanticTokenKind::Keyword),
        TokenKind::IntLit(_) => Some(SemanticTokenKind::Number),
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

fn inlay_hints(program: &Program, range: Span, uri: &str) -> Vec<Value> {
    let index = SymbolIndex::new(program, uri);
    let stdlib_root = uri_to_path(uri).and_then(|path| find_stdlib_root(&path));
    let stdlib = stdlib_signatures(program, stdlib_root.as_deref());
    let mut hints = Vec::new();
    for func in &program.functions {
        let mut env = TypeEnv::new();
        for param in &func.params {
            env.insert(param.name.clone(), param.ty.clone());
        }
        collect_inlay_hints_in_stmts(
            &func.body,
            &mut env,
            &index,
            &stdlib,
            range,
            &mut hints,
        );
    }
    for book in &program.books {
        for method in &book.methods {
            let mut env = TypeEnv::new();
            for param in &method.params {
                env.insert(param.name.clone(), param.ty.clone());
            }
            collect_inlay_hints_in_stmts(
                &method.body,
                &mut env,
                &index,
                &stdlib,
                range,
                &mut hints,
            );
        }
    }
    hints
}

fn stdlib_signatures(
    program: &Program,
    root: Option<&Path>,
) -> HashMap<String, CallSignature> {
    let mut signatures = HashMap::new();
    if has_import(program, &["std", "string"]) {
        signatures.insert(
            "std::string::len".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::string::concat".to_string(),
            CallSignature {
                params: vec!["left".to_string(), "right".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::string::eq".to_string(),
            CallSignature {
                params: vec!["left".to_string(), "right".to_string()],
                return_type: Type::Bool,
            },
        );
        signatures.insert(
            "std::string::bytes".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::Array(Box::new(Type::U8)),
            },
        );
        signatures.insert(
            "std::string::from_bytes".to_string(),
            CallSignature {
                params: vec!["bytes".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::string::to_i64".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::string::from_i64".to_string(),
            CallSignature {
                params: vec!["value".to_string()],
                return_type: Type::String,
            },
        );
    }
    if has_import(program, &["std", "bytes"]) {
        signatures.insert(
            "std::bytes::len".to_string(),
            CallSignature {
                params: vec!["bytes".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::bytes::eq".to_string(),
            CallSignature {
                params: vec!["left".to_string(), "right".to_string()],
                return_type: Type::Bool,
            },
        );
    }
    if has_import(program, &["std", "io"]) {
        signatures.insert(
            "std::io::print".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::Void,
            },
        );
        signatures.insert(
            "std::io::read_line".to_string(),
            CallSignature {
                params: Vec::new(),
                return_type: Type::String,
            },
        );
    }
    if has_import(program, &["std", "time"]) {
        signatures.insert(
            "std::time::now_ms".to_string(),
            CallSignature {
                params: Vec::new(),
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::time::sleep_ms".to_string(),
            CallSignature {
                params: vec!["ms".to_string()],
                return_type: Type::I64,
            },
        );
    }
    if has_import(program, &["std", "fs"]) {
        signatures.insert(
            "std::fs::read_text".to_string(),
            CallSignature {
                params: vec!["path".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::fs::write_text".to_string(),
            CallSignature {
                params: vec!["path".to_string(), "text".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::fs::read_bytes".to_string(),
            CallSignature {
                params: vec!["path".to_string()],
                return_type: Type::Array(Box::new(Type::U8)),
            },
        );
        signatures.insert(
            "std::fs::write_bytes".to_string(),
            CallSignature {
                params: vec!["path".to_string(), "bytes".to_string()],
                return_type: Type::I64,
            },
        );
    }
    if has_import(program, &["std", "path"]) {
        signatures.insert(
            "std::path::join".to_string(),
            CallSignature {
                params: vec!["left".to_string(), "right".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::path::normalize".to_string(),
            CallSignature {
                params: vec!["path".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::path::basename".to_string(),
            CallSignature {
                params: vec!["path".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::path::dirname".to_string(),
            CallSignature {
                params: vec!["path".to_string()],
                return_type: Type::String,
            },
        );
    }
    if has_import(program, &["std", "env"]) {
        signatures.insert(
            "std::env::args".to_string(),
            CallSignature {
                params: Vec::new(),
                return_type: Type::Array(Box::new(Type::String)),
            },
        );
        signatures.insert(
            "std::env::get".to_string(),
            CallSignature {
                params: vec!["name".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::env::set_var".to_string(),
            CallSignature {
                params: vec!["name".to_string(), "value".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::env::cwd".to_string(),
            CallSignature {
                params: Vec::new(),
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::env::set_cwd".to_string(),
            CallSignature {
                params: vec!["path".to_string()],
                return_type: Type::I64,
            },
        );
    }
    if has_import(program, &["std", "json"]) {
        signatures.insert(
            "std::json::encode_i64".to_string(),
            CallSignature {
                params: vec!["value".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::json::encode_bool".to_string(),
            CallSignature {
                params: vec!["value".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::json::encode_string".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::json::decode_i64".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::json::decode_bool".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::Bool,
            },
        );
        signatures.insert(
            "std::json::decode_string".to_string(),
            CallSignature {
                params: vec!["text".to_string()],
                return_type: Type::String,
            },
        );
    }
    if let Some(root) = root {
        for import in &program.imports {
            let module_name = import.path.join("::");
            if module_name.starts_with("std::string")
                || module_name.starts_with("std::bytes")
                || module_name.starts_with("std::io")
                || module_name.starts_with("std::time")
                || module_name.starts_with("std::fs")
                || module_name.starts_with("std::path")
                || module_name.starts_with("std::env")
                || module_name.starts_with("std::json")
            {
                continue;
            }
            if let Some(path) = stdlib_module_path(root, &import.path) {
                let module_signatures = parse_stdlib_module(&module_name, &path);
                signatures.extend(module_signatures);
            }
        }
    }
    signatures
}

fn parse_stdlib_module(module_name: &str, path: &Path) -> HashMap<String, CallSignature> {
    let Ok(source) = std::fs::read_to_string(path) else {
        return HashMap::new();
    };
    let Ok(tokens) = lexer::lex(&source) else {
        return HashMap::new();
    };
    let Ok(program) = birddisk_core::parser::parse(&tokens) else {
        return HashMap::new();
    };
    let mut signatures = HashMap::new();
    for func in program.functions {
        signatures.insert(
            format!("{module_name}::{}", func.name),
            CallSignature {
                params: func.params.iter().map(|param| param.name.clone()).collect(),
                return_type: func.return_type,
            },
        );
    }
    signatures
}

fn stdlib_module_path(root: &Path, segments: &[String]) -> Option<PathBuf> {
    if segments.is_empty() {
        return None;
    }
    let mut path = root.to_path_buf();
    for (idx, part) in segments.iter().enumerate() {
        if idx + 1 == segments.len() {
            path.push(format!("{part}.bd"));
        } else {
            path.push(part);
        }
    }
    Some(path)
}

fn has_import(program: &Program, segments: &[&str]) -> bool {
    program.imports.iter().any(|import| {
        if import.path.len() != segments.len() {
            return false;
        }
        import
            .path
            .iter()
            .zip(segments.iter())
            .all(|(left, right)| left == right)
    })
}

fn collect_inlay_hints_in_stmts(
    stmts: &[Stmt],
    env: &mut TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
    range: Span,
    hints: &mut Vec<Value>,
) {
    for stmt in stmts {
        collect_inlay_hints_in_stmt(stmt, env, index, stdlib, range, hints);
    }
}

fn collect_inlay_hints_in_stmt(
    stmt: &Stmt,
    env: &mut TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
    range: Span,
    hints: &mut Vec<Value>,
) {
    match stmt {
        Stmt::Set { name, ty, expr, .. } => {
            let inferred = collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            if let Some(ty) = ty {
                env.insert(name.clone(), ty.clone());
            } else if let Some(inferred) = inferred {
                env.insert(name.clone(), inferred);
            }
        }
        Stmt::Expr { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Put { name, expr, .. } => {
            let inferred = collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            if env.get(name).is_none() {
                if let Some(inferred) = inferred {
                    env.insert(name.clone(), inferred);
                }
            }
        }
        Stmt::PutIndex { index: idx_expr, expr, .. } => {
            collect_inlay_hints_in_expr(idx_expr, env, index, stdlib, range, hints);
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::PutField { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Yield { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Throw { expr, .. } => {
            collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
        }
        Stmt::Try {
            try_body,
            catch_name,
            catch_body,
            ..
        } => {
            let mut try_env = env.clone();
            try_env.push();
            collect_inlay_hints_in_stmts(try_body, &mut try_env, index, stdlib, range, hints);
            let mut catch_env = env.clone();
            catch_env.push();
            catch_env.insert(catch_name.clone(), Type::String);
            collect_inlay_hints_in_stmts(catch_body, &mut catch_env, index, stdlib, range, hints);
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            collect_inlay_hints_in_expr(cond, env, index, stdlib, range, hints);
            let mut then_env = env.clone();
            then_env.push();
            collect_inlay_hints_in_stmts(then_body, &mut then_env, index, stdlib, range, hints);
            let mut else_env = env.clone();
            else_env.push();
            collect_inlay_hints_in_stmts(else_body, &mut else_env, index, stdlib, range, hints);
        }
        Stmt::Repeat { cond, body, .. } => {
            collect_inlay_hints_in_expr(cond, env, index, stdlib, range, hints);
            let mut loop_env = env.clone();
            loop_env.push();
            collect_inlay_hints_in_stmts(body, &mut loop_env, index, stdlib, range, hints);
        }
    }
}

fn collect_inlay_hints_in_expr(
    expr: &Expr,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
    range: Span,
    hints: &mut Vec<Value>,
) -> Option<Type> {
    match &expr.kind {
        ExprKind::Call { name, args } => {
            for arg in args {
                collect_inlay_hints_in_expr(arg, env, index, stdlib, range, hints);
            }
            if let Some(signature) = resolve_call_signature(name, env, index, stdlib) {
                let count = std::cmp::min(args.len(), signature.params.len());
                for idx in 0..count {
                    let span = args[idx].span;
                    if !span_in_range(span, range) {
                        continue;
                    }
                    let label = format!("{}:", signature.params[idx]);
                    let hint = json!({
                        "position": position_to_lsp(span.start),
                        "label": label,
                        "kind": 2,
                        "paddingRight": true
                    });
                    hints.push(hint);
                }
                return Some(signature.return_type);
            }
            None
        }
        ExprKind::New { book, args } => {
            for arg in args {
                collect_inlay_hints_in_expr(arg, env, index, stdlib, range, hints);
            }
            Some(Type::Book(book.clone()))
        }
        ExprKind::ArrayLit(elements) => {
            let mut element_type = None;
            for element in elements {
                let ty = collect_inlay_hints_in_expr(element, env, index, stdlib, range, hints);
                match (element_type.as_ref(), ty) {
                    (None, Some(found)) => element_type = Some(found),
                    (Some(existing), Some(found)) if existing == &found => {}
                    _ => element_type = None,
                }
            }
            element_type.map(|inner| Type::Array(Box::new(inner)))
        }
        ExprKind::ArrayNew { len } => {
            collect_inlay_hints_in_expr(len, env, index, stdlib, range, hints);
            None
        }
        ExprKind::Index { base, index: idx } => {
            let base_ty = collect_inlay_hints_in_expr(base, env, index, stdlib, range, hints);
            collect_inlay_hints_in_expr(idx, env, index, stdlib, range, hints);
            match base_ty {
                Some(Type::Array(inner)) => Some(*inner),
                _ => None,
            }
        }
        ExprKind::MemberAccess { base, field } => {
            if let Some(Type::Book(book)) = env.get(base) {
                if let Some(book_info) = index.books.get(&book) {
                    if let Some(field_info) = book_info.fields.get(field) {
                        return Some(field_info.ty.clone());
                    }
                }
            }
            None
        }
        ExprKind::Unary { op, expr } => {
            let inner = collect_inlay_hints_in_expr(expr, env, index, stdlib, range, hints);
            match op {
                birddisk_core::ast::UnaryOp::Neg => Some(Type::I64),
                birddisk_core::ast::UnaryOp::Not => Some(Type::Bool),
            }
            .or(inner)
        }
        ExprKind::Binary { op, left, right } => {
            let left_ty = collect_inlay_hints_in_expr(left, env, index, stdlib, range, hints);
            let right_ty = collect_inlay_hints_in_expr(right, env, index, stdlib, range, hints);
            match op {
                birddisk_core::ast::BinaryOp::EqEq
                | birddisk_core::ast::BinaryOp::NotEq
                | birddisk_core::ast::BinaryOp::Lt
                | birddisk_core::ast::BinaryOp::LtEq
                | birddisk_core::ast::BinaryOp::Gt
                | birddisk_core::ast::BinaryOp::GtEq
                | birddisk_core::ast::BinaryOp::AndAnd
                | birddisk_core::ast::BinaryOp::OrOr => Some(Type::Bool),
                birddisk_core::ast::BinaryOp::Add
                | birddisk_core::ast::BinaryOp::Sub
                | birddisk_core::ast::BinaryOp::Mul
                | birddisk_core::ast::BinaryOp::Div
                | birddisk_core::ast::BinaryOp::Mod => {
                    if left_ty == Some(Type::I64) || right_ty == Some(Type::I64) {
                        Some(Type::I64)
                    } else {
                        None
                    }
                }
            }
        }
        ExprKind::Ident(name) => env.get(name),
        ExprKind::Int(_) => Some(Type::I64),
        ExprKind::Bool(_) => Some(Type::Bool),
        ExprKind::String(_) => Some(Type::String),
    }
}

fn resolve_call_signature(
    name: &str,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<CallSignature> {
    if let Some(signature) = stdlib.get(name) {
        return Some(signature.clone());
    }
    if let Some(info) = index.functions.get(name) {
        return Some(CallSignature {
            params: info.params.iter().map(|(param, _)| param.clone()).collect(),
            return_type: info.return_type.clone(),
        });
    }
    if let Some((base, method)) = name.split_once("::") {
        if let Some(Type::Book(book_name)) = env.get(base) {
            if let Some(book_info) = index.books.get(&book_name) {
                if let Some(method_info) = book_info.methods.get(method) {
                    return Some(method_signature(method_info));
                }
            }
        }
        if let Some(book_info) = index.books.get(base) {
            if let Some(method_info) = book_info.methods.get(method) {
                return Some(method_signature(method_info));
            }
        }
    }
    None
}

fn method_signature(info: &FunctionInfo) -> CallSignature {
    let mut params: Vec<String> = info.params.iter().map(|(param, _)| param.clone()).collect();
    if params.first().map(|param| param == "self").unwrap_or(false) {
        params.remove(0);
    }
    CallSignature {
        params,
        return_type: info.return_type.clone(),
    }
}

fn span_in_range(span: Span, range: Span) -> bool {
    position_leq(range.start, span.start) && position_leq(span.start, range.end)
}

fn position_to_lsp(position: Position) -> Value {
    json!({
        "line": position.line.saturating_sub(1),
        "character": position.col.saturating_sub(1)
    })
}

fn hover_info(
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

struct DefinitionLocation {
    uri: String,
    span: Span,
}

struct ReferenceLocation {
    uri: String,
    span: Span,
}

fn definition_location(
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

fn type_definition_location(
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

enum CallKind {
    Rule(String),
    New(String),
}

struct CallInfo {
    kind: CallKind,
    args: Vec<Expr>,
}

struct SignatureInfo {
    label: String,
    params: Vec<String>,
}

fn signature_help_at_position(
    program: &Program,
    pos: Position,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<Value> {
    let call = find_call_in_program(program, pos)?;
    let env = env_for_position(program, pos, index, stdlib);
    let signature = match &call.kind {
        CallKind::Rule(name) => resolve_signature_info(name, &env, index, stdlib)?,
        CallKind::New(book) => resolve_constructor_signature(book, index),
    };
    let active = active_param_index(&call.args, pos);
    let params: Vec<Value> = signature
        .params
        .iter()
        .map(|label| json!({"label": label}))
        .collect();
    Some(json!({
        "signatures": [{
            "label": signature.label,
            "parameters": params
        }],
        "activeSignature": 0,
        "activeParameter": active
    }))
}

fn find_call_in_program(program: &Program, pos: Position) -> Option<CallInfo> {
    for func in &program.functions {
        if span_contains(func.span, pos) {
            if let Some(call) = find_call_in_stmts(&func.body, pos) {
                return Some(call);
            }
        }
    }
    for book in &program.books {
        for method in &book.methods {
            if span_contains(method.span, pos) {
                if let Some(call) = find_call_in_stmts(&method.body, pos) {
                    return Some(call);
                }
            }
        }
    }
    None
}

fn find_call_in_stmts(stmts: &[Stmt], pos: Position) -> Option<CallInfo> {
    for stmt in stmts {
        if let Some(call) = find_call_in_stmt(stmt, pos) {
            return Some(call);
        }
    }
    None
}

fn find_call_in_stmt(stmt: &Stmt, pos: Position) -> Option<CallInfo> {
    match stmt {
        Stmt::Set { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Expr { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Put { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::PutIndex { index, expr, .. } => {
            find_call_in_expr(index, pos).or_else(|| find_call_in_expr(expr, pos))
        }
        Stmt::PutField { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Yield { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Throw { expr, .. } => find_call_in_expr(expr, pos),
        Stmt::Try {
            try_body,
            catch_body,
            ..
        } => find_call_in_stmts(try_body, pos).or_else(|| find_call_in_stmts(catch_body, pos)),
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => find_call_in_expr(cond, pos)
            .or_else(|| find_call_in_stmts(then_body, pos))
            .or_else(|| find_call_in_stmts(else_body, pos)),
        Stmt::Repeat { cond, body, .. } => {
            find_call_in_expr(cond, pos).or_else(|| find_call_in_stmts(body, pos))
        }
    }
}

fn find_call_in_expr(expr: &Expr, pos: Position) -> Option<CallInfo> {
    if !span_contains(expr.span, pos) {
        return None;
    }
    match &expr.kind {
        ExprKind::Call { name, args } => {
            for arg in args {
                if let Some(call) = find_call_in_expr(arg, pos) {
                    return Some(call);
                }
            }
            Some(CallInfo {
                kind: CallKind::Rule(name.clone()),
                args: args.clone(),
            })
        }
        ExprKind::New { book, args } => {
            for arg in args {
                if let Some(call) = find_call_in_expr(arg, pos) {
                    return Some(call);
                }
            }
            Some(CallInfo {
                kind: CallKind::New(book.clone()),
                args: args.clone(),
            })
        }
        ExprKind::ArrayLit(elements) => elements
            .iter()
            .find_map(|element| find_call_in_expr(element, pos)),
        ExprKind::ArrayNew { len } => find_call_in_expr(len, pos),
        ExprKind::Index { base, index } => {
            find_call_in_expr(base, pos).or_else(|| find_call_in_expr(index, pos))
        }
        ExprKind::Unary { expr, .. } => find_call_in_expr(expr, pos),
        ExprKind::Binary { left, right, .. } => {
            find_call_in_expr(left, pos).or_else(|| find_call_in_expr(right, pos))
        }
        _ => None,
    }
}

fn active_param_index(args: &[Expr], pos: Position) -> usize {
    if args.is_empty() {
        return 0;
    }
    let mut count = 0;
    for arg in args {
        if position_leq(arg.span.end, pos) {
            count += 1;
        } else {
            break;
        }
    }
    if count >= args.len() {
        args.len() - 1
    } else {
        count
    }
}

fn env_for_position(
    program: &Program,
    pos: Position,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> TypeEnv {
    for func in &program.functions {
        if span_contains(func.span, pos) {
            return env_for_function(func, index, stdlib);
        }
    }
    for book in &program.books {
        for method in &book.methods {
            if span_contains(method.span, pos) {
                return env_for_function(method, index, stdlib);
            }
        }
    }
    TypeEnv::new()
}

fn env_for_function(
    func: &Function,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> TypeEnv {
    let mut env = TypeEnv::new();
    for param in &func.params {
        env.insert(param.name.clone(), param.ty.clone());
    }
    for stmt in &func.body {
        update_env_from_stmt(stmt, &mut env, index, stdlib);
    }
    env
}

fn update_env_from_stmt(
    stmt: &Stmt,
    env: &mut TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) {
    match stmt {
        Stmt::Set { name, ty, expr, .. } => {
            let inferred = infer_expr_type(expr, env, index, stdlib);
            if let Some(ty) = ty {
                env.insert(name.clone(), ty.clone());
            } else if let Some(inferred) = inferred {
                env.insert(name.clone(), inferred);
            }
        }
        Stmt::Expr { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Put { name, expr, .. } => {
            let inferred = infer_expr_type(expr, env, index, stdlib);
            if env.get(name).is_none() {
                if let Some(inferred) = inferred {
                    env.insert(name.clone(), inferred);
                }
            }
        }
        Stmt::PutIndex { index: idx_expr, expr, .. } => {
            infer_expr_type(idx_expr, env, index, stdlib);
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::PutField { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Yield { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Throw { expr, .. } => {
            infer_expr_type(expr, env, index, stdlib);
        }
        Stmt::Try {
            try_body,
            catch_name,
            catch_body,
            ..
        } => {
            let mut try_env = env.clone();
            try_env.push();
            for stmt in try_body {
                update_env_from_stmt(stmt, &mut try_env, index, stdlib);
            }
            let mut catch_env = env.clone();
            catch_env.push();
            catch_env.insert(catch_name.clone(), Type::String);
            for stmt in catch_body {
                update_env_from_stmt(stmt, &mut catch_env, index, stdlib);
            }
        }
        Stmt::When {
            cond,
            then_body,
            else_body,
            ..
        } => {
            infer_expr_type(cond, env, index, stdlib);
            let mut then_env = env.clone();
            then_env.push();
            for stmt in then_body {
                update_env_from_stmt(stmt, &mut then_env, index, stdlib);
            }
            let mut else_env = env.clone();
            else_env.push();
            for stmt in else_body {
                update_env_from_stmt(stmt, &mut else_env, index, stdlib);
            }
        }
        Stmt::Repeat { cond, body, .. } => {
            infer_expr_type(cond, env, index, stdlib);
            let mut loop_env = env.clone();
            loop_env.push();
            for stmt in body {
                update_env_from_stmt(stmt, &mut loop_env, index, stdlib);
            }
        }
    }
}

fn infer_expr_type(
    expr: &Expr,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<Type> {
    let mut hints = Vec::new();
    let range = Span::new(Position::new(0, 0), Position::new(0, 0));
    collect_inlay_hints_in_expr(expr, env, index, stdlib, range, &mut hints)
}

fn resolve_signature_info(
    name: &str,
    env: &TypeEnv,
    index: &SymbolIndex,
    stdlib: &HashMap<String, CallSignature>,
) -> Option<SignatureInfo> {
    if let Some(signature) = stdlib.get(name) {
        let params = signature.params.clone();
        let label = format!(
            "rule {}({}) -> {}",
            name,
            params.join(", "),
            type_name(&signature.return_type)
        );
        return Some(SignatureInfo { label, params });
    }
    if let Some(info) = index.functions.get(name) {
        return Some(signature_from_function(info));
    }
    if let Some((base, method)) = name.split_once("::") {
        if let Some(Type::Book(book_name)) = env.get(base) {
            if let Some(book_info) = index.books.get(&book_name) {
                if let Some(method_info) = book_info.methods.get(method) {
                    return Some(signature_from_method(&book_info.name, method_info));
                }
            }
        }
        if let Some(book_info) = index.books.get(base) {
            if let Some(method_info) = book_info.methods.get(method) {
                return Some(signature_from_method(&book_info.name, method_info));
            }
        }
    }
    None
}

fn resolve_constructor_signature(book: &str, index: &SymbolIndex) -> SignatureInfo {
    let mut params = Vec::new();
    let mut return_type = Type::Book(book.to_string());
    if let Some(book_info) = index.books.get(book) {
        if let Some(method_info) = book_info.methods.get("init") {
            params = method_info
                .params
                .iter()
                .map(|(name, ty)| format!("{}: {}", name, type_name(ty)))
                .collect();
            if params
                .first()
                .map(|param| param.starts_with("self:"))
                .unwrap_or(false)
            {
                params.remove(0);
            }
            return_type = method_info.return_type.clone();
        }
    }
    let label = format!(
        "new {}({}) -> {}",
        book,
        params.join(", "),
        type_name(&return_type)
    );
    SignatureInfo { label, params }
}

fn signature_from_function(info: &FunctionInfo) -> SignatureInfo {
    let params: Vec<String> = info
        .params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, type_name(ty)))
        .collect();
    let label = format!(
        "rule {}({}) -> {}",
        info.name,
        params.join(", "),
        type_name(&info.return_type)
    );
    SignatureInfo { label, params }
}

fn signature_from_method(book_name: &str, info: &FunctionInfo) -> SignatureInfo {
    let mut params: Vec<String> = info
        .params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, type_name(ty)))
        .collect();
    if params.first().map(|param| param.starts_with("self:")).unwrap_or(false) {
        params.remove(0);
    }
    let label = format!(
        "rule {}::{}({}) -> {}",
        book_name,
        info.name,
        params.join(", "),
        type_name(&info.return_type)
    );
    SignatureInfo { label, params }
}

fn qualified_path_at_position(tokens: &[Token], pos: Position) -> Option<Vec<String>> {
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

fn member_context(tokens: &[Token], pos: Position) -> Option<(String, String)> {
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
    if is_type_context(tokens, idx) {
        Some(name.clone())
    } else {
        None
    }
}

fn is_type_context(tokens: &[Token], idx: usize) -> bool {
    if idx == 0 {
        return false;
    }
    matches!(
        tokens[idx - 1].kind,
        TokenKind::Colon | TokenKind::Arrow | TokenKind::New | TokenKind::Book
    )
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
        while cursor + 2 < tokens.len()
            && matches!(tokens[cursor + 1].kind, TokenKind::DoubleColon)
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

fn document_symbols(program: &Program) -> Vec<Value> {
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

fn completion_items(
    uri: &str,
    text: &str,
    tokens: &[Token],
    program: Option<&Program>,
    pos: Position,
) -> Vec<Value> {
    let mut items = Vec::new();
    let keywords = [
        "rule", "set", "put", "yield", "when", "otherwise", "repeat", "while", "end",
        "import", "book", "field", "new",
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
                let stdlib_root = uri_to_path(uri).and_then(|path| find_stdlib_root(&path));
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
                let stdlib_root = uri_to_path(uri).and_then(|path| find_stdlib_root(&path));
                let stdlib = stdlib_signatures(program, stdlib_root.as_deref());
                let env = env_for_position(program, pos, &index, &stdlib);
                let book_name = match env.get(&base) {
                    Some(Type::Book(name)) => Some(name),
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

fn completion_item(label: &str, kind: i32) -> Value {
    json!({"label": label, "kind": kind})
}

fn import_modules(uri: &str) -> Vec<String> {
    let mut modules = vec![
        "std::string".to_string(),
        "std::bytes".to_string(),
        "std::io".to_string(),
        "std::time".to_string(),
        "std::fs".to_string(),
        "std::path".to_string(),
        "std::env".to_string(),
        "std::json".to_string(),
    ];
    if let Some(path) = uri_to_path(uri) {
        if let Some(root) = find_stdlib_root(&path) {
            modules.extend(scan_stdlib_modules(&root));
        }
    }
    modules.sort();
    modules.dedup();
    modules
}

fn stdlib_module_functions(module: &str, root: Option<&Path>) -> Vec<String> {
    let mut names = builtin_stdlib_functions(module);
    if let Some(root) = root {
        let segments: Vec<String> = module.split("::").map(|part| part.to_string()).collect();
        if let Some(path) = stdlib_module_path(root, &segments) {
            let module_signatures = parse_stdlib_module(module, &path);
            for (name, _) in module_signatures {
                if let Some((_, func)) = name.rsplit_once("::") {
                    names.push(func.to_string());
                }
            }
        }
    }
    names.sort();
    names.dedup();
    names
}

fn user_module_functions(entry_path: &Path, segments: &[String]) -> Vec<String> {
    let mut names = Vec::new();
    let Some(path) = resolve_user_module_path(entry_path, segments) else {
        return names;
    };
    let Some(program) = parse_program_from_path(&path) else {
        return names;
    };
    for func in program.functions {
        names.push(func.name);
    }
    names.sort();
    names.dedup();
    names
}

fn builtin_stdlib_functions(module: &str) -> Vec<String> {
    match module {
        "std::string" => vec![
            "len", "concat", "eq", "bytes", "from_bytes", "to_i64", "from_i64",
        ],
        "std::bytes" => vec!["len", "eq"],
        "std::io" => vec!["print", "read_line"],
        "std::time" => vec!["now_ms", "sleep_ms"],
        "std::fs" => vec!["read_text", "write_text", "read_bytes", "write_bytes"],
        "std::path" => vec!["join", "normalize", "basename", "dirname"],
        "std::env" => vec!["args", "get", "set_var", "cwd", "set_cwd"],
        "std::json" => vec![
            "encode_i64",
            "encode_bool",
            "encode_string",
            "decode_i64",
            "decode_bool",
            "decode_string",
        ],
        _ => Vec::new(),
    }
    .into_iter()
    .map(|name| name.to_string())
    .collect()
}

fn find_stdlib_root(path: &Path) -> Option<PathBuf> {
    let mut current = path.parent();
    while let Some(dir) = current {
        let candidate = dir.join("stdlib");
        if candidate.is_dir() && candidate.join("std").is_dir() {
            return Some(candidate);
        }
        current = dir.parent();
    }
    None
}

fn project_root(entry_path: &Path) -> Option<PathBuf> {
    find_stdlib_root(entry_path).and_then(|root| root.parent().map(|dir| dir.to_path_buf()))
}

fn module_path_from_base(base: &Path, module_path: &[String]) -> PathBuf {
    let mut path = base.to_path_buf();
    for part in module_path {
        path.push(part);
    }
    path.set_extension("bd");
    path
}

fn user_module_candidates(entry_path: &Path, module_path: &[String]) -> Vec<PathBuf> {
    let mut candidates = Vec::new();
    if let Some(entry_dir) = entry_path.parent() {
        candidates.push(module_path_from_base(entry_dir, module_path));
    }
    if let Some(root) = project_root(entry_path) {
        let candidate = module_path_from_base(&root, module_path);
        if !candidates.iter().any(|existing| *existing == candidate) {
            candidates.push(candidate);
        }
    }
    candidates
}

fn resolve_user_module_path(entry_path: &Path, module_path: &[String]) -> Option<PathBuf> {
    for candidate in user_module_candidates(entry_path, module_path) {
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

fn parse_program_from_path(path: &Path) -> Option<Program> {
    let source = std::fs::read_to_string(path).ok()?;
    let tokens = lexer::lex(&source).ok()?;
    birddisk_core::parser::parse(&tokens).ok()
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

fn build_symbol_index_with_paths(uri: &str, program: &Program) -> (SymbolIndex, Vec<PathBuf>) {
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

fn build_symbol_index(uri: &str, program: &Program) -> SymbolIndex {
    build_symbol_index_with_paths(uri, program).0
}

fn scan_stdlib_modules(root: &Path) -> Vec<String> {
    let mut modules = Vec::new();
    let mut stack = vec![root.to_path_buf()];
    while let Some(dir) = stack.pop() {
        let entries = match std::fs::read_dir(&dir) {
            Ok(entries) => entries,
            Err(_) => continue,
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
                continue;
            }
            if path.extension().and_then(|ext| ext.to_str()) != Some("bd") {
                continue;
            }
            if let Ok(rel) = path.strip_prefix(root) {
                let parts: Vec<String> = rel
                    .iter()
                    .filter_map(|part| part.to_str())
                    .map(|part| part.trim_end_matches(".bd").to_string())
                    .collect();
                if !parts.is_empty() {
                    modules.push(parts.join("::"));
                }
            }
        }
    }
    modules
}

fn line_at_position(text: &str, line: u32) -> Option<&str> {
    let target = line.saturating_sub(1) as usize;
    text.lines().nth(target)
}

fn format_params(params: &[(String, Type)]) -> String {
    params
        .iter()
        .map(|(name, ty)| format!("{}: {}", name, type_name(ty)))
        .collect::<Vec<_>>()
        .join(", ")
}

fn format_ast_params(params: &[birddisk_core::ast::Param]) -> String {
    params
        .iter()
        .map(|param| format!("{}: {}", param.name, type_name(&param.ty)))
        .collect::<Vec<_>>()
        .join(", ")
}

fn type_name(ty: &Type) -> String {
    match ty {
        Type::I64 => "i64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String => "string".to_string(),
        Type::U8 => "u8".to_string(),
        Type::Void => "void".to_string(),
        Type::Array(inner) => format!("{}[]", type_name(inner)),
        Type::Book(name) => name.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn temp_dir(name: &str) -> PathBuf {
        let mut path = std::env::temp_dir();
        let stamp = std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap()
            .as_millis();
        path.push(format!("birddisk_lsp_{name}_{stamp}"));
        std::fs::create_dir_all(&path).unwrap();
        path
    }

    #[test]
    fn span_contains_position() {
        let span = Span::new(Position::new(2, 3), Position::new(2, 6));
        assert!(span_contains(span, Position::new(2, 4)));
        assert!(!span_contains(span, Position::new(1, 1)));
    }

    #[test]
    fn completion_collects_symbols() {
        let source = "rule main() -> i64:\n  yield 0.\nend\nbook Test:\n  field value: i64.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let pos = Position::new(1, 1);
        let items = completion_items("file:///tmp/test.bd", source, &tokens, Some(&program), pos);
        let labels: Vec<String> = items
            .into_iter()
            .filter_map(|item| item.get("label").and_then(|v| v.as_str()).map(|s| s.to_string()))
            .collect();
        assert!(labels.contains(&"main".to_string()));
        assert!(labels.contains(&"Test".to_string()));
    }

    #[test]
    fn semantic_tokens_include_keywords() {
        let source = "rule main() -> i64:\n  yield 1.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let data = semantic_tokens(&tokens);
        assert!(!data.is_empty());
    }

    #[test]
    fn completion_stdlib_functions() {
        let source = "rule main() -> i64:\n  set x = std::string::\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let pos = Position::new(2, 24);
        let items = completion_items("file:///tmp/test.bd", source, &tokens, None, pos);
        let labels: Vec<String> = items
            .into_iter()
            .filter_map(|item| item.get("label").and_then(|v| v.as_str()).map(|s| s.to_string()))
            .collect();
        assert!(labels.contains(&"len".to_string()));
        assert!(labels.contains(&"concat".to_string()));
    }

    #[test]
    fn inlay_hints_for_calls() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\nrule main() -> i64:\n  yield add(1, 2).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let range = Span::new(Position::new(1, 1), Position::new(10, 1));
        let hints = inlay_hints(&program, range, "file:///tmp/test.bd");
        let labels: Vec<String> = hints
            .iter()
            .filter_map(|hint| hint.get("label").and_then(|value| value.as_str()))
            .map(|value| value.to_string())
            .collect();
        assert!(labels.contains(&"a:".to_string()));
        assert!(labels.contains(&"b:".to_string()));
    }

    #[test]
    fn inlay_hints_for_methods() {
        let source = "book Counter:\n  field value: i64.\n  rule init(self: Counter, start: i64) -> Counter:\n    put self::value = start.\n    yield self.\n  end\n  rule add(self: Counter, delta: i64) -> i64:\n    yield self::value + delta.\n  end\nend\nrule main() -> i64:\n  set c: Counter = new Counter(0).\n  yield c::add(5).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let range = Span::new(Position::new(1, 1), Position::new(30, 1));
        let hints = inlay_hints(&program, range, "file:///tmp/test.bd");
        let labels: Vec<String> = hints
            .iter()
            .filter_map(|hint| hint.get("label").and_then(|value| value.as_str()))
            .map(|value| value.to_string())
            .collect();
        assert!(labels.contains(&"delta:".to_string()));
        assert!(!labels.contains(&"self:".to_string()));
    }

    #[test]
    fn inlay_hints_for_stdlib() {
        let source = "import std::string.\nrule main() -> i64:\n  set value: string = std::string::concat(\"a\", \"b\").\n  yield std::string::len(value).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let range = Span::new(Position::new(1, 1), Position::new(10, 1));
        let hints = inlay_hints(&program, range, "file:///tmp/test.bd");
        let labels: Vec<String> = hints
            .iter()
            .filter_map(|hint| hint.get("label").and_then(|value| value.as_str()))
            .map(|value| value.to_string())
            .collect();
        assert!(labels.contains(&"left:".to_string()));
        assert!(labels.contains(&"right:".to_string()));
        assert!(labels.contains(&"text:".to_string()));
    }

    #[test]
    fn signature_help_for_function() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\nrule main() -> i64:\n  yield add(1, 2).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let index = SymbolIndex::new(&program, "file:///tmp/test.bd");
        let stdlib = stdlib_signatures(&program, None);
        let pos = tokens
            .iter()
            .find_map(|token| match token.kind {
                TokenKind::IntLit(1) => Some(token.span.start),
                _ => None,
            })
            .unwrap();
        let help = signature_help_at_position(&program, pos, &index, &stdlib).unwrap();
        let label = help["signatures"][0]["label"].as_str().unwrap();
        assert!(label.contains("rule add"));
        assert_eq!(help["activeParameter"].as_u64().unwrap(), 0);
    }

    #[test]
    fn signature_help_for_constructor() {
        let source = "book Counter:\n  field value: i64.\n  rule init(self: Counter, start: i64) -> Counter:\n    put self::value = start.\n    yield self.\n  end\nend\nrule main() -> i64:\n  set c: Counter = new Counter(5).\n  yield c::value.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let index = SymbolIndex::new(&program, "file:///tmp/test.bd");
        let stdlib = stdlib_signatures(&program, None);
        let pos = tokens
            .iter()
            .find_map(|token| match token.kind {
                TokenKind::IntLit(5) => Some(token.span.start),
                _ => None,
            })
            .unwrap();
        let help = signature_help_at_position(&program, pos, &index, &stdlib).unwrap();
        let label = help["signatures"][0]["label"].as_str().unwrap();
        assert!(label.contains("new Counter"));
        assert!(label.contains("start: i64"));
        assert_eq!(help["activeParameter"].as_u64().unwrap(), 0);
    }

    #[test]
    fn signature_help_for_stdlib() {
        let source = "import std::string.\nrule main() -> i64:\n  yield std::string::len(\"hi\").\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let index = SymbolIndex::new(&program, "file:///tmp/test.bd");
        let stdlib = stdlib_signatures(&program, None);
        let pos = tokens
            .iter()
            .find_map(|token| match &token.kind {
                TokenKind::StringLit(value) if value == "hi" => Some(token.span.start),
                _ => None,
            })
            .unwrap();
        let help = signature_help_at_position(&program, pos, &index, &stdlib).unwrap();
        let label = help["signatures"][0]["label"].as_str().unwrap();
        assert!(label.contains("std::string::len"));
        assert_eq!(help["activeParameter"].as_u64().unwrap(), 0);
    }

    #[test]
    fn type_definition_for_book() {
        let source = "book Counter:\n  field value: i64.\nend\nrule main() -> i64:\n  set c: Counter = new Counter().\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let index = SymbolIndex::new(&program, "file:///tmp/test.bd");
        let pos = tokens
            .iter()
            .enumerate()
            .find_map(|(idx, token)| match &token.kind {
                TokenKind::Ident(name) if name == "Counter" => {
                    if idx > 0 && matches!(tokens[idx - 1].kind, TokenKind::Colon) {
                        Some(token.span.start)
                    } else {
                        None
                    }
                }
                _ => None,
            })
            .unwrap();
        let location = type_definition_location(&tokens, &index, pos).unwrap();
        assert_eq!(location.span.start.line, 1);
    }

    #[test]
    fn document_symbols_include_books_and_rules() {
        let source = "book Counter:\n  field value: i64.\n  rule add(self: Counter, delta: i64) -> i64:\n    yield self::value + delta.\n  end\nend\nrule main() -> i64:\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let symbols = document_symbols(&program);
        let names: Vec<String> = symbols
            .iter()
            .filter_map(|symbol| symbol.get("name").and_then(|value| value.as_str()))
            .map(|value| value.to_string())
            .collect();
        assert!(names.contains(&"Counter".to_string()));
        assert!(names.contains(&"main".to_string()));
    }

    #[test]
    fn references_include_open_importers() {
        let dir = temp_dir("references");
        let module_dir = dir.join("testlib");
        std::fs::create_dir_all(&module_dir).unwrap();
        let module_path = module_dir.join("util.bd");
        let module_source =
            "rule double(value: i64) -> i64:\n  yield value + value.\nend\n";
        std::fs::write(&module_path, module_source).unwrap();

        let main_path = dir.join("main.bd");
        let main_source =
            "import testlib::util.\nrule main() -> i64:\n  yield testlib::util::double(1).\nend\n";
        std::fs::write(&main_path, main_source).unwrap();

        let other_path = dir.join("other.bd");
        let other_source =
            "import testlib::util.\nrule run() -> i64:\n  yield testlib::util::double(2).\nend\n";
        std::fs::write(&other_path, other_source).unwrap();

        let main_tokens = lexer::lex(main_source).unwrap();
        let pos = main_tokens
            .iter()
            .find_map(|token| match &token.kind {
                TokenKind::Ident(name) if name == "double" => Some(token.span.start),
                _ => None,
            })
            .unwrap();
        let mut server = Server::new();
        let main_uri = path_to_uri(&main_path);
        let other_uri = path_to_uri(&other_path);
        server.docs.insert(
            main_uri.clone(),
            Document {
                text: main_source.to_string(),
            },
        );
        server.docs.insert(
            other_uri.clone(),
            Document {
                text: other_source.to_string(),
            },
        );
        let params = json!({
            "textDocument": {"uri": main_uri},
            "position": {"line": pos.line - 1, "character": pos.col - 1}
        });
        let result = server.handle_references(params);
        let items = result.as_array().unwrap();
        let mut uris: Vec<String> = items
            .iter()
            .filter_map(|item| item.get("uri").and_then(|value| value.as_str()))
            .map(|value| value.to_string())
            .collect();
        uris.sort();
        uris.dedup();
        assert!(uris.contains(&main_uri));
        assert!(uris.contains(&other_uri));
    }

    #[test]
    fn completion_methods_for_typed_binding() {
        let source = "book Counter:\n  field value: i64.\n  rule init(self: Counter) -> Counter:\n    yield self.\n  end\n  rule add(self: Counter, delta: i64) -> i64:\n    yield self::value + delta.\n  end\nend\nrule main() -> i64:\n  set c: Counter = new Counter().\n  set x = c::add(1).\n  yield x.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let pos = tokens
            .iter()
            .enumerate()
            .find_map(|(idx, token)| {
                if matches!(token.kind, TokenKind::DoubleColon) {
                    if idx > 0 {
                        if let TokenKind::Ident(name) = &tokens[idx - 1].kind {
                            if name == "c" {
                                return Some(token.span.end);
                            }
                        }
                    }
                }
                None
            })
            .unwrap();
        let items = completion_items("file:///tmp/test.bd", source, &tokens, Some(&program), pos);
        let labels: Vec<String> = items
            .into_iter()
            .filter_map(|item| item.get("label").and_then(|v| v.as_str()).map(|s| s.to_string()))
            .collect();
        assert!(labels.contains(&"value".to_string()));
        assert!(labels.contains(&"add".to_string()));
    }

    #[test]
    fn completion_for_module_prefix_and_functions() {
        let dir = temp_dir("completion_modules");
        let module_dir = dir.join("testlib");
        std::fs::create_dir_all(&module_dir).unwrap();
        let module_path = module_dir.join("util.bd");
        let module_source =
            "rule double(value: i64) -> i64:\n  yield value + value.\nend\n";
        std::fs::write(&module_path, module_source).unwrap();

        let main_path = dir.join("main.bd");
        let main_source =
            "import testlib::util.\nrule main() -> i64:\n  set x = testlib::util::double(1).\n  yield x.\nend\n";
        std::fs::write(&main_path, main_source).unwrap();
        let uri = path_to_uri(&main_path);
        let tokens = lexer::lex(main_source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();

        let prefix_pos = tokens
            .iter()
            .enumerate()
            .find_map(|(idx, token)| {
                if matches!(token.kind, TokenKind::DoubleColon) {
                    if idx > 0 {
                        if let TokenKind::Ident(name) = &tokens[idx - 1].kind {
                            if name == "testlib" && token.span.start.line == 3 {
                                return Some(token.span.end);
                            }
                        }
                    }
                }
                None
            })
            .unwrap();
        let prefix_items =
            completion_items(&uri, main_source, &tokens, Some(&program), prefix_pos);
        let prefix_labels: Vec<String> = prefix_items
            .into_iter()
            .filter_map(|item| item.get("label").and_then(|v| v.as_str()).map(|s| s.to_string()))
            .collect();
        assert!(prefix_labels.contains(&"util".to_string()));

        let func_pos = tokens
            .iter()
            .enumerate()
            .find_map(|(idx, token)| {
                if matches!(token.kind, TokenKind::DoubleColon) {
                    if idx > 0 {
                        if let TokenKind::Ident(name) = &tokens[idx - 1].kind {
                            if name == "util" && token.span.start.line == 3 {
                                return Some(token.span.end);
                            }
                        }
                    }
                }
                None
            })
            .unwrap();
        let func_items = completion_items(&uri, main_source, &tokens, Some(&program), func_pos);
        let func_labels: Vec<String> = func_items
            .into_iter()
            .filter_map(|item| item.get("label").and_then(|v| v.as_str()).map(|s| s.to_string()))
            .collect();
        assert!(func_labels.contains(&"double".to_string()));
    }

    #[test]
    fn definition_resolves_imported_module_function() {
        let dir = temp_dir("definition");
        let module_dir = dir.join("testlib");
        std::fs::create_dir_all(&module_dir).unwrap();
        let module_path = module_dir.join("util.bd");
        let module_source =
            "rule double(value: i64) -> i64:\n  yield value + value.\nend\n";
        std::fs::write(&module_path, module_source).unwrap();

        let main_path = dir.join("main.bd");
        let main_source =
            "import testlib::util.\nrule main() -> i64:\n  yield testlib::util::double(3).\nend\n";
        std::fs::write(&main_path, main_source).unwrap();

        let tokens = lexer::lex(main_source).unwrap();
        let program = birddisk_core::parser::parse(&tokens).unwrap();
        let uri = path_to_uri(&main_path);
        let index = build_symbol_index(&uri, &program);
        assert!(index.functions.contains_key("testlib::util::double"));
        let pos = tokens
            .iter()
            .find_map(|token| match &token.kind {
                TokenKind::Ident(name) if name == "double" => Some(token.span.start),
                _ => None,
            })
            .unwrap();
        let path = qualified_path_at_position(&tokens, pos).unwrap();
        assert_eq!(path, vec!["testlib", "util", "double"]);
        let location = definition_location(&tokens, &index, pos).expect("definition");
        let location_path = uri_to_path(&location.uri).unwrap();
        let expected_path = module_path.canonicalize().unwrap_or(module_path.clone());
        let actual_path = location_path.canonicalize().unwrap_or(location_path);
        assert_eq!(actual_path, expected_path);
        assert_eq!(location.span.start.line, 1);
    }
}
