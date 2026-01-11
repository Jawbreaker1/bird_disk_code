use birddisk_core::ast::{Book, Expr, ExprKind, Function, Program, Stmt, Type};
use birddisk_core::lexer::{self, Token, TokenKind};
use birddisk_core::{Diagnostic, Position, Span};
use serde_json::{json, Value};
use std::cmp::Ordering;
use std::collections::HashMap;
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
                                "renameProvider": true,
                                "completionProvider": {"triggerCharacters": [":"]},
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
                    "textDocument/completion" => {
                        let result = self.handle_completion(params);
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
        let index = SymbolIndex::new(&program);
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
        let index = SymbolIndex::new(&program);
        match definition_span(&tokens, &index, pos) {
            Some(span) => json!([{"uri": uri, "range": span_to_range(span)}]),
            None => Value::Null,
        }
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

fn span_contains(span: Span, pos: Position) -> bool {
    position_leq(span.start, pos) && position_leq(pos, span.end)
}

fn position_leq(left: Position, right: Position) -> bool {
    match left.line.cmp(&right.line) {
        Ordering::Less => true,
        Ordering::Greater => false,
        Ordering::Equal => left.col <= right.col,
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
}

#[derive(Clone)]
struct FieldInfo {
    name: String,
    ty: Type,
    span: Span,
}

#[derive(Clone)]
struct BookInfo {
    name: String,
    span: Span,
    fields: HashMap<String, FieldInfo>,
    methods: HashMap<String, FunctionInfo>,
}

impl SymbolIndex {
    fn new(program: &Program) -> Self {
        let mut functions = HashMap::new();
        let mut books = HashMap::new();
        for func in &program.functions {
            functions.insert(func.name.clone(), function_info(func));
        }
        for book in &program.books {
            books.insert(book.name.clone(), book_info(book));
        }
        Self { functions, books }
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

fn function_info(func: &Function) -> FunctionInfo {
    FunctionInfo {
        name: func.name.clone(),
        params: func
            .params
            .iter()
            .map(|param| (param.name.clone(), param.ty.clone()))
            .collect(),
        return_type: func.return_type.clone(),
        span: func.span,
    }
}

fn book_info(book: &Book) -> BookInfo {
    let mut fields = HashMap::new();
    for field in &book.fields {
        fields.insert(
            field.name.clone(),
            FieldInfo {
                name: field.name.clone(),
                ty: field.ty.clone(),
                span: field.span,
            },
        );
    }
    let mut methods = HashMap::new();
    for method in &book.methods {
        methods.insert(method.name.clone(), function_info(method));
    }
    BookInfo {
        name: book.name.clone(),
        span: book.span,
        fields,
        methods,
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
        | TokenKind::TypeU8 => Some(SemanticTokenKind::Type),
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
    let index = SymbolIndex::new(program);
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
                return_type: Type::I64,
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
    if let Some(root) = root {
        for import in &program.imports {
            let module_name = import.path.join("::");
            if module_name.starts_with("std::string")
                || module_name.starts_with("std::bytes")
                || module_name.starts_with("std::io")
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

fn definition_span(tokens: &[Token], index: &SymbolIndex, pos: Position) -> Option<Span> {
    let (idx, token) = token_at_position(tokens, pos)?;
    let TokenKind::Ident(name) = &token.kind else {
        return None;
    };
    if let Some((book, member)) = member_context(tokens, pos) {
        if let Some(book_info) = index.books.get(&book) {
            if let Some(field) = book_info.fields.get(&member) {
                return Some(field.span);
            }
            if let Some(method) = book_info.methods.get(&member) {
                return Some(method.span);
            }
        }
    }
    if let Some(info) = index.functions.get(name) {
        return Some(info.span);
    }
    if let Some(book_info) = index.books.get(name) {
        return Some(book_info.span);
    }
    if idx > 0 {
        if let Some(TokenKind::Ident(base)) = tokens.get(idx - 1).map(|tok| &tok.kind) {
            if let Some(book_info) = index.books.get(base) {
                if let Some(field) = book_info.fields.get(name) {
                    return Some(field.span);
                }
            }
        }
    }
    None
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
                let index = SymbolIndex::new(program);
                if let Some(book) = index.books.get(&base) {
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
        if !member.is_empty() {
            return items;
        }
    }

    for keyword in &keywords {
        items.push(completion_item(keyword, 14));
    }
    if let Some(program) = program {
        let index = SymbolIndex::new(program);
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

fn find_stdlib_root(path: &Path) -> Option<PathBuf> {
    let mut current = path.parent();
    while let Some(dir) = current {
        let candidate = dir.join("stdlib");
        if candidate.is_dir() {
            return Some(candidate);
        }
        current = dir.parent();
    }
    None
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
        Type::Array(inner) => format!("{}[]", type_name(inner)),
        Type::Book(name) => name.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
}
