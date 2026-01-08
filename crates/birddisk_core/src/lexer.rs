use crate::diagnostics::{Position, Span};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Import,
    Rule,
    Set,
    Put,
    Yield,
    When,
    Otherwise,
    Repeat,
    While,
    Book,
    Field,
    New,
    End,
    Array,
    TypeI64,
    TypeBool,
    TypeString,
    TypeU8,
    BoolLit(bool),
    IntLit(i64),
    StringLit(String),
    Ident(String),
    LParen,
    RParen,
    LBracket,
    RBracket,
    Comma,
    Colon,
    DoubleColon,
    Dot,
    Arrow,
    Assign,
    EqEq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Bang,
    AndAnd,
    OrOr,
    Eof,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexError {
    pub code: &'static str,
    pub message: String,
    pub span: Span,
}

pub fn lex(source: &str) -> Result<Vec<Token>, LexError> {
    Lexer::new(source).lex()
}

struct Lexer {
    chars: Vec<char>,
    index: usize,
    line: u32,
    col: u32,
}

impl Lexer {
    fn new(source: &str) -> Self {
        Self {
            chars: source.chars().collect(),
            index: 0,
            line: 1,
            col: 1,
        }
    }

    fn lex(mut self) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        while let Some(ch) = self.peek() {
            if is_whitespace(ch) {
                self.advance();
                continue;
            }

            let start = self.position();
            let token = match ch {
                '(' => {
                    self.advance();
                    TokenKind::LParen
                }
                ')' => {
                    self.advance();
                    TokenKind::RParen
                }
                '[' => {
                    self.advance();
                    TokenKind::LBracket
                }
                ']' => {
                    self.advance();
                    TokenKind::RBracket
                }
                ',' => {
                    self.advance();
                    TokenKind::Comma
                }
                ':' => {
                    self.advance();
                    if self.peek() == Some(':') {
                        self.advance();
                        TokenKind::DoubleColon
                    } else {
                        TokenKind::Colon
                    }
                }
                '.' => {
                    self.advance();
                    TokenKind::Dot
                }
                '+' => {
                    self.advance();
                    TokenKind::Plus
                }
                '*' => {
                    self.advance();
                    TokenKind::Star
                }
                '%' => {
                    self.advance();
                    TokenKind::Percent
                }
                '-' => {
                    self.advance();
                    if self.peek() == Some('>') {
                        self.advance();
                        TokenKind::Arrow
                    } else {
                        TokenKind::Minus
                    }
                }
                '=' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::EqEq
                    } else {
                        TokenKind::Assign
                    }
                }
                '!' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::NotEq
                    } else {
                        TokenKind::Bang
                    }
                }
                '<' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::LtEq
                    } else {
                        TokenKind::Lt
                    }
                }
                '>' => {
                    self.advance();
                    if self.peek() == Some('=') {
                        self.advance();
                        TokenKind::GtEq
                    } else {
                        TokenKind::Gt
                    }
                }
                '&' => {
                    self.advance();
                    if self.peek() == Some('&') {
                        self.advance();
                        TokenKind::AndAnd
                    } else {
                        return Err(self.error(
                            "E0100",
                            "Unexpected character '&'.",
                            start,
                        ));
                    }
                }
                '|' => {
                    self.advance();
                    if self.peek() == Some('|') {
                        self.advance();
                        TokenKind::OrOr
                    } else {
                        return Err(self.error(
                            "E0100",
                            "Unexpected character '|'.",
                            start,
                        ));
                    }
                }
                '/' => {
                    self.advance();
                    match self.peek() {
                        Some('/') => {
                            self.advance();
                            return Err(self.error(
                                "E0102",
                                "Comments are not supported in v0.1.",
                                start,
                            ));
                        }
                        Some('*') => {
                            self.advance();
                            return Err(self.error(
                                "E0102",
                                "Comments are not supported in v0.1.",
                                start,
                            ));
                        }
                        _ => TokenKind::Slash,
                    }
                }
                '"' => {
                    let (value, span) = self.lex_string(start)?;
                    tokens.push(Token {
                        kind: TokenKind::StringLit(value),
                        span,
                    });
                    continue;
                }
                ch if is_letter(ch) => self.lex_identifier(),
                ch if is_digit(ch) => {
                    let (value, span) = self.lex_number(start)?;
                    tokens.push(Token {
                        kind: TokenKind::IntLit(value),
                        span,
                    });
                    continue;
                }
                _ => {
                    self.advance();
                    let message = format!("Unexpected character '{ch}'.");
                    return Err(self.error("E0100", &message, start));
                }
            };

            let span = Span::new(start, self.position());
            tokens.push(Token { kind: token, span });
        }

        let eof_span = Span::new(self.position(), self.position());
        tokens.push(Token {
            kind: TokenKind::Eof,
            span: eof_span,
        });
        Ok(tokens)
    }

    fn lex_identifier(&mut self) -> TokenKind {
        let mut buf = String::new();
        while let Some(ch) = self.peek() {
            if is_ident_continue(ch) {
                buf.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        match buf.as_str() {
            "import" => TokenKind::Import,
            "rule" => TokenKind::Rule,
            "set" => TokenKind::Set,
            "put" => TokenKind::Put,
            "yield" => TokenKind::Yield,
            "when" => TokenKind::When,
            "otherwise" => TokenKind::Otherwise,
            "repeat" => TokenKind::Repeat,
            "while" => TokenKind::While,
            "book" => TokenKind::Book,
            "field" => TokenKind::Field,
            "new" => TokenKind::New,
            "end" => TokenKind::End,
            "array" => TokenKind::Array,
            "i64" => TokenKind::TypeI64,
            "bool" => TokenKind::TypeBool,
            "string" => TokenKind::TypeString,
            "u8" => TokenKind::TypeU8,
            "true" => TokenKind::BoolLit(true),
            "false" => TokenKind::BoolLit(false),
            _ => TokenKind::Ident(buf),
        }
    }

    fn lex_number(&mut self, start: Position) -> Result<(i64, Span), LexError> {
        let mut buf = String::new();
        while let Some(ch) = self.peek() {
            if is_digit(ch) {
                buf.push(ch);
                self.advance();
            } else {
                break;
            }
        }

        let end = self.position();
        let span = Span::new(start, end);
        match buf.parse::<i64>() {
            Ok(value) => Ok((value, span)),
            Err(_) => Err(LexError {
                code: "E0101",
                message: "Integer literal out of range.".to_string(),
                span,
            }),
        }
    }

    fn lex_string(&mut self, start: Position) -> Result<(String, Span), LexError> {
        let mut buf = String::new();
        self.advance();
        while let Some(ch) = self.peek() {
            match ch {
                '"' => {
                    self.advance();
                    let span = Span::new(start, self.position());
                    return Ok((buf, span));
                }
                '\\' => {
                    self.advance();
                    let escaped = match self.peek() {
                        Some('"') => '"',
                        Some('\\') => '\\',
                        Some('n') => '\n',
                        Some(other) => {
                            self.advance();
                            let message = format!("Invalid escape '\\{other}'.");
                            return Err(self.error("E0104", &message, start));
                        }
                        None => {
                            return Err(self.error("E0103", "Unterminated string literal.", start));
                        }
                    };
                    self.advance();
                    buf.push(escaped);
                }
                '\n' => {
                    return Err(self.error("E0103", "Unterminated string literal.", start));
                }
                _ => {
                    buf.push(ch);
                    self.advance();
                }
            }
        }

        Err(self.error("E0103", "Unterminated string literal.", start))
    }

    fn error(&self, code: &'static str, message: &str, start: Position) -> LexError {
        LexError {
            code,
            message: message.to_string(),
            span: Span::new(start, self.position()),
        }
    }

    fn peek(&self) -> Option<char> {
        self.chars.get(self.index).copied()
    }

    fn advance(&mut self) -> Option<char> {
        let ch = self.peek()?;
        self.index += 1;
        if ch == '\n' {
            self.line += 1;
            self.col = 1;
        } else {
            self.col += 1;
        }
        Some(ch)
    }

    fn position(&self) -> Position {
        Position::new(self.line, self.col)
    }
}

fn is_whitespace(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\n' | '\r')
}

fn is_letter(ch: char) -> bool {
    ch.is_ascii_alphabetic()
}

fn is_digit(ch: char) -> bool {
    ch.is_ascii_digit()
}

fn is_ident_continue(ch: char) -> bool {
    is_letter(ch) || is_digit(ch) || ch == '_'
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn lex_simple_function() {
        let source = "rule main() -> i64:\n  yield 0.\nend\n";
        let tokens = lex(source).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Rule);
        assert_eq!(tokens[1].kind, TokenKind::Ident("main".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::LParen);
        assert_eq!(tokens[3].kind, TokenKind::RParen);
    }

    #[test]
    fn lex_rejects_comments() {
        let source = "rule main() -> i64:\n  // nope\n  yield 0.\nend\n";
        let err = lex(source).unwrap_err();
        assert_eq!(err.code, "E0102");
    }

    #[test]
    fn lex_rejects_unknown_character() {
        let err = lex("@").unwrap_err();
        assert_eq!(err.code, "E0100");
    }

    #[test]
    fn lex_string_literal_with_escape() {
        let source = "rule main() -> i64:\n  set s: string = \"hi\\n\".\n  yield 0.\nend\n";
        let tokens = lex(source).unwrap();
        assert!(tokens.iter().any(|token| {
            matches!(token.kind, TokenKind::StringLit(ref value) if value == "hi\n")
        }));
    }

    #[test]
    fn lex_import_path() {
        let source = "import std::string.\nrule main() -> i64:\n  yield 0.\nend\n";
        let tokens = lex(source).unwrap();
        assert_eq!(tokens[0].kind, TokenKind::Import);
        assert_eq!(tokens[1].kind, TokenKind::Ident("std".to_string()));
        assert_eq!(tokens[2].kind, TokenKind::DoubleColon);
        assert_eq!(tokens[3].kind, TokenKind::TypeString);
        assert_eq!(tokens[4].kind, TokenKind::Dot);
    }
}
