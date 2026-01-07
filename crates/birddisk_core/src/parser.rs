use crate::ast::{
    BinaryOp, Expr, ExprKind, Function, Param, Program, Stmt, Type, UnaryOp,
};
use crate::diagnostics::{Position, Span};
use crate::lexer::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseError {
    pub code: &'static str,
    pub message: String,
    pub span: Span,
    pub fixit: Option<FixItHint>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FixItHint {
    pub title: &'static str,
    pub span: Span,
    pub replacement: String,
}

pub fn parse(tokens: &[Token]) -> Result<Program, ParseError> {
    Parser::new(tokens).parse_program()
}

pub fn parse_with_recovery(tokens: &[Token]) -> Result<Program, Vec<ParseError>> {
    let mut parser = Parser::new_with_recovery(tokens);
    let mut functions = Vec::new();
    let mut errors = Vec::new();

    while !parser.is_eof() {
        if matches!(parser.peek_kind(), TokenKind::Rule) {
            if let Some(function) = parser.parse_function_with_recovery(&mut errors) {
                functions.push(function);
            }
        } else {
            errors.push(parser.error("E0201", "Expected 'rule' to start a function."));
            parser.sync_to_next_rule();
        }
    }

    if functions.is_empty() && errors.is_empty() {
        errors.push(parser.error("E0201", "Expected at least one function rule."));
    }

    if errors.is_empty() {
        Ok(Program { functions })
    } else {
        Err(errors)
    }
}

struct Parser<'a> {
    tokens: &'a [Token],
    index: usize,
    recovering: bool,
    pending_errors: Vec<ParseError>,
}

impl<'a> Parser<'a> {
    fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            index: 0,
            recovering: false,
            pending_errors: Vec::new(),
        }
    }

    fn new_with_recovery(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            index: 0,
            recovering: true,
            pending_errors: Vec::new(),
        }
    }

    fn parse_program(mut self) -> Result<Program, ParseError> {
        let mut functions = Vec::new();
        while !self.is_eof() {
            if matches!(self.peek_kind(), TokenKind::Rule) {
                functions.push(self.parse_function()?);
            } else {
                return Err(self.error("E0201", "Expected 'rule' to start a function."));
            }
        }

        if functions.is_empty() {
            return Err(self.error("E0201", "Expected at least one function rule."));
        }

        Ok(Program { functions })
    }

    fn parse_function(&mut self) -> Result<Function, ParseError> {
        let start = self.expect_simple(TokenKind::Rule, "Expected 'rule' keyword.")?;
        let name = self.expect_ident("Expected function name.")?;
        self.expect_simple(TokenKind::LParen, "Expected '(' after function name.")?;
        let params = self.parse_params()?;
        self.expect_simple(TokenKind::RParen, "Expected ')' after params.")?;
        self.expect_arrow()?;
        let return_type = self.parse_type("Expected return type.")?;
        self.expect_colon_with_fixit()?;
        let body = self.parse_statements_until(Terminator::End)?;
        let end = self.expect_simple(TokenKind::End, "Expected 'end' to close function.")?;
        let span = Span::new(start.span.start, end.span.end);

        Ok(Function {
            name,
            params,
            return_type,
            body,
            span,
        })
    }

    fn parse_function_with_recovery(&mut self, errors: &mut Vec<ParseError>) -> Option<Function> {
        let start = match self.expect_simple(TokenKind::Rule, "Expected 'rule' keyword.") {
            Ok(token) => token,
            Err(err) => {
                errors.push(err);
                self.sync_to_next_rule();
                return None;
            }
        };

        let name = match self.expect_ident("Expected function name.") {
            Ok(name) => name,
            Err(err) => {
                errors.push(err);
                self.sync_to_next_rule();
                return None;
            }
        };

        if let Err(err) = self.expect_simple(TokenKind::LParen, "Expected '(' after function name.") {
            errors.push(err);
            self.sync_to_next_rule();
            return None;
        }

        let params = match self.parse_params() {
            Ok(params) => params,
            Err(err) => {
                errors.push(err);
                self.sync_to_next_rule();
                return None;
            }
        };

        if let Err(err) = self.expect_simple(TokenKind::RParen, "Expected ')' after params.") {
            errors.push(err);
            self.sync_to_next_rule();
            return None;
        }

        if let Err(err) = self.expect_arrow() {
            errors.push(err);
        }

        let return_type = match self.parse_type("Expected return type.") {
            Ok(ty) => ty,
            Err(err) => {
                errors.push(err);
                Type::I64
            }
        };

        if let Err(err) = self.expect_colon_with_fixit() {
            errors.push(err);
        }

        let (body, missing_end) = self.parse_statements_until_recover(Terminator::End, errors);
        self.drain_pending_errors(errors);

        let end_token = if matches!(self.peek_kind(), TokenKind::End) {
            Some(self.bump())
        } else if missing_end {
            None
        } else {
            let insert_at = self.tokens[self.index].span.start;
            errors.push(self.error_with_fixit(
                "E0202",
                "Expected 'end' to close function.",
                FixItHint {
                    title: "Insert 'end' to close function",
                    span: Span::new(insert_at, insert_at),
                    replacement: "end\n".to_string(),
                },
            ));
            None
        };

        let span_end = end_token
            .as_ref()
            .map(|token| token.span.end)
            .unwrap_or_else(|| self.previous_span_end());

        Some(Function {
            name,
            params,
            return_type,
            body,
            span: Span::new(start.span.start, span_end),
        })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut params = Vec::new();
        if matches!(self.peek_kind(), TokenKind::RParen) {
            return Ok(params);
        }

        loop {
            let name_token = self.expect_ident_token("Expected parameter name.")?;
            self.expect_simple(TokenKind::Colon, "Expected ':' after parameter name.")?;
            let ty = self.parse_type("Expected parameter type.")?;
            let span = Span::new(name_token.span.start, self.previous_span_end());
            params.push(Param {
                name: extract_ident(&name_token),
                ty,
                span,
            });

            if matches!(self.peek_kind(), TokenKind::Comma) {
                self.bump();
            } else if matches!(self.peek_kind(), TokenKind::Ident(_)) {
                let insert_at = self.tokens[self.index].span.start;
                return Err(self.error_with_fixit(
                    "E0200",
                    "Expected ',' between parameters.",
                    FixItHint {
                        title: "Insert ',' between parameters",
                        span: Span::new(insert_at, insert_at),
                        replacement: ", ".to_string(),
                    },
                ));
            } else {
                break;
            }
        }

        Ok(params)
    }

    fn parse_type(&mut self, message: &str) -> Result<Type, ParseError> {
        let mut ty = match self.peek_kind() {
            TokenKind::TypeI64 => {
                self.bump();
                Type::I64
            }
            TokenKind::TypeBool => {
                self.bump();
                Type::Bool
            }
            _ => return Err(self.error("E0206", message)),
        };

        while matches!(self.peek_kind(), TokenKind::LBracket) {
            self.bump();
            self.expect_rbracket_with_fixit("Expected ']' after '[' in array type.")?;
            ty = Type::Array(Box::new(ty));
        }

        Ok(ty)
    }

    fn parse_statements_until(&mut self, terminator: Terminator) -> Result<Vec<Stmt>, ParseError> {
        let mut stmts = Vec::new();
        loop {
            match self.peek_kind() {
                TokenKind::Eof => {
                    return Err(self.error_with_fixit(
                        "E0202",
                        "Unexpected end of file while parsing block.",
                        FixItHint {
                            title: "Insert 'end' to close block",
                            span: self.tokens[self.index].span,
                            replacement: "end\n".to_string(),
                        },
                    ));
                }
                TokenKind::End if terminator == Terminator::End => break,
                TokenKind::Otherwise if terminator == Terminator::Otherwise => break,
                TokenKind::End if terminator == Terminator::Otherwise => {
                    return Err(self.error_with_fixit(
                        "E0207",
                        "Missing 'otherwise' block for when statement.",
                        FixItHint {
                            title: "Insert 'otherwise' block",
                            span: self.tokens[self.index].span,
                            replacement: "otherwise:\n  yield 0.\n".to_string(),
                        },
                    ));
                }
                TokenKind::Otherwise => {
                    return Err(self.error("E0208", "Unexpected 'otherwise' here."));
                }
                _ => stmts.push(self.parse_statement()?),
            }
        }
        Ok(stmts)
    }

    fn parse_statements_until_recover(
        &mut self,
        terminator: Terminator,
        errors: &mut Vec<ParseError>,
    ) -> (Vec<Stmt>, bool) {
        let mut stmts = Vec::new();
        let mut missing_end = false;
        loop {
            match self.peek_kind() {
                TokenKind::Eof => {
                    errors.push(self.error_with_fixit(
                        "E0202",
                        "Unexpected end of file while parsing block.",
                        FixItHint {
                            title: "Insert 'end' to close block",
                            span: self.tokens[self.index].span,
                            replacement: "end\n".to_string(),
                        },
                    ));
                    self.sync_to_next_rule();
                    missing_end = true;
                    break;
                }
                TokenKind::End if terminator == Terminator::End => break,
                TokenKind::Otherwise if terminator == Terminator::Otherwise => break,
                TokenKind::End if terminator == Terminator::Otherwise => {
                    errors.push(self.error_with_fixit(
                        "E0207",
                        "Missing 'otherwise' block for when statement.",
                        FixItHint {
                            title: "Insert 'otherwise' block",
                            span: self.tokens[self.index].span,
                            replacement: "otherwise:\n  yield 0.\n".to_string(),
                        },
                    ));
                    break;
                }
                TokenKind::Rule if terminator == Terminator::End => {
                    errors.push(self.error_with_fixit(
                        "E0202",
                        "Expected 'end' to close block.",
                        FixItHint {
                            title: "Insert 'end' to close block",
                            span: self.tokens[self.index].span,
                            replacement: "end\n".to_string(),
                        },
                    ));
                    missing_end = true;
                    break;
                }
                TokenKind::Otherwise => {
                    errors.push(self.error("E0208", "Unexpected 'otherwise' here."));
                    self.sync_to_statement_boundary(terminator);
                }
                _ => match self.parse_statement() {
                    Ok(stmt) => {
                        stmts.push(stmt);
                        self.drain_pending_errors(errors);
                    }
                    Err(err) => {
                        let code = err.code;
                        errors.push(err);
                        if code == "E0207" && matches!(self.peek_kind(), TokenKind::End) {
                            // Consume the end of the malformed when block and keep going.
                            self.bump();
                        } else {
                            self.sync_to_statement_boundary(terminator);
                        }
                    }
                },
            }
        }
        (stmts, missing_end)
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            TokenKind::Set => self.parse_set(),
            TokenKind::Put => self.parse_put(),
            TokenKind::Yield => self.parse_yield(),
            TokenKind::When => self.parse_when(),
            TokenKind::Repeat => self.parse_repeat(),
            _ => Err(self.error("E0200", "Unexpected token in statement.")),
        }
    }

    fn parse_set(&mut self) -> Result<Stmt, ParseError> {
        let start = self.expect_simple(TokenKind::Set, "Expected 'set'.")?;
        let name = self.expect_ident("Expected binding name.")?;
        let ty = if matches!(self.peek_kind(), TokenKind::Colon) {
            self.bump();
            Some(self.parse_type("Expected type after ':'.")?)
        } else {
            None
        };
        self.expect_simple(TokenKind::Assign, "Expected '=' in set statement.")?;
        let expr = self.parse_expr("Expected expression after '='.")?;
        let end = self.expect_dot(expr.span.end)?;
        Ok(Stmt::Set {
            name,
            ty,
            expr,
            span: Span::new(start.span.start, end.span.end),
        })
    }

    fn parse_put(&mut self) -> Result<Stmt, ParseError> {
        let start = self.expect_simple(TokenKind::Put, "Expected 'put'.")?;
        let name_token = self.expect_ident_token("Expected binding name.")?;
        let name = extract_ident(&name_token);
        let index = if matches!(self.peek_kind(), TokenKind::LBracket) {
            self.bump();
            let expr = self.parse_expr("Expected index expression.")?;
            self.expect_rbracket_with_fixit("Expected ']' after index expression.")?;
            Some(expr)
        } else {
            None
        };
        self.expect_simple(TokenKind::Assign, "Expected '=' in put statement.")?;
        let expr = self.parse_expr("Expected expression after '='.")?;
        let end = self.expect_dot(expr.span.end)?;
        if let Some(index) = index {
            Ok(Stmt::PutIndex {
                name,
                index,
                expr,
                span: Span::new(start.span.start, end.span.end),
            })
        } else {
            Ok(Stmt::Put {
                name,
                expr,
                span: Span::new(start.span.start, end.span.end),
            })
        }
    }

    fn parse_yield(&mut self) -> Result<Stmt, ParseError> {
        let start = self.expect_simple(TokenKind::Yield, "Expected 'yield'.")?;
        let expr = self.parse_expr("Expected expression after 'yield'.")?;
        let end = self.expect_dot(expr.span.end)?;
        Ok(Stmt::Yield {
            expr,
            span: Span::new(start.span.start, end.span.end),
        })
    }

    fn parse_when(&mut self) -> Result<Stmt, ParseError> {
        let start = self.expect_simple(TokenKind::When, "Expected 'when'.")?;
        let cond = self.parse_expr("Expected condition after 'when'.")?;
        self.expect_colon_with_fixit()?;
        let then_body = self.parse_statements_until(Terminator::Otherwise)?;
        self.expect_simple(TokenKind::Otherwise, "Expected 'otherwise'.")?;
        self.expect_colon_with_fixit()?;
        let else_body = self.parse_statements_until(Terminator::End)?;
        let end = self.expect_simple(TokenKind::End, "Expected 'end' to close when.")?;
        Ok(Stmt::When {
            cond,
            span: Span::new(start.span.start, end.span.end),
            then_body,
            else_body,
        })
    }

    fn parse_repeat(&mut self) -> Result<Stmt, ParseError> {
        let start = self.expect_simple(TokenKind::Repeat, "Expected 'repeat'.")?;
        self.expect_simple(TokenKind::While, "Expected 'while' after 'repeat'.")?;
        let cond = self.parse_expr("Expected condition after 'repeat while'.")?;
        self.expect_colon_with_fixit()?;
        let body = self.parse_statements_until(Terminator::End)?;
        let end = self.expect_simple(TokenKind::End, "Expected 'end' to close repeat.")?;
        Ok(Stmt::Repeat {
            cond,
            span: Span::new(start.span.start, end.span.end),
            body,
        })
    }

    fn parse_expr(&mut self, message: &str) -> Result<Expr, ParseError> {
        let expr = self.parse_logic_or()?;
        if matches!(
            self.peek_kind(),
            TokenKind::Dot
                | TokenKind::Colon
                | TokenKind::Comma
                | TokenKind::Otherwise
                | TokenKind::End
                | TokenKind::RParen
                | TokenKind::RBracket
        ) {
            Ok(expr)
        } else if matches!(
            self.peek_kind(),
            TokenKind::Ident(_)
                | TokenKind::IntLit(_)
                | TokenKind::BoolLit(_)
                | TokenKind::LBracket
                | TokenKind::Array
        ) {
            let insert_at = self.tokens[self.index].span.start;
            Err(self.error_with_fixit(
                "E0203",
                message,
                FixItHint {
                    title: "Insert ',' between arguments",
                    span: Span::new(insert_at, insert_at),
                    replacement: ", ".to_string(),
                },
            ))
        } else if matches!(self.peek_kind(), TokenKind::Eof) {
            Ok(expr)
        } else {
            Err(self.error("E0203", message))
        }
    }

    fn parse_logic_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_logic_and()?;
        while matches!(self.peek_kind(), TokenKind::OrOr) {
            let op = self.bump();
            let right = self.parse_logic_and()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::OrOr,
                    right: Box::new(right),
                },
                span,
            };
            if matches!(op.kind, TokenKind::OrOr) {
                continue;
            }
        }
        Ok(expr)
    }

    fn parse_logic_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        while matches!(self.peek_kind(), TokenKind::AndAnd) {
            self.bump();
            let right = self.parse_equality()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op: BinaryOp::AndAnd,
                    right: Box::new(right),
                },
                span,
            };
        }
        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_compare()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::EqEq => BinaryOp::EqEq,
                TokenKind::NotEq => BinaryOp::NotEq,
                _ => break,
            };
            self.bump();
            let right = self.parse_compare()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span,
            };
        }
        Ok(expr)
    }

    fn parse_compare(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_add()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::Lt => BinaryOp::Lt,
                TokenKind::LtEq => BinaryOp::LtEq,
                TokenKind::Gt => BinaryOp::Gt,
                TokenKind::GtEq => BinaryOp::GtEq,
                _ => break,
            };
            self.bump();
            let right = self.parse_add()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span,
            };
        }
        Ok(expr)
    }

    fn parse_add(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_mul()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::Plus => BinaryOp::Add,
                TokenKind::Minus => BinaryOp::Sub,
                _ => break,
            };
            self.bump();
            let right = self.parse_mul()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span,
            };
        }
        Ok(expr)
    }

    fn parse_mul(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_unary()?;
        loop {
            let op = match self.peek_kind() {
                TokenKind::Star => BinaryOp::Mul,
                TokenKind::Slash => BinaryOp::Div,
                TokenKind::Percent => BinaryOp::Mod,
                _ => break,
            };
            self.bump();
            let right = self.parse_unary()?;
            let span = Span::new(expr.span.start, right.span.end);
            expr = Expr {
                kind: ExprKind::Binary {
                    left: Box::new(expr),
                    op,
                    right: Box::new(right),
                },
                span,
            };
        }
        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
        match self.peek_kind() {
            TokenKind::Minus => {
                let start = self.bump().span.start;
                let expr = self.parse_unary()?;
                let span = Span::new(start, expr.span.end);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Neg,
                        expr: Box::new(expr),
                    },
                    span,
                })
            }
            TokenKind::Bang => {
                let start = self.bump().span.start;
                let expr = self.parse_unary()?;
                let span = Span::new(start, expr.span.end);
                Ok(Expr {
                    kind: ExprKind::Unary {
                        op: UnaryOp::Not,
                        expr: Box::new(expr),
                    },
                    span,
                })
            }
            _ => self.parse_primary(),
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
        let token = self.peek_kind().clone();
        let expr = match token {
            TokenKind::IntLit(value) => {
                let token = self.bump();
                Ok(Expr {
                    kind: ExprKind::Int(value),
                    span: token.span,
                })
            }
            TokenKind::BoolLit(value) => {
                let token = self.bump();
                Ok(Expr {
                    kind: ExprKind::Bool(value),
                    span: token.span,
                })
            }
            TokenKind::Ident(_) => self.parse_ident_or_call(),
            TokenKind::Array => self.parse_array_new(),
            TokenKind::LBracket => self.parse_array_literal(),
            TokenKind::LParen => {
                let start = self.bump().span.start;
                let expr = self.parse_expr("Expected expression in parentheses.")?;
                match self.expect_rparen_with_fixit("Expected ')' after expression.") {
                    Ok(end) => Ok(Expr {
                        kind: expr.kind,
                        span: Span::new(start, end.span.end),
                    }),
                    Err(err) => {
                        if self.recovering {
                            self.pending_errors.push(err);
                            Ok(Expr {
                                kind: expr.kind,
                                span: Span::new(start, expr.span.end),
                            })
                        } else {
                            Err(err)
                        }
                    }
                }
            }
            _ => Err(self.error("E0203", "Expected expression.")),
        }?;

        self.parse_postfix(expr)
    }

    fn parse_ident_or_call(&mut self) -> Result<Expr, ParseError> {
        let token = self.expect_ident_token("Expected identifier.")?;
        let name = extract_ident(&token);
        if matches!(self.peek_kind(), TokenKind::LParen) {
            let start = token.span.start;
            self.bump();
            let mut args = Vec::new();
            if !matches!(self.peek_kind(), TokenKind::RParen) {
                loop {
                    let expr = self.parse_expr("Expected call argument.")?;
                    args.push(expr);
                    if matches!(self.peek_kind(), TokenKind::Comma) {
                        self.bump();
                    } else if matches!(self.peek_kind(), TokenKind::Ident(_)) {
                        let insert_at = self.tokens[self.index].span.start;
                        return Err(self.error_with_fixit(
                            "E0200",
                            "Expected ',' between arguments.",
                            FixItHint {
                                title: "Insert ',' between arguments",
                                span: Span::new(insert_at, insert_at),
                                replacement: ", ".to_string(),
                            },
                        ));
                    } else {
                        break;
                    }
                }
            }
            match self.expect_rparen_with_fixit("Expected ')' after arguments.") {
                Ok(end) => Ok(Expr {
                    kind: ExprKind::Call { name, args },
                    span: Span::new(start, end.span.end),
                }),
                Err(err) => {
                    if self.recovering {
                        self.pending_errors.push(err);
                        let span_end = args
                            .last()
                            .map(|expr| expr.span.end)
                            .unwrap_or(start);
                        Ok(Expr {
                            kind: ExprKind::Call { name, args },
                            span: Span::new(start, span_end),
                        })
                    } else {
                        Err(err)
                    }
                }
            }
        } else {
            Ok(Expr {
                kind: ExprKind::Ident(name),
                span: token.span,
            })
        }
    }

    fn parse_array_literal(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        let mut elements = Vec::new();
        if !matches!(self.peek_kind(), TokenKind::RBracket) {
            loop {
                let expr = self.parse_expr("Expected array element expression.")?;
                elements.push(expr);
                if matches!(self.peek_kind(), TokenKind::Comma) {
                    self.bump();
                } else {
                    break;
                }
            }
        }

        match self.expect_rbracket_with_fixit("Expected ']' after array literal.") {
            Ok(end) => Ok(Expr {
                kind: ExprKind::ArrayLit(elements),
                span: Span::new(start, end.span.end),
            }),
            Err(err) => {
                if self.recovering {
                    self.pending_errors.push(err);
                    let span_end = elements
                        .last()
                        .map(|expr| expr.span.end)
                        .unwrap_or(start);
                    Ok(Expr {
                        kind: ExprKind::ArrayLit(elements),
                        span: Span::new(start, span_end),
                    })
                } else {
                    Err(err)
                }
            }
        }
    }

    fn parse_array_new(&mut self) -> Result<Expr, ParseError> {
        let start = self.bump().span.start;
        self.expect_simple(TokenKind::LParen, "Expected '(' after array.")?;
        let len = self.parse_expr("Expected length expression for array.")?;
        let end = self.expect_rparen_with_fixit("Expected ')' after array length.")?;
        Ok(Expr {
            kind: ExprKind::ArrayNew {
                len: Box::new(len),
            },
            span: Span::new(start, end.span.end),
        })
    }

    fn parse_postfix(&mut self, mut expr: Expr) -> Result<Expr, ParseError> {
        loop {
            if matches!(self.peek_kind(), TokenKind::LBracket) {
                self.bump();
                let index = self.parse_expr("Expected index expression.")?;
                match self.expect_rbracket_with_fixit("Expected ']' after index expression.") {
                    Ok(end) => {
                        let span = Span::new(expr.span.start, end.span.end);
                        expr = Expr {
                            kind: ExprKind::Index {
                                base: Box::new(expr),
                                index: Box::new(index),
                            },
                            span,
                        };
                    }
                    Err(err) => {
                        if self.recovering {
                            self.pending_errors.push(err);
                            let span = Span::new(expr.span.start, index.span.end);
                            expr = Expr {
                                kind: ExprKind::Index {
                                    base: Box::new(expr),
                                    index: Box::new(index),
                                },
                                span,
                            };
                            break;
                        } else {
                            return Err(err);
                        }
                    }
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn expect_simple(&mut self, expected: TokenKind, message: &str) -> Result<Token, ParseError> {
        if self.peek_kind() == &expected {
            Ok(self.bump())
        } else {
            Err(self.error("E0200", message))
        }
    }

    fn expect_dot(&mut self, insert_at: Position) -> Result<Token, ParseError> {
        if matches!(self.peek_kind(), TokenKind::Dot) {
            Ok(self.bump())
        } else {
            Err(self.error_with_fixit(
                "E0200",
                "Expected '.' to end statement.",
                FixItHint {
                    title: "Insert '.' statement terminator",
                    span: Span::new(insert_at, insert_at),
                    replacement: ".".to_string(),
                },
            ))
        }
    }

    fn expect_arrow(&mut self) -> Result<Token, ParseError> {
        if matches!(self.peek_kind(), TokenKind::Arrow) {
            Ok(self.bump())
        } else {
            let insert_at = self.tokens[self.index].span.start;
            Err(self.error_with_fixit(
                "E0200",
                "Expected '->' before return type.",
                FixItHint {
                    title: "Insert '->' before return type",
                    span: Span::new(insert_at, insert_at),
                    replacement: "-> ".to_string(),
                },
            ))
        }
    }

    fn expect_colon_with_fixit(&mut self) -> Result<Token, ParseError> {
        if matches!(self.peek_kind(), TokenKind::Colon) {
            Ok(self.bump())
        } else {
            let insert_at = self.tokens[self.index].span.start;
            Err(self.error_with_fixit(
                "E0200",
                "Expected ':' after condition or header.",
                FixItHint {
                    title: "Insert ':' to start block",
                    span: Span::new(insert_at, insert_at),
                    replacement: ":\n".to_string(),
                },
            ))
        }
    }

    fn expect_rparen_with_fixit(&mut self, message: &str) -> Result<Token, ParseError> {
        if matches!(self.peek_kind(), TokenKind::RParen) {
            Ok(self.bump())
        } else {
            let insert_at = self.tokens[self.index].span.start;
            Err(self.error_with_fixit(
                "E0200",
                message,
                FixItHint {
                    title: "Insert ')' to close",
                    span: Span::new(insert_at, insert_at),
                    replacement: ")".to_string(),
                },
            ))
        }
    }

    fn expect_rbracket_with_fixit(&mut self, message: &str) -> Result<Token, ParseError> {
        if matches!(self.peek_kind(), TokenKind::RBracket) {
            Ok(self.bump())
        } else {
            let insert_at = self.tokens[self.index].span.start;
            Err(self.error_with_fixit(
                "E0200",
                message,
                FixItHint {
                    title: "Insert ']' to close",
                    span: Span::new(insert_at, insert_at),
                    replacement: "]".to_string(),
                },
            ))
        }
    }

    fn sync_to_next_rule(&mut self) {
        if self.is_eof() {
            return;
        }

        // Skip tokens until we find a new function boundary.
        while !self.is_eof() && !matches!(self.peek_kind(), TokenKind::Rule) {
            self.bump();
        }
    }

    fn sync_to_statement_boundary(&mut self, terminator: Terminator) {
        while !self.is_eof() {
            match self.peek_kind() {
                TokenKind::Dot => {
                    self.bump();
                    break;
                }
                TokenKind::End => break,
                TokenKind::Otherwise if terminator == Terminator::Otherwise => break,
                TokenKind::Otherwise => break,
                TokenKind::Rule => break,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn drain_pending_errors(&mut self, errors: &mut Vec<ParseError>) {
        if !self.pending_errors.is_empty() {
            errors.extend(self.pending_errors.drain(..));
        }
    }

    fn expect_ident(&mut self, message: &str) -> Result<String, ParseError> {
        let token = self.expect_ident_token(message)?;
        Ok(extract_ident(&token))
    }

    fn expect_ident_token(&mut self, message: &str) -> Result<Token, ParseError> {
        match self.peek_kind() {
            TokenKind::Ident(_) => Ok(self.bump()),
            _ => Err(self.error("E0205", message)),
        }
    }

    fn is_eof(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.tokens[self.index].kind
    }

    fn bump(&mut self) -> Token {
        let token = self.tokens[self.index].clone();
        self.index += 1;
        token
    }

    fn previous_span_end(&self) -> Position {
        if self.index == 0 {
            return Position::new(1, 1);
        }
        self.tokens[self.index - 1].span.end
    }

    fn error(&self, code: &'static str, message: &str) -> ParseError {
        let span = self.tokens.get(self.index).map(|t| t.span).unwrap_or_else(|| {
            let pos = Position::new(1, 1);
            Span::new(pos, pos)
        });
        ParseError {
            code,
            message: message.to_string(),
            span,
            fixit: None,
        }
    }

    fn error_with_fixit(
        &self,
        code: &'static str,
        message: &str,
        fixit: FixItHint,
    ) -> ParseError {
        let span = self.tokens.get(self.index).map(|t| t.span).unwrap_or_else(|| {
            let pos = Position::new(1, 1);
            Span::new(pos, pos)
        });
        ParseError {
            code,
            message: message.to_string(),
            span,
            fixit: Some(fixit),
        }
    }
}

fn extract_ident(token: &Token) -> String {
    if let TokenKind::Ident(name) = &token.kind {
        name.clone()
    } else {
        String::new()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Terminator {
    End,
    Otherwise,
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer;

    #[test]
    fn parse_minimal_rule() {
        let source = "rule main() -> i64:\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_when_with_otherwise() {
        let source = "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 2.\n  end\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_repeat() {
        let source = "rule main() -> i64:\n  repeat while true:\n    yield 1.\n  end\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_expression_precedence() {
        let source = "rule main() -> i64:\n  yield 1 + 2 * 3.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_call_expression() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1, 2).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 2);
    }

    #[test]
    fn parse_missing_dot_fixit() {
        let source = "rule main() -> i64:\n  yield 0\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0200");
        let fixit = err.fixit.expect("missing fixit");
        assert_eq!(fixit.replacement, ".");
    }

    #[test]
    fn parse_missing_otherwise_fixit() {
        let source = "rule main() -> i64:\n  when true:\n    yield 1.\n  end\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0207");
        let fixit = err.fixit.expect("missing fixit");
        assert!(fixit.replacement.contains("otherwise"));
    }

    #[test]
    fn parse_missing_end_fixit() {
        let source = "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 2.\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0202");
        let fixit = err.fixit.expect("missing fixit");
        assert_eq!(fixit.replacement, "end\n");
    }

    #[test]
    fn parse_missing_colon_fixit() {
        let source = "rule main() -> i64\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0200");
        let fixit = err.fixit.expect("missing fixit");
        assert!(fixit.replacement.contains(":"));
    }

    #[test]
    fn parse_missing_arrow_fixit() {
        let source = "rule main() i64:\n  yield 0.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0200");
        let fixit = err.fixit.expect("missing fixit");
        assert!(fixit.replacement.contains("->"));
    }

    #[test]
    fn parse_missing_comma_in_params_fixit() {
        let source = "rule add(a: i64 b: i64) -> i64:\n  yield a + b.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0200");
        let fixit = err.fixit.expect("missing fixit");
        assert!(fixit.replacement.contains(","));
    }

    #[test]
    fn parse_missing_comma_in_args_fixit() {
        let source = "rule add(a: i64, b: i64) -> i64:\n  yield a + b.\nend\n\nrule main() -> i64:\n  yield add(1 2).\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0203");
        let fixit = err.fixit.expect("missing fixit");
        assert!(fixit.replacement.contains(","));
    }

    #[test]
    fn parse_missing_rparen_fixit() {
        let source = "rule main() -> i64:\n  yield (1 + 2.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0200");
        let fixit = err.fixit.expect("missing fixit");
        assert!(fixit.replacement.contains(")"));
    }

    #[test]
    fn parse_array_literal_and_index() {
        let source = "rule main() -> i64:\n  set xs: i64[] = [1, 2].\n  yield xs[0].\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_put_index() {
        let source = "rule main() -> i64:\n  set xs: i64[] = [0].\n  put xs[0] = 1.\n  yield xs[0].\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let program = parse(&tokens).unwrap();
        assert_eq!(program.functions.len(), 1);
    }

    #[test]
    fn parse_missing_rbracket_fixit() {
        let source = "rule main() -> i64:\n  set xs: i64[] = [1, 2.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse(&tokens).unwrap_err();
        assert_eq!(err.code, "E0200");
        let fixit = err.fixit.expect("missing fixit");
        assert_eq!(fixit.replacement, "]");
    }

    #[test]
    fn parse_with_recovery_reports_multiple_errors() {
        let source = "rule main() -> i64:\n  yield 0\nend\n\nrule other() -> i64:\n  yield 1\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse_with_recovery(&tokens).unwrap_err();
        assert!(err.len() >= 2);
    }

    #[test]
    fn parse_with_recovery_continues_after_missing_otherwise() {
        let source = "rule main() -> i64:\n  when true:\n    yield 1.\n  end\n  yield 2\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse_with_recovery(&tokens).unwrap_err();
        assert!(err.len() >= 2);
    }

    #[test]
    fn parse_with_recovery_continues_after_missing_end() {
        let source = "rule main() -> i64:\n  when true:\n    yield 1.\n  otherwise:\n    yield 2.\n\nrule second() -> i64:\n  yield 3.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse_with_recovery(&tokens).unwrap_err();
        assert!(err.len() >= 1);
    }

    #[test]
    fn parse_with_recovery_missing_end_reports_once() {
        let source = "rule main() -> i64:\n  yield 1.\n\nrule second() -> i64:\n  yield 2.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse_with_recovery(&tokens).unwrap_err();
        let missing_end = err.iter().filter(|e| e.code == "E0202").count();
        assert_eq!(missing_end, 1);
    }

    #[test]
    fn parse_with_recovery_continues_after_missing_rparen() {
        let source = "rule main() -> i64:\n  yield (1 + 2.\n  yield 3.\nend\n";
        let tokens = lexer::lex(source).unwrap();
        let err = parse_with_recovery(&tokens).unwrap_err();
        assert!(!err.is_empty());
    }
}
