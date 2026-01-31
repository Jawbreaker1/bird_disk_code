pub(crate) mod completion;
pub(crate) mod definitions;
pub(crate) mod hover;
pub(crate) mod imports;
pub(crate) mod server;
pub(crate) mod signature;
pub(crate) mod stdlib;

pub(crate) use server::Server;

#[cfg(test)]
mod tests {
    use super::completion::completion_items;
    use super::definitions::{
        build_symbol_index, definition_location, document_symbols, qualified_path_at_position,
        type_definition_location, SymbolIndex,
    };
    use super::server::{path_to_uri, span_contains, uri_to_path, Document};
    use super::signature::{inlay_hints, signature_help_at_position};
    use super::stdlib::stdlib_signatures;
    use birddisk_core::lexer::{self, TokenKind};
    use birddisk_core::{Position, Span};
    use serde_json::json;
    use std::path::PathBuf;

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
        let data = super::server::semantic_tokens(&tokens);
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
        let mut server = super::server::Server::new();
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
        let prefix_items = completion_items(&uri, main_source, &tokens, Some(&program), prefix_pos);
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
