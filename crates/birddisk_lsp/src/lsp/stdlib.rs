use birddisk_core::ast::{Program, Type};
use birddisk_core::lexer;
use std::collections::HashMap;
use std::path::{Path, PathBuf};

#[derive(Clone)]
pub(crate) struct CallSignature {
    pub(crate) params: Vec<String>,
    pub(crate) return_type: Type,
}

pub(crate) fn stdlib_signatures(
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
            "std::string::slice".to_string(),
            CallSignature {
                params: vec!["text".to_string(), "start".to_string(), "len".to_string()],
                return_type: Type::String,
            },
        );
        signatures.insert(
            "std::string::index_of".to_string(),
            CallSignature {
                params: vec!["text".to_string(), "needle".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::string::contains".to_string(),
            CallSignature {
                params: vec!["text".to_string(), "needle".to_string()],
                return_type: Type::Bool,
            },
        );
        signatures.insert(
            "std::string::replace".to_string(),
            CallSignature {
                params: vec![
                    "text".to_string(),
                    "needle".to_string(),
                    "replacement".to_string(),
                ],
                return_type: Type::String,
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
        signatures.insert(
            "std::bytes::slice".to_string(),
            CallSignature {
                params: vec!["bytes".to_string(), "start".to_string(), "len".to_string()],
                return_type: Type::Array(Box::new(Type::U8)),
            },
        );
        signatures.insert(
            "std::bytes::index_of".to_string(),
            CallSignature {
                params: vec!["bytes".to_string(), "needle".to_string()],
                return_type: Type::I64,
            },
        );
        signatures.insert(
            "std::bytes::contains".to_string(),
            CallSignature {
                params: vec!["bytes".to_string(), "needle".to_string()],
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
    if has_import(program, &["std", "rand"]) {
        signatures.insert(
            "std::rand::seed".to_string(),
            CallSignature {
                params: vec!["seed".to_string()],
                return_type: Type::Void,
            },
        );
        signatures.insert(
            "std::rand::range".to_string(),
            CallSignature {
                params: vec!["min".to_string(), "max".to_string()],
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
                || module_name.starts_with("std::rand")
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

pub(crate) fn parse_stdlib_module(
    module_name: &str,
    path: &Path,
) -> HashMap<String, CallSignature> {
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

pub(crate) fn stdlib_module_path(root: &Path, segments: &[String]) -> Option<PathBuf> {
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

pub(crate) fn has_import(program: &Program, segments: &[&str]) -> bool {
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

pub(crate) fn builtin_stdlib_functions(module: &str) -> Vec<String> {
    match module {
        "std::string" => vec![
            "len",
            "concat",
            "eq",
            "bytes",
            "slice",
            "index_of",
            "contains",
            "replace",
            "from_bytes",
            "to_i64",
            "from_i64",
        ],
        "std::bytes" => vec!["len", "eq", "slice", "index_of", "contains"],
        "std::io" => vec!["print", "read_line"],
        "std::time" => vec!["now_ms", "sleep_ms"],
        "std::rand" => vec!["seed", "range"],
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
