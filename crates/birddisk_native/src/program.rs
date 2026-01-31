use crate::analysis::is_ref_type;
use crate::error::{native_error, NativeError};
use crate::NATIVE_MAIN_SYMBOL;
use birddisk_core::ast::{Program, Type};
use birddisk_core::TraceFrame;
use cranelift_codegen::ir::{types, AbiParam};
use cranelift_module::{FuncId, Linkage, Module};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub(crate) struct FunctionSig {
    pub(crate) params: Vec<Type>,
    pub(crate) return_type: Type,
}

#[derive(Debug, Clone)]
pub(crate) struct BookLayout {
    pub(crate) id: u32,
    pub(crate) fields: Vec<Type>,
    pub(crate) field_index: HashMap<String, usize>,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumVariantInfo {
    pub(crate) id: u32,
    pub(crate) payload: Option<Type>,
}

#[derive(Debug, Clone)]
pub(crate) struct EnumInfo {
    pub(crate) id: u32,
    pub(crate) variants: HashMap<String, EnumVariantInfo>,
}

pub(crate) struct TraceTable {
    pub(crate) frames: Vec<TraceFrame>,
    pub(crate) ids: HashMap<String, i64>,
}

pub(crate) fn collect_function_sigs(program: &Program) -> Result<HashMap<String, FunctionSig>, NativeError> {
    let mut functions = HashMap::new();
    for func in &program.functions {
        insert_function_sig(&mut functions, &func.name, func)?;
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            insert_function_sig(&mut functions, &name, method)?;
        }
    }
    Ok(functions)
}

fn insert_function_sig(
    functions: &mut HashMap<String, FunctionSig>,
    name: &str,
    func: &birddisk_core::ast::Function,
) -> Result<(), NativeError> {
    if functions.contains_key(name) {
        return Err(native_error(format!(
            "native backend does not support duplicate function '{}'.",
            name
        )));
    }
    functions.insert(
        name.to_string(),
        FunctionSig {
            params: func.params.iter().map(|p| p.ty.clone()).collect(),
            return_type: func.return_type.clone(),
        },
    );
    Ok(())
}

pub(crate) fn declare_functions<M: Module>(
    module: &mut M,
    program: &Program,
    functions: &HashMap<String, FunctionSig>,
    name_mangler: impl Fn(&str) -> String,
) -> Result<HashMap<String, FuncId>, NativeError> {
    let mut ids = HashMap::new();
    for (func, name) in collect_functions(program) {
        let sig = make_signature(module, func);
        let symbol = name_mangler(&name);
        let linkage = if name == "main" {
            Linkage::Export
        } else {
            Linkage::Local
        };
        let id = module
            .declare_function(&symbol, linkage, &sig)
            .map_err(|err| native_error(format!("native declare failed: {err}")))?;
        ids.insert(name, id);
    }
    for name in functions.keys() {
        if !ids.contains_key(name) {
            return Err(native_error(format!("missing declared function '{name}'.")));
        }
    }
    Ok(ids)
}

pub(crate) fn make_signature(
    module: &mut dyn Module,
    function: &birddisk_core::ast::Function,
) -> cranelift_codegen::ir::Signature {
    let mut sig = module.make_signature();
    sig.params.push(AbiParam::new(types::I64));
    for param in &function.params {
        sig.params.push(AbiParam::new(abi_type(&param.ty)));
    }
    if !matches!(function.return_type, Type::Void) {
        sig.returns.push(AbiParam::new(abi_type(&function.return_type)));
    }
    sig
}

fn abi_type(ty: &Type) -> types::Type {
    match ty {
        Type::F64 => types::F64,
        _ => types::I64,
    }
}

pub(crate) fn type_name(ty: &Type) -> &'static str {
    match ty {
        Type::I64 => "i64",
        Type::F64 => "f64",
        Type::Bool => "bool",
        Type::String => "string",
        Type::U8 => "u8",
        Type::Void => "void",
        Type::Array(_) => "array",
        Type::Book(_) => "book",
    }
}

pub(crate) fn mangle_symbol(name: &str) -> String {
    if name == "main" {
        return NATIVE_MAIN_SYMBOL.to_string();
    }
    let mut out = String::from("bd_");
    for ch in name.chars() {
        if ch.is_ascii_alphanumeric() || ch == '_' {
            out.push(ch);
        } else {
            use std::fmt::Write;
            let _ = write!(&mut out, "_u{:04x}", ch as u32);
        }
    }
    out
}

pub(crate) fn stdlib_signature(name: &str) -> Option<FunctionSig> {
    match name {
        "std::string::len" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::string::concat" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::String,
        }),
        "std::string::eq" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::Bool,
        }),
        "std::string::bytes" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::Array(Box::new(Type::U8)),
        }),
        "std::string::slice" => Some(FunctionSig {
            params: vec![Type::String, Type::I64, Type::I64],
            return_type: Type::String,
        }),
        "std::string::index_of" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::I64,
        }),
        "std::string::contains" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::Bool,
        }),
        "std::string::replace" => Some(FunctionSig {
            params: vec![Type::String, Type::String, Type::String],
            return_type: Type::String,
        }),
        "std::string::from_bytes" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8))],
            return_type: Type::String,
        }),
        "std::string::to_i64" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::string::from_i64" => Some(FunctionSig {
            params: vec![Type::I64],
            return_type: Type::String,
        }),
        "std::bytes::len" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8))],
            return_type: Type::I64,
        }),
        "std::bytes::eq" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8)), Type::Array(Box::new(Type::U8))],
            return_type: Type::Bool,
        }),
        "std::bytes::slice" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8)), Type::I64, Type::I64],
            return_type: Type::Array(Box::new(Type::U8)),
        }),
        "std::bytes::index_of" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8)), Type::U8],
            return_type: Type::I64,
        }),
        "std::bytes::contains" => Some(FunctionSig {
            params: vec![Type::Array(Box::new(Type::U8)), Type::U8],
            return_type: Type::Bool,
        }),
        "std::io::print" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::Void,
        }),
        "std::io::read_line" => Some(FunctionSig {
            params: Vec::new(),
            return_type: Type::String,
        }),
        "std::time::now_ms" => Some(FunctionSig {
            params: Vec::new(),
            return_type: Type::I64,
        }),
        "std::time::sleep_ms" => Some(FunctionSig {
            params: vec![Type::I64],
            return_type: Type::I64,
        }),
        "std::rand::seed" => Some(FunctionSig {
            params: vec![Type::I64],
            return_type: Type::Void,
        }),
        "std::rand::range" => Some(FunctionSig {
            params: vec![Type::I64, Type::I64],
            return_type: Type::I64,
        }),
        "std::test::assert" => Some(FunctionSig {
            params: vec![Type::Bool, Type::String],
            return_type: Type::Void,
        }),
        "std::test::assert_eq_i64" => Some(FunctionSig {
            params: vec![Type::I64, Type::I64, Type::String],
            return_type: Type::Void,
        }),
        "std::test::assert_eq_bool" => Some(FunctionSig {
            params: vec![Type::Bool, Type::Bool, Type::String],
            return_type: Type::Void,
        }),
        "std::test::assert_eq_string" => Some(FunctionSig {
            params: vec![Type::String, Type::String, Type::String],
            return_type: Type::Void,
        }),
        "std::fs::read_text" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        "std::fs::write_text" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::I64,
        }),
        "std::fs::read_bytes" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::Array(Box::new(Type::U8)),
        }),
        "std::fs::write_bytes" => Some(FunctionSig {
            params: vec![Type::String, Type::Array(Box::new(Type::U8))],
            return_type: Type::I64,
        }),
        "std::path::join" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::String,
        }),
        "std::path::normalize" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        "std::path::basename" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        "std::path::dirname" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        "std::env::args" => Some(FunctionSig {
            params: Vec::new(),
            return_type: Type::Array(Box::new(Type::String)),
        }),
        "std::env::get" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        "std::env::set_var" => Some(FunctionSig {
            params: vec![Type::String, Type::String],
            return_type: Type::I64,
        }),
        "std::env::cwd" => Some(FunctionSig {
            params: Vec::new(),
            return_type: Type::String,
        }),
        "std::env::set_cwd" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::json::encode_i64" => Some(FunctionSig {
            params: vec![Type::I64],
            return_type: Type::String,
        }),
        "std::json::encode_bool" => Some(FunctionSig {
            params: vec![Type::Bool],
            return_type: Type::String,
        }),
        "std::json::encode_string" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        "std::json::decode_i64" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::I64,
        }),
        "std::json::decode_bool" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::Bool,
        }),
        "std::json::decode_string" => Some(FunctionSig {
            params: vec![Type::String],
            return_type: Type::String,
        }),
        _ => None,
    }
}

pub(crate) fn collect_functions(program: &Program) -> Vec<(&birddisk_core::ast::Function, String)> {
    let mut out = Vec::new();
    for func in &program.functions {
        out.push((func, func.name.clone()));
    }
    for book in &program.books {
        for method in &book.methods {
            out.push((method, format!("{}::{}", book.name, method.name)));
        }
    }
    out
}

pub(crate) fn build_trace_table(program: &Program) -> TraceTable {
    let mut frames = Vec::new();
    let mut ids = HashMap::new();
    let mut insert = |name: String, file: String, source: String, span| {
        let id = frames.len() as i64;
        frames.push(TraceFrame {
            function: name.clone(),
            file,
            span,
            source,
        });
        ids.insert(name, id);
    };
    for func in &program.functions {
        insert(
            func.name.clone(),
            func.file.clone(),
            func.source.clone(),
            func.span,
        );
    }
    for book in &program.books {
        for method in &book.methods {
            let name = format!("{}::{}", book.name, method.name);
            insert(
                name,
                method.file.clone(),
                method.source.clone(),
                method.span,
            );
        }
    }
    TraceTable { frames, ids }
}

pub(crate) fn build_book_layouts(
    program: &Program,
) -> Result<(HashMap<String, BookLayout>, Vec<Vec<usize>>), NativeError> {
    let mut books = HashMap::new();
    let mut ref_fields = Vec::new();
    for (book_id, book) in program.books.iter().enumerate() {
        if books.contains_key(&book.name) {
            return Err(native_error(format!(
                "native backend does not support duplicate book '{}'.",
                book.name
            )));
        }
        let mut field_index = HashMap::new();
        let mut fields = Vec::new();
        let mut refs = Vec::new();
        for (index, field) in book.fields.iter().enumerate() {
            field_index.insert(field.name.clone(), index);
            fields.push(field.ty.clone());
            if is_ref_type(&field.ty) {
                refs.push(index);
            }
        }
        ref_fields.push(refs.clone());
        books.insert(
            book.name.clone(),
            BookLayout {
                id: book_id as u32,
                fields,
                field_index,
            },
        );
    }
    Ok((books, ref_fields))
}

pub(crate) fn build_enum_layouts(
    program: &Program,
) -> Result<HashMap<String, EnumInfo>, NativeError> {
    let mut enums = HashMap::new();
    for (enum_id, enum_decl) in program.enums.iter().enumerate() {
        if enums.contains_key(&enum_decl.name) {
            return Err(native_error(format!(
                "native backend does not support duplicate enum '{}'.",
                enum_decl.name
            )));
        }
        let mut variants = HashMap::new();
        for (variant_id, variant) in enum_decl.variants.iter().enumerate() {
            variants.insert(
                variant.name.clone(),
                EnumVariantInfo {
                    id: variant_id as u32,
                    payload: variant.payload.as_ref().map(|payload| payload.ty.clone()),
                },
            );
        }
        enums.insert(
            enum_decl.name.clone(),
            EnumInfo {
                id: enum_id as u32,
                variants,
            },
        );
    }
    Ok(enums)
}
