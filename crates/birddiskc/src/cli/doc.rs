use birddisk_core::ast::{EnumDecl, EnumVariant, Function, Program, Type};
use std::cmp::Ordering;

pub(crate) fn render_docs(
    path: &str,
    config: &birddisk_core::ModuleConfig,
) -> Result<String, String> {
    let program = birddisk_core::parse_and_typecheck_with_config(path, config)
        .map_err(|diags| {
            let message = diags
                .first()
                .map(|diag| diag.message.as_str())
                .unwrap_or("parse/typecheck failed");
            message.to_string()
        })?;
    Ok(render_program(&program, path))
}

fn render_program(program: &Program, entry_path: &str) -> String {
    let mut out = String::new();
    out.push_str("# BirdDisk Docs\n");
    out.push_str(&format!("Entry: {entry_path}\n\n"));
    if !program.imports.is_empty() {
        out.push_str("## Entry Imports\n");
        for import in &program.imports {
            out.push_str("- ");
            out.push_str(&import.path.join("::"));
            out.push('\n');
        }
        out.push('\n');
    }

    render_enums(&mut out, &program.enums);
    render_books(&mut out, &program.books);
    render_functions(&mut out, &program.functions);
    out
}

fn render_enums(out: &mut String, enums: &[EnumDecl]) {
    if enums.is_empty() {
        return;
    }
    out.push_str("## Enums\n");
    let mut enums_sorted = enums.to_vec();
    enums_sorted.sort_by(|a, b| a.name.cmp(&b.name));
    for en in enums_sorted {
        out.push_str(&format!("### {}\n", en.name));
        for variant in en.variants {
            render_variant(out, &variant);
        }
        out.push('\n');
    }
}

fn render_variant(out: &mut String, variant: &EnumVariant) {
    out.push_str("- ");
    out.push_str(&variant.name);
    if let Some(payload) = &variant.payload {
        out.push_str(&format!("({}: {})", payload.name, format_type(&payload.ty)));
    }
    out.push('\n');
}

fn render_books(out: &mut String, books: &[birddisk_core::ast::Book]) {
    if books.is_empty() {
        return;
    }
    out.push_str("## Books\n");
    let mut books_sorted = books.to_vec();
    books_sorted.sort_by(|a, b| a.name.cmp(&b.name));
    for book in books_sorted {
        out.push_str(&format!("### {}\n", book.name));
        if !book.fields.is_empty() {
            out.push_str("Fields:\n");
            for field in &book.fields {
                out.push_str(&format!("- {}: {}\n", field.name, format_type(&field.ty)));
            }
        }
        if !book.methods.is_empty() {
            out.push_str("Methods:\n");
            let mut methods = book.methods.clone();
            methods.sort_by(|a, b| a.name.cmp(&b.name));
            for method in methods {
                out.push_str(&format!(
                    "- rule {}({}) -> {}\n",
                    method.name,
                    format_params(&method),
                    format_type(&method.return_type)
                ));
            }
        }
        out.push('\n');
    }
}

fn render_functions(out: &mut String, functions: &[Function]) {
    if functions.is_empty() {
        return;
    }
    out.push_str("## Functions\n");
    let mut funcs = functions.to_vec();
    funcs.sort_by(|a, b| {
        let name = a.name.cmp(&b.name);
        if name == Ordering::Equal {
            a.file.cmp(&b.file)
        } else {
            name
        }
    });
    for func in funcs {
        out.push_str(&format!(
            "- rule {}({}) -> {} ({})\n",
            func.name,
            format_params(&func),
            format_type(&func.return_type),
            func.file
        ));
    }
    out.push('\n');
}

fn format_params(func: &Function) -> String {
    let mut out = String::new();
    for (idx, param) in func.params.iter().enumerate() {
        if idx > 0 {
            out.push_str(", ");
        }
        out.push_str(&format!("{}: {}", param.name, format_type(&param.ty)));
    }
    out
}

fn format_type(ty: &Type) -> String {
    match ty {
        Type::I64 => "i64".to_string(),
        Type::F64 => "f64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::String => "string".to_string(),
        Type::U8 => "u8".to_string(),
        Type::Void => "void".to_string(),
        Type::Array(inner) => format!("{}[]", format_type(inner)),
        Type::Book(name) => name.clone(),
    }
}

#[cfg(test)]
mod tests {
    use super::render_docs;
    use std::env;
    use std::fs;

    #[test]
    fn render_docs_contains_functions() {
        let source = "rule add_one(value: i64) -> i64:\n  yield value + 1.\nend\n\nrule main() -> i64:\n  yield add_one(1).\nend\n";
        let mut path = env::temp_dir();
        path.push(format!("birddisk_doc_{}.bd", std::process::id()));
        fs::write(&path, source).expect("write temp file");
        let path_str = path.to_string_lossy().to_string();

        let output =
            render_docs(&path_str, &birddisk_core::ModuleConfig::default()).expect("docs");
        assert!(output.contains("# BirdDisk Docs"));
        assert!(output.contains("## Functions"));
        assert!(output.contains("rule add_one"));

        let _ = fs::remove_file(&path);
    }
}
