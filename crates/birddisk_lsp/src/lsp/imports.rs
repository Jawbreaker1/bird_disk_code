use super::server::uri_to_path;
use super::stdlib::{builtin_stdlib_functions, parse_stdlib_module, stdlib_module_path};
use birddisk_core::ast::Program;
use birddisk_core::lexer;
use std::path::{Path, PathBuf};

pub(crate) fn import_modules(uri: &str) -> Vec<String> {
    let mut modules = vec![
        "std::string".to_string(),
        "std::bytes".to_string(),
        "std::io".to_string(),
        "std::time".to_string(),
        "std::rand".to_string(),
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

pub(crate) fn stdlib_module_functions(module: &str, root: Option<&Path>) -> Vec<String> {
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

pub(crate) fn user_module_functions(entry_path: &Path, segments: &[String]) -> Vec<String> {
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

pub(crate) fn find_stdlib_root(path: &Path) -> Option<PathBuf> {
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

pub(crate) fn project_root(entry_path: &Path) -> Option<PathBuf> {
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

pub(crate) fn resolve_user_module_path(entry_path: &Path, module_path: &[String]) -> Option<PathBuf> {
    for candidate in user_module_candidates(entry_path, module_path) {
        if candidate.exists() {
            return Some(candidate);
        }
    }
    None
}

pub(crate) fn parse_program_from_path(path: &Path) -> Option<Program> {
    let source = std::fs::read_to_string(path).ok()?;
    let tokens = lexer::lex(&source).ok()?;
    birddisk_core::parser::parse(&tokens).ok()
}

pub(crate) fn scan_stdlib_modules(root: &Path) -> Vec<String> {
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
