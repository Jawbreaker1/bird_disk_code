use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::Vm;
use std::path::{Component, Path, PathBuf};

impl<'a> Vm<'a> {
    pub(super) fn eval_path_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::path::join" => {
                if args.len() != 2 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::join expects 2 arguments",
                    ));
                }
                match (&args[0], &args[1]) {
                    (Value::String(left), Value::String(right)) => {
                        let left = self.string_text(*left)?;
                        let right = self.string_text(*right)?;
                        let joined = path_join(&left, &right)?;
                        Ok(Some(self.alloc_string(&joined)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::join expects string arguments",
                    )),
                }
            }
            "std::path::normalize" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::normalize expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let normalized = path_normalize(&path)?;
                        Ok(Some(self.alloc_string(&normalized)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::normalize expects string argument",
                    )),
                }
            }
            "std::path::basename" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::basename expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let name = path_basename(&path)?;
                        Ok(Some(self.alloc_string(&name)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::basename expects string argument",
                    )),
                }
            }
            "std::path::dirname" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        "std::path::dirname expects 1 argument",
                    ));
                }
                match &args[0] {
                    Value::String(handle) => {
                        let path = self.string_text(*handle)?;
                        let name = path_dirname(&path)?;
                        Ok(Some(self.alloc_string(&name)))
                    }
                    _ => Err(runtime_error(
                        "E0400",
                        "std::path::dirname expects string argument",
                    )),
                }
            }
            _ => Ok(None),
        }
    }
}

fn path_join(left: &str, right: &str) -> Result<String, RuntimeError> {
    let joined = Path::new(left).join(right);
    path_to_string(&joined, "std::path::join")
}

fn path_normalize(path: &str) -> Result<String, RuntimeError> {
    let mut out = PathBuf::new();
    let mut parts: Vec<std::ffi::OsString> = Vec::new();
    let mut has_root = false;
    for component in Path::new(path).components() {
        match component {
            Component::Prefix(prefix) => out.push(prefix.as_os_str()),
            Component::RootDir => {
                out.push(component.as_os_str());
                has_root = true;
            }
            Component::CurDir => {}
            Component::ParentDir => {
                if let Some(last) = parts.last() {
                    if last != std::ffi::OsStr::new("..") {
                        parts.pop();
                    } else {
                        parts.push(std::ffi::OsString::from(".."));
                    }
                } else if !has_root {
                    parts.push(std::ffi::OsString::from(".."));
                }
            }
            Component::Normal(part) => parts.push(part.to_os_string()),
        }
    }
    for part in parts {
        out.push(part);
    }
    if out.as_os_str().is_empty() {
        return Ok(".".to_string());
    }
    path_to_string(&out, "std::path::normalize")
}

fn path_basename(path: &str) -> Result<String, RuntimeError> {
    let name = Path::new(path)
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("");
    Ok(name.to_string())
}

fn path_dirname(path: &str) -> Result<String, RuntimeError> {
    let path = Path::new(path);
    if let Some(parent) = path.parent() {
        if parent.as_os_str().is_empty() {
            return Ok(".".to_string());
        }
        return path_to_string(parent, "std::path::dirname");
    }
    if path.has_root() {
        return path_to_string(path, "std::path::dirname");
    }
    Ok(".".to_string())
}

fn path_to_string(path: &Path, op: &str) -> Result<String, RuntimeError> {
    path.to_str()
        .map(|value| value.to_string())
        .ok_or_else(|| runtime_error("E0400", format!("{op} produced invalid UTF-8.")))
}
