use super::{Checker, FunctionSig, Ty};
use crate::ast::Program;

impl<'a> Checker<'a> {
    pub(super) fn register_stdlib(&mut self, program: &Program) {
        let has_std_string = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "string"
        });
        let has_std_bytes = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "bytes"
        });
        let has_std_io = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "io"
        });
        if has_std_string {
            self.insert_function(
                "std::string::len",
                vec![Ty::String],
                Ty::I64,
            );
            self.insert_function(
                "std::string::concat",
                vec![Ty::String, Ty::String],
                Ty::String,
            );
            self.insert_function(
                "std::string::eq",
                vec![Ty::String, Ty::String],
                Ty::Bool,
            );
            self.insert_function(
                "std::string::bytes",
                vec![Ty::String],
                Ty::Array(Box::new(Ty::U8)),
            );
            self.insert_function(
                "std::string::from_bytes",
                vec![Ty::Array(Box::new(Ty::U8))],
                Ty::String,
            );
            self.insert_function(
                "std::string::to_i64",
                vec![Ty::String],
                Ty::I64,
            );
            self.insert_function(
                "std::string::from_i64",
                vec![Ty::I64],
                Ty::String,
            );
        }
        if has_std_bytes {
            let bytes = Ty::Array(Box::new(Ty::U8));
            self.insert_function("std::bytes::len", vec![bytes.clone()], Ty::I64);
            self.insert_function(
                "std::bytes::eq",
                vec![bytes.clone(), bytes],
                Ty::Bool,
            );
        }
        if has_std_io {
            self.insert_function("std::io::print", vec![Ty::String], Ty::I64);
            self.insert_function("std::io::read_line", Vec::new(), Ty::String);
        }
    }

    fn insert_function(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.functions.entry(name.to_string()).or_insert(FunctionSig {
            params,
            return_type,
        });
    }
}
