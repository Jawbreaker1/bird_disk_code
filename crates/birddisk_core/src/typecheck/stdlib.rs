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
        let has_std_time = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "time"
        });
        let has_std_fs = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "fs"
        });
        let has_std_path = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "path"
        });
        let has_std_env = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "env"
        });
        let has_std_json = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "json"
        });
        let has_std_rand = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "rand"
        });
        let has_std_test = program.imports.iter().any(|import| {
            import.path.len() == 2
                && import.path[0] == "std"
                && import.path[1] == "test"
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
                "std::string::slice",
                vec![Ty::String, Ty::I64, Ty::I64],
                Ty::String,
            );
            self.insert_function(
                "std::string::index_of",
                vec![Ty::String, Ty::String],
                Ty::I64,
            );
            self.insert_function(
                "std::string::contains",
                vec![Ty::String, Ty::String],
                Ty::Bool,
            );
            self.insert_function(
                "std::string::replace",
                vec![Ty::String, Ty::String, Ty::String],
                Ty::String,
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
            let bytes = Ty::Array(Box::new(Ty::U8));
            self.insert_function(
                "std::bytes::slice",
                vec![bytes.clone(), Ty::I64, Ty::I64],
                bytes.clone(),
            );
            self.insert_function(
                "std::bytes::index_of",
                vec![bytes.clone(), Ty::U8],
                Ty::I64,
            );
            self.insert_function(
                "std::bytes::contains",
                vec![bytes, Ty::U8],
                Ty::Bool,
            );
        }
        if has_std_io {
            self.insert_function("std::io::print", vec![Ty::String], Ty::Void);
            self.insert_function("std::io::read_line", Vec::new(), Ty::String);
        }
        if has_std_time {
            self.insert_function("std::time::now_ms", Vec::new(), Ty::I64);
            self.insert_function("std::time::sleep_ms", vec![Ty::I64], Ty::I64);
        }
        if has_std_fs {
            self.insert_function("std::fs::read_text", vec![Ty::String], Ty::String);
            self.insert_function(
                "std::fs::write_text",
                vec![Ty::String, Ty::String],
                Ty::I64,
            );
            self.insert_function(
                "std::fs::read_bytes",
                vec![Ty::String],
                Ty::Array(Box::new(Ty::U8)),
            );
            self.insert_function(
                "std::fs::write_bytes",
                vec![Ty::String, Ty::Array(Box::new(Ty::U8))],
                Ty::I64,
            );
        }
        if has_std_path {
            self.insert_function(
                "std::path::join",
                vec![Ty::String, Ty::String],
                Ty::String,
            );
            self.insert_function("std::path::normalize", vec![Ty::String], Ty::String);
            self.insert_function("std::path::basename", vec![Ty::String], Ty::String);
            self.insert_function("std::path::dirname", vec![Ty::String], Ty::String);
        }
        if has_std_env {
            self.insert_function(
                "std::env::args",
                Vec::new(),
                Ty::Array(Box::new(Ty::String)),
            );
            self.insert_function("std::env::get", vec![Ty::String], Ty::String);
            self.insert_function(
                "std::env::set_var",
                vec![Ty::String, Ty::String],
                Ty::I64,
            );
            self.insert_function("std::env::cwd", Vec::new(), Ty::String);
            self.insert_function("std::env::set_cwd", vec![Ty::String], Ty::I64);
        }
        if has_std_json {
            self.insert_function("std::json::encode_i64", vec![Ty::I64], Ty::String);
            self.insert_function("std::json::encode_bool", vec![Ty::Bool], Ty::String);
            self.insert_function(
                "std::json::encode_string",
                vec![Ty::String],
                Ty::String,
            );
            self.insert_function("std::json::decode_i64", vec![Ty::String], Ty::I64);
            self.insert_function("std::json::decode_bool", vec![Ty::String], Ty::Bool);
            self.insert_function("std::json::decode_string", vec![Ty::String], Ty::String);
        }
        if has_std_rand {
            self.insert_function("std::rand::seed", vec![Ty::I64], Ty::Void);
            self.insert_function("std::rand::range", vec![Ty::I64, Ty::I64], Ty::I64);
        }
        if has_std_test {
            self.insert_function(
                "std::test::assert",
                vec![Ty::Bool, Ty::String],
                Ty::Void,
            );
            self.insert_function(
                "std::test::assert_eq_i64",
                vec![Ty::I64, Ty::I64, Ty::String],
                Ty::Void,
            );
            self.insert_function(
                "std::test::assert_eq_bool",
                vec![Ty::Bool, Ty::Bool, Ty::String],
                Ty::Void,
            );
            self.insert_function(
                "std::test::assert_eq_string",
                vec![Ty::String, Ty::String, Ty::String],
                Ty::Void,
            );
        }
    }

    fn insert_function(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.functions.entry(name.to_string()).or_insert(FunctionSig {
            params,
            return_type,
        });
    }
}
