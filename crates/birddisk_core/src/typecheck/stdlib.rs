use super::{BookInfo, Checker, EnumInfo, EnumVariantInfo, FunctionSig, Ty};
use crate::ast::Program;
use std::collections::HashMap;

impl<'a> Checker<'a> {
    pub(super) fn register_stdlib(&mut self, program: &Program) {
        let has_std_string = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "string"
        });
        let has_std_bytes = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "bytes"
        });
        let has_std_io = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "io"
        });
        let has_std_time = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "time"
        });
        let has_std_profiler = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "profiler"
        });
        let has_std_fs = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "fs"
        });
        let has_std_path = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "path"
        });
        let has_std_env = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "env"
        });
        let has_std_json = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "json"
        });
        let has_std_rand = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "rand"
        });
        let has_std_test = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "test"
        });
        let has_std_channel = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "channel"
        });
        let has_std_thread = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "thread"
        });
        let has_std_net = program.imports.iter().any(|import| {
            import.path.len() == 2 && import.path[0] == "std" && import.path[1] == "net"
        });
        if has_std_string {
            self.insert_function("std::string::len", vec![Ty::String], Ty::I64);
            self.insert_function(
                "std::string::concat",
                vec![Ty::String, Ty::String],
                Ty::String,
            );
            self.insert_function("std::string::eq", vec![Ty::String, Ty::String], Ty::Bool);
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
            self.insert_function("std::string::to_i64", vec![Ty::String], Ty::I64);
            self.insert_function("std::string::from_i64", vec![Ty::I64], Ty::String);
        }
        if has_std_bytes {
            let bytes = Ty::Array(Box::new(Ty::U8));
            self.insert_function("std::bytes::len", vec![bytes.clone()], Ty::I64);
            self.insert_function("std::bytes::eq", vec![bytes.clone(), bytes], Ty::Bool);
            let bytes = Ty::Array(Box::new(Ty::U8));
            self.insert_function(
                "std::bytes::slice",
                vec![bytes.clone(), Ty::I64, Ty::I64],
                bytes.clone(),
            );
            self.insert_function("std::bytes::index_of", vec![bytes.clone(), Ty::U8], Ty::I64);
            self.insert_function("std::bytes::contains", vec![bytes, Ty::U8], Ty::Bool);
        }
        if has_std_io {
            self.insert_function("std::io::print", vec![Ty::String], Ty::Void);
            self.insert_function("std::io::read_line", Vec::new(), Ty::String);
        }
        if has_std_time {
            self.insert_function("std::time::now_ms", Vec::new(), Ty::I64);
            self.insert_function("std::time::sleep_ms", vec![Ty::I64], Ty::I64);
        }
        if has_std_profiler {
            self.insert_function("std::profiler::uptime_ms", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::alloc_count", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::bytes_allocated", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::bytes_in_use", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::peak_bytes_in_use", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::gc_runs", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::last_freed", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::last_live", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::last_freed_bytes", Vec::new(), Ty::I64);
            self.insert_function("std::profiler::last_live_bytes", Vec::new(), Ty::I64);
        }
        if has_std_fs {
            self.insert_function("std::fs::read_text", vec![Ty::String], Ty::String);
            self.insert_function("std::fs::write_text", vec![Ty::String, Ty::String], Ty::I64);
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
            self.insert_function("std::path::join", vec![Ty::String, Ty::String], Ty::String);
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
            self.insert_function("std::env::set_var", vec![Ty::String, Ty::String], Ty::I64);
            self.insert_function("std::env::cwd", Vec::new(), Ty::String);
            self.insert_function("std::env::set_cwd", vec![Ty::String], Ty::I64);
        }
        if has_std_json {
            self.insert_function("std::json::encode_i64", vec![Ty::I64], Ty::String);
            self.insert_function("std::json::encode_bool", vec![Ty::Bool], Ty::String);
            self.insert_function("std::json::encode_string", vec![Ty::String], Ty::String);
            self.insert_function("std::json::decode_i64", vec![Ty::String], Ty::I64);
            self.insert_function("std::json::decode_bool", vec![Ty::String], Ty::Bool);
            self.insert_function("std::json::decode_string", vec![Ty::String], Ty::String);
        }
        if has_std_rand {
            self.insert_function("std::rand::seed", vec![Ty::I64], Ty::Void);
            self.insert_function("std::rand::range", vec![Ty::I64, Ty::I64], Ty::I64);
        }
        if has_std_test {
            self.insert_function("std::test::assert", vec![Ty::Bool, Ty::String], Ty::Void);
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
        if has_std_channel {
            self.register_channel_stdlib();
        }
        if has_std_thread {
            self.register_thread_stdlib();
        }
        if has_std_net {
            self.register_net_stdlib();
        }
    }

    fn register_channel_stdlib(&mut self) {
        let channels = [
            ("I64", "i64", Ty::I64),
            ("Bool", "bool", Ty::Bool),
            ("F64", "f64", Ty::F64),
            ("U8", "u8", Ty::U8),
            ("String", "string", Ty::String),
            ("Bytes", "bytes", Ty::Array(Box::new(Ty::U8))),
        ];
        for (suffix, ctor, payload) in channels {
            let channel_name = format!("Channel{suffix}");
            if !self.books.contains_key(&channel_name) {
                self.books.insert(
                    channel_name.clone(),
                    BookInfo {
                        fields: HashMap::new(),
                    },
                );
            }

            let recv_name = format!("Recv{suffix}");
            if !self.enums.contains_key(&recv_name) {
                let mut variants = HashMap::new();
                variants.insert(
                    "Ok".to_string(),
                    EnumVariantInfo {
                        payload: Some(payload.clone()),
                    },
                );
                variants.insert("Closed".to_string(), EnumVariantInfo { payload: None });
                self.enums.insert(recv_name.clone(), EnumInfo { variants });
            }

            let channel_ty = Ty::Book(channel_name.clone());
            self.insert_function(
                &format!("std::channel::{ctor}"),
                Vec::new(),
                channel_ty.clone(),
            );
            self.insert_function(
                &format!("{channel_name}::send"),
                vec![channel_ty.clone(), payload.clone()],
                Ty::Bool,
            );
            self.insert_function(
                &format!("{channel_name}::recv"),
                vec![channel_ty.clone()],
                Ty::Enum(recv_name.clone()),
            );
            self.insert_function(
                &format!("{channel_name}::close"),
                vec![channel_ty],
                Ty::Void,
            );
        }
    }

    fn register_thread_stdlib(&mut self) {
        if !self.books.contains_key("Thread") {
            self.books.insert(
                "Thread".to_string(),
                BookInfo {
                    fields: HashMap::new(),
                },
            );
        }
        self.insert_function(
            "std::thread::join",
            vec![Ty::Book("Thread".to_string())],
            Ty::I64,
        );
    }

    fn register_net_stdlib(&mut self) {
        if !self.books.contains_key("TcpStream") {
            self.books.insert(
                "TcpStream".to_string(),
                BookInfo {
                    fields: HashMap::new(),
                },
            );
        }
        if !self.books.contains_key("TcpListener") {
            self.books.insert(
                "TcpListener".to_string(),
                BookInfo {
                    fields: HashMap::new(),
                },
            );
        }
        if !self.books.contains_key("TcpPool") {
            self.books.insert(
                "TcpPool".to_string(),
                BookInfo {
                    fields: HashMap::new(),
                },
            );
        }
        self.insert_function(
            "std::net::connect",
            vec![Ty::String],
            Ty::Book("TcpStream".to_string()),
        );
        self.insert_function(
            "std::net::listen",
            vec![Ty::String],
            Ty::Book("TcpListener".to_string()),
        );
        self.insert_function(
            "std::net::accept",
            vec![Ty::Book("TcpListener".to_string())],
            Ty::Book("TcpStream".to_string()),
        );
        self.insert_function(
            "std::net::write_text",
            vec![Ty::Book("TcpStream".to_string()), Ty::String],
            Ty::I64,
        );
        self.insert_function(
            "std::net::read_line",
            vec![Ty::Book("TcpStream".to_string())],
            Ty::String,
        );
        self.insert_function(
            "std::net::read_exact",
            vec![Ty::Book("TcpStream".to_string()), Ty::I64],
            Ty::String,
        );
        self.insert_function(
            "std::net::read_to_end",
            vec![Ty::Book("TcpStream".to_string())],
            Ty::String,
        );
        self.insert_function(
            "std::net::set_read_timeout_ms",
            vec![Ty::Book("TcpStream".to_string()), Ty::I64],
            Ty::I64,
        );
        self.insert_function(
            "std::net::close_stream",
            vec![Ty::Book("TcpStream".to_string())],
            Ty::Void,
        );
        self.insert_function(
            "std::net::close_listener",
            vec![Ty::Book("TcpListener".to_string())],
            Ty::Void,
        );
        self.insert_function(
            "std::net::pool",
            vec![Ty::String, Ty::I64],
            Ty::Book("TcpPool".to_string()),
        );
        self.insert_function(
            "std::net::pool_get",
            vec![Ty::Book("TcpPool".to_string())],
            Ty::Book("TcpStream".to_string()),
        );
        self.insert_function(
            "std::net::pool_put",
            vec![
                Ty::Book("TcpPool".to_string()),
                Ty::Book("TcpStream".to_string()),
            ],
            Ty::Bool,
        );
        self.insert_function(
            "std::net::pool_close",
            vec![Ty::Book("TcpPool".to_string())],
            Ty::Void,
        );
    }

    fn insert_function(&mut self, name: &str, params: Vec<Ty>, return_type: Ty) {
        self.functions
            .entry(name.to_string())
            .or_insert(FunctionSig {
                params,
                return_type,
            });
    }
}
