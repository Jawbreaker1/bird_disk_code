use crate::heap::HeapHandle;
use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::{TcpPoolState, Vm};
use std::io::{Read, Write};
use std::net::Shutdown;
use std::time::Duration;

impl<'a> Vm<'a> {
    pub(super) fn eval_net_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::net::connect" | "std::net::host_connect" => self.eval_net_connect(args).map(Some),
            "std::net::listen" | "std::net::host_listen" => self.eval_net_listen(args).map(Some),
            "std::net::listener_addr" | "std::net::host_listener_addr" => {
                self.eval_net_listener_addr(args).map(Some)
            }
            "std::net::accept" | "std::net::host_accept" => self.eval_net_accept(args).map(Some),
            "std::net::write_text" | "std::net::host_write_text" => {
                self.eval_net_write_text(args).map(Some)
            }
            "std::net::read_line" | "std::net::host_read_line" => {
                self.eval_net_read_line(args).map(Some)
            }
            "std::net::read_exact" | "std::net::host_read_exact" => {
                self.eval_net_read_exact(args).map(Some)
            }
            "std::net::read_to_end" | "std::net::host_read_to_end" => {
                self.eval_net_read_to_end(args).map(Some)
            }
            "std::net::set_read_timeout_ms" | "std::net::host_set_read_timeout_ms" => {
                self.eval_net_set_read_timeout_ms(args).map(Some)
            }
            "std::net::close_stream" | "std::net::host_close_stream" => {
                self.eval_net_close_stream(args).map(Some)
            }
            "std::net::close_listener" | "std::net::host_close_listener" => {
                self.eval_net_close_listener(args).map(Some)
            }
            "std::net::pool" | "std::net::host_pool" => self.eval_net_pool(args).map(Some),
            "std::net::pool_get" | "std::net::host_pool_get" => {
                self.eval_net_pool_get(args).map(Some)
            }
            "std::net::pool_put" | "std::net::host_pool_put" => {
                self.eval_net_pool_put(args).map(Some)
            }
            "std::net::pool_close" | "std::net::host_pool_close" => {
                self.eval_net_pool_close(args).map(Some)
            }
            _ => Ok(None),
        }
    }

    fn eval_net_connect(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::connect expects 1 argument",
            ));
        }
        let Value::String(addr_handle) = args[0] else {
            return Err(runtime_error(
                "E0400",
                "std::net::connect expects address string argument",
            ));
        };
        let addr = self.string_text(addr_handle)?;
        let stream = std::net::TcpStream::connect(addr.as_str())
            .map_err(|err| runtime_error("E0408", format!("std::net::connect failed: {err}")))?;
        let value = self.alloc_object("TcpStream")?;
        let handle = tcp_handle_from_value(&value, "TcpStream")?;
        self.register_tcp_stream(handle, stream);
        Ok(value)
    }

    fn eval_net_listen(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::listen expects 1 argument",
            ));
        }
        let Value::String(addr_handle) = args[0] else {
            return Err(runtime_error(
                "E0400",
                "std::net::listen expects address string argument",
            ));
        };
        let addr = self.string_text(addr_handle)?;
        let listener = std::net::TcpListener::bind(addr.as_str())
            .map_err(|err| runtime_error("E0408", format!("std::net::listen failed: {err}")))?;
        let value = self.alloc_object("TcpListener")?;
        let handle = tcp_handle_from_value(&value, "TcpListener")?;
        self.register_tcp_listener(handle, listener);
        Ok(value)
    }

    fn eval_net_accept(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::accept expects 1 argument",
            ));
        }
        let listener_handle = tcp_handle_from_value(&args[0], "TcpListener")?;
        let (stream, _) = self
            .tcp_listener_mut(listener_handle)?
            .accept()
            .map_err(|err| runtime_error("E0408", format!("std::net::accept failed: {err}")))?;
        let value = self.alloc_object("TcpStream")?;
        let stream_handle = tcp_handle_from_value(&value, "TcpStream")?;
        self.register_tcp_stream(stream_handle, stream);
        Ok(value)
    }

    fn eval_net_listener_addr(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::listener_addr expects 1 argument",
            ));
        }
        let listener_handle = tcp_handle_from_value(&args[0], "TcpListener")?;
        let addr = self
            .tcp_listener_mut(listener_handle)?
            .local_addr()
            .map_err(|err| {
                runtime_error("E0408", format!("std::net::listener_addr failed: {err}"))
            })?;
        let addr_text = addr.to_string();
        Ok(self.alloc_string(addr_text.as_str()))
    }

    fn eval_net_write_text(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(runtime_error(
                "E0400",
                "std::net::write_text expects 2 arguments",
            ));
        }
        let stream_handle = tcp_handle_from_value(&args[0], "TcpStream")?;
        let Value::String(text_handle) = args[1] else {
            return Err(runtime_error(
                "E0400",
                "std::net::write_text expects string payload argument",
            ));
        };
        let text = self.string_text(text_handle)?;
        self.tcp_stream_mut(stream_handle)?
            .write_all(text.as_bytes())
            .map_err(|err| runtime_error("E0408", format!("std::net::write_text failed: {err}")))?;
        Ok(Value::I64(text.len() as i64))
    }

    fn eval_net_read_line(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::read_line expects 1 argument",
            ));
        }
        let stream_handle = tcp_handle_from_value(&args[0], "TcpStream")?;
        let mut bytes = Vec::new();
        let mut byte = [0u8; 1];
        loop {
            match self.tcp_stream_mut(stream_handle)?.read(&mut byte) {
                Ok(0) => break,
                Ok(_) => {
                    if byte[0] == b'\n' {
                        break;
                    }
                    bytes.push(byte[0]);
                }
                Err(err) => {
                    return Err(runtime_error(
                        "E0408",
                        format!("std::net::read_line failed: {err}"),
                    ))
                }
            }
        }
        if bytes.last() == Some(&b'\r') {
            bytes.pop();
        }
        let line = std::str::from_utf8(&bytes)
            .map_err(|_| runtime_error("E0408", "std::net::read_line received invalid UTF-8."))?;
        Ok(self.alloc_string(line))
    }

    fn eval_net_read_exact(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(runtime_error(
                "E0400",
                "std::net::read_exact expects 2 arguments",
            ));
        }
        let stream_handle = tcp_handle_from_value(&args[0], "TcpStream")?;
        let Value::I64(len) = args[1] else {
            return Err(runtime_error(
                "E0400",
                "std::net::read_exact expects i64 length argument",
            ));
        };
        if len < 0 {
            return Err(runtime_error(
                "E0408",
                "std::net::read_exact expects len >= 0.",
            ));
        }
        let target_len = len as usize;
        let mut bytes = vec![0u8; target_len];
        let mut offset = 0usize;
        while offset < target_len {
            match self
                .tcp_stream_mut(stream_handle)?
                .read(&mut bytes[offset..])
            {
                Ok(0) => {
                    return Err(runtime_error(
                        "E0408",
                        "std::net::read_exact reached EOF before reading requested length.",
                    ))
                }
                Ok(n) => offset += n,
                Err(err) => {
                    return Err(runtime_error(
                        "E0408",
                        format!("std::net::read_exact failed: {err}"),
                    ))
                }
            }
        }
        let text = std::str::from_utf8(&bytes).map_err(|_| {
            runtime_error(
                "E0408",
                "std::net::read_exact received invalid UTF-8 bytes.",
            )
        })?;
        Ok(self.alloc_string(text))
    }

    fn eval_net_read_to_end(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::read_to_end expects 1 argument",
            ));
        }
        let stream_handle = tcp_handle_from_value(&args[0], "TcpStream")?;
        let mut bytes = Vec::new();
        let mut chunk = [0u8; 4096];
        loop {
            match self.tcp_stream_mut(stream_handle)?.read(&mut chunk) {
                Ok(0) => break,
                Ok(n) => bytes.extend_from_slice(&chunk[..n]),
                Err(err) => {
                    return Err(runtime_error(
                        "E0408",
                        format!("std::net::read_to_end failed: {err}"),
                    ))
                }
            }
        }
        let text = std::str::from_utf8(&bytes).map_err(|_| {
            runtime_error(
                "E0408",
                "std::net::read_to_end received invalid UTF-8 bytes.",
            )
        })?;
        Ok(self.alloc_string(text))
    }

    fn eval_net_set_read_timeout_ms(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(runtime_error(
                "E0400",
                "std::net::set_read_timeout_ms expects 2 arguments",
            ));
        }
        let stream_handle = tcp_handle_from_value(&args[0], "TcpStream")?;
        let Value::I64(ms) = args[1] else {
            return Err(runtime_error(
                "E0400",
                "std::net::set_read_timeout_ms expects i64 timeout argument",
            ));
        };
        if ms < 0 {
            return Err(runtime_error(
                "E0408",
                "std::net::set_read_timeout_ms expects timeout >= 0.",
            ));
        }
        let timeout = if ms == 0 {
            None
        } else {
            Some(Duration::from_millis(ms as u64))
        };
        self.tcp_stream_mut(stream_handle)?
            .set_read_timeout(timeout)
            .map_err(|err| {
                runtime_error(
                    "E0408",
                    format!("std::net::set_read_timeout_ms failed: {err}"),
                )
            })?;
        Ok(Value::I64(ms))
    }

    fn eval_net_close_stream(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::close_stream expects 1 argument",
            ));
        }
        let handle = tcp_handle_from_value(&args[0], "TcpStream")?;
        let Some(stream) = self.close_tcp_stream(handle) else {
            return Err(runtime_error("E0408", "TcpStream handle is invalid."));
        };
        let _ = stream.shutdown(Shutdown::Both);
        Ok(Value::Void)
    }

    fn eval_net_close_listener(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::close_listener expects 1 argument",
            ));
        }
        let handle = tcp_handle_from_value(&args[0], "TcpListener")?;
        if !self.close_tcp_listener(handle) {
            return Err(runtime_error("E0408", "TcpListener handle is invalid."));
        }
        Ok(Value::Void)
    }

    fn eval_net_pool(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(runtime_error("E0400", "std::net::pool expects 2 arguments"));
        }
        let Value::String(addr_handle) = args[0] else {
            return Err(runtime_error(
                "E0400",
                "std::net::pool expects address string argument",
            ));
        };
        let Value::I64(max_idle) = args[1] else {
            return Err(runtime_error(
                "E0400",
                "std::net::pool expects i64 max_idle argument",
            ));
        };
        if max_idle < 0 {
            return Err(runtime_error(
                "E0408",
                "std::net::pool expects max_idle >= 0.",
            ));
        }
        let addr = self.string_text(addr_handle)?;
        let value = self.alloc_object("TcpPool")?;
        let handle = tcp_pool_handle_from_value(&value)?;
        self.register_tcp_pool(
            handle,
            TcpPoolState {
                addr,
                max_idle: max_idle as usize,
                idle: Vec::new(),
            },
        );
        Ok(value)
    }

    fn eval_net_pool_get(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::pool_get expects 1 argument",
            ));
        }
        let pool_handle = tcp_pool_handle_from_value(&args[0])?;
        let stream = if let Some(stream) = self.tcp_pool_mut(pool_handle)?.idle.pop() {
            stream
        } else {
            let addr = self.tcp_pool_mut(pool_handle)?.addr.clone();
            std::net::TcpStream::connect(addr.as_str()).map_err(|err| {
                runtime_error("E0408", format!("std::net::pool_get failed: {err}"))
            })?
        };
        let value = self.alloc_object("TcpStream")?;
        let stream_handle = tcp_handle_from_value(&value, "TcpStream")?;
        self.register_tcp_stream(stream_handle, stream);
        Ok(value)
    }

    fn eval_net_pool_put(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 2 {
            return Err(runtime_error(
                "E0400",
                "std::net::pool_put expects 2 arguments",
            ));
        }
        let pool_handle = tcp_pool_handle_from_value(&args[0])?;
        let stream_handle = tcp_handle_from_value(&args[1], "TcpStream")?;
        let Some(stream) = self.close_tcp_stream(stream_handle) else {
            return Err(runtime_error("E0408", "TcpStream handle is invalid."));
        };
        let mut stream_slot = Some(stream);
        let keep = {
            let pool = self.tcp_pool_mut(pool_handle)?;
            if pool.idle.len() < pool.max_idle {
                let stream = stream_slot
                    .take()
                    .ok_or_else(|| runtime_error("E0408", "TcpStream handle is invalid."))?;
                pool.idle.push(stream);
                true
            } else {
                false
            }
        };
        if !keep {
            let stream = stream_slot
                .take()
                .ok_or_else(|| runtime_error("E0408", "TcpStream handle is invalid."))?;
            let _ = stream.shutdown(Shutdown::Both);
        }
        Ok(Value::Bool(keep))
    }

    fn eval_net_pool_close(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                "std::net::pool_close expects 1 argument",
            ));
        }
        let handle = tcp_pool_handle_from_value(&args[0])?;
        let Some(pool) = self.close_tcp_pool(handle) else {
            return Err(runtime_error("E0408", "TcpPool handle is invalid."));
        };
        for stream in pool.idle {
            let _ = stream.shutdown(Shutdown::Both);
        }
        Ok(Value::Void)
    }
}

fn tcp_handle_from_value(value: &Value, expected_book: &str) -> Result<HeapHandle, RuntimeError> {
    match value {
        Value::Object { book, handle } if book == expected_book => Ok(*handle),
        _ => Err(runtime_error(
            "E0400",
            format!("Expected {expected_book} value."),
        )),
    }
}

fn tcp_pool_handle_from_value(value: &Value) -> Result<HeapHandle, RuntimeError> {
    match value {
        Value::Object { book, handle } if book == "TcpPool" => Ok(*handle),
        _ => Err(runtime_error("E0400", "Expected TcpPool value.")),
    }
}
