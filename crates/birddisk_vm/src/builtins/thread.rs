use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::Value;
use crate::vm::{Vm, VmOptions};

impl<'a> Vm<'a> {
    pub(crate) fn eval_thread_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        match name {
            "std::thread::spawn" => self.eval_thread_spawn(args).map(Some),
            "std::thread::join" => self.eval_thread_join(args).map(Some),
            _ => Ok(None),
        }
    }

    fn eval_thread_spawn(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.is_empty() {
            return Err(runtime_error(
                "E0400",
                "std::thread::spawn expects at least 1 argument",
            ));
        }
        let entry_name = match args[0] {
            Value::String(handle) => self.string_text(handle)?,
            _ => {
                return Err(runtime_error(
                    "E0400",
                    "std::thread::spawn entry must be a string value.",
                ))
            }
        };

        if self.function_by_name(&entry_name).is_none() {
            return Err(runtime_error(
                "E0400",
                format!("Unknown thread entry rule '{entry_name}'."),
            ));
        }

        let thread = self.alloc_object("Thread")?;
        let handle = match &thread {
            Value::Object { handle, book } if book == "Thread" => handle,
            _ => return Err(runtime_error("E0400", "Failed to allocate thread handle.")),
        };
        if self.is_deterministic() {
            self.register_thread_running(*handle);
            self.enqueue_thread_job(*handle, entry_name, args[1..].to_vec());
            return Ok(thread);
        }

        let mut host_args = Vec::with_capacity(args.len().saturating_sub(1));
        let mut host_spawn_supported = true;
        for arg in &args[1..] {
            match arg {
                Value::I64(value) => host_args.push(*value),
                _ => {
                    host_spawn_supported = false;
                    break;
                }
            }
        }
        if host_spawn_supported {
            let program = self.program_clone();
            let entry = entry_name.clone();
            let join = std::thread::spawn(move || -> Result<i64, RuntimeError> {
                let mut vm = Vm::new(&program, "", &[], VmOptions::default());
                let function = vm.function_by_name(&entry).ok_or_else(|| {
                    runtime_error("E0400", format!("Unknown thread entry rule '{entry}'."))
                })?;
                let call_args: Vec<Value> = host_args.into_iter().map(Value::I64).collect();
                let result = vm.eval_function(function, &call_args)?;
                match result {
                    Value::I64(value) => Ok(value),
                    _ => Err(runtime_error("E0400", "Thread entry rule must return i64.")),
                }
            });
            self.register_thread_running_host(*handle, join);
            return Ok(thread);
        }

        let function = self.function_by_name(&entry_name).ok_or_else(|| {
            runtime_error(
                "E0400",
                format!("Unknown thread entry rule '{entry_name}'."),
            )
        })?;
        let result = self.eval_function(function, &args[1..])?;
        let result_i64 = match result {
            Value::I64(value) => value,
            _ => return Err(runtime_error("E0400", "Thread entry rule must return i64.")),
        };
        self.register_thread(*handle, result_i64);
        Ok(thread)
    }

    fn eval_thread_join(&mut self, args: &[Value]) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(runtime_error(
                "E0400",
                format!("std::thread::join expects 1 argument, got {}.", args.len()),
            ));
        }
        let handle = match args[0] {
            Value::Object { handle, ref book } if book == "Thread" => handle,
            _ => {
                return Err(runtime_error(
                    "E0405",
                    "std::thread::join expects a Thread handle.",
                ))
            }
        };
        let result = self.join_thread(handle)?;
        Ok(Value::I64(result))
    }
}
