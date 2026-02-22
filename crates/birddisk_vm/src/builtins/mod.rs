mod bytes;
mod channel;
mod env;
mod fs;
mod io;
mod json;
mod net;
mod path;
mod profiler;
mod rand;
mod string;
mod test;
mod thread;
mod time;

use crate::runtime_error::RuntimeError;
use crate::value::Value;
use crate::vm::Vm;

impl<'a> Vm<'a> {
    pub(crate) fn eval_builtin_call(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        if let Some(value) = self.eval_string_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_bytes_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_json_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_channel_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_thread_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_io_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_time_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_profiler_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_rand_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_fs_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_path_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_env_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_net_builtin(name, args)? {
            return Ok(Some(value));
        }
        if let Some(value) = self.eval_test_builtin(name, args)? {
            return Ok(Some(value));
        }
        Ok(None)
    }
}
