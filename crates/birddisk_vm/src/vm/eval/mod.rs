mod expr;
mod ops;
mod stmt;

use super::*;
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(super) fn eval_main(&mut self) -> Result<i64, RuntimeError> {
        let main = self
            .functions
            .get("main")
            .ok_or_else(|| runtime_error("E0400", "missing main entry point in entry file"))?;
        let value = match self.eval_function(main, &[]) {
            Ok(value) => value,
            Err(err) => return Err(self.with_trace(err)),
        };
        match value {
            Value::I64(value) => Ok(value),
            Value::Bool(_)
            | Value::F64(_)
            | Value::String(_)
            | Value::U8(_)
            | Value::Void
            | Value::Array { .. }
            | Value::Object { .. }
            | Value::Enum { .. } => Err(runtime_error("E0400", "main must return i64")),
        }
    }

    pub(crate) fn eval_function(
        &mut self,
        function: &birddisk_core::ast::Function,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if function.params.len() != args.len() {
            return Err(runtime_error(
                "E0400",
                format!(
                    "Wrong number of arguments for '{}': expected {}, got {}.",
                    function.name,
                    function.params.len(),
                    args.len()
                ),
            ));
        }
        self.push_scope();
        for (param, value) in function.params.iter().zip(args.iter()) {
            let value = coerce_value(value.clone(), &param.ty)?;
            self.bind_local(param.name.clone(), value);
        }
        self.push_trace(function);
        let result = self.eval_block(&function.body);
        let result = match result {
            Ok(value) => Ok(value),
            Err(err) => Err(self.with_trace(err)),
        };
        self.pop_trace();
        self.pop_scope();
        match result {
            Ok(Some(value)) => Ok(value),
            Ok(None) => {
                if function.return_type == Type::Void {
                    Ok(Value::Void)
                } else {
                    Err(runtime_error("E0400", "missing return value."))
                }
            }
            Err(err) => Err(err),
        }
    }

    fn push_trace(&mut self, function: &birddisk_core::ast::Function) {
        self.trace.push(TraceFrame {
            function: function.name.clone(),
            file: function.file.clone(),
            span: function.span,
            source: function.source.clone(),
        });
    }

    fn pop_trace(&mut self) {
        self.trace.pop();
    }

    fn with_trace(&self, mut err: RuntimeError) -> RuntimeError {
        if err.trace.is_empty() {
            err.trace = self.trace.iter().cloned().rev().collect();
        }
        err
    }
}
