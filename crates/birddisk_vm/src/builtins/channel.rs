use crate::runtime_error::{runtime_error, RuntimeError};
use crate::value::{coerce_value, Value};
use crate::vm::{ChannelKind, ChannelValue, Vm};
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(crate) fn eval_channel_builtin(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(kind) = ChannelKind::from_ctor(name) else {
            return Ok(None);
        };
        if !args.is_empty() {
            return Err(runtime_error(
                "E0400",
                format!("{name} expects 0 arguments"),
            ));
        }
        let value = self.alloc_channel(kind)?;
        Ok(Some(value))
    }

    pub(crate) fn eval_channel_method(
        &mut self,
        base: &Value,
        method: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let Value::Object { book, handle } = base else {
            return Ok(None);
        };
        let Some(kind) = ChannelKind::from_book(book) else {
            return Ok(None);
        };
        let handle = *handle;
        match method {
            "send" => {
                if args.len() != 1 {
                    return Err(runtime_error(
                        "E0400",
                        format!("{book}::send expects 1 argument"),
                    ));
                }
                let expected = kind.payload_type();
                let coerced = coerce_value(args[0].clone(), &expected)?;
                let payload = match coerced {
                    Value::I64(value) => ChannelValue::I64(value),
                    Value::Bool(value) => ChannelValue::Bool(value),
                    Value::F64(value) => ChannelValue::F64(value),
                    Value::U8(value) => ChannelValue::U8(value),
                    Value::String(handle) => ChannelValue::Ref(handle),
                    Value::Array { handle, .. } => ChannelValue::Ref(handle),
                    _ => {
                        return Err(runtime_error(
                            "E0400",
                            format!("{book}::send payload type mismatch"),
                        ))
                    }
                };
                let state = self.channel_state_mut(handle)?;
                if state.kind != kind {
                    return Err(runtime_error(
                        "E0400",
                        "Channel kind mismatch at runtime.",
                    ));
                }
                if state.closed {
                    Ok(Some(Value::Bool(false)))
                } else {
                    state.queue.push_back(payload);
                    Ok(Some(Value::Bool(true)))
                }
            }
            "recv" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        format!("{book}::recv expects 0 arguments"),
                    ));
                }
                let (closed, value) = {
                    let state = self.channel_state_mut(handle)?;
                    if state.kind != kind {
                        return Err(runtime_error(
                            "E0400",
                            "Channel kind mismatch at runtime.",
                        ));
                    }
                    if let Some(value) = state.queue.pop_front() {
                        (state.closed, Some(value))
                    } else {
                        (state.closed, None)
                    }
                };
                if let Some(value) = value {
                    let payload = match value {
                        ChannelValue::I64(value) => Value::I64(value),
                        ChannelValue::Bool(value) => Value::Bool(value),
                        ChannelValue::F64(value) => Value::F64(value),
                        ChannelValue::U8(value) => Value::U8(value),
                        ChannelValue::Ref(handle) => match kind.payload_type() {
                            Type::String => Value::String(handle),
                            Type::Array(inner) => Value::Array {
                                handle,
                                elem_type: *inner,
                            },
                            _ => {
                                return Err(runtime_error(
                                    "E0400",
                                    "Channel recv payload mismatch.",
                                ))
                            }
                        },
                    };
                    let result = self.alloc_enum_variant(kind.recv_name(), "Ok", Some(payload))?;
                    return Ok(Some(result));
                }
                if closed {
                    let result = self.alloc_enum_variant(kind.recv_name(), "Closed", None)?;
                    return Ok(Some(result));
                }
                Err(runtime_error("E0400", "Channel recv would block."))
            }
            "close" => {
                if !args.is_empty() {
                    return Err(runtime_error(
                        "E0400",
                        format!("{book}::close expects 0 arguments"),
                    ));
                }
                let state = self.channel_state_mut(handle)?;
                if state.kind != kind {
                    return Err(runtime_error(
                        "E0400",
                        "Channel kind mismatch at runtime.",
                    ));
                }
                state.closed = true;
                Ok(Some(Value::Void))
            }
            _ => Err(runtime_error(
                "E0400",
                format!("Unknown channel method '{method}' at runtime."),
            )),
        }
    }
}
