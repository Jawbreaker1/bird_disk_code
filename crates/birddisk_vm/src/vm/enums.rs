use super::*;
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(super) fn eval_enum_constructor(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let Some((enum_name, variant_name)) = name.split_once("::") else {
            return Ok(None);
        };
        if self.functions.contains_key(name) {
            return Ok(None);
        }
        if variant_name.contains("::") {
            return Ok(None);
        }
        if self.lookup(enum_name).is_some() {
            return Ok(None);
        }
        let Some(enum_info) = self.enums.get(enum_name) else {
            return Ok(None);
        };
        let Some(variant) = enum_info.variants.get(variant_name) else {
            return Err(runtime_error(
                "E0400",
                format!("Unknown enum variant '{name}' at runtime."),
            ));
        };

        let expected_args = if variant.payload.is_some() { 1 } else { 0 };
        if args.len() != expected_args {
            return Err(runtime_error(
                "E0400",
                format!(
                    "Wrong number of arguments for '{name}': expected {}, got {}.",
                    expected_args,
                    args.len()
                ),
            ));
        }

        let (payload_kind, payload_len, payload_bytes) = if let Some(payload_ty) = &variant.payload
        {
            let Some(arg) = args.first() else {
                return Err(runtime_error("E0400", "Missing enum payload value."));
            };
            let value = coerce_value(arg.clone(), payload_ty)?;
            let kind = elem_kind_for_type(payload_ty)?;
            let bytes = self.encode_enum_payload(&value, payload_ty)?;
            (kind as u32, 8usize, bytes)
        } else {
            (0u32, 0usize, Vec::new())
        };

        let handle = self.heap.alloc_enum(
            enum_info.id,
            variant.id,
            payload_kind,
            payload_len,
        );
        if !payload_bytes.is_empty() {
            let payload = self.heap.payload_mut(handle);
            payload[..payload_bytes.len()].copy_from_slice(&payload_bytes);
        }
        Ok(Some(Value::Enum {
            handle,
            name: enum_name.to_string(),
        }))
    }

    fn encode_enum_payload(&self, value: &Value, ty: &Type) -> Result<Vec<u8>, RuntimeError> {
        match (ty, value) {
            (Type::I64, Value::I64(value)) => Ok(value.to_le_bytes().to_vec()),
            (Type::F64, Value::F64(value)) => Ok(value.to_le_bytes().to_vec()),
            (Type::Bool, Value::Bool(value)) => Ok(vec![*value as u8]),
            (Type::U8, Value::U8(value)) => Ok(vec![*value]),
            (Type::String, Value::String(handle)) => {
                Ok((handle.as_u32() as u64).to_le_bytes().to_vec())
            }
            (Type::Array(_), Value::Array { handle, .. }) => {
                Ok((handle.as_u32() as u64).to_le_bytes().to_vec())
            }
            (Type::Book(name), Value::Object { handle, book }) => {
                if name != book {
                    return Err(runtime_error("E0400", "Enum payload type mismatch."));
                }
                Ok((handle.as_u32() as u64).to_le_bytes().to_vec())
            }
            (Type::Book(name), Value::Enum { handle, name: enum_name }) => {
                if name != enum_name {
                    return Err(runtime_error("E0400", "Enum payload type mismatch."));
                }
                Ok((handle.as_u32() as u64).to_le_bytes().to_vec())
            }
            _ => Err(runtime_error("E0400", "Enum payload type mismatch.")),
        }
    }

    pub(super) fn read_enum_payload(
        &self,
        handle: HeapHandle,
        ty: &Type,
    ) -> Result<Value, RuntimeError> {
        let payload = self.heap.payload(handle);
        match ty {
            Type::I64 => {
                let bytes = payload
                    .get(0..8)
                    .ok_or_else(|| runtime_error("E0400", "Enum payload missing."))?;
                Ok(Value::I64(i64::from_le_bytes(
                    bytes.try_into().unwrap(),
                )))
            }
            Type::F64 => {
                let bytes = payload
                    .get(0..8)
                    .ok_or_else(|| runtime_error("E0400", "Enum payload missing."))?;
                Ok(Value::F64(f64::from_le_bytes(
                    bytes.try_into().unwrap(),
                )))
            }
            Type::Bool => {
                let byte = *payload.get(0).unwrap_or(&0);
                Ok(Value::Bool(byte != 0))
            }
            Type::U8 => {
                let byte = *payload.get(0).unwrap_or(&0);
                Ok(Value::U8(byte))
            }
            Type::String | Type::Array(_) | Type::Book(_) => {
                let bytes = payload
                    .get(0..8)
                    .ok_or_else(|| runtime_error("E0400", "Enum payload missing."))?;
                let raw = u64::from_le_bytes(bytes.try_into().unwrap());
                self.value_from_handle(HeapHandle::from_u32(raw as u32), ty)
            }
            Type::Void => Err(runtime_error(
                "E0400",
                "Enum payload cannot be void.",
            )),
        }
    }
}
