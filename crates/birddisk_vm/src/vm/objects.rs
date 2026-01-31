use super::*;
use birddisk_core::ast::Type;

impl<'a> Vm<'a> {
    pub(super) fn alloc_object(&mut self, book: &str) -> Result<Value, RuntimeError> {
        let Some(info) = self.books.get(book) else {
            return Err(runtime_error(
                "E0400",
                format!("Unknown book '{book}' at runtime."),
            ));
        };
        let book_id = info.id;
        let field_types = info.field_types.clone();
        self.maybe_collect();
        let handle = self.heap.alloc_object(book_id, field_types.len());
        let root_base = self.roots.push_frame(1);
        self.roots.set_slot(root_base, RootValue::Ptr(handle));
        for (index, field_ty) in field_types.iter().enumerate() {
            let value = match self.default_value(field_ty) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(1);
                    return Err(err);
                }
            };
            if let Err(err) = self.write_object_field(handle, index, field_ty, value) {
                self.roots.pop_frame(1);
                return Err(err);
            }
        }
        self.roots.pop_frame(1);
        Ok(Value::Object {
            handle,
            book: book.to_string(),
        })
    }

    pub(super) fn default_value(&mut self, ty: &Type) -> Result<Value, RuntimeError> {
        match ty {
            Type::I64 => Ok(Value::I64(0)),
            Type::F64 => Ok(Value::F64(0.0)),
            Type::Bool => Ok(Value::Bool(false)),
            Type::String => Ok(self.alloc_string("")),
            Type::U8 => Ok(Value::U8(0)),
            Type::Void => Err(runtime_error("E0400", "Void has no default value.")),
            Type::Array(inner) => self.alloc_array(&*inner.clone(), Vec::new()),
            Type::Book(name) => self.alloc_object(name),
        }
    }

    pub(crate) fn alloc_string(&mut self, text: &str) -> Value {
        let bytes = text.as_bytes();
        self.alloc_string_from_bytes(bytes)
    }

    pub(crate) fn alloc_string_from_bytes(&mut self, bytes: &[u8]) -> Value {
        self.maybe_collect();
        let handle = self.heap.alloc_string(bytes.len());
        let payload = self.heap.payload_mut(handle);
        if let Some(target) = payload.get_mut(..bytes.len()) {
            target.copy_from_slice(bytes);
        }
        Value::String(handle)
    }

    pub(crate) fn string_len(&self, handle: HeapHandle) -> Result<usize, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::String {
            return Err(runtime_error("E0400", "Expected string handle."));
        }
        Ok(header.len_or_size as usize)
    }

    pub(crate) fn string_bytes(&self, handle: HeapHandle) -> Result<Vec<u8>, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::String {
            return Err(runtime_error("E0400", "Expected string handle."));
        }
        let len = header.len_or_size as usize;
        let payload = self.heap.payload(handle);
        let bytes = payload.get(..len).ok_or_else(|| {
            runtime_error("E0400", "String payload out of bounds.")
        })?;
        Ok(bytes.to_vec())
    }

    pub(crate) fn string_text(&self, handle: HeapHandle) -> Result<String, RuntimeError> {
        let bytes = self.string_bytes(handle)?;
        String::from_utf8(bytes)
            .map_err(|_| runtime_error("E0400", "Invalid UTF-8 in string value."))
    }

    pub(super) fn read_object_field(
        &self,
        handle: HeapHandle,
        field_ty: &Type,
        index: usize,
    ) -> Result<Value, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Object {
            return Err(runtime_error("E0400", "Expected book handle."));
        }
        let field_count = header.len_or_size as usize;
        if index >= field_count {
            return Err(runtime_error("E0400", "Field index out of bounds."));
        }
        let payload = self.heap.payload(handle);
        let offset = index * 8;
        let bytes = payload.get(offset..offset + 8).ok_or_else(|| {
            runtime_error("E0400", "Object payload out of bounds.")
        })?;
        let raw = u64::from_le_bytes(bytes.try_into().unwrap());
        match field_ty {
            Type::I64 => Ok(Value::I64(i64::from_le_bytes(bytes.try_into().unwrap()))),
            Type::F64 => Ok(Value::F64(f64::from_le_bytes(bytes.try_into().unwrap()))),
            Type::Bool => Ok(Value::Bool(raw != 0)),
            Type::U8 => Ok(Value::U8(raw as u8)),
            Type::String | Type::Array(_) | Type::Book(_) => {
                self.value_from_handle(HeapHandle::from_u32(raw as u32), field_ty)
            }
            Type::Void => Err(runtime_error("E0400", "Void is not a valid field type.")),
        }
    }

    pub(super) fn write_object_field(
        &mut self,
        handle: HeapHandle,
        index: usize,
        field_ty: &Type,
        value: Value,
    ) -> Result<(), RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Object {
            return Err(runtime_error("E0400", "Expected book handle."));
        }
        let field_count = header.len_or_size as usize;
        if index >= field_count {
            return Err(runtime_error("E0400", "Field index out of bounds."));
        }
        let value = coerce_value(value, field_ty)?;
        let payload = self.heap.payload_mut(handle);
        let offset = index * 8;
        match value {
            Value::I64(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&value.to_le_bytes());
            }
            Value::F64(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&value.to_le_bytes());
            }
            Value::Bool(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(value as u64).to_le_bytes());
            }
            Value::U8(value) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(value as u64).to_le_bytes());
            }
            Value::String(handle) => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(handle.as_u32() as u64).to_le_bytes());
            }
            Value::Array { handle, .. }
            | Value::Object { handle, .. }
            | Value::Enum { handle, .. } => {
                let target = payload.get_mut(offset..offset + 8).ok_or_else(|| {
                    runtime_error("E0400", "Object payload out of bounds.")
                })?;
                target.copy_from_slice(&(handle.as_u32() as u64).to_le_bytes());
            }
            Value::Void => {
                return Err(runtime_error("E0400", "Void is not a valid field value."));
            }
        }
        Ok(())
    }

    pub(super) fn value_from_handle(
        &self,
        handle: HeapHandle,
        ty: &Type,
    ) -> Result<Value, RuntimeError> {
        match ty {
            Type::String => Ok(Value::String(handle)),
            Type::Array(inner) => Ok(Value::Array {
                handle,
                elem_type: (*inner.clone()),
            }),
            Type::Book(book) => {
                if self.enums.contains_key(book) {
                    let header = self.heap.header(handle);
                    if header.kind() != HeapKind::Enum {
                        return Err(runtime_error("E0400", "Expected enum value."));
                    }
                    Ok(Value::Enum {
                        handle,
                        name: book.clone(),
                    })
                } else {
                    Ok(Value::Object {
                        handle,
                        book: book.clone(),
                    })
                }
            }
            _ => Err(runtime_error("E0400", "Expected reference type.")),
        }
    }
}
