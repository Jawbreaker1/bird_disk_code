use super::*;
use birddisk_core::ast::{Expr, Type};

impl<'a> Vm<'a> {
    pub(super) fn eval_index_value(&mut self, index: &Expr) -> Result<i64, RuntimeError> {
        match self.eval_expr(index)? {
            Value::I64(value) => Ok(value),
            _ => Err(runtime_error("E0400", "index must be i64")),
        }
    }

    pub(super) fn eval_index_expr(
        &mut self,
        base: &Expr,
        index: &Expr,
    ) -> Result<Value, RuntimeError> {
        let index = self.eval_index_value(index)?;
        let value = self.eval_expr(base)?;
        match value {
            Value::Array { handle, elem_type } => self.read_array_elem(handle, &elem_type, index),
            _ => Err(runtime_error("E0400", "Indexing on non-array.")),
        }
    }

    pub(super) fn eval_array_new(
        &mut self,
        len: &Expr,
        elem_ty: &Type,
    ) -> Result<Value, RuntimeError> {
        let len = self.eval_index_value(len)?;
        if len < 0 {
            return Err(runtime_error("E0400", "array length must be >= 0"));
        }
        let len = len as usize;
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let elem_size = elem_size(elem_kind);
        self.maybe_collect();
        let handle = self.heap.alloc_array(elem_kind, len, elem_size);
        let root_base = self.roots.push_frame(1);
        self.roots.set_slot(root_base, RootValue::Ptr(handle));
        for index in 0..len {
            let value = match self.default_value(elem_ty) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(1);
                    return Err(err);
                }
            };
            if let Err(err) = self.write_array_elem(handle, elem_ty, index as i64, value) {
                self.roots.pop_frame(1);
                return Err(err);
            }
        }
        self.roots.pop_frame(1);
        Ok(Value::Array {
            handle,
            elem_type: elem_ty.clone(),
        })
    }

    pub(super) fn eval_array_literal(
        &mut self,
        elements: &[Expr],
        elem_ty: Option<&Type>,
    ) -> Result<Value, RuntimeError> {
        if elements.is_empty() {
            let Some(elem_ty) = elem_ty else {
                return Err(runtime_error(
                    "E0400",
                    "array literal requires explicit array type",
                ));
            };
            return self.alloc_array(elem_ty, Vec::new());
        }
        let frame_len = elements.len();
        let base = self.roots.push_frame(frame_len);
        let mut values = Vec::with_capacity(frame_len);
        if let Some(elem_ty) = elem_ty {
            for (index, element) in elements.iter().enumerate() {
                let value = match self.eval_expr(element) {
                    Ok(value) => value,
                    Err(err) => {
                        self.roots.pop_frame(frame_len);
                        return Err(err);
                    }
                };
                let value = match coerce_value(value, elem_ty) {
                    Ok(value) => value,
                    Err(err) => {
                        self.roots.pop_frame(frame_len);
                        return Err(err);
                    }
                };
                self.update_root_slot(base + index, &value);
                values.push(value);
            }
            let result = self.alloc_array(elem_ty, values);
            self.roots.pop_frame(frame_len);
            return result;
        }
        for (index, element) in elements.iter().enumerate() {
            let value = match self.eval_expr(element) {
                Ok(value) => value,
                Err(err) => {
                    self.roots.pop_frame(frame_len);
                    return Err(err);
                }
            };
            self.update_root_slot(base + index, &value);
            values.push(value);
        }
        let elem_type = value_type(&values[0])?;
        for value in values.iter().skip(1) {
            let ty = value_type(value)?;
            if ty != elem_type {
                self.roots.pop_frame(frame_len);
                return Err(runtime_error(
                    "E0400",
                    "array literal elements must have the same type",
                ));
            }
        }
        let result = self.alloc_array(&elem_type, values);
        self.roots.pop_frame(frame_len);
        result
    }

    pub(crate) fn alloc_u8_array(&mut self, bytes: &[u8]) -> Value {
        self.maybe_collect();
        let handle = self.heap.alloc_array(ElemKind::U8, bytes.len(), 1);
        let payload = self.heap.payload_mut(handle);
        if let Some(target) = payload.get_mut(..bytes.len()) {
            target.copy_from_slice(bytes);
        }
        Value::Array {
            handle,
            elem_type: Type::U8,
        }
    }

    pub(crate) fn read_u8_array(
        &self,
        handle: HeapHandle,
        elem_type: &Type,
    ) -> Result<Vec<u8>, RuntimeError> {
        if *elem_type != Type::U8 {
            return Err(runtime_error("E0400", "std::bytes expects u8 array."));
        }
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Array || header.aux != ElemKind::U8 as u32 {
            return Err(runtime_error("E0400", "std::bytes expects u8 array."));
        }
        let len = header.len_or_size as usize;
        let payload = self.heap.payload(handle);
        let bytes = payload
            .get(..len)
            .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
        Ok(bytes.to_vec())
    }

    pub(super) fn alloc_array(
        &mut self,
        elem_ty: &Type,
        elements: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        self.maybe_collect();
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let elem_size = elem_size(elem_kind);
        let handle = self.heap.alloc_array(elem_kind, elements.len(), elem_size);
        let root_base = self.roots.push_frame(1);
        self.roots.set_slot(root_base, RootValue::Ptr(handle));
        for (index, value) in elements.into_iter().enumerate() {
            if let Err(err) = self.write_array_elem(handle, elem_ty, index as i64, value) {
                self.roots.pop_frame(1);
                return Err(err);
            }
        }
        self.roots.pop_frame(1);
        Ok(Value::Array {
            handle,
            elem_type: elem_ty.clone(),
        })
    }

    pub(crate) fn array_len(&self, handle: HeapHandle) -> Result<usize, RuntimeError> {
        let header = self.heap.header(handle);
        if header.kind() != HeapKind::Array {
            return Err(runtime_error("E0400", "Expected array handle."));
        }
        Ok(header.len_or_size as usize)
    }

    fn read_array_elem(
        &self,
        handle: HeapHandle,
        elem_ty: &Type,
        index: i64,
    ) -> Result<Value, RuntimeError> {
        if index < 0 {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let index = index as usize;
        let len = self.array_len(handle)?;
        if index >= len {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let header = self.heap.header(handle);
        if header.aux != elem_kind as u32 {
            return Err(runtime_error("E0400", "Array element type mismatch."));
        }
        let payload = self.heap.payload(handle);
        let offset = index * elem_size(elem_kind);
        let value = match elem_kind {
            ElemKind::I64 => {
                let bytes = payload
                    .get(offset..offset + 8)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                Value::I64(i64::from_le_bytes(bytes.try_into().unwrap()))
            }
            ElemKind::F64 => {
                let bytes = payload
                    .get(offset..offset + 8)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                Value::F64(f64::from_le_bytes(bytes.try_into().unwrap()))
            }
            ElemKind::Bool => {
                let byte = *payload
                    .get(offset)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                Value::Bool(byte != 0)
            }
            ElemKind::U8 => {
                let byte = *payload
                    .get(offset)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                Value::U8(byte)
            }
            ElemKind::Ref => {
                let bytes = payload
                    .get(offset..offset + 8)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                let raw = u64::from_le_bytes(bytes.try_into().unwrap());
                let handle = HeapHandle::from_u32(raw as u32);
                self.value_from_handle(handle, elem_ty)?
            }
        };
        Ok(value)
    }

    pub(super) fn write_array_elem(
        &mut self,
        handle: HeapHandle,
        elem_ty: &Type,
        index: i64,
        value: Value,
    ) -> Result<(), RuntimeError> {
        if index < 0 {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let index = index as usize;
        let len = self.array_len(handle)?;
        if index >= len {
            return Err(runtime_error("E0403", "Array index out of bounds."));
        }
        let elem_kind = elem_kind_for_type(elem_ty)?;
        let header = self.heap.header(handle);
        if header.aux != elem_kind as u32 {
            return Err(runtime_error("E0400", "Array element type mismatch."));
        }
        let value = coerce_value(value, elem_ty)?;
        let payload = self.heap.payload_mut(handle);
        let offset = index * elem_size(elem_kind);
        match (elem_kind, value) {
            (ElemKind::I64, Value::I64(value)) => {
                let target = payload
                    .get_mut(offset..offset + 8)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                target.copy_from_slice(&value.to_le_bytes());
            }
            (ElemKind::F64, Value::F64(value)) => {
                let target = payload
                    .get_mut(offset..offset + 8)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                target.copy_from_slice(&value.to_le_bytes());
            }
            (ElemKind::Bool, Value::Bool(value)) => {
                let slot = payload
                    .get_mut(offset)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                *slot = if value { 1 } else { 0 };
            }
            (ElemKind::U8, Value::U8(value)) => {
                let slot = payload
                    .get_mut(offset)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                *slot = value;
            }
            (ElemKind::Ref, value) => {
                let handle = value
                    .heap_handle()
                    .ok_or_else(|| runtime_error("E0400", "Expected reference value."))?;
                let target = payload
                    .get_mut(offset..offset + 8)
                    .ok_or_else(|| runtime_error("E0400", "Array payload out of bounds."))?;
                target.copy_from_slice(&(handle.as_u32() as u64).to_le_bytes());
            }
            _ => {
                return Err(runtime_error("E0400", "Array element type mismatch."));
            }
        }
        Ok(())
    }

    pub(crate) fn alloc_env_args(&mut self) -> Result<Value, RuntimeError> {
        if self.args.is_empty() {
            return self.alloc_array(&Type::String, Vec::new());
        }
        let args = self.args.clone();
        let frame_len = args.len();
        let base = self.roots.push_frame(frame_len);
        let mut values = Vec::with_capacity(frame_len);
        for (index, arg) in args.iter().enumerate() {
            let value = self.alloc_string(arg);
            self.update_root_slot(base + index, &value);
            values.push(value);
        }
        let result = self.alloc_array(&Type::String, values);
        self.roots.pop_frame(frame_len);
        result
    }
}
