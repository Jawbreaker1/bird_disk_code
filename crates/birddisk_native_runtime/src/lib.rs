#![allow(dead_code)]

use birddisk_core::runtime as abi;
use std::cell::RefCell;
use std::collections::VecDeque;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct HeapHandle(u32);

impl HeapHandle {
    pub(crate) fn from_u32(value: u32) -> Self {
        Self(value)
    }

    pub(crate) fn as_u32(self) -> u32 {
        self.0
    }
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeapKind {
    String = abi::HEAP_KIND_STRING as u8,
    Array = abi::HEAP_KIND_ARRAY as u8,
    Object = abi::HEAP_KIND_OBJECT as u8,
    Free = abi::HEAP_KIND_FREE as u8,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ElemKind {
    I64 = abi::ARRAY_KIND_I64 as u8,
    Bool = abi::ARRAY_KIND_BOOL as u8,
    U8 = abi::ARRAY_KIND_U8 as u8,
    Ref = abi::ARRAY_KIND_REF as u8,
}

#[derive(Debug, Clone)]
pub struct NativeTrap {
    pub code: &'static str,
    pub message: &'static str,
}

fn set_error(rt: &Runtime, code: &'static str, message: &'static str) {
    let mut error = rt.error.borrow_mut();
    if error.is_none() {
        *error = Some(NativeTrap { code, message });
    }
}

fn runtime_error(rt: &Runtime, message: &'static str) {
    set_error(rt, "E0400", message);
}

fn array_oob_error(rt: &Runtime) {
    set_error(rt, "E0403", "Array index out of bounds.");
}

fn oom_error(rt: &Runtime) {
    runtime_error(rt, "Out of memory during allocation.");
}

fn invalid_heap_error(rt: &Runtime) {
    runtime_error(rt, "Invalid heap header.");
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct HeapHeader {
    pub(crate) tag: u32,
    pub(crate) flags: u32,
    pub(crate) len_or_size: u32,
    pub(crate) aux: u32,
}

impl HeapHeader {
    pub(crate) fn new(kind: HeapKind, type_id: u32, len_or_size: u32, aux: u32) -> Self {
        let tag = pack_tag(kind, type_id);
        Self {
            tag,
            flags: 0,
            len_or_size,
            aux,
        }
    }

    pub(crate) fn kind(self) -> HeapKind {
        let kind = (self.tag >> abi::HEAP_KIND_SHIFT) as u8;
        match kind {
            value if value == abi::HEAP_KIND_STRING as u8 => HeapKind::String,
            value if value == abi::HEAP_KIND_ARRAY as u8 => HeapKind::Array,
            value if value == abi::HEAP_KIND_OBJECT as u8 => HeapKind::Object,
            value if value == abi::HEAP_KIND_FREE as u8 => HeapKind::Free,
            _ => HeapKind::Free,
        }
    }

    pub(crate) fn type_id(self) -> u32 {
        self.tag & abi::HEAP_TYPE_ID_MASK
    }

    pub(crate) fn is_marked(self) -> bool {
        self.flags & 1 == 1
    }

    pub(crate) fn set_marked(&mut self, marked: bool) {
        if marked {
            self.flags |= 1;
        } else {
            self.flags &= !1;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum RootValue {
    Null,
    Ptr(HeapHandle),
}

#[derive(Debug, Default, Clone)]
pub(crate) struct RootStack {
    frames: Vec<usize>,
    slots: Vec<RootValue>,
}

impl RootStack {
    pub(crate) fn new() -> Self {
        Self {
            frames: Vec::new(),
            slots: Vec::new(),
        }
    }

    pub(crate) fn push_frame(&mut self, slot_count: usize) -> Option<usize> {
        let base = self.slots.len();
        if self.frames.try_reserve_exact(1).is_err() {
            return None;
        }
        if self.slots.try_reserve_exact(slot_count).is_err() {
            return None;
        }
        self.frames.push(slot_count);
        self.slots.resize(base + slot_count, RootValue::Null);
        Some(base)
    }

    pub(crate) fn set_slot(&mut self, slot: usize, value: RootValue) {
        if let Some(target) = self.slots.get_mut(slot) {
            *target = value;
        }
    }

    pub(crate) fn pop_frame(&mut self, slot_count: usize) {
        let expected = self.frames.pop().unwrap_or(0);
        debug_assert_eq!(expected, slot_count, "root frame size mismatch");
        let new_len = self.slots.len().saturating_sub(slot_count);
        self.slots.truncate(new_len);
    }

    pub(crate) fn slots(&self) -> &[RootValue] {
        &self.slots
    }
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub(crate) struct HeapStats {
    pub(crate) alloc_count: usize,
    pub(crate) bytes_allocated: usize,
    pub(crate) bytes_in_use: usize,
    pub(crate) peak_bytes_in_use: usize,
    pub(crate) gc_runs: usize,
    pub(crate) last_freed: usize,
    pub(crate) last_live: usize,
    pub(crate) last_freed_bytes: usize,
    pub(crate) last_live_bytes: usize,
}

#[derive(Debug, Clone)]
struct HeapObject {
    header: HeapHeader,
    payload: Vec<u8>,
}

#[derive(Debug, Default)]
pub(crate) struct Heap {
    objects: Vec<HeapObject>,
    stats: HeapStats,
}

#[derive(Debug, Default)]
pub struct Runtime {
    heap: Heap,
    roots: RootStack,
    input: VecDeque<String>,
    output: String,
    layout: Vec<Vec<usize>>,
    gc_threshold: usize,
    error: RefCell<Option<NativeTrap>>,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            roots: RootStack::new(),
            input: VecDeque::new(),
            output: String::new(),
            layout: Vec::new(),
            gc_threshold: usize::MAX,
            error: RefCell::new(None),
        }
    }

    fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    fn heap_ref(&self) -> &Heap {
        &self.heap
    }

    pub fn set_layout(&mut self, layout: Vec<Vec<usize>>) {
        self.layout = layout;
    }

    pub fn set_input(&mut self, input: &str) {
        self.input = split_lines(input);
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    pub fn has_error(&self) -> bool {
        self.error.borrow().is_some()
    }

    pub fn take_error(&self) -> Option<NativeTrap> {
        self.error.borrow_mut().take()
    }

    fn push_output(&mut self, value: &str) {
        self.output.push_str(value);
    }

    fn read_line(&mut self) -> String {
        self.input.pop_front().unwrap_or_default()
    }
}

impl Heap {
    pub(crate) fn new() -> Self {
        Self {
            objects: Vec::new(),
            stats: HeapStats::default(),
        }
    }

    pub(crate) fn alloc_string(&mut self, byte_len: usize) -> Option<HeapHandle> {
        if byte_len > u32::MAX as usize {
            return None;
        }
        let header = HeapHeader::new(HeapKind::String, 0, byte_len as u32, 0);
        self.alloc(header, byte_len)
    }

    pub(crate) fn alloc_array(
        &mut self,
        elem_kind: ElemKind,
        elem_count: usize,
        elem_size: usize,
    ) -> Option<HeapHandle> {
        if elem_count > u32::MAX as usize {
            return None;
        }
        let header = HeapHeader::new(
            HeapKind::Array,
            0,
            elem_count as u32,
            elem_kind as u32,
        );
        let payload_len = elem_count
            .checked_mul(elem_size)
            ?;
        self.alloc(header, payload_len)
    }

    pub(crate) fn alloc_object(&mut self, book_id: u32, field_count: usize) -> Option<HeapHandle> {
        if field_count > u32::MAX as usize {
            return None;
        }
        let header = HeapHeader::new(
            HeapKind::Object,
            book_id,
            field_count as u32,
            0,
        );
        let payload_len = field_count
            .checked_mul(abi::OBJECT_FIELD_SIZE as usize)
            ?;
        self.alloc(header, payload_len)
    }

    pub(crate) fn header(&self, handle: HeapHandle) -> HeapHeader {
        self.objects[handle.0 as usize].header
    }

    pub(crate) fn payload(&self, handle: HeapHandle) -> &[u8] {
        &self.objects[handle.0 as usize].payload
    }

    pub(crate) fn payload_mut(&mut self, handle: HeapHandle) -> &mut [u8] {
        &mut self.objects[handle.0 as usize].payload
    }

    pub(crate) fn stats(&self) -> HeapStats {
        self.stats
    }

    pub(crate) fn gc(&mut self, roots: &RootStack) -> GcReport {
        self.gc_with_layout(roots, &NoLayout)
    }

    pub(crate) fn gc_with_layout<L: HeapLayout>(
        &mut self,
        roots: &RootStack,
        layout: &L,
    ) -> GcReport {
        let marked = self.mark_from_roots(roots.slots(), layout);
        let mut freed = 0;
        let mut live = 0;
        let mut freed_bytes = 0;
        for obj in &mut self.objects {
            if obj.header.kind() == HeapKind::Free {
                continue;
            }
            if obj.header.is_marked() {
                obj.header.set_marked(false);
                live += 1;
            } else {
                let reclaimed = free_object(obj);
                freed_bytes += reclaimed;
                self.stats.bytes_in_use = self.stats.bytes_in_use.saturating_sub(reclaimed);
                freed += 1;
            }
        }
        let live_bytes = self.stats.bytes_in_use;
        self.stats.gc_runs += 1;
        self.stats.last_freed = freed;
        self.stats.last_live = live;
        self.stats.last_freed_bytes = freed_bytes;
        self.stats.last_live_bytes = live_bytes;
        GcReport {
            marked,
            freed,
            live,
            freed_bytes,
            live_bytes,
        }
    }

    fn alloc(&mut self, header: HeapHeader, payload_len: usize) -> Option<HeapHandle> {
        let payload_len = align_up(payload_len, abi::OBJECT_FIELD_SIZE as usize)?;
        let mut payload = Vec::new();
        if payload.try_reserve_exact(payload_len).is_err() {
            return None;
        }
        payload.resize(payload_len, 0);
        let id = self.objects.len() as u32;
        self.objects.push(HeapObject { header, payload });
        self.stats.alloc_count += 1;
        self.stats.bytes_allocated += payload_len;
        self.stats.bytes_in_use += payload_len;
        if self.stats.bytes_in_use > self.stats.peak_bytes_in_use {
            self.stats.peak_bytes_in_use = self.stats.bytes_in_use;
        }
        Some(HeapHandle(id))
    }

    fn mark_from_roots<L: HeapLayout>(&mut self, roots: &[RootValue], layout: &L) -> usize {
        let mut marked = 0;
        let mut stack = Vec::new();
        for root in roots {
            if let RootValue::Ptr(handle) = root {
                stack.push(*handle);
            }
        }
        while let Some(handle) = stack.pop() {
            if self.mark_handle(handle) {
                marked += 1;
                self.push_children(handle, layout, &mut stack);
            }
        }
        marked
    }

    fn mark_handle(&mut self, handle: HeapHandle) -> bool {
        let Some(obj) = self.objects.get_mut(handle.0 as usize) else {
            return false;
        };
        if obj.header.kind() == HeapKind::Free || obj.header.is_marked() {
            return false;
        }
        obj.header.set_marked(true);
        true
    }

    fn push_children<L: HeapLayout>(
        &self,
        handle: HeapHandle,
        layout: &L,
        stack: &mut Vec<HeapHandle>,
    ) {
        let Some(obj) = self.objects.get(handle.as_u32() as usize) else {
            return;
        };
        match obj.header.kind() {
            HeapKind::Array => {
                if obj.header.aux == ElemKind::Ref as u32 {
                    let len = obj.header.len_or_size as usize;
                    for index in 0..len {
                        let offset = index * abi::OBJECT_FIELD_SIZE as usize;
                        if let Some(handle) = read_handle(&obj.payload, offset) {
                            stack.push(handle);
                        }
                    }
                }
            }
            HeapKind::Object => {
                for field_index in layout.object_ref_fields(obj.header.type_id()) {
                    let offset = field_index * abi::OBJECT_FIELD_SIZE as usize;
                    if let Some(handle) = read_handle(&obj.payload, offset) {
                        stack.push(handle);
                    }
                }
            }
            _ => {}
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct GcReport {
    pub(crate) marked: usize,
    pub(crate) freed: usize,
    pub(crate) live: usize,
    pub(crate) freed_bytes: usize,
    pub(crate) live_bytes: usize,
}

pub(crate) trait HeapLayout {
    fn object_ref_fields(&self, type_id: u32) -> &[usize];
}

pub(crate) struct NoLayout;

impl HeapLayout for NoLayout {
    fn object_ref_fields(&self, _type_id: u32) -> &[usize] {
        &[]
    }
}

fn free_object(obj: &mut HeapObject) -> usize {
    let freed = obj.payload.len();
    obj.header = HeapHeader::new(HeapKind::Free, 0, 0, 0);
    obj.payload.clear();
    freed
}

fn pack_tag(kind: HeapKind, type_id: u32) -> u32 {
    let kind = kind as u32;
    let type_id = type_id & abi::HEAP_TYPE_ID_MASK;
    (kind << abi::HEAP_KIND_SHIFT) | type_id
}

fn align_up(value: usize, align: usize) -> Option<usize> {
    if align == 0 {
        return Some(value);
    }
    let mask = align - 1;
    let added = value.checked_add(mask)?;
    Some(added & !mask)
}

fn read_handle(payload: &[u8], offset: usize) -> Option<HeapHandle> {
    let bytes = payload.get(offset..offset + 8)?;
    let raw = u64::from_le_bytes(bytes.try_into().ok()?);
    if raw > u32::MAX as u64 {
        return None;
    }
    Some(HeapHandle::from_u32(raw as u32))
}

fn runtime_mut<'a>(ptr: *mut Runtime) -> &'a mut Runtime {
    unsafe { &mut *ptr }
}

fn runtime_ref<'a>(ptr: *mut Runtime) -> &'a Runtime {
    unsafe { &*ptr }
}

fn heap_handle(rt: &Runtime, value: u64) -> Option<HeapHandle> {
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return None;
    }
    Some(HeapHandle::from_u32(value as u32))
}

fn heap_object<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a HeapObject> {
    let idx = handle.as_u32() as usize;
    let len = rt.heap_ref().objects.len();
    if idx >= len {
        invalid_heap_error(rt);
        return None;
    }
    Some(&rt.heap_ref().objects[idx])
}

fn heap_object_mut<'a>(rt: &'a mut Runtime, handle: HeapHandle) -> Option<&'a mut HeapObject> {
    let idx = handle.as_u32() as usize;
    let len = rt.heap_ref().objects.len();
    if idx >= len {
        invalid_heap_error(rt);
        return None;
    }
    let heap = rt.heap_mut();
    Some(&mut heap.objects[idx])
}

fn heap_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    heap_object(rt, handle).map(|obj| obj.header)
}

fn heap_payload<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a [u8]> {
    heap_object(rt, handle).map(|obj| obj.payload.as_slice())
}

fn heap_payload_mut<'a>(rt: &'a mut Runtime, handle: HeapHandle) -> Option<&'a mut [u8]> {
    heap_object_mut(rt, handle).map(|obj| obj.payload.as_mut_slice())
}

fn elem_size(kind: ElemKind) -> usize {
    match kind {
        ElemKind::I64 => 8,
        ElemKind::Bool => 1,
        ElemKind::U8 => 1,
        ElemKind::Ref => 8,
    }
}

fn parse_elem_kind(rt: &Runtime, value: u64) -> Option<ElemKind> {
    match value as u32 {
        abi::ARRAY_KIND_I64 => Some(ElemKind::I64),
        abi::ARRAY_KIND_BOOL => Some(ElemKind::Bool),
        abi::ARRAY_KIND_U8 => Some(ElemKind::U8),
        abi::ARRAY_KIND_REF => Some(ElemKind::Ref),
        _ => {
            invalid_heap_error(rt);
            None
        }
    }
}

fn array_index(rt: &Runtime, len: usize, index: i64) -> Option<usize> {
    if index < 0 {
        array_oob_error(rt);
        return None;
    }
    let index = index as usize;
    if index >= len {
        array_oob_error(rt);
        return None;
    }
    Some(index)
}

fn array_header(rt: &Runtime, handle: HeapHandle, expected: ElemKind) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Array {
        runtime_error(rt, "Expected array handle.");
        return None;
    }
    if header.aux != expected as u32 {
        runtime_error(rt, "Array element type mismatch.");
        return None;
    }
    Some(header)
}

fn string_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::String {
        runtime_error(rt, "Expected string handle.");
        return None;
    }
    Some(header)
}

fn string_bytes_slice<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a [u8]> {
    let header = string_header(rt, handle)?;
    let len = header.len_or_size as usize;
    let payload = heap_payload(rt, handle)?;
    match payload.get(..len) {
        Some(slice) => Some(slice),
        None => {
            runtime_error(rt, "String payload out of bounds.");
            None
        }
    }
}

fn bytes_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Array || header.aux != ElemKind::U8 as u32 {
        runtime_error(rt, "std::bytes expects u8 array.");
        return None;
    }
    Some(header)
}

fn bytes_slice<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a [u8]> {
    let header = bytes_header(rt, handle)?;
    let len = header.len_or_size as usize;
    let payload = heap_payload(rt, handle)?;
    match payload.get(..len) {
        Some(slice) => Some(slice),
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            None
        }
    }
}

fn alloc_string_from_bytes(rt: &mut Runtime, bytes: &[u8]) -> Option<HeapHandle> {
    let handle = rt.heap_mut().alloc_string(bytes.len())?;
    let payload = heap_payload_mut(rt, handle)?;
    payload[..bytes.len()].copy_from_slice(bytes);
    Some(handle)
}

const GC_MIN_THRESHOLD: usize = 1024 * 64;

fn maybe_collect(rt: &mut Runtime) {
    let stats = rt.heap_ref().stats();
    if stats.bytes_in_use < rt.gc_threshold {
        return;
    }
    let roots = rt.roots.clone();
    let layout_snapshot = rt.layout.clone();
    let layout = RuntimeLayout {
        ref_fields: &layout_snapshot,
    };
    let report = rt.heap_mut().gc_with_layout(&roots, &layout);
    let mut next = report.live_bytes.saturating_mul(2);
    if next < GC_MIN_THRESHOLD {
        next = GC_MIN_THRESHOLD;
    }
    rt.gc_threshold = next;
}

struct RuntimeLayout<'a> {
    ref_fields: &'a [Vec<usize>],
}

impl<'a> HeapLayout for RuntimeLayout<'a> {
    fn object_ref_fields(&self, type_id: u32) -> &[usize] {
        self.ref_fields
            .get(type_id as usize)
            .map(|items| items.as_slice())
            .unwrap_or(&[])
    }
}

fn split_lines(input: &str) -> VecDeque<String> {
    if input.is_empty() {
        return VecDeque::new();
    }
    input.split('\n').map(|line| line.to_string()).collect()
}

#[no_mangle]
pub extern "C-unwind" fn bd_root_push(rt: *mut Runtime, slots: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let slots = match usize::try_from(slots) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    match rt.roots.push_frame(slots) {
        Some(base) => base as u64,
        None => {
            oom_error(rt);
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_root_pop(rt: *mut Runtime, slots: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let slots = match usize::try_from(slots) {
        Ok(value) => value,
        Err(_) => {
            invalid_heap_error(rt);
            return;
        }
    };
    rt.roots.pop_frame(slots);
}

#[no_mangle]
pub extern "C-unwind" fn bd_root_set(rt: *mut Runtime, slot: u64, handle: u64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match usize::try_from(slot) {
        Ok(value) => value,
        Err(_) => {
            invalid_heap_error(rt);
            return;
        }
    };
    rt.roots.set_slot(slot, RootValue::Ptr(handle));
}

#[no_mangle]
pub extern "C-unwind" fn bd_has_error(rt: *mut Runtime) -> i64 {
    let rt = runtime_ref(rt);
    if rt.has_error() { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_string(rt: *mut Runtime, ptr: *const u8, len: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    let len = match usize::try_from(len) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    let handle = match rt.heap_mut().alloc_string(len) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = unsafe { std::slice::from_raw_parts(ptr, len) };
    payload[..len].copy_from_slice(bytes);
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_array(
    rt: *mut Runtime,
    elem_kind: u64,
    elem_size_arg: u64,
    len: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    let kind = match parse_elem_kind(rt, elem_kind) {
        Some(value) => value,
        None => return 0,
    };
    let expected_size = elem_size(kind);
    if elem_size_arg as usize != expected_size {
        invalid_heap_error(rt);
        return 0;
    }
    let len_i64 = len as i64;
    if len_i64 < 0 {
        runtime_error(rt, "Array length must be >= 0.");
        return 0;
    }
    let len = match usize::try_from(len_i64) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    let handle = match rt.heap_mut().alloc_array(kind, len, expected_size) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_alloc_object(
    rt: *mut Runtime,
    book_id: u64,
    field_count: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    maybe_collect(rt);
    let field_count = match usize::try_from(field_count) {
        Ok(value) => value,
        Err(_) => {
            oom_error(rt);
            return 0;
        }
    };
    if book_id > u32::MAX as u64 {
        invalid_heap_error(rt);
        return 0;
    }
    let handle = rt
        .heap_mut()
        .alloc_object(book_id as u32, field_count);
    let handle = match handle {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_i64(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::I64) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::I64);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    i64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_i64(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::I64) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::I64);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_bool(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::Bool) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::Bool);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let byte = match payload.get(offset) {
        Some(value) => *value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    if byte == 0 { 0 } else { 1 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_bool(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::Bool) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::Bool);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(offset) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    *slot = if value == 0 { 0 } else { 1 };
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_u8(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::U8) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::U8);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let byte = match payload.get(offset) {
        Some(value) => *value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    byte as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_u8(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if !(0..=u8::MAX as i64).contains(&value) {
        runtime_error(rt, "u8 value out of range.");
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::U8) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::U8);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let slot = match payload.get_mut(offset) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    *slot = value as u8;
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_get_ref(rt: *mut Runtime, handle: u64, index: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match array_header(rt, handle, ElemKind::Ref) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * elem_size(ElemKind::Ref);
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    raw
}

#[no_mangle]
pub extern "C-unwind" fn bd_array_set_ref(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: u64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match array_header(rt, handle, ElemKind::Ref) {
        Some(value) => value,
        None => return,
    };
    let idx = match array_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * elem_size(ElemKind::Ref);
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Array payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

fn object_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Object {
        runtime_error(rt, "Expected book handle.");
        return None;
    }
    Some(header)
}

fn object_index(rt: &Runtime, field_count: usize, index: i64) -> Option<usize> {
    if index < 0 {
        runtime_error(rt, "Field index out of bounds.");
        return None;
    }
    let index = index as usize;
    if index >= field_count {
        runtime_error(rt, "Field index out of bounds.");
        return None;
    }
    Some(index)
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_i64(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    i64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_i64(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_bool(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    if raw == 0 { 0 } else { 1 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_bool(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    let raw = if value == 0 { 0u64 } else { 1u64 };
    target.copy_from_slice(&raw.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_u8(rt: *mut Runtime, handle: u64, index: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    let raw = u64::from_le_bytes(bytes.try_into().unwrap());
    raw as u8 as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_u8(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: i64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if !(0..=u8::MAX as i64).contains(&value) {
        runtime_error(rt, "u8 value out of range.");
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&(value as u64).to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_get_ref(rt: *mut Runtime, handle: u64, index: i64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return 0,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match payload.get(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return 0;
        }
    };
    u64::from_le_bytes(bytes.try_into().unwrap())
}

#[no_mangle]
pub extern "C-unwind" fn bd_object_set_ref(
    rt: *mut Runtime,
    handle: u64,
    index: i64,
    value: u64,
) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let header = match object_header(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let idx = match object_index(rt, header.len_or_size as usize, index) {
        Some(value) => value,
        None => return,
    };
    let offset = idx * abi::OBJECT_FIELD_SIZE as usize;
    let payload = match heap_payload_mut(rt, handle) {
        Some(value) => value,
        None => return,
    };
    let target = match payload.get_mut(offset..offset + 8) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Object payload out of bounds.");
            return;
        }
    };
    target.copy_from_slice(&value.to_le_bytes());
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_len(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match string_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    header.len_or_size as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_concat(
    rt: *mut Runtime,
    left: u64,
    right: u64,
) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let left_handle = match heap_handle(rt, left) {
        Some(value) => value,
        None => return 0,
    };
    let right_handle = match heap_handle(rt, right) {
        Some(value) => value,
        None => return 0,
    };
    let combined = {
        let left_bytes = match string_bytes_slice(rt, left_handle) {
            Some(value) => value,
            None => return 0,
        };
        let right_bytes = match string_bytes_slice(rt, right_handle) {
            Some(value) => value,
            None => return 0,
        };
        let total_len = match left_bytes.len().checked_add(right_bytes.len()) {
            Some(value) => value,
            None => {
                oom_error(rt);
                return 0;
            }
        };
        let mut combined = Vec::new();
        if combined.try_reserve_exact(total_len).is_err() {
            oom_error(rt);
            return 0;
        }
        combined.extend_from_slice(left_bytes);
        combined.extend_from_slice(right_bytes);
        combined
    };
    let handle = match alloc_string_from_bytes(rt, &combined) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_eq(rt: *mut Runtime, left: u64, right: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let left_handle = match heap_handle(rt, left) {
        Some(value) => value,
        None => return 0,
    };
    let right_handle = match heap_handle(rt, right) {
        Some(value) => value,
        None => return 0,
    };
    let left_bytes = match string_bytes_slice(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right_bytes = match string_bytes_slice(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    if left_bytes == right_bytes { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_bytes(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value.to_vec(),
        None => return 0,
    };
    let array = match rt.heap_mut().alloc_array(ElemKind::U8, bytes.len(), 1) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    let payload = match heap_payload_mut(rt, array) {
        Some(value) => value,
        None => return 0,
    };
    payload[..bytes.len()].copy_from_slice(&bytes);
    array.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_from_bytes(rt: *mut Runtime, handle: u64) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match bytes_slice(rt, handle) {
        Some(value) => value.to_vec(),
        None => return 0,
    };
    if std::str::from_utf8(&bytes).is_err() {
        runtime_error(rt, "Invalid UTF-8 in std::string::from_bytes.");
        return 0;
    }
    let handle = match alloc_string_from_bytes(rt, &bytes) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_to_i64(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let bytes = match string_bytes_slice(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let text = match std::str::from_utf8(bytes) {
        Ok(value) => value,
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    match parse_string_i64(text) {
        Some(value) => value,
        None => {
            runtime_error(rt, "Invalid integer in std::string::to_i64.");
            0
        }
    }
}

#[no_mangle]
pub extern "C-unwind" fn bd_string_from_i64(rt: *mut Runtime, value: i64) -> u64 {
    let text = value.to_string();
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match alloc_string_from_bytes(rt, text.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_len(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let header = match bytes_header(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    header.len_or_size as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_bytes_eq(rt: *mut Runtime, left: u64, right: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let left_handle = match heap_handle(rt, left) {
        Some(value) => value,
        None => return 0,
    };
    let right_handle = match heap_handle(rt, right) {
        Some(value) => value,
        None => return 0,
    };
    let left_bytes = match bytes_slice(rt, left_handle) {
        Some(value) => value,
        None => return 0,
    };
    let right_bytes = match bytes_slice(rt, right_handle) {
        Some(value) => value,
        None => return 0,
    };
    if left_bytes == right_bytes { 1 } else { 0 }
}

#[no_mangle]
pub extern "C-unwind" fn bd_io_print(rt: *mut Runtime, handle: u64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let handle = match heap_handle(rt, handle) {
        Some(value) => value,
        None => return 0,
    };
    let text = match {
        let bytes = match string_bytes_slice(rt, handle) {
            Some(value) => value,
            None => return 0,
        };
        std::str::from_utf8(bytes)
    } {
        Ok(value) => value.to_string(),
        Err(_) => {
            runtime_error(rt, "Invalid UTF-8 in string value.");
            return 0;
        }
    };
    rt.push_output(&text);
    text.len() as i64
}

#[no_mangle]
pub extern "C-unwind" fn bd_io_read_line(rt: *mut Runtime) -> u64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    let line = rt.read_line();
    let handle = match alloc_string_from_bytes(rt, line.as_bytes()) {
        Some(value) => value,
        None => {
            oom_error(rt);
            return 0;
        }
    };
    handle.as_u32() as u64
}

fn parse_string_i64(text: &str) -> Option<i64> {
    if text.is_empty() {
        return None;
    }
    let bytes = text.as_bytes();
    let mut idx = 0;
    let mut sign: i128 = 1;
    if bytes[0] == b'-' {
        sign = -1;
        idx = 1;
        if idx == bytes.len() {
            return None;
        }
    }
    let mut value: i128 = 0;
    while idx < bytes.len() {
        let ch = bytes[idx];
        if !(b'0'..=b'9').contains(&ch) {
            return None;
        }
        value = value * 10 + (ch - b'0') as i128;
        idx += 1;
    }
    value *= sign;
    if value < i64::MIN as i128 || value > i64::MAX as i128 {
        return None;
    }
    Some(value as i64)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn heap_header_encodes_kind_and_type() {
        let header = HeapHeader::new(HeapKind::Object, 42, 3, 0);
        assert_eq!(header.kind(), HeapKind::Object);
        assert_eq!(header.type_id(), 42);
    }

    #[test]
    fn root_stack_tracks_frames() {
        let mut roots = RootStack::new();
        let base = roots.push_frame(2).expect("root frame allocation");
        roots.set_slot(base, RootValue::Null);
        roots.set_slot(base + 1, RootValue::Null);
        roots.pop_frame(2);
        assert!(roots.slots().is_empty());
    }

    #[test]
    fn gc_reclaims_unrooted_values() {
        let mut heap = Heap::new();
        let keep = heap.alloc_string(3).expect("alloc keep");
        let drop = heap.alloc_string(2).expect("alloc drop");

        let mut roots = RootStack::new();
        let base = roots.push_frame(1).expect("root frame");
        roots.set_slot(base, RootValue::Ptr(keep));

        let report = heap.gc(&roots);
        assert_eq!(report.freed, 1);
        assert_eq!(heap.header(drop).kind(), HeapKind::Free);
    }
}
