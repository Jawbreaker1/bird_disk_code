use birddisk_core::runtime as abi;
use birddisk_core::TraceFrame;
use std::cell::RefCell;
use std::collections::VecDeque;
use std::io::{BufRead, Write};
use std::time::Instant;

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
    Enum = abi::HEAP_KIND_ENUM as u8,
    Free = abi::HEAP_KIND_FREE as u8,
}

#[repr(u8)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ElemKind {
    I64 = abi::ARRAY_KIND_I64 as u8,
    Bool = abi::ARRAY_KIND_BOOL as u8,
    U8 = abi::ARRAY_KIND_U8 as u8,
    Ref = abi::ARRAY_KIND_REF as u8,
    F64 = abi::ARRAY_KIND_F64 as u8,
}

#[derive(Debug, Clone)]
pub struct NativeTrap {
    pub code: &'static str,
    pub message: String,
    pub(crate) message_handle: Option<HeapHandle>,
    pub trace: Vec<TraceFrame>,
}

pub(crate) fn set_error(
    rt: &Runtime,
    code: &'static str,
    message: impl Into<String>,
    message_handle: Option<HeapHandle>,
) {
    let mut error = rt.error.borrow_mut();
    if error.is_none() {
        *error = Some(NativeTrap {
            code,
            message: message.into(),
            message_handle,
            trace: rt.trace_snapshot(),
        });
    }
}

pub(crate) fn runtime_error(rt: &Runtime, message: &'static str) {
    set_error(rt, "E0400", message, None);
}

pub(crate) fn array_oob_error(rt: &Runtime) {
    set_error(rt, "E0403", "Array index out of bounds.", None);
}

pub(crate) fn oom_error(rt: &Runtime) {
    runtime_error(rt, "Out of memory during allocation.");
}

pub(crate) fn invalid_heap_error(rt: &Runtime) {
    runtime_error(rt, "Invalid heap header.");
}

pub(crate) fn throw_error(rt: &Runtime, handle: HeapHandle, message: String) {
    set_error(rt, "E0404", message, Some(handle));
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
            value if value == abi::HEAP_KIND_ENUM as u8 => HeapKind::Enum,
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

#[derive(Debug, Default, Clone)]
pub(crate) struct TraceStack {
    frames: Vec<usize>,
}

impl TraceStack {
    pub(crate) fn new() -> Self {
        Self { frames: Vec::new() }
    }

    pub(crate) fn push(&mut self, id: usize) -> bool {
        if self.frames.try_reserve_exact(1).is_err() {
            return false;
        }
        self.frames.push(id);
        true
    }

    pub(crate) fn pop(&mut self) {
        self.frames.pop();
    }

    pub(crate) fn clear(&mut self) {
        self.frames.clear();
    }

    pub(crate) fn snapshot(&self, table: &[TraceFrame]) -> Vec<TraceFrame> {
        let mut trace = Vec::new();
        for id in self.frames.iter().rev() {
            if let Some(frame) = table.get(*id) {
                trace.push(frame.clone());
            }
        }
        trace
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
pub(crate) struct HeapObject {
    header: HeapHeader,
    payload: Vec<u8>,
}

#[derive(Debug, Default)]
pub(crate) struct Heap {
    objects: Vec<HeapObject>,
    stats: HeapStats,
}

#[derive(Debug)]
pub struct Runtime {
    heap: Heap,
    pub(crate) roots: RootStack,
    pub(crate) trace: TraceStack,
    pub(crate) args: Vec<String>,
    input: VecDeque<String>,
    output: String,
    layout: Vec<Vec<usize>>,
    pub(crate) trace_frames: Vec<TraceFrame>,
    gc_threshold: usize,
    error: RefCell<Option<NativeTrap>>,
    start_time: Instant,
    stdin_fallback: bool,
    stdout_live: bool,
    pub(crate) rand_state: u64,
}

impl Runtime {
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            roots: RootStack::new(),
            trace: TraceStack::new(),
            args: Vec::new(),
            input: VecDeque::new(),
            output: String::new(),
            layout: Vec::new(),
            trace_frames: Vec::new(),
            gc_threshold: usize::MAX,
            error: RefCell::new(None),
            start_time: Instant::now(),
            stdin_fallback: false,
            stdout_live: false,
            rand_state: 0x9E37_79B9_7F4A_7C15,
        }
    }

    pub(crate) fn heap_mut(&mut self) -> &mut Heap {
        &mut self.heap
    }

    fn heap_ref(&self) -> &Heap {
        &self.heap
    }

    pub fn set_layout(&mut self, layout: Vec<Vec<usize>>) {
        self.layout = layout;
    }

    pub fn set_trace(&mut self, frames: Vec<TraceFrame>) {
        self.trace_frames = frames;
        self.trace.clear();
    }

    pub fn set_input(&mut self, input: &str) {
        self.input = split_lines(input);
    }

    pub fn set_stdin_fallback(&mut self, enabled: bool) {
        self.stdin_fallback = enabled;
    }

    pub fn set_stdout_live(&mut self, enabled: bool) {
        self.stdout_live = enabled;
    }

    pub fn set_args(&mut self, args: &[String]) {
        self.args = args.to_vec();
    }

    pub fn take_output(&mut self) -> String {
        std::mem::take(&mut self.output)
    }

    pub fn has_error(&self) -> bool {
        self.error.borrow().is_some()
    }

    pub fn error_is_throw(&self) -> bool {
        self.error
            .borrow()
            .as_ref()
            .map(|trap| trap.code == "E0404")
            .unwrap_or(false)
    }

    pub(crate) fn error_message_handle(&self) -> Option<HeapHandle> {
        self.error
            .borrow()
            .as_ref()
            .and_then(|trap| trap.message_handle)
    }

    pub fn take_error(&self) -> Option<NativeTrap> {
        self.error.borrow_mut().take()
    }

    pub fn clear_error(&self) {
        self.error.borrow_mut().take();
    }

    fn trace_snapshot(&self) -> Vec<TraceFrame> {
        self.trace.snapshot(&self.trace_frames)
    }

    pub(crate) fn push_output(&mut self, value: &str) {
        if self.stdout_live {
            print!("{value}");
            let _ = std::io::stdout().flush();
        }
        self.output.push_str(value);
    }

    pub(crate) fn read_line(&mut self) -> String {
        if let Some(line) = self.input.pop_front() {
            return line;
        }
        if !self.stdin_fallback {
            return String::new();
        }
        let mut buf = String::new();
        let stdin = std::io::stdin();
        let _ = stdin.lock().read_line(&mut buf);
        trim_line_end(buf)
    }

    pub(crate) fn now_ms(&self) -> i64 {
        let elapsed = self.start_time.elapsed().as_millis();
        i64::try_from(elapsed).unwrap_or(i64::MAX)
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

    pub(crate) fn alloc_enum(
        &mut self,
        enum_id: u32,
        variant_id: u32,
        payload_kind: u32,
        payload_len: usize,
    ) -> Option<HeapHandle> {
        if payload_len > u32::MAX as usize {
            return None;
        }
        let header = HeapHeader::new(HeapKind::Enum, enum_id, variant_id, payload_kind);
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
            HeapKind::Enum => {
                if obj.header.aux == ElemKind::Ref as u32 {
                    if let Some(handle) = read_handle(&obj.payload, 0) {
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

pub(crate) fn runtime_mut<'a>(ptr: *mut Runtime) -> &'a mut Runtime {
    unsafe { &mut *ptr }
}

pub(crate) fn runtime_ref<'a>(ptr: *mut Runtime) -> &'a Runtime {
    unsafe { &*ptr }
}

pub(crate) fn heap_handle(rt: &Runtime, value: u64) -> Option<HeapHandle> {
    if value > u32::MAX as u64 {
        invalid_heap_error(rt);
        return None;
    }
    Some(HeapHandle::from_u32(value as u32))
}

pub(crate) fn heap_object<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a HeapObject> {
    let idx = handle.as_u32() as usize;
    let len = rt.heap_ref().objects.len();
    if idx >= len {
        invalid_heap_error(rt);
        return None;
    }
    Some(&rt.heap_ref().objects[idx])
}

pub(crate) fn heap_object_mut<'a>(rt: &'a mut Runtime, handle: HeapHandle) -> Option<&'a mut HeapObject> {
    let idx = handle.as_u32() as usize;
    let len = rt.heap_ref().objects.len();
    if idx >= len {
        invalid_heap_error(rt);
        return None;
    }
    let heap = rt.heap_mut();
    Some(&mut heap.objects[idx])
}

pub(crate) fn heap_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    heap_object(rt, handle).map(|obj| obj.header)
}

pub(crate) fn heap_payload<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a [u8]> {
    heap_object(rt, handle).map(|obj| obj.payload.as_slice())
}

pub(crate) fn heap_payload_mut<'a>(rt: &'a mut Runtime, handle: HeapHandle) -> Option<&'a mut [u8]> {
    heap_object_mut(rt, handle).map(|obj| obj.payload.as_mut_slice())
}

pub(crate) fn elem_size(kind: ElemKind) -> usize {
    match kind {
        ElemKind::I64 => 8,
        ElemKind::F64 => 8,
        ElemKind::Bool => 1,
        ElemKind::U8 => 1,
        ElemKind::Ref => 8,
    }
}

pub(crate) fn parse_elem_kind(rt: &Runtime, value: u64) -> Option<ElemKind> {
    match value as u32 {
        abi::ARRAY_KIND_I64 => Some(ElemKind::I64),
        abi::ARRAY_KIND_F64 => Some(ElemKind::F64),
        abi::ARRAY_KIND_BOOL => Some(ElemKind::Bool),
        abi::ARRAY_KIND_U8 => Some(ElemKind::U8),
        abi::ARRAY_KIND_REF => Some(ElemKind::Ref),
        _ => {
            invalid_heap_error(rt);
            None
        }
    }
}

pub(crate) fn array_index(rt: &Runtime, len: usize, index: i64) -> Option<usize> {
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

pub(crate) fn array_header(rt: &Runtime, handle: HeapHandle, expected: ElemKind) -> Option<HeapHeader> {
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

pub(crate) fn string_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::String {
        runtime_error(rt, "Expected string handle.");
        return None;
    }
    Some(header)
}

pub(crate) fn string_bytes_slice<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a [u8]> {
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

pub(crate) fn bytes_header(rt: &Runtime, handle: HeapHandle) -> Option<HeapHeader> {
    let header = heap_header(rt, handle)?;
    if header.kind() != HeapKind::Array || header.aux != ElemKind::U8 as u32 {
        runtime_error(rt, "std::bytes expects u8 array.");
        return None;
    }
    Some(header)
}

pub(crate) fn bytes_slice<'a>(rt: &'a Runtime, handle: HeapHandle) -> Option<&'a [u8]> {
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

pub(crate) fn alloc_string_from_bytes(rt: &mut Runtime, bytes: &[u8]) -> Option<HeapHandle> {
    let handle = rt.heap_mut().alloc_string(bytes.len())?;
    let payload = heap_payload_mut(rt, handle)?;
    payload[..bytes.len()].copy_from_slice(bytes);
    Some(handle)
}

const GC_MIN_THRESHOLD: usize = 1024 * 64;

pub(crate) fn maybe_collect(rt: &mut Runtime) {
    let stats = rt.heap_ref().stats();
    if stats.bytes_in_use < rt.gc_threshold {
        return;
    }
    let mut roots = rt.roots.clone();
    if let Some(handle) = rt.error_message_handle() {
        if let Some(base) = roots.push_frame(1) {
            roots.set_slot(base, RootValue::Ptr(handle));
        }
    }
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

pub(crate) fn split_lines(input: &str) -> VecDeque<String> {
    if input.is_empty() {
        return VecDeque::new();
    }
    input
        .split('\n')
        .map(|line| strip_cr(line).to_string())
        .collect()
}

pub(crate) fn strip_cr(line: &str) -> &str {
    line.strip_suffix('\r').unwrap_or(line)
}

pub(crate) fn trim_line_end(mut line: String) -> String {
    if line.ends_with('\n') {
        line.pop();
    }
    if line.ends_with('\r') {
        line.pop();
    }
    line
}
