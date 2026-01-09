#![allow(dead_code)]

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum HeapKind {
    String = 1,
    Array = 2,
    Object = 3,
    Free = 255,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ElemKind {
    I64 = 1,
    Bool = 2,
    U8 = 3,
    Ref = 4,
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
        let kind = (self.tag >> 24) as u8;
        match kind {
            1 => HeapKind::String,
            2 => HeapKind::Array,
            3 => HeapKind::Object,
            255 => HeapKind::Free,
            _ => HeapKind::Free,
        }
    }

    pub(crate) fn type_id(self) -> u32 {
        self.tag & 0x00FF_FFFF
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

#[derive(Debug, Clone)]
struct HeapObject {
    header: HeapHeader,
    payload: Vec<u8>,
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

#[derive(Debug, Default)]
pub(crate) struct Heap {
    objects: Vec<HeapObject>,
    stats: HeapStats,
}

impl Heap {
    pub(crate) fn new() -> Self {
        Self {
            objects: Vec::new(),
            stats: HeapStats::default(),
        }
    }

    pub(crate) fn alloc_string(&mut self, byte_len: usize) -> HeapHandle {
        let header = HeapHeader::new(HeapKind::String, 0, byte_len as u32, 0);
        self.alloc(header, byte_len)
    }

    pub(crate) fn alloc_array(
        &mut self,
        elem_kind: ElemKind,
        elem_count: usize,
        elem_size: usize,
    ) -> HeapHandle {
        let header = HeapHeader::new(
            HeapKind::Array,
            0,
            elem_count as u32,
            elem_kind as u32,
        );
        let payload_len = elem_count.saturating_mul(elem_size);
        self.alloc(header, payload_len)
    }

    pub(crate) fn alloc_object(&mut self, book_id: u32, field_count: usize) -> HeapHandle {
        let header = HeapHeader::new(
            HeapKind::Object,
            book_id,
            field_count as u32,
            0,
        );
        let payload_len = field_count.saturating_mul(8);
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

    fn alloc(&mut self, header: HeapHeader, payload_len: usize) -> HeapHandle {
        let payload_len = align_up(payload_len, 8);
        let payload = vec![0; payload_len];
        let id = self.objects.len() as u32;
        self.objects.push(HeapObject { header, payload });
        self.stats.alloc_count += 1;
        self.stats.bytes_allocated += payload_len;
        self.stats.bytes_in_use += payload_len;
        if self.stats.bytes_in_use > self.stats.peak_bytes_in_use {
            self.stats.peak_bytes_in_use = self.stats.bytes_in_use;
        }
        HeapHandle(id)
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
                        let offset = index * 8;
                        if let Some(handle) = read_handle(&obj.payload, offset) {
                            stack.push(handle);
                        }
                    }
                }
            }
            HeapKind::Object => {
                for field_index in layout.object_ref_fields(obj.header.type_id()) {
                    let offset = field_index * 8;
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
pub(crate) enum RootValue {
    Null,
    Ptr(HeapHandle),
}

pub(crate) trait HeapLayout {
    fn object_ref_fields(&self, type_id: u32) -> &[usize];
}

struct NoLayout;

impl HeapLayout for NoLayout {
    fn object_ref_fields(&self, _type_id: u32) -> &[usize] {
        &[]
    }
}

#[derive(Debug, Default)]
pub(crate) struct RootStack {
    slots: Vec<RootValue>,
    frames: Vec<usize>,
}

impl RootStack {
    pub(crate) fn new() -> Self {
        Self {
            slots: Vec::new(),
            frames: Vec::new(),
        }
    }

    pub(crate) fn push_frame(&mut self, slot_count: usize) -> usize {
        let base = self.slots.len();
        self.slots.resize(base + slot_count, RootValue::Null);
        self.frames.push(slot_count);
        base
    }

    pub(crate) fn extend_frame(&mut self, slot_count: usize) -> usize {
        let base = self.slots.len();
        self.slots.resize(base + slot_count, RootValue::Null);
        if let Some(current) = self.frames.last_mut() {
            *current += slot_count;
        } else {
            self.frames.push(slot_count);
        }
        base
    }

    pub(crate) fn set_slot(&mut self, slot: usize, value: RootValue) {
        if let Some(entry) = self.slots.get_mut(slot) {
            *entry = value;
        }
    }

    pub(crate) fn pop_frame(&mut self, slot_count: usize) {
        let Some(expected) = self.frames.pop() else {
            return;
        };
        debug_assert_eq!(expected, slot_count, "root frame size mismatch");
        let new_len = self.slots.len().saturating_sub(slot_count);
        self.slots.truncate(new_len);
    }

    pub(crate) fn pop_frame_auto(&mut self) {
        let Some(slot_count) = self.frames.pop() else {
            return;
        };
        let new_len = self.slots.len().saturating_sub(slot_count);
        self.slots.truncate(new_len);
    }

    pub(crate) fn slots(&self) -> &[RootValue] {
        &self.slots
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

fn free_object(obj: &mut HeapObject) -> usize {
    let freed = obj.payload.len();
    obj.header = HeapHeader::new(HeapKind::Free, 0, 0, 0);
    obj.payload.clear();
    freed
}

fn pack_tag(kind: HeapKind, type_id: u32) -> u32 {
    let kind = kind as u32;
    let type_id = type_id & 0x00FF_FFFF;
    (kind << 24) | type_id
}

fn align_up(value: usize, align: usize) -> usize {
    if align == 0 {
        return value;
    }
    let mask = align - 1;
    (value + mask) & !mask
}

fn read_handle(payload: &[u8], offset: usize) -> Option<HeapHandle> {
    let bytes = payload.get(offset..offset + 8)?;
    let raw = u64::from_le_bytes(bytes.try_into().ok()?);
    Some(HeapHandle::from_u32(raw as u32))
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
    fn heap_alloc_string_sets_len() {
        let mut heap = Heap::new();
        let handle = heap.alloc_string(12);
        let header = heap.header(handle);
        assert_eq!(header.kind(), HeapKind::String);
        assert_eq!(header.len_or_size, 12);
    }

    #[test]
    fn heap_alloc_array_sets_metadata() {
        let mut heap = Heap::new();
        let handle = heap.alloc_array(ElemKind::I64, 4, 8);
        let header = heap.header(handle);
        assert_eq!(header.kind(), HeapKind::Array);
        assert_eq!(header.len_or_size, 4);
        assert_eq!(header.aux, ElemKind::I64 as u32);
    }

    #[test]
    fn root_stack_push_and_pop() {
        let mut roots = RootStack::new();
        let base = roots.push_frame(2);
        roots.set_slot(base, RootValue::Ptr(HeapHandle(1)));
        roots.set_slot(base + 1, RootValue::Null);
        assert_eq!(roots.slots().len(), 2);
        roots.pop_frame(2);
        assert_eq!(roots.slots().len(), 0);
    }

    #[test]
    fn gc_marks_and_frees_unrooted() {
        let mut heap = Heap::new();
        let keep = heap.alloc_string(4);
        let drop1 = heap.alloc_array(ElemKind::I64, 2, 8);
        let drop2 = heap.alloc_object(7, 1);

        let mut roots = RootStack::new();
        let base = roots.push_frame(1);
        roots.set_slot(base, RootValue::Ptr(keep));

        let report = heap.gc(&roots);
        assert_eq!(report.marked, 1);
        assert_eq!(report.freed, 2);
        assert_eq!(heap.header(keep).kind(), HeapKind::String);
        assert_eq!(heap.header(drop1).kind(), HeapKind::Free);
        assert_eq!(heap.header(drop2).kind(), HeapKind::Free);
    }

    #[test]
    fn gc_updates_stats() {
        let mut heap = Heap::new();
        let handle = heap.alloc_string(8);
        let mut roots = RootStack::new();
        let base = roots.push_frame(1);
        roots.set_slot(base, RootValue::Ptr(handle));

        let stats_before = heap.stats();
        assert_eq!(stats_before.alloc_count, 1);
        assert!(stats_before.bytes_allocated >= 8);
        assert_eq!(stats_before.bytes_in_use, align_up(8, 8));
        assert_eq!(stats_before.peak_bytes_in_use, align_up(8, 8));

        heap.gc(&roots);
        let stats_after = heap.stats();
        assert_eq!(stats_after.gc_runs, 1);
        assert_eq!(stats_after.last_freed, 0);
        assert_eq!(stats_after.last_live, 1);
        assert_eq!(stats_after.last_freed_bytes, 0);
        assert_eq!(stats_after.last_live_bytes, align_up(8, 8));
    }

    #[test]
    fn gc_reclaims_bytes() {
        let mut heap = Heap::new();
        let keep = heap.alloc_string(5);
        let _drop = heap.alloc_object(1, 2);

        let mut roots = RootStack::new();
        let base = roots.push_frame(1);
        roots.set_slot(base, RootValue::Ptr(keep));

        let stats_before = heap.stats();
        let keep_bytes = align_up(5, 8);
        let drop_bytes = align_up(2 * 8, 8);
        assert_eq!(stats_before.bytes_in_use, keep_bytes + drop_bytes);
        assert_eq!(stats_before.peak_bytes_in_use, keep_bytes + drop_bytes);

        let report = heap.gc(&roots);
        assert_eq!(report.freed, 1);
        assert_eq!(report.freed_bytes, drop_bytes);
        assert_eq!(report.live_bytes, keep_bytes);

        let stats_after = heap.stats();
        assert_eq!(stats_after.bytes_in_use, keep_bytes);
        assert_eq!(stats_after.last_freed_bytes, drop_bytes);
        assert_eq!(stats_after.last_live_bytes, keep_bytes);
        assert_eq!(stats_after.peak_bytes_in_use, keep_bytes + drop_bytes);
    }

    #[test]
    fn gc_marks_ref_array_elements() {
        let mut heap = Heap::new();
        let target = heap.alloc_string(3);
        let array = heap.alloc_array(ElemKind::Ref, 1, 8);
        let payload = heap.payload_mut(array);
        payload[..8].copy_from_slice(&(target.as_u32() as u64).to_le_bytes());

        let mut roots = RootStack::new();
        let base = roots.push_frame(1);
        roots.set_slot(base, RootValue::Ptr(array));

        let report = heap.gc(&roots);
        assert_eq!(report.freed, 0);
        assert_eq!(heap.header(target).kind(), HeapKind::String);
    }

    #[test]
    fn gc_marks_object_ref_fields() {
        struct Layout {
            fields: Vec<Vec<usize>>,
        }

        impl HeapLayout for Layout {
            fn object_ref_fields(&self, type_id: u32) -> &[usize] {
                self.fields
                    .get(type_id as usize)
                    .map(|fields| fields.as_slice())
                    .unwrap_or(&[])
            }
        }

        let mut heap = Heap::new();
        let child = heap.alloc_string(2);
        let parent = heap.alloc_object(0, 2);
        let payload = heap.payload_mut(parent);
        payload[8..16].copy_from_slice(&(child.as_u32() as u64).to_le_bytes());

        let layout = Layout {
            fields: vec![vec![1]],
        };

        let mut roots = RootStack::new();
        let base = roots.push_frame(1);
        roots.set_slot(base, RootValue::Ptr(parent));

        let report = heap.gc_with_layout(&roots, &layout);
        assert_eq!(report.freed, 0);
        assert_eq!(heap.header(child).kind(), HeapKind::String);
    }
}
