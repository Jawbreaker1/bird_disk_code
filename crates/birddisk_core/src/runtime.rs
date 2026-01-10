// Shared runtime ABI constants for VM/WASM/native backends.

pub const HEAP_KIND_STRING: u32 = 1;
pub const HEAP_KIND_ARRAY: u32 = 2;
pub const HEAP_KIND_OBJECT: u32 = 3;
pub const HEAP_KIND_FREE: u32 = 255;
pub const HEAP_KIND_SHIFT: u32 = 24;
pub const HEAP_TYPE_ID_MASK: u32 = 0x00FF_FFFF;

pub const HEAP_HEADER_SIZE: u32 = 16;
pub const HEAP_FLAGS_OFFSET: u32 = 4;
pub const HEAP_LEN_OFFSET: u32 = 8;
pub const HEAP_AUX_OFFSET: u32 = 12;

pub const ARRAY_HEADER_SIZE: u32 = HEAP_HEADER_SIZE;
pub const STRING_HEADER_SIZE: u32 = HEAP_HEADER_SIZE;
pub const OBJECT_HEADER_SIZE: u32 = HEAP_HEADER_SIZE;
pub const OBJECT_FIELD_SIZE: u32 = 8;

pub const ARRAY_KIND_I64: u32 = 1;
pub const ARRAY_KIND_BOOL: u32 = 2;
pub const ARRAY_KIND_U8: u32 = 3;
pub const ARRAY_KIND_REF: u32 = 4;
