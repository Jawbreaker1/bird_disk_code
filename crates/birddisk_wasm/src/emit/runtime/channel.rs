use crate::emit::{
    WatEmitter, ARRAY_HEADER_SIZE, ARRAY_KIND_BOOL, ARRAY_KIND_F64, ARRAY_KIND_I64, ARRAY_KIND_REF,
    ARRAY_KIND_U8, HEAP_AUX_OFFSET, HEAP_FLAGS_OFFSET, HEAP_HEADER_SIZE, HEAP_KIND_ARRAY,
    HEAP_KIND_ENUM, HEAP_KIND_OBJECT, HEAP_KIND_SHIFT, HEAP_LEN_OFFSET, OBJECT_FIELD_SIZE,
    TRAP_CHANNEL_BLOCK,
};

const CHANNEL_FIELD_QUEUE: i32 = 0;
const CHANNEL_FIELD_CLOSED: i32 = 1;
const CHANNEL_FIELD_COUNT: i32 = 2;

#[derive(Clone, Copy)]
pub(crate) enum ChannelPayload {
    I64,
    F64,
    Bool,
    U8,
    Ref,
}

impl ChannelPayload {
    fn wat_param(self) -> &'static str {
        match self {
            ChannelPayload::I64 => "i64",
            ChannelPayload::F64 => "f64",
            ChannelPayload::Bool | ChannelPayload::U8 | ChannelPayload::Ref => "i32",
        }
    }

    fn elem_kind(self) -> i32 {
        match self {
            ChannelPayload::I64 => ARRAY_KIND_I64,
            ChannelPayload::F64 => ARRAY_KIND_F64,
            ChannelPayload::Bool => ARRAY_KIND_BOOL,
            ChannelPayload::U8 => ARRAY_KIND_U8,
            ChannelPayload::Ref => ARRAY_KIND_REF,
        }
    }

    fn elem_size(self) -> i32 {
        match self {
            ChannelPayload::I64 | ChannelPayload::F64 => 8,
            ChannelPayload::Bool | ChannelPayload::Ref => 4,
            ChannelPayload::U8 => 1,
        }
    }

    fn array_load(self) -> &'static str {
        match self {
            ChannelPayload::I64 => "i64.load",
            ChannelPayload::F64 => "f64.load",
            ChannelPayload::Bool | ChannelPayload::Ref => "i32.load",
            ChannelPayload::U8 => "i32.load8_u",
        }
    }

    fn array_store(self) -> &'static str {
        match self {
            ChannelPayload::I64 => "i64.store",
            ChannelPayload::F64 => "f64.store",
            ChannelPayload::Bool | ChannelPayload::Ref => "i32.store",
            ChannelPayload::U8 => "i32.store8",
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) struct ChannelSpec {
    pub(crate) name: &'static str,
    pub(crate) book_id: u32,
    pub(crate) enum_id: u32,
    pub(crate) ok_variant: u32,
    pub(crate) closed_variant: u32,
    pub(crate) payload: ChannelPayload,
}

pub(crate) fn emit_channel_runtime(emitter: &mut WatEmitter, specs: &[ChannelSpec]) {
    for spec in specs {
        emit_channel_ctor(emitter, spec);
        emit_channel_send(emitter, spec);
        emit_channel_recv(emitter, spec);
        emit_channel_close(emitter, spec);
    }
}

fn emit_channel_ctor(emitter: &mut WatEmitter, spec: &ChannelSpec) {
    let obj_size = OBJECT_FIELD_SIZE * CHANNEL_FIELD_COUNT + HEAP_HEADER_SIZE;
    let tag = (HEAP_KIND_OBJECT << HEAP_KIND_SHIFT) | spec.book_id as i32;
    let elem_kind = spec.payload.elem_kind();
    let queue_offset = OBJECT_FIELD_SIZE * CHANNEL_FIELD_QUEUE + HEAP_HEADER_SIZE;
    let closed_offset = OBJECT_FIELD_SIZE * CHANNEL_FIELD_CLOSED + HEAP_HEADER_SIZE;

    emitter.push_line(format!(
        "(func $bd_channel_{} (result i32) (local $obj i32) (local $queue i32) (local $root i32)",
        spec.name
    ));
    emitter.indent();
    emitter.push_line(format!("i32.const {obj_size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $obj");
    emitter.push_line("local.get $obj");
    emitter.push_line(format!("i32.const {tag}"));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $obj");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $obj");
    emitter.push_line(format!("i32.const {CHANNEL_FIELD_COUNT}"));
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $obj");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $root");
    emitter.push_line("local.get $root");
    emitter.push_line("local.get $obj");
    emitter.push_line("call $bd_root_set");

    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $queue");
    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $queue");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $queue");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.const {elem_kind}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("local.get $obj");
    emitter.push_line(format!("i32.const {queue_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $queue");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.store");

    emitter.push_line("local.get $obj");
    emitter.push_line(format!("i32.const {closed_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("i64.const 0");
    emitter.push_line("i64.store");

    emitter.push_line("i32.const 1");
    emitter.push_line("call $bd_root_pop");
    emitter.push_line("local.get $obj");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_channel_send(emitter: &mut WatEmitter, spec: &ChannelSpec) {
    let queue_offset = OBJECT_FIELD_SIZE * CHANNEL_FIELD_QUEUE + HEAP_HEADER_SIZE;
    let elem_size = spec.payload.elem_size();
    let elem_kind = spec.payload.elem_kind();
    let load = spec.payload.array_load();
    let store = spec.payload.array_store();
    let value_ty = spec.payload.wat_param();
    let root_slots = if matches!(spec.payload, ChannelPayload::Ref) {
        3
    } else {
        2
    };
    emitter.push_line(format!(
        "(func $bd_channel_send_{} (param $chan i32) (param $value {value_ty}) (result i32) (local $queue i32) (local $len i32) (local $new_len i32) (local $new i32) (local $i i32) (local $closed i32) (local $tmp {value_ty}) (local $root i32)",
        spec.name
    ));
    emitter.indent();

    emitter.push_line("local.get $chan");
    emitter.push_line(format!("i32.const {queue_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("i64.load");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("local.set $queue");
    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.load offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("i32.const 2");
    emitter.push_line("i32.and");
    emitter.push_line("local.set $closed");
    emitter.push_line("local.get $closed");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("i32.const 0");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {root_slots}"));
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $root");
    emitter.push_line("local.get $root");
    emitter.push_line("local.get $chan");
    emitter.push_line("call $bd_root_set");
    emitter.push_line("local.get $root");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $queue");
    emitter.push_line("call $bd_root_set");
    if matches!(spec.payload, ChannelPayload::Ref) {
        emitter.push_line("local.get $root");
        emitter.push_line("i32.const 2");
        emitter.push_line("i32.add");
        emitter.push_line("local.get $value");
        emitter.push_line("call $bd_root_set");
    }

    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $new_len");

    emitter.push_line("local.get $new_len");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $new");

    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $new");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $new");
    emitter.push_line("local.get $new_len");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {elem_kind}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("i32.const 0");
    emitter.push_line("local.set $i");
    emitter.push_line("block $copy_exit");
    emitter.indent();
    emitter.push_line("loop $copy_loop");
    emitter.indent();
    emitter.push_line("local.get $i");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.ge_u");
    emitter.push_line("br_if $copy_exit");

    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $i");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line(load);
    emitter.push_line("local.set $tmp");

    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $i");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $tmp");
    emitter.push_line(store);

    emitter.push_line("local.get $i");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $i");
    emitter.push_line("br $copy_loop");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $len");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $value");
    emitter.push_line(store);

    emitter.push_line("local.get $chan");
    emitter.push_line(format!("i32.const {queue_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $new");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.store");

    emitter.push_line(format!("i32.const {root_slots}"));
    emitter.push_line("call $bd_root_pop");
    emitter.push_line("i32.const 1");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_channel_recv(emitter: &mut WatEmitter, spec: &ChannelSpec) {
    let queue_offset = OBJECT_FIELD_SIZE * CHANNEL_FIELD_QUEUE + HEAP_HEADER_SIZE;
    let elem_size = spec.payload.elem_size();
    let elem_kind = spec.payload.elem_kind();
    let load = spec.payload.array_load();
    let store = spec.payload.array_store();
    let value_ty = spec.payload.wat_param();
    let enum_tag = (HEAP_KIND_ENUM << HEAP_KIND_SHIFT) | spec.enum_id as i32;
    let enum_size = HEAP_HEADER_SIZE + 8;
    let closed_tag = (HEAP_KIND_ENUM << HEAP_KIND_SHIFT) | spec.enum_id as i32;
    let payload_kind = elem_kind;

    emitter.push_line(format!(
        "(func $bd_channel_recv_{} (param $chan i32) (result i32) (local $queue i32) (local $len i32) (local $new_len i32) (local $new i32) (local $i i32) (local $closed i32) (local $value {value_ty}) (local $tmp {value_ty}) (local $enum i32) (local $root i32)",
        spec.name
    ));
    emitter.indent();

    emitter.push_line("local.get $chan");
    emitter.push_line(format!("i32.const {queue_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("i64.load");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("local.set $queue");

    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.load offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.set $len");
    emitter.push_line("i32.const 3");
    emitter.push_line("call $bd_root_push");
    emitter.push_line("local.set $root");
    emitter.push_line("local.get $root");
    emitter.push_line("local.get $chan");
    emitter.push_line("call $bd_root_set");
    emitter.push_line("local.get $root");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $queue");
    emitter.push_line("call $bd_root_set");
    emitter.push_line("local.get $root");
    emitter.push_line("i32.const 2");
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 0");
    emitter.push_line("call $bd_root_set");
    emitter.push_line("local.get $len");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line("nop");
    emitter.dedent();
    emitter.push_line("else");
    emitter.indent();
    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.load offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("i32.const 2");
    emitter.push_line("i32.and");
    emitter.push_line("local.set $closed");
    emitter.push_line("local.get $closed");
    emitter.push_line("if");
    emitter.indent();
    emitter.push_line(format!("i32.const {HEAP_HEADER_SIZE}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $enum");
    emitter.push_line("local.get $enum");
    emitter.push_line(format!("i32.const {closed_tag}"));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $enum");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $enum");
    emitter.push_line(format!("i32.const {}", spec.closed_variant));
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $enum");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));
    emitter.push_line("i32.const 3");
    emitter.push_line("call $bd_root_pop");
    emitter.push_line("local.get $enum");
    emitter.push_line("return");
    emitter.dedent();
    emitter.push_line("end");
    emitter.push_line(format!("i32.const {TRAP_CHANNEL_BLOCK}"));
    emitter.push_line("call $bd_trap");
    emitter.push_line("unreachable");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("i32.const 0");
    emitter.push_line("i32.add");
    emitter.push_line(load);
    emitter.push_line("local.set $value");

    emitter.push_line("local.get $len");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line("local.set $new_len");

    emitter.push_line("local.get $new_len");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $new");
    emitter.push_line("local.get $root");
    emitter.push_line("i32.const 2");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $new");
    emitter.push_line("call $bd_root_set");

    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {}", HEAP_KIND_ARRAY << HEAP_KIND_SHIFT));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $new");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $new");
    emitter.push_line("local.get $new_len");
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {elem_kind}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("i32.const 1");
    emitter.push_line("local.set $i");
    emitter.push_line("block $copy_exit");
    emitter.indent();
    emitter.push_line("loop $copy_loop");
    emitter.indent();
    emitter.push_line("local.get $i");
    emitter.push_line("local.get $len");
    emitter.push_line("i32.ge_u");
    emitter.push_line("br_if $copy_exit");

    emitter.push_line("local.get $queue");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $i");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line(load);
    emitter.push_line("local.set $tmp");

    emitter.push_line("local.get $new");
    emitter.push_line(format!("i32.const {ARRAY_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $i");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.sub");
    emitter.push_line(format!("i32.const {elem_size}"));
    emitter.push_line("i32.mul");
    emitter.push_line("i32.add");
    emitter.push_line("local.get $tmp");
    emitter.push_line(store);

    emitter.push_line("local.get $i");
    emitter.push_line("i32.const 1");
    emitter.push_line("i32.add");
    emitter.push_line("local.set $i");
    emitter.push_line("br $copy_loop");
    emitter.dedent();
    emitter.push_line("end");
    emitter.dedent();
    emitter.push_line("end");

    emitter.push_line("local.get $chan");
    emitter.push_line(format!("i32.const {queue_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $new");
    emitter.push_line("i64.extend_i32_u");
    emitter.push_line("i64.store");

    emitter.push_line(format!("i32.const {enum_size}"));
    emitter.push_line("call $bd_alloc");
    emitter.push_line("local.set $enum");
    emitter.push_line("local.get $enum");
    emitter.push_line(format!("i32.const {enum_tag}"));
    emitter.push_line("i32.store");
    emitter.push_line("local.get $enum");
    emitter.push_line("i32.const 0");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("local.get $enum");
    emitter.push_line(format!("i32.const {}", spec.ok_variant));
    emitter.push_line(format!("i32.store offset={HEAP_LEN_OFFSET}"));
    emitter.push_line("local.get $enum");
    emitter.push_line(format!("i32.const {payload_kind}"));
    emitter.push_line(format!("i32.store offset={HEAP_AUX_OFFSET}"));

    emitter.push_line("local.get $enum");
    emitter.push_line(format!("i32.const {HEAP_HEADER_SIZE}"));
    emitter.push_line("i32.add");
    emitter.push_line("local.get $value");
    match spec.payload {
        ChannelPayload::I64 => emitter.push_line("i64.store"),
        ChannelPayload::F64 => emitter.push_line("f64.store"),
        ChannelPayload::Bool | ChannelPayload::U8 => emitter.push_line("i32.store8"),
        ChannelPayload::Ref => {
            emitter.push_line("i64.extend_i32_u");
            emitter.push_line("i64.store");
        }
    }

    emitter.push_line("i32.const 3");
    emitter.push_line("call $bd_root_pop");
    emitter.push_line("local.get $enum");
    emitter.dedent();
    emitter.push_line(")");
}

fn emit_channel_close(emitter: &mut WatEmitter, spec: &ChannelSpec) {
    let queue_offset = OBJECT_FIELD_SIZE * CHANNEL_FIELD_QUEUE + HEAP_HEADER_SIZE;
    emitter.push_line(format!(
        "(func $bd_channel_close_{} (param $chan i32)",
        spec.name
    ));
    emitter.indent();
    emitter.push_line("local.get $chan");
    emitter.push_line(format!("i32.const {queue_offset}"));
    emitter.push_line("i32.add");
    emitter.push_line("i64.load");
    emitter.push_line("i32.wrap_i64");
    emitter.push_line("local.set $chan");
    emitter.push_line("local.get $chan");
    emitter.push_line("local.get $chan");
    emitter.push_line(format!("i32.load offset={HEAP_FLAGS_OFFSET}"));
    emitter.push_line("i32.const 2");
    emitter.push_line("i32.or");
    emitter.push_line(format!("i32.store offset={HEAP_FLAGS_OFFSET}"));
    emitter.dedent();
    emitter.push_line(")");
}
