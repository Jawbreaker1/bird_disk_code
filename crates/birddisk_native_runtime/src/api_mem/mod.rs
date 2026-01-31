mod alloc;
mod array;
mod error;
mod object;
mod r#enum;

pub use alloc::{
    bd_alloc_array, bd_alloc_enum, bd_alloc_object, bd_alloc_string, bd_root_pop, bd_root_push,
    bd_root_set, bd_trace_pop, bd_trace_push,
};
pub use array::{
    bd_array_get_bool, bd_array_get_f64, bd_array_get_i64, bd_array_get_ref, bd_array_get_u8,
    bd_array_set_bool, bd_array_set_f64, bd_array_set_i64, bd_array_set_ref, bd_array_set_u8,
};
pub use error::{
    bd_clear_error, bd_error_is_throw, bd_error_message, bd_has_error, bd_throw,
};
pub use object::{
    bd_object_get_bool, bd_object_get_f64, bd_object_get_i64, bd_object_get_ref,
    bd_object_get_u8, bd_object_set_bool, bd_object_set_f64, bd_object_set_i64,
    bd_object_set_ref, bd_object_set_u8,
};
pub use r#enum::{
    bd_enum_payload_bool, bd_enum_payload_f64, bd_enum_payload_i64, bd_enum_payload_ref,
    bd_enum_payload_u8, bd_enum_set_payload_bool, bd_enum_set_payload_f64,
    bd_enum_set_payload_i64, bd_enum_set_payload_ref, bd_enum_set_payload_u8, bd_enum_variant,
};
