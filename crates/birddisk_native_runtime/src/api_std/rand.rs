use crate::rt_core::*;

const RAND_SEED_DEFAULT: u64 = 0x9E37_79B9_7F4A_7C15;
const RAND_MULT: u64 = 0x2545_F491_4F6C_DD1D;

#[no_mangle]
pub extern "C-unwind" fn bd_rand_seed(rt: *mut Runtime, seed: i64) {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return;
    }
    let mut value = seed as u64;
    if value == 0 {
        value = RAND_SEED_DEFAULT;
    }
    rt.rand_state = value;
}

#[no_mangle]
pub extern "C-unwind" fn bd_rand_range(rt: *mut Runtime, min: i64, max: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if min >= max {
        runtime_error(rt, "std::rand::range expects min < max.");
        return 0;
    }
    let span = (max as i128 - min as i128) as u128;
    let value = rand_next(rt) as u128;
    let offset = (value % span) as i128;
    (min as i128 + offset) as i64
}

fn rand_next(rt: &mut Runtime) -> u64 {
    let mut x = rt.rand_state;
    if x == 0 {
        x = RAND_SEED_DEFAULT;
    }
    x ^= x >> 12;
    x ^= x << 25;
    x ^= x >> 27;
    rt.rand_state = x;
    x.wrapping_mul(RAND_MULT)
}
