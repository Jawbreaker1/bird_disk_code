use crate::rt_core::*;
use std::time::Duration;

#[no_mangle]
pub extern "C-unwind" fn bd_time_now_ms(rt: *mut Runtime) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    rt.now_ms()
}

#[no_mangle]
pub extern "C-unwind" fn bd_time_sleep_ms(rt: *mut Runtime, millis: i64) -> i64 {
    let rt = runtime_mut(rt);
    if rt.has_error() {
        return 0;
    }
    if millis < 0 {
        runtime_error(rt, "Sleep duration must be >= 0.");
        return 0;
    }
    std::thread::sleep(Duration::from_millis(millis as u64));
    millis
}
