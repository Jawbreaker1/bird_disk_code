use super::*;
use std::io::{BufRead, Write};
use std::time::Duration;

impl<'a> Vm<'a> {
    pub(crate) fn push_output(&mut self, value: &str) {
        if self.stdout_live {
            print!("{value}");
            let _ = std::io::stdout().flush();
        }
        self.output.push_str(value);
    }

    pub(crate) fn read_input_line(&mut self) -> String {
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
        if self.deterministic {
            return self.virtual_time_ms;
        }
        let elapsed = self.start_time.elapsed().as_millis();
        i64::try_from(elapsed).unwrap_or(i64::MAX)
    }

    pub(crate) fn heap_stats(&self) -> crate::heap::HeapStats {
        self.heap.stats()
    }

    pub(crate) fn sleep_ms(&mut self, millis: i64) -> Result<i64, RuntimeError> {
        if millis < 0 {
            return Err(runtime_error("E0400", "Sleep duration must be >= 0."));
        }
        if self.deterministic {
            let next = self.virtual_time_ms.saturating_add(millis.max(0));
            self.virtual_time_ms = next;
            return Ok(millis);
        }
        std::thread::sleep(Duration::from_millis(millis as u64));
        Ok(millis)
    }

    pub(crate) fn rand_seed(&mut self, seed: i64) {
        let mut value = seed as u64;
        if value == 0 {
            value = RAND_SEED_DEFAULT;
        }
        self.rng_state = value;
    }

    pub(crate) fn rand_range(&mut self, min: i64, max: i64) -> Result<i64, RuntimeError> {
        if min >= max {
            return Err(runtime_error(
                "E0400",
                "std::rand::range expects min < max.",
            ));
        }
        let span = (max as i128 - min as i128) as u128;
        let value = self.rand_next_u64() as u128;
        let offset = (value % span) as i128;
        Ok((min as i128 + offset) as i64)
    }

    fn rand_next_u64(&mut self) -> u64 {
        let mut x = self.rng_state;
        if x == 0 {
            x = RAND_SEED_DEFAULT;
        }
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.rng_state = x;
        x.wrapping_mul(RAND_MULT)
    }

    pub(super) fn set_stdin_fallback(&mut self, enabled: bool) {
        self.stdin_fallback = enabled;
    }

    pub(super) fn set_stdout_live(&mut self, enabled: bool) {
        self.stdout_live = enabled;
    }
}
