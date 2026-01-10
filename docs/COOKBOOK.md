# BirdDisk Cookbook (v0.1)

All examples must parse, typecheck, and run in the VM (golden).
Unless noted, they should also run in WASM.

---

## 1) Minimal main
```birddisk
rule main() -> i64:
  yield 0.
end
```

## 2) when / otherwise
```birddisk
rule main() -> i64:
  set x = 10.
  when x > 5:
    yield 1.
  otherwise:
    yield 2.
  end
end
```

## 3) repeat while
```birddisk
rule main() -> i64:
  set i = 0.
  set sum: i64 = 0.

  repeat while i < 10:
    put sum = sum + i.
    put i = i + 1.
  end

  yield sum.
end
```

## 4) Function call
```birddisk
rule add(a: i64, b: i64) -> i64:
  yield a + b.
end

rule main() -> i64:
  set x = add(2, 3).
  yield x.
end
```

## 5) Fibonacci (iterative)
```birddisk
rule main() -> i64:
  set n = 10.
  set a: i64 = 0.
  set b: i64 = 1.
  set i: i64 = 0.

  repeat while i < n:
    set next = a + b.
    put a = b.
    put b = next.
    put i = i + 1.
  end

  yield a.
end
```

## 6) Arrays
```birddisk
rule main() -> i64:
  set xs: i64[] = array(3).
  put xs[0] = 2.
  put xs[1] = 3.
  put xs[2] = 5.
  yield xs[0] + xs[1] + xs[2].
end
```

## 7) Objects (book)
```birddisk
book Counter:
  field value: i64.

  rule init(self: Counter, start: i64) -> Counter:
    put self::value = start.
    yield self.
  end

  rule add(self: Counter, delta: i64) -> i64:
    put self::value = self::value + delta.
    yield self::value.
  end
end

rule main() -> i64:
  set c: Counter = new Counter(10).
  yield c::add(5).
end
```

## 8) Math (std::math)
```birddisk
import std::math.

rule main() -> i64:
  set base: i64 = 2.
  set power: i64 = std::math::pow(base, 5).
  set reduced: i64 = std::math::div(power, 4).
  set rem: i64 = std::math::mod(reduced, 7).
  set g: i64 = std::math::gcd(48, 18).
  yield std::math::add(rem, g).
end
```

## 9) Strings (std::string)
```birddisk
import std::string.

rule main() -> i64:
  set name: string = "Bird".
  set suffix: string = "Disk".
  set full: string = std::string::concat(name, suffix).
  yield std::string::len(full).
end
```

## 10) String to i64
```birddisk
import std::string.

rule main() -> i64:
  set value: i64 = std::string::to_i64("42").
  set text: string = std::string::from_i64(value).
  yield std::string::len(text).
end
```

## 11) Bytes (u8[])
```birddisk
import std::string.
import std::bytes.

rule main() -> i64:
  set data: u8[] = std::string::bytes("hi").
  yield std::bytes::len(data).
end
```

## 12) Bytes to string
```birddisk
import std::string.

rule main() -> i64:
  set data: u8[] = [66, 105, 114, 100].
  set text: string = std::string::from_bytes(data).
  yield std::string::len(text).
end
```

## 13) IO (read + print)
```birddisk
import std::io.
import std::string.

rule main() -> i64:
  set line: string = std::io::read_line().
  set out: string = std::string::concat(line, "!").
  yield std::io::print(out).
end
```

## 14) Native AOT (emit exe)
```birddisk
rule main() -> i64:
  yield 0.
end
```
Build a native executable on the host:
```sh
./target/debug/birddisk run path/to/file.bd --engine native --emit exe --out ./bird_app
./bird_app
```

---

Notes:
- Division or modulo by zero is a runtime error.
- Array index out of bounds is a runtime error.
