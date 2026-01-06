# BirdDisk Cookbook (v0.1)

All examples must parse, typecheck, and run in the VM (golden)
and the WASM backend.

---

## 1) Minimal main
```birddisk
rule main() -> i64:
  yield 0.
end

2) when / otherwise
rule main() -> i64:
  set x = 10.
  when x > 5:
    yield 1.
  otherwise:
    yield 2.
  end
end

3) repeat while
rule main() -> i64:
  set i = 0.
  set sum: i64 = 0.

  repeat while i < 10:
    put sum = sum + i.
    put i = i + 1.
  end

  yield sum.
end

4) Function call
rule add(a: i64, b: i64) -> i64:
  yield a + b.
end

rule main() -> i64:
  set x = add(2, 3).
  yield x.
end

5) Fibonacci (iterative)
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

---

Notes:
- Division or modulo by zero is a runtime error.
