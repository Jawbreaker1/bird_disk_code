# VM error fixtures

This folder contains BirdDisk programs that are expected to fail.
Use them to validate diagnostics and runtime error codes.

Examples:
```sh
./target/debug/birddisk check vm_error_tests/arrays/array_index_type_error.bd --json
./target/debug/birddisk run vm_error_tests/arrays/array_index_out_of_bounds.bd --engine vm --json
```
