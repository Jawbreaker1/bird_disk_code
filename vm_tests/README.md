# VM/native fixtures

This folder contains BirdDisk fixtures focused on VM/native behavior.
Use it for features that are not yet available in WASM.

Run them with:
```sh
./target/debug/birddiskc test --json --engine vm --dir vm_tests
./target/debug/birddiskc test --json --engine native --dir vm_tests
```
