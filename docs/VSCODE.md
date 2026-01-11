# VSCode extension (local install)

BirdDisk includes a minimal VSCode extension in `editors/vscode` with syntax highlighting,
language configuration, snippets, and basic completions/diagnostics.

## Install from a local folder
1) Open VSCode.
2) Run "Extensions: Install Extension from Location...".
3) Select the `editors/vscode` folder in this repo.

The extension activates for `.bd` files.

Notes:
- Comments are reserved in the language today; the editor config uses `//` for convenience.
- By default, diagnostics are provided by `birddisk check --json`.
- Configure the compiler path via the `birddisk.compilerPath` setting if needed.
- Formatting uses `birddisk fmt` on the current document.

## LSP (optional)
BirdDisk ships with a minimal LSP server (`birddisk-lsp`) that adds hover, go-to definition,
rename, LSP-driven completions/diagnostics, semantic tokens, and inlay hints.
Completions and definitions are file-local and best-effort in v0.x.

Build the server:
```sh
cargo build -p birddisk_lsp
```

Enable in VSCode settings:
```json
{
  "birddisk.enableLsp": true,
  "birddisk.lspPath": "./target/debug/birddisk-lsp"
}
```
