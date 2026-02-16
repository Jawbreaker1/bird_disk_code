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
- By default, diagnostics include `birddiskc check --json` plus `birddiskc lint --json`.
- You can disable lint warnings via `birddisk.enableLintDiagnostics: false`.
- Configure the compiler path via the `birddisk.compilerPath` setting if needed.
- Use `birddisk.entryFile` to suppress missing-main diagnostics in module files.
- Formatting uses `birddiskc fmt` on the current document.
- The extension sets BirdDisk minimap defaults to reduce garbled glyphs (you can override them in user settings).

## LSP (optional)
BirdDisk ships with a minimal LSP server (`birddisk-lsp`) that adds hover, go-to definition,
go-to type definition, rename, references, signature help, document/workspace symbols, LSP-driven
completions/diagnostics, semantic tokens, and inlay hints. Completions are file-local; go-to-definition
and references follow imported modules when available (open files are included too).
LSP diagnostics include lint warnings as well.

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
