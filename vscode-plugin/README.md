# Kip VS Code Extension

This extension provides Kip language support via `kip-lsp`.

## Features
- Diagnostics (parse/typecheck)
- Hover type information
- Go to definition
- Completions
- Document formatting

## Requirements
- `kip-lsp` available on your `PATH`, or set `kip.languageServerPath` to the executable.

## Settings
- `kip.languageServerPath`: path to `kip-lsp` (default: `kip-lsp`)
- `kip.languageServerArgs`: additional arguments
- `kip.trace.server`: LSP trace level (`off`, `messages`, `verbose`)

## Development
```bash
cd vscode-plugin
npm install
npm run compile
code --extensionDevelopmentPath=.
```

Use "Run Extension" in VS Code or `vsce package` to build a `.vsix`.
