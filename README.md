# Bagel Language Server

A Language Server Protocol (LSP) implementation for the Bagel programming language (.bgl files), built with Rust and tower-lsp.

## Features

- Hover support: Hover over any code to see "Hello from Bagel Language Server!"
- Full LSP integration with VSCode

## Building and Running

### Prerequisites

- Rust (latest stable)
- Node.js and npm
- VSCode

### Build the Language Server

```bash
cargo build --release
```

### Set up the VSCode Extension

```bash
cd vscode-extension
npm install
npm run compile
```

### Install the Extension

1. Open the `vscode-extension` folder in VSCode
2. Press F5 to launch a new VSCode window with the extension loaded
3. Create a new file with the `.bgl` extension
4. Hover over any text to see the hover message

Alternatively, package and install the extension:

```bash
cd vscode-extension
npm run package
code --install-extension bagel-language-support-0.1.0.vsix
```

## Testing

Create a test file `test.bgl` with any content and hover over it to see the language server in action.

## Project Structure

- `src/main.rs` - Rust language server implementation
- `vscode-extension/` - VSCode extension
  - `src/extension.ts` - Extension activation and client setup
  - `package.json` - Extension manifest
  - `language-configuration.json` - Language configuration (brackets, comments, etc.)

## Next Steps

Extend the language server with additional features:
- Syntax highlighting
- Code completion
- Diagnostics (error checking)
- Go to definition
- Find references
- Code formatting
