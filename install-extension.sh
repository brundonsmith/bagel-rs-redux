#!/bin/bash

set -e

echo "Building Rust language server..."
cargo build --release

echo ""
echo "Installing VSCode extension dependencies..."
cd vscode-extension
npm install

echo ""
echo "Compiling TypeScript..."
npm run compile

echo ""
echo "Packaging extension..."
npm run package

echo ""
echo "Installing extension in VSCode..."
code --install-extension bagel-language-support-0.1.0.vsix

echo ""
echo "✓ Installation complete!"
echo ""
echo "To test the extension:"
echo "  1. Create a test file: touch test.bgl"
echo "  2. Open it in VSCode: code test.bgl"
echo "  3. Hover over any text to see 'Hello from Bagel Language Server!'"
echo ""
echo "To uninstall: code --uninstall-extension bagel-language-support"
