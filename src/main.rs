pub mod ast;
pub mod check;
pub mod compile;
pub mod config;
pub mod emit;
pub mod parse;
pub mod types;

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use ast::container::AST;
use ast::grammar::{Any, Expression};
use ast::slice::Slice;
use types::infer::InferTypeContext;

#[derive(Debug)]
struct BagelLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<String, String>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for BagelLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        eprintln!("[DEBUG] initialize() called");
        let result = InitializeResult {
            server_info: Some(ServerInfo {
                name: "Bagel Language Server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                ..Default::default()
            },
        };
        eprintln!("[DEBUG] initialize() completed - capabilities: hover=true, inlay_hints=true");
        Ok(result)
    }

    async fn initialized(&self, _: InitializedParams) {
        eprintln!("[DEBUG] initialized() called - server is ready");
        self.client
            .log_message(MessageType::INFO, "Bagel Language Server initialized!")
            .await;
        eprintln!("[DEBUG] initialized() completed");
    }

    async fn shutdown(&self) -> Result<()> {
        eprintln!("[DEBUG] shutdown() called");
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] did_open() called - uri: {}", uri);
        let text = params.text_document.text;
        eprintln!("[DEBUG] did_open() - document length: {} bytes, {} lines",
            text.len(), text.lines().count());
        self.documents.write().await.insert(uri.clone(), text);
        eprintln!("[DEBUG] did_open() completed - stored document for {}", uri);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] did_change() called - uri: {}", uri);
        let num_changes = params.content_changes.len();
        eprintln!("[DEBUG] did_change() - number of changes: {}", num_changes);
        if let Some(change) = params.content_changes.into_iter().next() {
            eprintln!("[DEBUG] did_change() - new document length: {} bytes, {} lines",
                change.text.len(), change.text.lines().count());
            self.documents.write().await.insert(uri.clone(), change.text);
            eprintln!("[DEBUG] did_change() completed - updated document for {}", uri);
        } else {
            eprintln!("[DEBUG] did_change() - no changes to process");
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] did_close() called - uri: {}", uri);
        let removed = self.documents.write().await.remove(&uri);
        if removed.is_some() {
            eprintln!("[DEBUG] did_close() completed - removed document for {}", uri);
        } else {
            eprintln!("[DEBUG] did_close() - document not found in store");
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;
        eprintln!("[DEBUG] hover() called - uri: {}, position: line={} char={}",
            uri, position.line, position.character);

        let documents = self.documents.read().await;
        eprintln!("[DEBUG] hover() - acquired document lock, total docs: {}", documents.len());
        let text = match documents.get(&uri) {
            Some(text) => {
                eprintln!("[DEBUG] hover() - found document, length: {} bytes", text.len());
                text.clone()
            }
            None => {
                eprintln!("[DEBUG] hover() - document not found in store, returning None");
                return Ok(None);
            }
        };
        drop(documents);

        // Parse the document
        eprintln!("[DEBUG] hover() - parsing document");
        let slice = Slice::new(Rc::new(text.clone()));
        let ast = match parse::parse::any(slice) {
            Ok((_, ast)) => {
                eprintln!("[DEBUG] hover() - parse successful");
                ast
            }
            Err(e) => {
                eprintln!("[DEBUG] hover() - parse failed: {:?}", e);
                return Ok(None);
            }
        };

        // Convert LSP position to byte offset
        let offset = position_to_offset(&text, position);
        eprintln!("[DEBUG] hover() - converted position to offset: {}", offset);

        // Find the AST node at this position
        eprintln!("[DEBUG] hover() - searching for node at offset {}", offset);
        if let Some(node) = find_node_at_offset(&ast, offset) {
            eprintln!("[DEBUG] hover() - found node at offset, slice: {}..{}",
                node.slice().start, node.slice().end);

            // Try to get the type if it's an expression
            let type_info = if let Some(expr) = node.clone().try_downcast::<Expression>() {
                eprintln!("[DEBUG] hover() - node is an Expression, inferring type");
                let ctx = InferTypeContext {};
                let inferred_type = expr.infer_type(ctx);
                eprintln!("[DEBUG] hover() - inferred type: {}", inferred_type);
                format!("**Type:** `{}`\n\n", inferred_type)
            } else {
                eprintln!("[DEBUG] hover() - node is not an Expression");
                String::new()
            };

            let debug_str = format!("{:#?}", node);
            eprintln!("[DEBUG] hover() - returning hover info");

            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("{}**AST:**\n```\n{}\n```", type_info, debug_str),
                }),
                range: None,
            }));
        }

        eprintln!("[DEBUG] hover() - no node found at offset, returning None");
        Ok(None)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] inlay_hint() called - uri: {}", uri);
        eprintln!("[DEBUG] inlay_hint() - range: {:?}", params.range);

        let documents = self.documents.read().await;
        eprintln!("[DEBUG] inlay_hint() - acquired document lock, total docs: {}", documents.len());
        let text = match documents.get(&uri) {
            Some(text) => {
                eprintln!("[DEBUG] inlay_hint() - found document, length: {} bytes", text.len());
                text.clone()
            }
            None => {
                eprintln!("[DEBUG] inlay_hint() - document not found in store, returning None");
                return Ok(None);
            }
        };
        drop(documents);

        // Parse the document
        eprintln!("[DEBUG] inlay_hint() - parsing document");
        let slice = Slice::new(Rc::new(text.clone()));
        let ast = match parse::parse::any(slice) {
            Ok((_, ast)) => {
                eprintln!("[DEBUG] inlay_hint() - parse successful");
                ast
            }
            Err(e) => {
                eprintln!("[DEBUG] inlay_hint() - parse failed: {:?}", e);
                return Ok(None);
            }
        };

        let mut hints = Vec::new();

        // Traverse the AST to find declarations
        eprintln!("[DEBUG] inlay_hint() - attempting to downcast to Module");
        if let Some(module) = ast.try_downcast::<ast::grammar::Module>() {
            let module_data = module.unpack();
            eprintln!("[DEBUG] inlay_hint() - found Module with {} declarations", module_data.declarations.len());

            for (idx, decl) in module_data.declarations.iter().enumerate() {
                let decl_data: ast::grammar::Declaration = decl.unpack();
                eprintln!("[DEBUG] inlay_hint() - processing declaration {}: identifier at {}..{}",
                    idx, decl_data.identifier.slice().start, decl_data.identifier.slice().end);

                // Infer the type of the value
                let ctx = InferTypeContext {};
                let inferred_type = decl_data.value.infer_type(ctx);
                eprintln!("[DEBUG] inlay_hint() - inferred type for decl {}: {}", idx, inferred_type);

                // Get the position after the identifier
                let identifier_slice = decl_data.identifier.slice();
                let position = offset_to_position(&text, identifier_slice.end);
                eprintln!("[DEBUG] inlay_hint() - hint position for decl {}: line={} char={}",
                    idx, position.line, position.character);

                hints.push(InlayHint {
                    position,
                    label: InlayHintLabel::String(format!(": {}", inferred_type)),
                    kind: Some(InlayHintKind::TYPE),
                    text_edits: None,
                    tooltip: None,
                    padding_left: None,
                    padding_right: None,
                    data: None,
                });
            }
        } else {
            eprintln!("[DEBUG] inlay_hint() - AST is not a Module");
        }

        eprintln!("[DEBUG] inlay_hint() - returning {} hints", hints.len());
        Ok(Some(hints))
    }
}

fn position_to_offset(text: &str, position: Position) -> usize {
    let mut offset = 0;
    let mut current_line = 0;

    for line in text.lines() {
        if current_line == position.line {
            return offset + (position.character as usize).min(line.len());
        }
        offset += line.len() + 1; // +1 for newline
        current_line += 1;
    }

    offset
}

fn offset_to_position(text: &str, offset: usize) -> Position {
    let mut current_offset = 0;
    let mut line: u32 = 0;

    for text_line in text.lines() {
        let line_end = current_offset + text_line.len();
        if offset <= line_end {
            let character = offset - current_offset;
            return Position {
                line,
                character: character as u32,
            };
        }
        current_offset = line_end + 1; // +1 for newline
        line += 1;
    }

    // If offset is beyond the end, return the last position
    Position {
        line: line.saturating_sub(1),
        character: text.lines().last().map(|l| l.len()).unwrap_or(0) as u32,
    }
}

fn find_node_at_offset(ast: &AST<Any>, offset: usize) -> Option<AST<Any>> {
    let slice = ast.slice();
    eprintln!("[DEBUG] find_node_at_offset() - checking node at {}..{}, offset={}",
        slice.start, slice.end, offset);

    // Check if offset is within this node's range
    if offset < slice.start || offset > slice.end {
        eprintln!("[DEBUG] find_node_at_offset() - offset {} outside node range {}..{}",
            offset, slice.start, slice.end);
        return None;
    }

    // Try to find a more specific child node
    let details = ast.details();

    match details {
        Any::Module(module) => {
            eprintln!("[DEBUG] find_node_at_offset() - node is Module with {} declarations",
                module.declarations.len());
            // Check each declaration
            for (idx, decl) in module.declarations.iter().enumerate() {
                eprintln!("[DEBUG] find_node_at_offset() - checking declaration {}", idx);
                if let Some(child) = find_node_at_offset(&decl.clone().upcast(), offset) {
                    eprintln!("[DEBUG] find_node_at_offset() - found in declaration {}", idx);
                    return Some(child);
                }
            }
        }
        Any::Declaration(decl) => {
            eprintln!("[DEBUG] find_node_at_offset() - node is Declaration");
            // Check if offset is in the value expression
            if let Some(child) = find_node_at_offset(&decl.value.clone().upcast(), offset) {
                eprintln!("[DEBUG] find_node_at_offset() - found in declaration value");
                return Some(child);
            }
        }
        Any::Expression(expr) => {
            eprintln!("[DEBUG] find_node_at_offset() - node is Expression: {:?}",
                std::mem::discriminant(expr));
            match expr {
                ast::grammar::Expression::BinaryOperation(bin_op) => {
                    eprintln!("[DEBUG] find_node_at_offset() - Expression is BinaryOperation");
                    // Check left operand
                    if let Some(child) = find_node_at_offset(&bin_op.left.clone().upcast(), offset)
                    {
                        eprintln!("[DEBUG] find_node_at_offset() - found in left operand");
                        return Some(child);
                    }
                    // Check right operand
                    if let Some(child) = find_node_at_offset(&bin_op.right.clone().upcast(), offset)
                    {
                        eprintln!("[DEBUG] find_node_at_offset() - found in right operand");
                        return Some(child);
                    }
                }
                _ => {
                    eprintln!("[DEBUG] find_node_at_offset() - Expression is a leaf type");
                }
            }
        }
        _ => {
            eprintln!("[DEBUG] find_node_at_offset() - node is other type");
        }
    }

    // If no child contains the offset, return this node
    eprintln!("[DEBUG] find_node_at_offset() - no child found, returning this node");
    Some(ast.clone())
}

#[tokio::main]
async fn main() {
    eprintln!("[DEBUG] main() - Bagel Language Server starting");
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    eprintln!("[DEBUG] main() - creating LSP service");
    let (service, socket) = LspService::new(|client| BagelLanguageServer {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });
    eprintln!("[DEBUG] main() - starting server");
    Server::new(stdin, stdout, socket).serve(service).await;
    eprintln!("[DEBUG] main() - server stopped");
}
