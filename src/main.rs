pub mod ast;
pub mod parse;

use std::collections::HashMap;
use std::rc::Rc;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use ast::container::AST;
use ast::grammar::Any;
use ast::slice::Slice;

#[derive(Debug)]
struct BagelLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<String, String>>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for BagelLanguageServer {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "Bagel Language Server".to_string(),
                version: Some("0.1.0".to_string()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..Default::default()
            },
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "Bagel Language Server initialized!")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        let text = params.text_document.text;
        self.documents.write().await.insert(uri, text);
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        if let Some(change) = params.content_changes.into_iter().next() {
            self.documents.write().await.insert(uri, change.text);
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        self.documents.write().await.remove(&uri);
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;

        let documents = self.documents.read().await;
        let text = match documents.get(&uri) {
            Some(text) => text.clone(),
            None => return Ok(None),
        };
        drop(documents);

        // Parse the document
        let slice = Slice::new(Rc::new(text.clone()));
        let ast = match parse::parse::any(slice) {
            Ok((_, ast)) => ast,
            Err(_) => return Ok(None),
        };

        // Convert LSP position to byte offset
        let offset = position_to_offset(&text, position);

        // Find the AST node at this position
        if let Some(node) = find_node_at_offset(&ast, offset) {
            let debug_str = format!("{:#?}", node);

            return Ok(Some(Hover {
                contents: HoverContents::Markup(MarkupContent {
                    kind: MarkupKind::Markdown,
                    value: format!("```\n{}\n```", debug_str),
                }),
                range: None,
            }));
        }

        Ok(None)
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

fn find_node_at_offset(ast: &AST<Any>, offset: usize) -> Option<AST<Any>> {
    let slice = ast.slice();

    // Check if offset is within this node's range
    if offset < slice.start || offset > slice.end {
        return None;
    }

    // Try to find a more specific child node
    let details = ast.details();

    match details {
        Any::Module(module) => {
            // Check each declaration
            for decl in &module.declarations {
                if let Some(child) = find_node_at_offset(&decl.clone().upcast(), offset) {
                    return Some(child);
                }
            }
        }
        Any::Declaration(decl) => {
            // Check if offset is in the value expression
            if let Some(child) = find_node_at_offset(&decl.value.clone().upcast(), offset) {
                return Some(child);
            }
        }
        Any::Expression(expr) => {
            match expr {
                ast::grammar::Expression::BinaryOperation(bin_op) => {
                    // Check left operand
                    if let Some(child) = find_node_at_offset(&bin_op.left.clone().upcast(), offset)
                    {
                        return Some(child);
                    }
                    // Check right operand
                    if let Some(child) = find_node_at_offset(&bin_op.right.clone().upcast(), offset)
                    {
                        return Some(child);
                    }
                }
                _ => {}
            }
        }
        _ => {}
    }

    // If no child contains the offset, return this node
    Some(ast.clone())
}

#[tokio::main]
async fn main() {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| BagelLanguageServer {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
