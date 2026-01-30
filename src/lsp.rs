use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::ast::container::AST;
use crate::ast::grammar::{Any, Declaration, Expression};
use crate::ast::slice::Slice;
use crate::check::{BagelError, CheckContext, Checkable};
use crate::config::Config;
use crate::emit::{EmitContext, Emittable};
use crate::types::infer::InferTypeContext;

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
                text_document_sync: Some(TextDocumentSyncCapability::Options(
                    TextDocumentSyncOptions {
                        open_close: Some(true),
                        change: Some(TextDocumentSyncKind::FULL),
                        will_save: None,
                        will_save_wait_until: None,
                        save: Some(TextDocumentSyncSaveOptions::SaveOptions(SaveOptions {
                            include_text: Some(false),
                        })),
                    },
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
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
        eprintln!(
            "[DEBUG] did_open() - document length: {} bytes, {} lines",
            text.len(),
            text.lines().count()
        );
        self.documents
            .write()
            .await
            .insert(uri.clone(), text.clone());
        eprintln!("[DEBUG] did_open() completed - stored document for {}", uri);

        // Publish diagnostics
        self.publish_diagnostics(&uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] did_change() called - uri: {}", uri);
        let num_changes = params.content_changes.len();
        eprintln!("[DEBUG] did_change() - number of changes: {}", num_changes);
        if let Some(change) = params.content_changes.into_iter().next() {
            eprintln!(
                "[DEBUG] did_change() - new document length: {} bytes, {} lines",
                change.text.len(),
                change.text.lines().count()
            );
            let text = change.text;
            self.documents
                .write()
                .await
                .insert(uri.clone(), text.clone());
            eprintln!(
                "[DEBUG] did_change() completed - updated document for {}",
                uri
            );

            // Publish diagnostics
            self.publish_diagnostics(&uri, &text).await;
        } else {
            eprintln!("[DEBUG] did_change() - no changes to process");
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] did_close() called - uri: {}", uri);
        let removed = self.documents.write().await.remove(&uri);
        if removed.is_some() {
            eprintln!(
                "[DEBUG] did_close() completed - removed document for {}",
                uri
            );
        } else {
            eprintln!("[DEBUG] did_close() - document not found in store");
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] did_save() called - uri: {}", uri);

        // Get the current document text
        let documents = self.documents.read().await;
        let text = match documents.get(&uri) {
            Some(text) => text.clone(),
            None => {
                eprintln!("[DEBUG] did_save() - document not found in store");
                return;
            }
        };
        drop(documents);

        // Parse the document
        let slice = Slice::new(Arc::new(text.clone()));
        let ast = match crate::parse::parse::module(slice) {
            Ok((_, ast)) => {
                eprintln!("[DEBUG] did_save() - parse successful");
                ast
            }
            Err(e) => {
                eprintln!("[DEBUG] did_save() - parse failed: {:?}", e);
                return;
            }
        };

        // Re-emit the AST to format it
        let config = Config::default();
        let ctx = EmitContext { config: &config };
        let mut formatted = String::new();
        if let Err(e) = ast.emit(ctx, &mut formatted) {
            eprintln!("[DEBUG] did_save() - emit failed: {:?}", e);
            return;
        }

        eprintln!("[DEBUG] did_save() - emitted {} bytes", formatted.len());

        // Only apply if the formatted version is different
        if formatted != text {
            eprintln!("[DEBUG] did_save() - content changed, applying edits");

            // Calculate the range of the entire document
            let line_count = text.lines().count() as u32;
            let last_line_length = text.lines().last().map(|l| l.len()).unwrap_or(0) as u32;

            let edit = TextEdit {
                range: Range {
                    start: Position {
                        line: 0,
                        character: 0,
                    },
                    end: Position {
                        line: line_count.saturating_sub(1),
                        character: last_line_length,
                    },
                },
                new_text: formatted.clone(),
            };

            // Apply the edit
            self.client
                .apply_edit(WorkspaceEdit {
                    changes: Some([(uri.parse().unwrap(), vec![edit])].into_iter().collect()),
                    document_changes: None,
                    change_annotations: None,
                })
                .await
                .ok();

            // Update the stored document
            self.documents.write().await.insert(uri.clone(), formatted);
            eprintln!("[DEBUG] did_save() - completed with formatting");
        } else {
            eprintln!("[DEBUG] did_save() - no formatting changes needed");
        }
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;
        eprintln!(
            "[DEBUG] hover() called - uri: {}, position: line={} char={}",
            uri, position.line, position.character
        );

        let documents = self.documents.read().await;
        eprintln!(
            "[DEBUG] hover() - acquired document lock, total docs: {}",
            documents.len()
        );
        let text = match documents.get(&uri) {
            Some(text) => {
                eprintln!(
                    "[DEBUG] hover() - found document, length: {} bytes",
                    text.len()
                );
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
        let slice = Slice::new(Arc::new(text.clone()));
        let ast = match crate::parse::parse::any(slice) {
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
            eprintln!(
                "[DEBUG] hover() - found node at offset, slice: {}..{}",
                node.slice().start,
                node.slice().end
            );

            // Try to get the type if it's an expression
            let type_info = if let Some(expr) = node.clone().try_downcast::<Expression>() {
                eprintln!("[DEBUG] hover() - node is an Expression, inferring type");
                let ctx = InferTypeContext {};
                let inferred_type = expr.infer_type(ctx).normalize();
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
        eprintln!(
            "[DEBUG] inlay_hint() - acquired document lock, total docs: {}",
            documents.len()
        );
        let text = match documents.get(&uri) {
            Some(text) => {
                eprintln!(
                    "[DEBUG] inlay_hint() - found document, length: {} bytes",
                    text.len()
                );
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
        let slice = Slice::new(Arc::new(text.clone()));
        let ast = match crate::parse::parse::any(slice) {
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
        if let Some(module) = ast.try_downcast::<crate::ast::grammar::Module>() {
            match module.details() {
                None => {
                    eprintln!("[DEBUG] inlay_hint() - module is malformed, skipping");
                }
                Some(_) => {
                    let module_data = module.unpack();
                    eprintln!(
                        "[DEBUG] inlay_hint() - found Module with {} declarations",
                        module_data.declarations.len()
                    );

                    for (idx, decl) in module_data.declarations.iter().enumerate() {
                        if decl.details().is_none() {
                            eprintln!(
                                "[DEBUG] inlay_hint() - declaration {} is malformed, skipping",
                                idx
                            );
                            continue;
                        }

                        match decl.unpack() {
                            Declaration::ConstDeclaration(decl_data) => {
                                if decl_data.type_annotation.is_some() {
                                    eprintln!("[DEBUG] inlay_hint() - skipping declaration {} (has explicit type annotation)", idx);
                                    continue;
                                }

                                eprintln!("[DEBUG] inlay_hint() - processing declaration {}: identifier at {}..{}",
                                    idx, decl_data.identifier.slice().start, decl_data.identifier.slice().end);

                                // Infer the type of the value
                                let ctx = InferTypeContext {};
                                let inferred_type = decl_data.value.infer_type(ctx).normalize();
                                eprintln!(
                                    "[DEBUG] inlay_hint() - inferred type for decl {}: {}",
                                    idx, inferred_type
                                );

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
                            Declaration::ImportDeclaration(_) => {
                                // No inlay hints for imports
                            }
                        }
                    }
                }
            }
        } else {
            eprintln!("[DEBUG] inlay_hint() - AST is not a Module");
        }

        eprintln!("[DEBUG] inlay_hint() - returning {} hints", hints.len());
        Ok(Some(hints))
    }
}

impl BagelLanguageServer {
    async fn publish_diagnostics(&self, uri: &str, text: &str) {
        eprintln!("[DEBUG] publish_diagnostics() called - uri: {}", uri);

        // Parse the document
        let slice = Slice::new(Arc::new(text.to_string()));
        let ast = match crate::parse::parse::module(slice) {
            Ok((_, ast)) => {
                eprintln!("[DEBUG] publish_diagnostics() - parse successful");
                ast
            }
            Err(e) => {
                eprintln!("[DEBUG] publish_diagnostics() - parse failed: {:?}", e);
                // On parse failure, publish empty diagnostics
                self.client
                    .publish_diagnostics(uri.parse().unwrap(), vec![], None)
                    .await;
                return;
            }
        };

        // Run check to collect errors
        let config = Config::default();
        let ctx = CheckContext { config: &config };
        let mut errors = Vec::new();
        ast.check(&ctx, &mut |error| {
            errors.push(error);
        });

        eprintln!(
            "[DEBUG] publish_diagnostics() - found {} errors",
            errors.len()
        );

        // Convert errors to LSP diagnostics
        let diagnostics: Vec<Diagnostic> = errors
            .into_iter()
            .map(|error| bagel_error_to_diagnostic(text, error))
            .collect();

        eprintln!(
            "[DEBUG] publish_diagnostics() - publishing {} diagnostics",
            diagnostics.len()
        );

        // Publish diagnostics
        self.client
            .publish_diagnostics(uri.parse().unwrap(), diagnostics, None)
            .await;

        eprintln!("[DEBUG] publish_diagnostics() completed");
    }
}

fn bagel_error_to_diagnostic(text: &str, error: BagelError) -> Diagnostic {
    use crate::check::BagelErrorDetails;
    use crate::config::RuleSeverity;

    let severity = match error.severity {
        RuleSeverity::Error => DiagnosticSeverity::ERROR,
        RuleSeverity::Warn => DiagnosticSeverity::WARNING,
        RuleSeverity::Autofix => DiagnosticSeverity::HINT,
    };

    let message = match error.details {
        BagelErrorDetails::ParseError { message } => message,
        BagelErrorDetails::MiscError { message } => message,
    };

    // Convert slice to LSP range
    let start_pos = offset_to_position(text, error.src.start);
    let end_pos = offset_to_position(text, error.src.end);

    Diagnostic {
        range: Range {
            start: start_pos,
            end: end_pos,
        },
        severity: Some(severity),
        code: None,
        code_description: None,
        source: Some("bagel".to_string()),
        message,
        related_information: None,
        tags: None,
        data: None,
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
    eprintln!(
        "[DEBUG] find_node_at_offset() - checking node at {}..{}, offset={}",
        slice.start, slice.end, offset
    );

    // Check if offset is within this node's range
    if offset < slice.start || offset > slice.end {
        eprintln!(
            "[DEBUG] find_node_at_offset() - offset {} outside node range {}..{}",
            offset, slice.start, slice.end
        );
        return None;
    }

    // Try to find a more specific child node, or return this node if malformed/leaf
    match ast.details() {
        // Malformed nodes can't be traversed, return as-is
        None => {
            eprintln!("[DEBUG] find_node_at_offset() - node is malformed");
            Some(ast.clone())
        }

        // Try to find more specific child nodes
        Some(details) => {
            let child = match details {
                Any::Module(module) => {
                    eprintln!(
                        "[DEBUG] find_node_at_offset() - node is Module with {} declarations",
                        module.declarations.len()
                    );
                    // Check each declaration
                    module
                        .declarations
                        .iter()
                        .enumerate()
                        .find_map(|(idx, decl)| {
                            eprintln!(
                                "[DEBUG] find_node_at_offset() - checking declaration {}",
                                idx
                            );
                            find_node_at_offset(&decl.clone().upcast(), offset).inspect(|_| {
                                eprintln!(
                                    "[DEBUG] find_node_at_offset() - found in declaration {}",
                                    idx
                                )
                            })
                        })
                }
                Any::Declaration(decl) => {
                    eprintln!("[DEBUG] find_node_at_offset() - node is Declaration");
                    match decl {
                        Declaration::ConstDeclaration(const_decl) => find_node_at_offset(
                            &const_decl.value.clone().upcast(),
                            offset,
                        )
                        .inspect(|_| {
                            eprintln!("[DEBUG] find_node_at_offset() - found in declaration value")
                        }),
                        Declaration::ImportDeclaration(_) => None,
                    }
                }
                Any::Expression(expr) => {
                    eprintln!(
                        "[DEBUG] find_node_at_offset() - node is Expression: {:?}",
                        std::mem::discriminant(expr)
                    );
                    match expr {
                        crate::ast::grammar::Expression::BinaryOperation(bin_op) => {
                            eprintln!(
                                "[DEBUG] find_node_at_offset() - Expression is BinaryOperation"
                            );
                            // Check left operand, then right operand
                            find_node_at_offset(&bin_op.left.clone().upcast(), offset)
                                .inspect(|_| eprintln!("[DEBUG] find_node_at_offset() - found in left operand"))
                                .or_else(|| {
                                    find_node_at_offset(&bin_op.right.clone().upcast(), offset)
                                        .inspect(|_| eprintln!("[DEBUG] find_node_at_offset() - found in right operand"))
                                })
                        }
                        _ => {
                            eprintln!("[DEBUG] find_node_at_offset() - Expression is a leaf type");
                            None
                        }
                    }
                }
                _ => {
                    eprintln!("[DEBUG] find_node_at_offset() - node is other type");
                    None
                }
            };

            // If no child contains the offset, return this node
            child.or_else(|| {
                eprintln!("[DEBUG] find_node_at_offset() - no child found, returning this node");
                Some(ast.clone())
            })
        }
    }
}

pub async fn run_lsp() {
    eprintln!("[DEBUG] Bagel Language Server starting");
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| BagelLanguageServer {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
