use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer, LspService, Server};

use crate::ast::container::{find_deepest, walk_ast, WalkAction, AST};
use crate::ast::grammar::{Any, Declaration, Expression};
use crate::ast::modules::{ModulePath, ModulesStore};
use crate::ast::slice::Slice;
use crate::check::{BagelError, CheckContext, Checkable};
use crate::config::Config;
use crate::emit::{EmitContext, Emittable};
use crate::types::infer::InferTypeContext;
use crate::types::{
    identifiers_in_scope, resolve_identifier, NormalizeContext, ResolvedIdentifier, Type,
};

#[derive(Debug)]
struct BagelLanguageServer {
    client: Client,
    documents: Arc<RwLock<HashMap<String, String>>>,
    modules: Arc<RwLock<ModulesStore>>,
}

#[tower_lsp::async_trait]
impl LanguageServer for BagelLanguageServer {
    async fn initialize(&self, params: InitializeParams) -> Result<InitializeResult> {
        eprintln!("[DEBUG] initialize() called");

        // Discover workspace root and load all .bgl files
        let workspace_root = params
            .workspace_folders
            .as_ref()
            .and_then(|folders| folders.first())
            .and_then(|folder| uri_to_path(folder.uri.as_str()))
            .or_else(|| {
                params
                    .root_uri
                    .as_ref()
                    .and_then(|u| uri_to_path(u.as_str()))
            });

        if let Some(root) = workspace_root {
            eprintln!("[DEBUG] initialize() - workspace root: {:?}", root);
            let pattern = root.join("**/*.bgl").to_string_lossy().to_string();
            let files: Vec<PathBuf> = glob::glob(&pattern)
                .into_iter()
                .flatten()
                .filter_map(|entry| entry.ok())
                .collect();
            eprintln!(
                "[DEBUG] initialize() - found {} .bgl files in workspace",
                files.len()
            );
            let mut store = self.modules.write().await;
            for file in files {
                eprintln!("[DEBUG] initialize() - loading {:?}", file);
                let _ = store.add_file(file);
            }
            eprintln!(
                "[DEBUG] initialize() - loaded {} modules",
                store.modules.len()
            );
        }
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
                definition_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Left(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_string()]),
                    ..Default::default()
                }),
                workspace: Some(WorkspaceServerCapabilities {
                    file_operations: Some(WorkspaceFileOperationsServerCapabilities {
                        did_create: Some(FileOperationRegistrationOptions {
                            filters: vec![FileOperationFilter {
                                scheme: Some("file".to_string()),
                                pattern: FileOperationPattern {
                                    glob: "**/*.bgl".to_string(),
                                    matches: None,
                                    options: None,
                                },
                            }],
                        }),
                        did_delete: Some(FileOperationRegistrationOptions {
                            filters: vec![FileOperationFilter {
                                scheme: Some("file".to_string()),
                                pattern: FileOperationPattern {
                                    glob: "**/*.bgl".to_string(),
                                    matches: None,
                                    options: None,
                                },
                            }],
                        }),
                        ..Default::default()
                    }),
                    ..Default::default()
                }),
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

        // Update the shared modules store
        if let Some(path) = uri_to_path(&uri) {
            let _ = self.modules.write().await.reload_file(&path, text.clone());
        }

        eprintln!("[DEBUG] did_open() completed - stored document for {}", uri);

        // Publish diagnostics
        self.publish_diagnostics(&uri).await;
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

            // Update the shared modules store
            if let Some(path) = uri_to_path(&uri) {
                let _ = self.modules.write().await.reload_file(&path, text.clone());
            }

            eprintln!(
                "[DEBUG] did_change() completed - updated document for {}",
                uri
            );

            // Publish diagnostics
            self.publish_diagnostics(&uri).await;
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

        // Re-publish diagnostics on save
        self.publish_diagnostics(&uri).await;
    }

    async fn formatting(&self, params: DocumentFormattingParams) -> Result<Option<Vec<TextEdit>>> {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] formatting() called - uri: {}", uri);

        let store = self.modules.read().await;
        let module = match uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p))) {
            Some(m) => m,
            None => return Ok(None),
        };
        let text = module.source.as_str().to_string();
        let ast = &module.ast;

        let config = Config::default();
        let mut formatted = String::new();
        {
            let ctx = EmitContext {
                config: &config,
                modules: &*store,
            };
            if ast.emit(ctx, &mut formatted).is_err() {
                return Ok(None);
            }
        }

        if formatted == text {
            return Ok(None);
        }

        let line_count = text.lines().count() as u32;
        let last_line_length = text.lines().last().map(|l| l.len()).unwrap_or(0) as u32;

        Ok(Some(vec![TextEdit {
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
            new_text: formatted,
        }]))
    }

    async fn did_create_files(&self, params: CreateFilesParams) {
        eprintln!("[DEBUG] did_create_files() called");
        let mut store = self.modules.write().await;
        for file in &params.files {
            if let Some(path) = uri_to_path(&file.uri) {
                eprintln!("[DEBUG] did_create_files() - adding {:?}", path);
                let _ = store.add_file(path);
            }
        }
    }

    async fn did_delete_files(&self, params: DeleteFilesParams) {
        eprintln!("[DEBUG] did_delete_files() called");
        let mut store = self.modules.write().await;
        for file in &params.files {
            if let Some(path) = uri_to_path(&file.uri) {
                eprintln!("[DEBUG] did_delete_files() - removing {:?}", path);
                store.remove_file(&path);
            }
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

        let store = self.modules.read().await;
        let module = match uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p))) {
            Some(m) => m,
            None => return Ok(None),
        };
        let text = module.source.as_str().to_string();
        let ast: AST<Any> = module.ast.clone().upcast();

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

            // Try to get the type: if this node is an expression, use it directly;
            // otherwise walk up to find the nearest ancestor expression (e.g.
            // PlainIdentifier -> LocalIdentifier or PropertyAccessExpression)
            let expr_node = node.clone().try_downcast::<Expression>().or_else(|| {
                let mut current = node.parent();
                while let Some(ancestor) = current {
                    if let Some(expr) = ancestor.clone().try_downcast::<Expression>() {
                        return Some(expr);
                    }
                    current = ancestor.parent();
                }
                None
            });
            let type_info = if let Some(expr) = expr_node {
                eprintln!("[DEBUG] hover() - node is an Expression, inferring type");
                let current_module =
                    uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p)));
                let ctx = InferTypeContext {
                    modules: Some(&*store),
                    current_module,
                };
                let norm_ctx = NormalizeContext {
                    modules: Some(&*store),
                    current_module,
                };
                let inferred_type = expr.infer_type(ctx).normalize(norm_ctx);
                eprintln!("[DEBUG] hover() - inferred type: {}", inferred_type);
                format!("```bagel\n{}\n```", inferred_type)
            } else {
                eprintln!("[DEBUG] hover() - node is not an Expression");
                String::new()
            };

            if !type_info.is_empty() {
                eprintln!("[DEBUG] hover() - returning hover info");
                return Ok(Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: type_info,
                    }),
                    range: None,
                }));
            }
        }

        eprintln!("[DEBUG] hover() - no node found at offset, returning None");
        Ok(None)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .to_string();
        let position = params.text_document_position_params.position;
        eprintln!(
            "[DEBUG] goto_definition() called - uri: {}, position: line={} char={}",
            uri, position.line, position.character
        );

        let store = self.modules.read().await;
        let module = match uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p))) {
            Some(m) => m,
            None => return Ok(None),
        };
        let text = module.source.as_str().to_string();
        let ast: AST<Any> = module.ast.clone().upcast();

        let offset = position_to_offset(&text, position);
        let node = match find_node_at_offset(&ast, offset) {
            Some(node) => node,
            None => return Ok(None),
        };

        let current_module =
            uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p)));

        // Check if the node is within an import declaration
        if let Some(decl) = node.clone().try_downcast::<Declaration>() {
            if let Some(Declaration::ImportDeclaration(import_decl)) = decl.unpack() {
                let Some(import_path) = import_decl
                    .path
                    .unpack()
                    .map(|p| p.contents.as_str().to_string())
                else {
                    return Ok(None);
                };

                // Check if the cursor is on the path string
                let path_slice = import_decl.path.slice();
                if offset >= path_slice.start && offset <= path_slice.end {
                    let link = current_module
                        .and_then(|m| store.find_imported(m, &import_path))
                        .map(|target_module| {
                            let target_uri = path_to_uri(&target_module.path);
                            let zero_pos = Position {
                                line: 0,
                                character: 0,
                            };
                            let zero_range = Range {
                                start: zero_pos,
                                end: zero_pos,
                            };
                            LocationLink {
                                origin_selection_range: Some(slice_to_lsp_range(
                                    &path_slice,
                                    &text,
                                )),
                                target_uri: target_uri.parse().unwrap(),
                                target_range: zero_range,
                                target_selection_range: zero_range,
                            }
                        });

                    eprintln!(
                        "[DEBUG] goto_definition() - import path resolved to {:?}",
                        link
                    );
                    return Ok(link.map(|l| GotoDefinitionResponse::Link(vec![l])));
                }

                // Check if the cursor is on an import specifier name
                let specifier_match = import_decl.imports.iter().find(|spec| {
                    let name_slice = spec.name.slice();
                    offset >= name_slice.start && offset <= name_slice.end
                });
                if let Some(spec) = specifier_match {
                    let original_name = spec.name.slice().as_str().to_string();
                    let link = current_module
                        .and_then(|m| store.find_imported(m, &import_path))
                        .and_then(|target_module| {
                            let target_text = target_module.source.as_str();
                            let target_uri = path_to_uri(&target_module.path);
                            let module_data = target_module.ast.unpack()?;
                            module_data
                                .declarations
                                .iter()
                                .filter_map(|d| match d.unpack() {
                                    Some(Declaration::ConstDeclaration(c)) => Some(c),
                                    _ => None,
                                })
                                .find(|c| c.identifier.slice().as_str() == original_name)
                                .map(|c| {
                                    let target_range =
                                        slice_to_lsp_range(&c.identifier.slice(), target_text);
                                    LocationLink {
                                        origin_selection_range: Some(slice_to_lsp_range(
                                            &spec.name.slice(),
                                            &text,
                                        )),
                                        target_uri: target_uri.parse().unwrap(),
                                        target_range,
                                        target_selection_range: target_range,
                                    }
                                })
                        });

                    eprintln!(
                        "[DEBUG] goto_definition() - import specifier resolved to {:?}",
                        link
                    );
                    return Ok(link.map(|l| GotoDefinitionResponse::Link(vec![l])));
                }
            }
        }

        // Handle LocalIdentifier expressions
        let expr = match node.clone().try_downcast::<Expression>() {
            Some(expr) => expr,
            None => return Ok(None),
        };
        let local_id = match expr.unpack() {
            Some(Expression::LocalIdentifier(id)) => id,
            _ => return Ok(None),
        };

        // Resolve the identifier using the shared resolution logic
        let ctx = NormalizeContext {
            modules: Some(&*store),
            current_module,
        };

        let resolved = match resolve_identifier(local_id.slice.as_str(), &node, ctx) {
            Some(r) => r,
            None => return Ok(None),
        };

        // Convert the resolved identifier to an LSP Location
        let location = match resolved {
            ResolvedIdentifier::ConstDeclaration { decl, module } => {
                let target_slice = decl.identifier.slice();
                match module {
                    Some(target_module) => {
                        let target_text = target_module.source.as_str();
                        let target_uri = path_to_uri(&target_module.path);
                        Location {
                            uri: target_uri.parse().unwrap(),
                            range: slice_to_lsp_range(&target_slice, target_text),
                        }
                    }
                    None => Location {
                        uri: uri.parse().unwrap(),
                        range: slice_to_lsp_range(&target_slice, &text),
                    },
                }
            }
            ResolvedIdentifier::FunctionParam { name, .. } => {
                let target_slice = name.slice();
                Location {
                    uri: uri.parse().unwrap(),
                    range: slice_to_lsp_range(&target_slice, &text),
                }
            }
        };

        eprintln!("[DEBUG] goto_definition() - resolved to {:?}", location);

        Ok(Some(GotoDefinitionResponse::Scalar(location)))
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri.to_string();
        let position = params.text_document_position.position;
        eprintln!(
            "[DEBUG] completion() called - uri: {}, position: line={} char={}",
            uri, position.line, position.character
        );

        let store = self.modules.read().await;
        let module = match uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p))) {
            Some(m) => m,
            None => return Ok(None),
        };
        let text = module.source.as_str().to_string();
        let ast: AST<Any> = module.ast.clone().upcast();

        // Convert LSP position to byte offset
        let cursor_offset = position_to_offset(&text, position);
        eprintln!("[DEBUG] completion() - cursor offset: {}", cursor_offset);

        // Scan backwards from cursor past any partial identifier the user may
        // have started typing, then check for a '.' character.
        let before_cursor = &text[..cursor_offset];
        let trimmed = before_cursor.trim_end_matches(|c: char| c.is_alphanumeric() || c == '_');
        if !trimmed.ends_with('.') {
            eprintln!("[DEBUG] completion() - no dot found, trying identifier completion");

            // Try identifier completion: find the node at the cursor and check
            // if it's a LocalIdentifier (or we're inside one)
            let node = find_node_at_offset(&ast, cursor_offset.saturating_sub(1));
            let ident_node = node.and_then(|n| {
                if matches!(
                    n.details(),
                    Some(Any::Expression(Expression::LocalIdentifier(_)))
                ) {
                    Some(n)
                } else {
                    let mut cur = n.parent();
                    while let Some(ancestor) = cur {
                        if matches!(
                            ancestor.details(),
                            Some(Any::Expression(Expression::LocalIdentifier(_)))
                        ) {
                            return Some(ancestor);
                        }
                        cur = ancestor.parent();
                    }
                    None
                }
            });

            let ident_node = match ident_node {
                Some(n) => n,
                None => return Ok(None),
            };

            let current_module =
                uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p)));
            let norm_ctx = NormalizeContext {
                modules: Some(&*store),
                current_module,
            };

            let mut items: Vec<CompletionItem> = identifiers_in_scope(&ident_node, norm_ctx)
                .iter()
                .map(|resolved| completion_item_for_identifier(resolved, norm_ctx))
                .collect();

            // Add the built-in `js` global
            items.push(CompletionItem {
                label: "js".to_string(),
                kind: Some(CompletionItemKind::MODULE),
                ..Default::default()
            });

            eprintln!(
                "[DEBUG] completion() - returning {} identifier completion items",
                items.len()
            );
            return Ok(Some(CompletionResponse::Array(items)));
        }
        let dot_offset = trimmed.len() - 1;

        eprintln!("[DEBUG] completion() - dot at {}", dot_offset);

        // Strategy: search the full AST for a PropertyAccessExpression whose
        // dot is at exactly `dot_offset`. This handles both cases:
        //   1. Parser greedily parsed `obj.\njs` as PropertyAccessExpression
        //      with subject "obj" — we extract the subject.
        //   2. Parser created a PropertyAccessExpression with malformed property
        //      (dot with no identifier after it) — we extract the subject.
        //
        // If no PropertyAccessExpression has its dot at this offset, fall back
        // to finding the node just before the dot and walking up to Expression.
        let subject_expr = match find_property_access_subject_at_dot(&ast, dot_offset) {
            Some(subject) => subject,
            None => {
                eprintln!(
                    "[DEBUG] completion() - no PropertyAccess with dot at offset, falling back"
                );
                let subject_offset = dot_offset.saturating_sub(1);
                let node = match find_node_at_offset(&ast, subject_offset) {
                    Some(n) => n,
                    None => {
                        eprintln!("[DEBUG] completion() - no node found at subject offset");
                        return Ok(None);
                    }
                };
                match node.clone().try_downcast::<Expression>().or_else(|| {
                    let mut current = node.parent();
                    while let Some(ancestor) = current {
                        if let Some(expr) = ancestor.clone().try_downcast::<Expression>() {
                            return Some(expr);
                        }
                        current = ancestor.parent();
                    }
                    None
                }) {
                    Some(e) => e,
                    None => {
                        eprintln!("[DEBUG] completion() - no Expression found");
                        return Ok(None);
                    }
                }
            }
        };

        eprintln!(
            "[DEBUG] completion() - subject expr: slice={:?}",
            subject_expr.slice().as_str(),
        );

        // Infer and normalize the type of the subject expression
        let current_module =
            uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p)));
        let ctx = InferTypeContext {
            modules: Some(&*store),
            current_module,
        };
        let norm_ctx = NormalizeContext {
            modules: Some(&*store),
            current_module,
        };
        let subject_type = subject_expr.infer_type(ctx).normalize(norm_ctx);
        eprintln!("[DEBUG] completion() - subject type: {}", subject_type);

        // Extract field names from the type
        let items = completion_items_for_type(&subject_type);

        if items.is_empty() {
            eprintln!("[DEBUG] completion() - no completion items");
            return Ok(None);
        }

        eprintln!(
            "[DEBUG] completion() - returning {} completion items",
            items.len()
        );
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri.to_string();
        eprintln!("[DEBUG] inlay_hint() called - uri: {}", uri);
        eprintln!("[DEBUG] inlay_hint() - range: {:?}", params.range);

        let store = self.modules.read().await;
        let module = match uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p))) {
            Some(m) => m,
            None => return Ok(None),
        };
        let text = module.source.as_str().to_string();

        let mut hints = Vec::new();
        let current_module =
            uri_to_path(&uri).and_then(|p| store.modules.get(&ModulePath::File(p)));
        let norm_ctx = NormalizeContext {
            modules: Some(&*store),
            current_module,
        };

        // Traverse the module's declarations
        eprintln!("[DEBUG] inlay_hint() - attempting to unpack Module");
        if let Some(module_data) = module.ast.unpack() {
            eprintln!(
                "[DEBUG] inlay_hint() - found Module with {} declarations",
                module_data.declarations.len()
            );

            for (idx, decl) in module_data
                .declarations
                .iter()
                .enumerate()
                .filter_map(|(idx, decl)| decl.unpack().map(|decl| (idx, decl)))
            {
                match decl {
                    Declaration::ConstDeclaration(decl_data) => {
                        if decl_data.type_annotation.is_none() {
                            eprintln!("[DEBUG] inlay_hint() - processing declaration {}: identifier at {}..{}",
                                        idx, decl_data.identifier.slice().start, decl_data.identifier.slice().end);

                            // Infer the type of the value
                            let ctx = InferTypeContext {
                                modules: Some(&*store),
                                current_module,
                            };
                            let inferred_type = decl_data.value.infer_type(ctx).normalize(norm_ctx);
                            eprintln!(
                                "[DEBUG] inlay_hint() - inferred type for decl {}: {}",
                                idx, inferred_type
                            );

                            // Get the position after the identifier
                            let identifier_slice = decl_data.identifier.slice();
                            let position = offset_to_position(&text, identifier_slice.end);
                            eprintln!(
                                "[DEBUG] inlay_hint() - hint position for decl {}: line={} char={}",
                                idx, position.line, position.character
                            );

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

                        // Collect parameter hints from function expressions in the value
                        collect_parameter_hints(&decl_data.value, &text, &mut hints);
                    }
                    Declaration::ImportDeclaration(_) => {
                        // No inlay hints for imports
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
    async fn publish_diagnostics(&self, uri: &str) {
        eprintln!("[DEBUG] publish_diagnostics() called - uri: {}", uri);

        let store = self.modules.read().await;
        let module = match uri_to_path(uri).and_then(|p| store.modules.get(&ModulePath::File(p))) {
            Some(m) => m,
            None => {
                eprintln!("[DEBUG] publish_diagnostics() - module not found in store");
                return;
            }
        };
        let text = module.source.as_str().to_string();
        let ast = &module.ast;

        // Run check to collect errors
        let config = Config::default();
        let ctx = CheckContext {
            config: &config,
            modules: &*store,
            current_module: Some(module),
        };
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
            .map(|error| bagel_error_to_diagnostic(uri, &text, error))
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

fn bagel_error_to_diagnostic(uri: &str, text: &str, error: BagelError) -> Diagnostic {
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

    let related_information = if error.related.is_empty() {
        None
    } else {
        Some(
            error
                .related
                .iter()
                .map(|rel| DiagnosticRelatedInformation {
                    location: Location {
                        uri: uri.parse().unwrap(),
                        range: Range {
                            start: offset_to_position(text, rel.src.start),
                            end: offset_to_position(text, rel.src.end),
                        },
                    },
                    message: rel.message.clone(),
                })
                .collect(),
        )
    };

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
        related_information,
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

/// Recursively walks an expression tree and collects inlay hints for function
/// parameters whose types can be inferred from context.
fn collect_parameter_hints(expr: &AST<Expression>, text: &str, hints: &mut Vec<InlayHint>) {
    walk_ast(&expr.clone().upcast(), &mut |node| {
        let Some(func_expr) = node.clone().try_downcast::<Expression>() else {
            return WalkAction::Continue;
        };
        let Some(Expression::FunctionExpression(func)) = func_expr.unpack() else {
            return WalkAction::Continue;
        };

        // Skip if all parameters already have type annotations
        if !func
            .parameters
            .iter()
            .all(|(_, type_ann)| type_ann.is_some())
        {
            // Get the contextual expected type for this function expression
            let norm_ctx = NormalizeContext {
                modules: None,
                current_module: None,
            };
            let expected_args =
                func_expr
                    .expected_type()
                    .and_then(|t| match t.normalize(norm_ctx) {
                        Type::FuncType { args, .. } => Some(args),
                        _ => None,
                    });

            if let Some(expected_args) = expected_args {
                func.parameters
                    .iter()
                    .enumerate()
                    .filter(|(_, (_, type_ann))| type_ann.is_none())
                    .for_each(|(i, (param_name, _))| {
                        if let Some(arg_type) = expected_args.get(i) {
                            if *arg_type != Type::Unknown {
                                let position = offset_to_position(text, param_name.slice().end);
                                hints.push(InlayHint {
                                    position,
                                    label: InlayHintLabel::String(format!(": {}", arg_type)),
                                    kind: Some(InlayHintKind::TYPE),
                                    text_edits: None,
                                    tooltip: None,
                                    padding_left: None,
                                    padding_right: None,
                                    data: None,
                                });
                            }
                        }
                    });
            }
        }

        WalkAction::Continue
    });
}

/// Search the AST for a PropertyAccessExpression whose dot is at `dot_offset`,
/// and return its subject expression. This handles the case where the parser
/// greedily parsed across a newline (e.g. `obj.\njs` → PropertyAccessExpression
/// { subject: "obj", property: "js" }).
fn find_property_access_subject_at_dot(
    ast: &AST<Any>,
    dot_offset: usize,
) -> Option<AST<Expression>> {
    let mut result: Option<AST<Expression>> = None;
    walk_ast(ast, &mut |node| {
        if let Some(expr) = node.clone().try_downcast::<Expression>() {
            if let Some(Expression::PropertyAccessExpression(pa)) = expr.unpack() {
                if pa.dot.start == dot_offset {
                    result = Some(pa.subject);
                    return WalkAction::Stop;
                }
            }
        }
        WalkAction::Continue
    });
    result
}

/// Given a normalized type, produce completion items for its fields.
fn completion_items_for_type(ty: &Type) -> Vec<CompletionItem> {
    match ty {
        Type::Object { fields } | Type::Interface { fields, .. } => fields
            .iter()
            .map(|(name, field_type)| CompletionItem {
                label: name.clone(),
                kind: Some(completion_item_kind_for_type(field_type)),
                ..Default::default()
            })
            .collect(),
        Type::Union { variants } => {
            // Collect the fields from every variant; only include names present
            // in all variants (intersection), using the first variant's type
            // for the CompletionItemKind.
            let field_maps: Vec<_> = variants
                .iter()
                .filter_map(|v| match v {
                    Type::Object { fields } | Type::Interface { fields, .. } => Some(fields),
                    _ => None,
                })
                .collect();

            if field_maps.is_empty() || field_maps.len() != variants.len() {
                return Vec::new();
            }

            let common_keys: std::collections::BTreeSet<_> = field_maps[0]
                .keys()
                .filter(|k| field_maps[1..].iter().all(|m| m.contains_key(*k)))
                .collect();

            common_keys
                .into_iter()
                .map(|name| CompletionItem {
                    label: name.clone(),
                    kind: Some(completion_item_kind_for_type(&field_maps[0][name])),
                    ..Default::default()
                })
                .collect()
        }
        _ => Vec::new(),
    }
}

/// Map a resolved identifier to a CompletionItem with a kind derived from the
/// identifier's fully-inferred, normalized type.
fn completion_item_for_identifier(
    resolved: &ResolvedIdentifier<'_>,
    ctx: NormalizeContext<'_>,
) -> CompletionItem {
    let ty = resolve_identifier_type(resolved, ctx);

    CompletionItem {
        label: resolved.name().to_string(),
        kind: Some(completion_item_kind_for_type(&ty)),
        ..Default::default()
    }
}

/// Infer and normalize the type of a resolved identifier, mirroring the logic
/// used when normalizing a `Type::LocalIdentifier`.
fn resolve_identifier_type(resolved: &ResolvedIdentifier<'_>, ctx: NormalizeContext<'_>) -> Type {
    match resolved {
        ResolvedIdentifier::ConstDeclaration { decl, module } => {
            let decl_ctx = match module {
                Some(m) => NormalizeContext {
                    modules: ctx.modules,
                    current_module: Some(m),
                },
                None => ctx,
            };
            let decl_infer_ctx = InferTypeContext {
                modules: decl_ctx.modules,
                current_module: decl_ctx.current_module,
            };
            decl.value.infer_type(decl_infer_ctx).normalize(decl_ctx)
        }
        ResolvedIdentifier::FunctionParam {
            param_index,
            func_node,
            ..
        } => match func_node.details() {
            Some(Any::Expression(Expression::FunctionExpression(func))) => {
                match &func.parameters[*param_index].1 {
                    Some((_colon, type_expr)) => {
                        type_expr.unpack().map(Type::from).unwrap_or(Type::Poisoned)
                    }
                    None => {
                        let func_expr_node: AST<Expression> = match func_node {
                            AST::Valid(inner, _) => AST::<Expression>::new(inner.clone()),
                            AST::Malformed(slice, message) => {
                                AST::Malformed(slice.clone(), message.clone())
                            }
                        };
                        func_expr_node
                            .expected_type()
                            .and_then(|t| match t.normalize(ctx) {
                                Type::FuncType { args, .. } => args.into_iter().nth(*param_index),
                                _ => None,
                            })
                            .unwrap_or(Type::Unknown)
                    }
                }
            }
            _ => Type::Unknown,
        },
    }
}

/// Map a normalized Type to the most appropriate LSP CompletionItemKind.
fn completion_item_kind_for_type(ty: &Type) -> CompletionItemKind {
    match ty {
        Type::FuncType { .. } => CompletionItemKind::FUNCTION,
        Type::Object { .. } => CompletionItemKind::STRUCT,
        Type::Interface { .. } => CompletionItemKind::MODULE,
        Type::Array { .. } | Type::Tuple { .. } => CompletionItemKind::VARIABLE,
        Type::Boolean { .. } | Type::Number { .. } | Type::String { .. } | Type::Nil => {
            CompletionItemKind::CONSTANT
        }
        Type::Union { .. } => CompletionItemKind::ENUM,
        _ => CompletionItemKind::VARIABLE,
    }
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
    find_deepest(ast, &|node| {
        let s = node.slice();
        offset >= s.start && offset <= s.end
    })
}

/// Convert an LSP document URI (e.g. "file:///path/to/foo.bgl") to a PathBuf.
fn uri_to_path(uri: &str) -> Option<PathBuf> {
    uri.strip_prefix("file://").map(|p| PathBuf::from(p))
}

/// Convert a ModulePath to an LSP document URI.
fn path_to_uri(path: &ModulePath) -> String {
    match path {
        ModulePath::File(p) => format!("file://{}", p.display()),
        ModulePath::Url(u) => u.clone(),
    }
}

/// Convert a Slice's byte range to an LSP Range using the source text.
fn slice_to_lsp_range(slice: &Slice, source_text: &str) -> Range {
    Range {
        start: offset_to_position(source_text, slice.start),
        end: offset_to_position(source_text, slice.end),
    }
}

pub async fn run_lsp() {
    eprintln!("[DEBUG] Bagel Language Server starting");
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(|client| BagelLanguageServer {
        client,
        documents: Arc::new(RwLock::new(HashMap::new())),
        modules: Arc::new(RwLock::new(ModulesStore::new())),
    });
    Server::new(stdin, stdout, socket).serve(service).await;
}
