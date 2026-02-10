use std::collections::HashSet;
use std::sync::Arc;
use std::time::Duration;

use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::document::{DocumentKind, DocumentStore};
use crate::providers::completion;
use crate::providers::diagnostics::{AnalysisTrigger, analyze_document};
use crate::providers::document_symbol;
use crate::providers::goto_def;
use crate::providers::hover;

#[derive(Debug, Default)]
struct ServerState {
    documents: DocumentStore,
}

#[derive(Debug, Clone)]
pub struct Backend {
    client: Client,
    state: Arc<Mutex<ServerState>>,
}

impl Backend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            state: Arc::new(Mutex::new(ServerState::default())),
        }
    }

    async fn publish_empty_diagnostics(&self, uri: Url) {
        let _ = self
            .client
            .publish_diagnostics(uri.clone(), Vec::new(), None)
            .await;
    }

    async fn reanalyze_uri(&self, uri: Url, trigger: AnalysisTrigger) {
        let (kind, open_documents) = {
            let state = self.state.lock().await;
            let kind = state
                .documents
                .get_document(&uri)
                .map(|doc| doc.kind)
                .unwrap_or_else(|| DocumentKind::from_uri(&uri));
            let open_documents = state.documents.open_documents().cloned().collect::<Vec<_>>();
            (kind, open_documents)
        };

        let result = analyze_document(&uri, kind, trigger, &open_documents);

        let mut new_uris = HashSet::new();
        for (target_uri, diagnostics) in &result.diagnostics_by_uri {
            new_uris.insert(target_uri.clone());
            let _ = self
                .client
                .publish_diagnostics(target_uri.clone(), diagnostics.clone(), None)
                .await;
        }

        if let Some(message) = result.parse_only_notice {
            let _ = self.client.show_message(MessageType::WARNING, message).await;
        }

        let mut stale_uris = Vec::new();
        {
            let mut state = self.state.lock().await;

            if let Some((project_key, snapshot)) = result.project {
                let stale = state
                    .documents
                    .projects
                    .get(&project_key)
                    .map(|old| {
                        old.schema_docs
                            .keys()
                            .filter(|uri| !new_uris.contains(*uri))
                            .cloned()
                            .collect::<Vec<_>>()
                    })
                    .unwrap_or_default();
                stale_uris.extend(stale);

                state.documents.projects.insert(project_key, snapshot);
            }

            if !new_uris.contains(&uri) {
                stale_uris.push(uri.clone());
            }

            state.documents.mark_diagnostics_published(new_uris);
        }

        stale_uris.sort();
        stale_uris.dedup();
        for stale_uri in stale_uris {
            self.publish_empty_diagnostics(stale_uri).await;
        }
    }

    async fn reanalyze_if_latest(&self, uri: Url, version: i32) {
        let is_latest = {
            let state = self.state.lock().await;
            state
                .documents
                .get_document(&uri)
                .is_some_and(|doc| doc.version == version)
        };

        if is_latest {
            self.reanalyze_uri(uri, AnalysisTrigger::DidChange).await;
        }
    }
}

#[tower_lsp::async_trait]
impl LanguageServer for Backend {
    async fn initialize(&self, _: InitializeParams) -> Result<InitializeResult> {
        Ok(InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![":".to_string()]),
                    ..CompletionOptions::default()
                }),
                document_symbol_provider: Some(OneOf::Left(true)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "rcfg-lsp".to_string(),
                version: Some(env!("CARGO_PKG_VERSION").to_string()),
            }),
        })
    }

    async fn initialized(&self, _: InitializedParams) {
        let _ = self
            .client
            .log_message(MessageType::INFO, "rcfg-lsp initialized")
            .await;
    }

    async fn shutdown(&self) -> Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let kind = DocumentKind::from_uri(&uri);
        let version = params.text_document.version;
        let text = params.text_document.text;

        {
            let mut state = self.state.lock().await;
            state.documents.upsert_document(uri.clone(), version, text, kind);
        }

        self.reanalyze_uri(uri, AnalysisTrigger::DidOpen).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        let version = params.text_document.version;

        let Some(change) = params.content_changes.last() else {
            return;
        };

        {
            let mut state = self.state.lock().await;
            let kind = state
                .documents
                .get_document(&uri)
                .map(|doc| doc.kind)
                .unwrap_or_else(|| DocumentKind::from_uri(&uri));
            state
                .documents
                .upsert_document(uri.clone(), version, change.text.clone(), kind);
        }

        let backend = self.clone();
        tokio::spawn(async move {
            tokio::time::sleep(Duration::from_millis(150)).await;
            backend.reanalyze_if_latest(uri, version).await;
        });
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        self.reanalyze_uri(params.text_document.uri, AnalysisTrigger::DidSave)
            .await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let closed_kind = {
            let mut state = self.state.lock().await;
            state.documents.close_document(&uri).map(|doc| doc.kind)
        };

        if matches!(closed_kind, Some(DocumentKind::Values)) {
            self.publish_empty_diagnostics(uri).await;
            return;
        }

        self.reanalyze_uri(uri, AnalysisTrigger::DidClose).await;
    }

    async fn hover(&self, params: HoverParams) -> Result<Option<Hover>> {
        let text_document_position = params.text_document_position_params;
        let uri = text_document_position.text_document.uri;
        let position = text_document_position.position;

        let state = self.state.lock().await;
        let output = state
            .documents
            .projects
            .values()
            .find_map(|project| hover::provide(project, &uri, position));
        Ok(output)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        let text_document_position = params.text_document_position_params;
        let uri = text_document_position.text_document.uri;
        let position = text_document_position.position;

        let state = self.state.lock().await;
        let output = state
            .documents
            .projects
            .values()
            .find_map(|project| goto_def::provide(project, &uri, position));
        Ok(output)
    }

    async fn completion(&self, params: CompletionParams) -> Result<Option<CompletionResponse>> {
        let text_document_position = params.text_document_position;
        let uri = text_document_position.text_document.uri;
        let position = text_document_position.position;

        let state = self.state.lock().await;
        let output = state
            .documents
            .projects
            .values()
            .find_map(|project| completion::provide(project, &uri, position));
        Ok(output)
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;

        let state = self.state.lock().await;
        let output = state
            .documents
            .projects
            .values()
            .find_map(|project| document_symbol::provide(project, &uri))
            .map(DocumentSymbolResponse::Nested);
        Ok(output)
    }
}
