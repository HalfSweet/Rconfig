use std::sync::Arc;
use std::time::Duration;

use tokio::sync::Mutex;
use tower_lsp::jsonrpc::Result;
use tower_lsp::lsp_types::*;
use tower_lsp::{Client, LanguageServer};

use crate::document::{DocumentKind, DocumentStore};

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

    async fn clear_diagnostics(&self, uri: Url) {
        let _ = self.client.publish_diagnostics(uri.clone(), Vec::new(), None).await;

        let mut state = self.state.lock().await;
        state.documents.published_diagnostic_uris.remove(&uri);
    }

    async fn reanalyze_uri(&self, uri: Url, reason: &'static str) {
        let _ = self
            .client
            .log_message(MessageType::LOG, format!("reanalyze {} ({reason})", uri))
            .await;
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
            self.reanalyze_uri(uri, "didChange").await;
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

        self.reanalyze_uri(uri, "didOpen").await;
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
        self.reanalyze_uri(params.text_document.uri, "didSave").await;
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let uri = params.text_document.uri;
        let closed_kind = {
            let mut state = self.state.lock().await;
            state.documents.close_document(&uri).map(|doc| doc.kind)
        };

        if matches!(closed_kind, Some(DocumentKind::Values)) {
            self.clear_diagnostics(uri).await;
            return;
        }

        self.reanalyze_uri(uri, "didClose").await;
    }

    async fn hover(&self, _: HoverParams) -> Result<Option<Hover>> {
        Ok(None)
    }

    async fn goto_definition(
        &self,
        _: GotoDefinitionParams,
    ) -> Result<Option<GotoDefinitionResponse>> {
        Ok(None)
    }

    async fn completion(&self, _: CompletionParams) -> Result<Option<CompletionResponse>> {
        Ok(None)
    }

    async fn document_symbol(
        &self,
        _: DocumentSymbolParams,
    ) -> Result<Option<DocumentSymbolResponse>> {
        Ok(None)
    }
}
