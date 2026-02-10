use tower_lsp::lsp_types::GotoDefinitionResponse;

pub fn provide() -> Option<GotoDefinitionResponse> {
    None
}
