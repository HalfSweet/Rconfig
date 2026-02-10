use tower_lsp::lsp_types::{GotoDefinitionResponse, Location, Position, Url};

use crate::document::ProjectSnapshot;
use crate::position::{lsp_position_to_offset, span_to_lsp_range};

pub fn provide(
    project: &ProjectSnapshot,
    uri: &Url,
    position: Position,
) -> Option<GotoDefinitionResponse> {
    let analysis = project.analysis.as_ref()?;
    let source = project.schema_docs.get(uri)?;
    let local_offset = lsp_position_to_offset(source, position);

    if let Some(binding) = analysis
        .alias_bindings
        .iter()
        .find(|binding| binding.uri == *uri && binding.contains_local_offset(local_offset))
        && let Some(location) = definition_location(project, &binding.target_path)
    {
        return Some(GotoDefinitionResponse::Scalar(location));
    }

    let base_offset = project.uri_base_offsets.get(uri).copied().unwrap_or(0);
    let global_offset = local_offset + base_offset;

    let occurrence = analysis
        .position_index
        .find_symbol_at_offset(global_offset)?;

    if let Some(location) = definition_location(project, &occurrence.path) {
        return Some(GotoDefinitionResponse::Scalar(location));
    }

    None
}

fn definition_location(project: &ProjectSnapshot, path: &str) -> Option<Location> {
    let analysis = project.analysis.as_ref()?;

    let span = analysis
        .symbols
        .symbol_span(path)
        .or_else(|| analysis.symbols.option_span(path))
        .or_else(|| analysis.symbols.enum_variant_span(path))?;

    for file in &analysis.schema_files {
        if !file.contains_global_offset(span.start) {
            continue;
        }

        let local_span = rcfg_lang::Span::new(
            file.global_to_local_offset(span.start),
            file.global_to_local_offset(span.end),
        );

        return Some(Location {
            uri: file.uri.clone(),
            range: span_to_lsp_range(file.text.as_str(), local_span),
        });
    }

    None
}
