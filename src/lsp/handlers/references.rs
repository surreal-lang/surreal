//! Find all references handler.

use tower_lsp::lsp_types::{Location, Position, Url};

use crate::compiler::Module;
use crate::lsp::lookup::find_all_references;
use crate::lsp::position::LineIndex;

/// Handle find all references request.
///
/// Finds all references to the symbol at the given position.
pub fn handle_references(
    module: &Module,
    line_index: &LineIndex,
    position: Position,
    uri: &Url,
    include_declaration: bool,
) -> Option<Vec<Location>> {
    let offset = line_index.position_to_offset(position)?;

    // Find all references to the symbol at this position
    let references = find_all_references(module, offset, include_declaration);

    if references.is_empty() {
        return None;
    }

    // Convert spans to Locations
    let locations: Vec<Location> = references
        .into_iter()
        .filter_map(|ref_info| {
            let range = line_index.span_to_range(ref_info.span)?;
            Some(Location {
                uri: uri.clone(),
                range,
            })
        })
        .collect();

    if locations.is_empty() {
        None
    } else {
        Some(locations)
    }
}
