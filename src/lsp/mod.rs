//! Language Server Protocol (LSP) implementation for Surreal.
//!
//! This module provides IDE features like diagnostics, hover, go-to-definition,
//! and completions through the standard LSP interface.

mod analysis;
mod backend;
mod document;
mod handlers;
mod lookup;
mod position;

pub use backend::SurrealLanguageServer;

use tokio::io::{stdin, stdout};
use tower_lsp::{LspService, Server};

/// Run the LSP server on stdin/stdout.
pub async fn run_server() {
    let (service, socket) = LspService::new(|client| SurrealLanguageServer::new(client));
    Server::new(stdin(), stdout(), socket).serve(service).await;
}
