//! Compiler for the Surreal language.
//!
//! This module contains the lexer, parser, AST types, and code generator
//! for a Rust-like language that compiles to Surreal bytecode.

mod ast;
pub mod ast_serde;
pub mod cfg;
mod codegen;
pub mod core_erlang;
pub mod derive;
mod error;
mod lexer;
mod loader;
pub mod macro_expander;
mod parser;
mod prelude;
pub mod quote_expand;
mod token;
pub mod typeck;

pub use ast::*;
pub use cfg::{
    get_derive_macro_name, get_proc_macro_derive_name, is_derive_macro, is_macro,
    is_proc_macro_derive, is_test, should_include,
};
pub use codegen::{Codegen, CodegenError, CodegenResult, compile, compile_file};
pub use core_erlang::{
    CoreErlangEmitter, CoreErlangError, GenericFunctionRegistry, SharedGenericRegistry,
    emit_core_erlang,
};
pub use derive::{DeriveError, MacroRegistry, expand_derives, expand_derives_with_registry};
pub use error::{
    CompilerError, CompilerWarning, ParseError, ParseResult, TypeError, TypeResult, Warning,
};
pub use lexer::{Lexer, Span};
pub use loader::{LoadError, LoadResult, ModuleLoader};
pub use macro_expander::{MacroError, MacroExpander, MacroResult};
pub use parser::Parser;
pub use quote_expand::expand_quotes;
pub use token::Token;
pub use typeck::{
    TypeCheckResult, check_module, check_modules, check_modules_with_metadata,
    resolve_stdlib_methods,
};
