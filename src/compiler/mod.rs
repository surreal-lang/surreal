//! Compiler for the ToyBEAM language.
//!
//! This module contains the lexer, parser, AST types, and code generator
//! for a Rust-like language that compiles to ToyBEAM bytecode.

mod ast;
mod codegen;
mod error;
mod lexer;
mod parser;
mod token;

pub use ast::*;
pub use codegen::{compile, Codegen, CodegenError, CodegenResult};
pub use error::{ParseError, ParseResult};
pub use lexer::Lexer;
pub use parser::Parser;
pub use token::Token;
