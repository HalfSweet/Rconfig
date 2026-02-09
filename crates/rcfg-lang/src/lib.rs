//! Core language crate for Rconfig.

pub mod ast;
pub mod error;
pub mod span;

pub use ast::*;
pub use error::{Diagnostic, Severity};
pub use span::{Span, Spanned};
