//! Core language crate for Rconfig.

pub mod error;
pub mod span;

pub use error::{Diagnostic, Severity};
pub use span::{Span, Spanned};
