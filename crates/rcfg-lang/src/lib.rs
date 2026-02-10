pub mod ast;
pub mod error;
pub mod lexer;
pub mod parser;
pub mod semantic;
pub mod span;

pub use ast::{
    Attr, AttrKind, ConstValue, EnumVariant, Expr, File, IntRange, Item, MatchCase, MatchPat,
    OptionAttachedConstraints, OptionDecl, Path, Type, UseStmt, ValueExpr, ValuesFile,
};
pub use error::{Diagnostic, DiagnosticArgValue, RelatedInfo, Severity};
pub use parser::{
    parse_schema, parse_schema_with_diagnostics, parse_values, parse_values_with_diagnostics,
};
pub use semantic::{
    ExportOptions, GeneratedExports, IntType, PlannedExport, ResolvedConfig, ResolvedOption,
    ResolvedValue, SemanticReport, SymbolInfo, SymbolKind, SymbolTable, ValueSource, ValueType,
    ValuesAnalysisReport, ValuesStmtOrigin, analyze_schema, analyze_schema_files,
    analyze_schema_files_strict, analyze_schema_strict, analyze_values, analyze_values_from_path,
    analyze_values_from_path_report, analyze_values_from_path_report_strict,
    analyze_values_from_path_report_with_context,
    analyze_values_from_path_report_with_context_strict, analyze_values_from_path_strict,
    analyze_values_strict, analyze_values_with_context, analyze_values_with_context_strict,
    expand_values_includes_from_path, expand_values_includes_with_origins, generate_exports,
    plan_c_header_exports, plan_c_header_exports_with_prefix, resolve_values,
    resolve_values_with_context,
};
pub use span::{Span, Spanned};
