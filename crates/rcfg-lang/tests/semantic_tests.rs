use std::path::PathBuf;

use rcfg_lang::parser::{parse_schema_with_diagnostics, parse_values_with_diagnostics};
use rcfg_lang::{
    BoolFalseExportStyle, DiagnosticArgValue, EnumExportStyle, ExportNameRule, ExportOptions,
    IntExportFormat, Severity, SymbolKind, SymbolTable, analyze_schema, analyze_schema_files,
    analyze_schema_strict, analyze_values, analyze_values_from_path,
    analyze_values_from_path_report, analyze_values_strict, expand_values_includes_from_path,
    expand_values_includes_from_path_with_root, expand_values_includes_with_origins,
    generate_exports, plan_c_header_exports, resolve_values,
};

fn symbols_from(src: &str) -> SymbolTable {
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );
    analyze_schema(&file).symbols
}

#[test]
fn allows_open_module_redeclaration() {
    let src = r#"
mod uart {
  option enable: bool = false;
}

mod uart {
  option baud: u32 = 115200;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_SYMBOL_REDEFINED" && diag.code != "E_SYMBOL_KIND_CONFLICT"),
        "semantic diagnostics: {:#?}",
        report.diagnostics
    );
    assert!(matches!(
        report.symbols.get("uart"),
        Some(info) if info.kind == SymbolKind::Mod
    ));
    assert!(matches!(
        report.symbols.get("uart::enable"),
        Some(info) if info.kind == SymbolKind::Option
    ));
    assert!(matches!(
        report.symbols.get("uart::baud"),
        Some(info) if info.kind == SymbolKind::Option
    ));
}

#[test]
fn symbol_table_associates_doc_comments_with_symbols() {
    let src = r#"
/// App module summary.
mod app {
  /// Mode summary.
  enum Mode {
    /// Fast variant summary.
    Fast,
  }

  /// Option summary.
  ///
  /// Option help.
  option enabled: bool = false;
}
"#;

    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);

    let module_docs = report
        .symbols
        .symbol_docs("app")
        .expect("expected module docs in symbol table");
    assert_eq!(module_docs[0].value, "App module summary.");

    let option_docs = report
        .symbols
        .symbol_docs("app::enabled")
        .expect("expected option docs in symbol table");
    assert_eq!(option_docs[0].value, "Option summary.");
    assert_eq!(option_docs[2].value, "Option help.");

    let variant_docs = report
        .symbols
        .enum_variant_docs("app::Mode::Fast")
        .expect("expected enum variant docs in symbol table");
    assert_eq!(variant_docs[0].value, "Fast variant summary.");
}

#[test]
fn symbol_table_tracks_symbol_references_and_position_lookup() {
    let src = r#"
mod app {
  option enabled: bool = false;

  when enabled {
    require!(enabled == true);
  }
}
"#;

    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    let references = report
        .symbols
        .symbol_references("app::enabled")
        .expect("expected references for app::enabled");
    assert!(references.len() >= 2, "references: {references:#?}");

    let index = report.symbols.build_position_index();

    let definition_offset = src
        .find("option enabled")
        .expect("missing declaration snippet")
        + "option ".len();
    let definition = index
        .find_symbol_at_offset(definition_offset)
        .expect("expected symbol at declaration offset");
    assert_eq!(definition.path, "app::enabled");
    assert_eq!(definition.role, rcfg_lang::SymbolOccurrenceRole::Definition);

    let reference_offset = src
        .find("when enabled")
        .expect("missing reference snippet")
        + "when ".len();
    let reference = index
        .find_symbol_at_offset(reference_offset)
        .expect("expected symbol at reference offset");
    assert_eq!(reference.path, "app::enabled");
    assert_eq!(reference.role, rcfg_lang::SymbolOccurrenceRole::Reference);
}

#[test]
fn symbol_table_tracks_enum_variant_references() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case on => { }
    case _ => { }
  }
}
"#;

    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    let references = report
        .symbols
        .symbol_references("app::Mode::on")
        .expect("expected references for enum variant");
    assert!(!references.is_empty(), "references: {references:#?}");
}

#[test]
fn symbol_table_tracks_mod_and_enum_declaration_spans() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option enabled: bool = false;
}
"#;

    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    let mod_span = report
        .symbols
        .symbol_span("app")
        .expect("expected module declaration span");
    let enum_span = report
        .symbols
        .symbol_span("app::Mode")
        .expect("expected enum declaration span");
    let option_span = report
        .symbols
        .symbol_span("app::enabled")
        .expect("expected option declaration span");

    assert!(mod_span.start <= enum_span.start, "{mod_span:#?} {enum_span:#?}");
    assert!(mod_span.end >= enum_span.end, "{mod_span:#?} {enum_span:#?}");
    assert!(mod_span.start <= option_span.start, "{mod_span:#?} {option_span:#?}");
    assert!(mod_span.end >= option_span.end, "{mod_span:#?} {option_span:#?}");
}

#[test]
fn analyze_schema_files_merges_items() {
    let src_a = r#"
mod uart {
  option enable: bool = false;
}
"#;
    let src_b = r#"
mod uart {
  option baud: u32 = 115200;
}
"#;

    let (file_a, parse_diags_a) = parse_schema_with_diagnostics(src_a);
    let (file_b, parse_diags_b) = parse_schema_with_diagnostics(src_b);
    assert!(
        parse_diags_a.is_empty(),
        "parse diagnostics: {parse_diags_a:#?}"
    );
    assert!(
        parse_diags_b.is_empty(),
        "parse diagnostics: {parse_diags_b:#?}"
    );

    let report = analyze_schema_files(&[file_a, file_b]);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_SYMBOL_REDEFINED" && diag.code != "E_SYMBOL_KIND_CONFLICT"),
        "semantic diagnostics: {:#?}",
        report.diagnostics
    );
    assert!(matches!(
        report.symbols.get("uart"),
        Some(info) if info.kind == SymbolKind::Mod
    ));
    assert!(matches!(
        report.symbols.get("uart::enable"),
        Some(info) if info.kind == SymbolKind::Option
    ));
    assert!(matches!(
        report.symbols.get("uart::baud"),
        Some(info) if info.kind == SymbolKind::Option
    ));
}

#[test]
fn reports_symbol_redefined() {
    let src = r#"
mod a {
  option x: bool = false;
}

mod a {
  option x: bool = true;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_SYMBOL_REDEFINED"),
        "expected E_SYMBOL_REDEFINED, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_symbol_kind_conflict() {
    let src = r#"
mod foo { }
option foo: bool = false;
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_SYMBOL_KIND_CONFLICT"),
        "expected E_SYMBOL_KIND_CONFLICT, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn checks_when_condition_must_be_bool() {
    let src = r#"
mod app {
  option retries: u32 = 3;
  when retries {
    option timeout: u32 = 100;
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_EXPECT_BOOL"),
        "expected E_EXPECT_BOOL, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn checks_binary_type_compatibility() {
    let src = r#"
mod app {
  option enabled: bool = false;
  option retries: u32 = 3;
  require!(enabled == retries);
}

"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_EXPECT_SAME_TYPE"),
        "expected E_EXPECT_SAME_TYPE, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn matches_builtin_uses_regex_semantics() {
    let src = r#"
mod app {
  option name: string = "uart0";
  require!(matches(name, "^uart[0-9]+$"));
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_REQUIRE_FAILED"),
        "unexpected E_REQUIRE_FAILED diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_relational_integer_type_mismatch() {
    let src = r#"
mod app {
  option small: u8 = 1;
  option wide: u32 = 2;
  require!(small < wide);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_EXPECT_SAME_TYPE"),
        "expected E_EXPECT_SAME_TYPE, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn allows_relational_with_typed_literal_context() {
    let src = r#"
mod app {
  option small: u8 = 1;
  require!(small < 2);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_EXPECT_SAME_TYPE" && diag.code != "E_TYPE_MISMATCH"),
        "unexpected integer type diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_relational_typed_literal_out_of_range() {
    let src = r#"
mod app {
  option small: u8 = 1;
  require!(small < 300);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_TYPE_MISMATCH"),
        "expected E_TYPE_MISMATCH, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_self_outside_attached_constraints() {
    let src = r#"
mod app {
  require!(self == 1);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_SELF_OUTSIDE_ATTACHED_BLOCK"),
        "expected E_SELF_OUTSIDE_ATTACHED_BLOCK, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_non_exhaustive_match() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = Mode::off;

  match mode {
    case Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "W_NON_EXHAUSTIVE_MATCH"),
        "expected W_NON_EXHAUSTIVE_MATCH, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn match_guard_false_does_not_fallthrough_to_later_case() {
    let schema_src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case Mode::off if false => {
      require!(false);
    }
    case Mode::off => {
      require!(true);
    }
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .all(|diag| diag.code != "E_REQUIRE_FAILED"),
        "guard=false should not fallthrough to later cases: {semantic_diags:#?}"
    );
}

#[test]
fn reports_match_scrutinee_not_always_active() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option gate: bool = false;

  when gate {
    option mode: Mode = off;
  }

  match mode {
    case Mode::off => { }
    case Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE"),
        "expected E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_match_overlap() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case Mode::off => { }
    case Mode::off | Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_MATCH_OVERLAP"),
        "expected E_MATCH_OVERLAP, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_unreachable_case_after_wildcard() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case _ => { }
    case Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "W_UNREACHABLE_CASE"),
        "expected W_UNREACHABLE_CASE, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn values_detect_type_mismatch() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::enabled = 1;
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_TYPE_MISMATCH"),
        "expected E_TYPE_MISMATCH, got: {semantic_diags:#?}"
    );
}

#[test]
fn values_reports_type_mismatch_for_out_of_type_bounds_assignment() {
    let schema_src = r#"
mod app {
  option channel: u8 = 1;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::channel = 300;
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_TYPE_MISMATCH"),
        "expected E_TYPE_MISMATCH, got: {semantic_diags:#?}"
    );
}

#[test]
fn values_detect_ctx_assignment() {
    let schema_src = r#"
mod ctx {
  option arch: string;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
ctx::arch = "arm";
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_CONTEXT_ASSIGNMENT_NOT_ALLOWED"),
        "expected E_CONTEXT_ASSIGNMENT_NOT_ALLOWED, got: {semantic_diags:#?}"
    );
}

#[test]
fn values_support_use_alias_for_assignment_target() {
    let schema_src = r#"
mod sdk {
  mod usart1 {
    option enable: bool = false;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
use sdk::usart1 as uart;
uart::enable = true;
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags.is_empty(),
        "expected no diagnostics, got: {semantic_diags:#?}"
    );
}

#[test]
fn schema_use_alias_supports_cross_package_require_reference() {
    let src = r#"
mod hal_uart {
  option enabled: bool = false;
}

mod app {
  use hal_uart as uart;
  option boot: bool = false;
  require!(uart::enabled == false);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_SYMBOL_NOT_FOUND" && diag.code != "E_AMBIGUOUS_PATH"),
        "unexpected diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn schema_use_alias_supports_cross_package_when_condition() {
    let src = r#"
mod hal_uart {
  option enabled: bool = true;
}

mod app {
  use hal_uart as uart;
  when uart::enabled {
    option boot: bool = false;
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_SYMBOL_NOT_FOUND" && diag.code != "E_AMBIGUOUS_PATH"),
        "unexpected diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn schema_use_alias_supports_cross_package_match_reference() {
    let src = r#"
mod hal_uart {
  enum Mode { off, on }
  option mode: Mode = Mode::off;
}

mod app {
  use hal_uart as uart;
  match uart::mode {
    case uart::Mode::off => {
      option boot: bool = false;
    }
    case uart::Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report.diagnostics.iter().all(|diag| {
            diag.code != "E_SYMBOL_NOT_FOUND"
                && diag.code != "E_AMBIGUOUS_PATH"
                && diag.code != "E_MATCH_SCRUTINEE_NOT_ALWAYS_ACTIVE"
        }),
        "unexpected diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn values_report_value_path_resolves_to_option() {
    let schema_src = r#"
mod app {
  option a: bool = false;
  option b: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::a = app::b;
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_VALUE_PATH_RESOLVES_TO_OPTION"),
        "expected E_VALUE_PATH_RESOLVES_TO_OPTION, got: {semantic_diags:#?}"
    );
}

#[test]
fn values_env_reports_missing_var() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);
    let missing = "RCFG_TEST_MISSING_ENV";
    unsafe {
        std::env::remove_var(missing);
    }

    let values_src = format!("app::enabled = env(\"{}\");", missing);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_ENV_NOT_SET"),
        "expected E_ENV_NOT_SET, got: {semantic_diags:#?}"
    );
}

#[test]
fn values_env_fallback_uses_value_when_variable_missing() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);
    let missing = "RCFG_TEST_ENV_FALLBACK_BOOL";
    unsafe {
        std::env::remove_var(missing);
    }

    let values_src = format!("app::enabled = env(\"{}\", \"true\");", missing);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .all(|diag| diag.code != "E_ENV_NOT_SET" && diag.code != "E_TYPE_MISMATCH"),
        "unexpected diagnostics: {semantic_diags:#?}"
    );
}

#[test]
fn values_env_fallback_still_checks_target_type() {
    let schema_src = r#"
mod app {
  option retries: u32 = 3;
}
"#;
    let symbols = symbols_from(schema_src);
    let missing = "RCFG_TEST_ENV_FALLBACK_INT";
    unsafe {
        std::env::remove_var(missing);
    }

    let values_src = format!("app::retries = env(\"{}\", \"oops\");", missing);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_ENV_PARSE_FAILED"),
        "expected E_ENV_PARSE_FAILED, got: {semantic_diags:#?}"
    );
}

#[test]
fn values_env_reports_parse_failure_for_bool() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);
    let key = "RCFG_TEST_BOOL_ENV";
    unsafe {
        std::env::set_var(key, "TRUE");
    }

    let values_src = format!("app::enabled = env(\"{}\");", key);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_ENV_PARSE_FAILED"),
        "expected E_ENV_PARSE_FAILED, got: {semantic_diags:#?}"
    );

    unsafe {
        std::env::remove_var(key);
    }
}

#[test]
fn values_env_parses_enum_variant_short_name() {
    let schema_src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;
}
"#;
    let symbols = symbols_from(schema_src);
    let key = "RCFG_TEST_ENUM_ENV_SHORT";
    unsafe {
        std::env::set_var(key, "on");
    }

    let values_src = format!("app::mode = env(\"{}\");", key);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .all(|diag| diag.code != "E_ENV_PARSE_FAILED" && diag.code != "E_TYPE_MISMATCH"),
        "unexpected diagnostics: {semantic_diags:#?}"
    );

    unsafe {
        std::env::remove_var(key);
    }
}

#[test]
fn values_env_parses_enum_variant_fully_qualified_name() {
    let schema_src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;
}
"#;
    let symbols = symbols_from(schema_src);
    let key = "RCFG_TEST_ENUM_ENV_FQ";
    unsafe {
        std::env::set_var(key, "app::Mode::on");
    }

    let values_src = format!("app::mode = env(\"{}\");", key);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .all(|diag| diag.code != "E_ENV_PARSE_FAILED" && diag.code != "E_TYPE_MISMATCH"),
        "unexpected diagnostics: {semantic_diags:#?}"
    );

    unsafe {
        std::env::remove_var(key);
    }
}

#[test]
fn values_env_reports_parse_failure_for_enum() {
    let schema_src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;
}
"#;
    let symbols = symbols_from(schema_src);
    let key = "RCFG_TEST_ENUM_ENV_BAD";
    unsafe {
        std::env::set_var(key, "invalid");
    }

    let values_src = format!("app::mode = env(\"{}\");", key);
    let (values, diags) = parse_values_with_diagnostics(&values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "E_ENV_PARSE_FAILED"),
        "expected E_ENV_PARSE_FAILED, got: {semantic_diags:#?}"
    );

    unsafe {
        std::env::remove_var(key);
    }
}

#[test]
fn values_warn_duplicate_assignments() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::enabled = true;
app::enabled = false;
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    assert!(
        semantic_diags
            .iter()
            .any(|diag| diag.code == "W_DUPLICATE_ASSIGNMENT"),
        "expected W_DUPLICATE_ASSIGNMENT, got: {semantic_diags:#?}"
    );

    let duplicate = semantic_diags
        .iter()
        .find(|diag| diag.code == "W_DUPLICATE_ASSIGNMENT")
        .expect("missing W_DUPLICATE_ASSIGNMENT");
    assert!(
        duplicate
            .note
            .as_deref()
            .is_some_and(|note| note.contains("fix-it:")),
        "expected fix-it note in duplicate assignment diagnostic: {duplicate:#?}"
    );
}

#[test]
fn include_expander_reports_not_found() {
    let missing = PathBuf::from("/tmp/rcfg-not-found-include.rcfgv");
    let (_values, diags) = expand_values_includes_from_path(&missing);
    assert!(
        diags.iter().any(|diag| diag.code == "E_INCLUDE_NOT_FOUND"),
        "expected E_INCLUDE_NOT_FOUND, got: {diags:#?}"
    );
    let diag = diags
        .iter()
        .find(|diag| diag.code == "E_INCLUDE_NOT_FOUND")
        .expect("missing E_INCLUDE_NOT_FOUND");
    assert!(diag.source.is_some(), "expected source in diagnostic");
    assert!(
        !diag.include_chain.is_empty(),
        "expected include chain in diagnostic"
    );
    assert!(
        diag.note
            .as_deref()
            .is_some_and(|note| note.contains("fix-it:")),
        "expected fix-it note for missing include diagnostic: {diag:#?}"
    );
}

#[test]
fn include_expander_reports_cycle() {
    let tmp = std::env::temp_dir().join(format!("rcfg_include_cycle_{}", std::process::id()));
    let _ = std::fs::create_dir_all(&tmp);

    let a = tmp.join("a.rcfgv");
    let b = tmp.join("b.rcfgv");
    std::fs::write(&a, "include \"b.rcfgv\";\n").expect("write a");
    std::fs::write(&b, "include \"a.rcfgv\";\n").expect("write b");

    let (_values, diags) = expand_values_includes_from_path(&a);
    assert!(
        diags.iter().any(|diag| diag.code == "E_INCLUDE_CYCLE"),
        "expected E_INCLUDE_CYCLE, got: {diags:#?}"
    );
    let diag = diags
        .iter()
        .find(|diag| diag.code == "E_INCLUDE_CYCLE")
        .expect("missing E_INCLUDE_CYCLE");
    assert!(diag.source.is_some(), "expected source in diagnostic");
    assert!(
        diag.include_chain.len() >= 2,
        "expected include chain with at least two nodes"
    );

    let _ = std::fs::remove_file(&a);
    let _ = std::fs::remove_file(&b);
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn include_expander_provides_stmt_origins() {
    let tmp = std::env::temp_dir().join(format!("rcfg_stmt_origin_{}", std::process::id()));
    let _ = std::fs::create_dir_all(&tmp);

    let base = tmp.join("base.rcfgv");
    let root = tmp.join("root.rcfgv");
    std::fs::write(&base, "app::enabled = true;\n").expect("write base");
    std::fs::write(&root, "include \"base.rcfgv\";\n").expect("write root");

    let (values, origins, diags) = expand_values_includes_with_origins(&root);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");
    assert_eq!(values.stmts.len(), 1);
    assert_eq!(origins.len(), 1);
    assert!(origins[0].source.ends_with("base.rcfgv"));
    assert!(
        origins[0]
            .include_chain
            .iter()
            .any(|item| item.ends_with("root.rcfgv")),
        "expected include chain to contain root file"
    );

    let _ = std::fs::remove_file(&base);
    let _ = std::fs::remove_file(&root);
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn include_expander_supports_root_prefixed_paths() {
    let tmp = std::env::temp_dir().join(format!("rcfg_include_root_{}", std::process::id()));
    let _ = std::fs::create_dir_all(tmp.join("profiles"));

    let base = tmp.join("base.rcfgv");
    let profile = tmp.join("profiles/dev.rcfgv");
    std::fs::write(&base, "app::enabled = true;\n").expect("write base");
    std::fs::write(&profile, "include \"@root/base.rcfgv\";\n").expect("write profile");

    let (values, diags) = expand_values_includes_from_path_with_root(&profile, &tmp);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");
    assert_eq!(values.stmts.len(), 1);

    let rcfg_lang::ast::ValuesStmt::Assign(assign) = &values.stmts[0] else {
        panic!("expected assignment after include expansion");
    };
    assert_eq!(assign.path.to_string(), "app::enabled");

    let _ = std::fs::remove_file(&base);
    let _ = std::fs::remove_file(&profile);
    let _ = std::fs::remove_dir_all(tmp.join("profiles"));
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn analyze_values_from_path_runs_include_and_semantic_checks() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let tmp = std::env::temp_dir().join(format!("rcfg_values_from_path_{}", std::process::id()));
    let _ = std::fs::create_dir_all(&tmp);

    let base = tmp.join("base.rcfgv");
    let root = tmp.join("root.rcfgv");

    std::fs::write(&base, "app::enabled = 1;\n").expect("write base");
    std::fs::write(&root, "include \"base.rcfgv\";\n").expect("write root");

    let (_expanded, diags) = analyze_values_from_path(&root, &symbols);
    assert!(
        diags.iter().any(|diag| diag.code == "E_TYPE_MISMATCH"),
        "expected E_TYPE_MISMATCH, got: {diags:#?}"
    );

    let _ = std::fs::remove_file(&base);
    let _ = std::fs::remove_file(&root);
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn analyze_values_from_path_report_returns_origins() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let tmp = std::env::temp_dir().join(format!("rcfg_report_origin_{}", std::process::id()));
    let _ = std::fs::create_dir_all(&tmp);

    let base = tmp.join("base.rcfgv");
    let root = tmp.join("root.rcfgv");
    std::fs::write(&base, "app::enabled = true;\n").expect("write base");
    std::fs::write(&root, "include \"base.rcfgv\";\n").expect("write root");

    let report = analyze_values_from_path_report(&root, &symbols);
    assert_eq!(report.values.stmts.len(), 1);
    assert_eq!(report.stmt_origins.len(), 1);
    assert_eq!(
        report.diagnostics.len(),
        report.diagnostic_stmt_indexes.len()
    );
    assert!(report.stmt_origins[0].source.ends_with("base.rcfgv"));

    let _ = std::fs::remove_file(&base);
    let _ = std::fs::remove_file(&root);
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn values_report_maps_semantic_diag_to_stmt_index() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let tmp = std::env::temp_dir().join(format!("rcfg_diag_stmt_map_{}", std::process::id()));
    let _ = std::fs::create_dir_all(&tmp);

    let values = tmp.join("values.rcfgv");
    std::fs::write(&values, "app::enabled = 1;\n").expect("write values");

    let report = analyze_values_from_path_report(&values, &symbols);
    let index = report
        .diagnostics
        .iter()
        .position(|diag| diag.code == "E_TYPE_MISMATCH")
        .expect("expected E_TYPE_MISMATCH");
    assert_eq!(report.diagnostic_stmt_indexes[index], Some(0));

    let _ = std::fs::remove_file(&values);
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn include_expander_attaches_source_to_parse_diagnostics() {
    let tmp = std::env::temp_dir().join(format!("rcfg_parse_diag_source_{}", std::process::id()));
    let _ = std::fs::create_dir_all(&tmp);

    let bad = tmp.join("bad.rcfgv");
    std::fs::write(&bad, "include ;\n").expect("write bad");

    let (_values, diags) = expand_values_includes_from_path(&bad);
    let diag = diags
        .iter()
        .find(|diag| diag.code == "E_PARSE_EXPECTED_TOKEN")
        .expect("expected parse diagnostic");
    assert!(
        diag.source
            .as_deref()
            .is_some_and(|source| source.ends_with("bad.rcfgv")),
        "expected source to include bad.rcfgv, got: {diag:#?}"
    );
    assert!(
        !diag.include_chain.is_empty(),
        "expected include_chain in parse diagnostic"
    );

    let _ = std::fs::remove_file(&bad);
    let _ = std::fs::remove_dir(&tmp);
}

#[test]
fn values_type_mismatch_diag_contains_option_path() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::enabled = 1;
"#;
    let (values, diags) = parse_values_with_diagnostics(values_src);
    assert!(diags.is_empty(), "parse diagnostics: {diags:#?}");

    let semantic_diags = analyze_values(&values, &symbols);
    let diag = semantic_diags
        .iter()
        .find(|diag| diag.code == "E_TYPE_MISMATCH")
        .expect("missing E_TYPE_MISMATCH");
    assert_eq!(diag.path.as_deref(), Some("app::enabled"));
}

#[test]
fn reports_context_default_not_allowed() {
    let src = r#"
mod ctx {
  option arch: string = "arm";
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_CONTEXT_DEFAULT_NOT_ALLOWED"),
        "expected E_CONTEXT_DEFAULT_NOT_ALLOWED, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_default_type_mismatch() {
    let src = r#"
mod app {
  option retries: u32 = true;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_DEFAULT_TYPE_MISMATCH"),
        "expected E_DEFAULT_TYPE_MISMATCH, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_default_not_constant_for_option_path() {
    let src = r#"
mod app {
  option base: u8 = 1;
  option channel: u8 = base;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_DEFAULT_NOT_CONSTANT"),
        "expected E_DEFAULT_NOT_CONSTANT, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_default_out_of_range() {
    let src = r#"
mod app {
  option channel: u8 = 300;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_DEFAULT_OUT_OF_RANGE"),
        "expected E_DEFAULT_OUT_OF_RANGE, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_missing_value_for_active_option_without_default() {
    let schema_src = r#"
mod app {
  option enabled: bool;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    let diag = diagnostics
        .iter()
        .find(|diag| diag.code == "E_MISSING_VALUE")
        .expect("expected E_MISSING_VALUE");
    assert_eq!(diag.path.as_deref(), Some("app::enabled"));
}

#[test]
fn default_value_satisfies_missing_value_check() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code != "E_MISSING_VALUE"),
        "unexpected E_MISSING_VALUE diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn conditional_option_is_not_required_by_missing_value_check() {
    let schema_src = r#"
mod app {
  option gate: bool = false;
  when gate {
    option hidden: u32;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code != "E_MISSING_VALUE"),
        "unexpected E_MISSING_VALUE diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn warns_duplicate_mod_metadata() {
    let src = r#"
/// uart module metadata A
mod uart {
  option enable: bool = false;
}

/// uart module metadata B
mod uart {
  option baud: u32 = 115200;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "W_DUPLICATE_MOD_METADATA"),
        "expected W_DUPLICATE_MOD_METADATA, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_in_set_option_path_not_constant() {
    let src = r#"
mod app {
  option low: u32 = 1;
  option value: u32 = 3;

  require!(value in { low, 2, 3 });
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_IN_NOT_CONSTANT"),
        "expected E_IN_NOT_CONSTANT, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_untyped_int_literal_without_typed_context() {
    let src = r#"
when 1 < 2 {
  option enabled: bool = false;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_UNTYPED_INT_LITERAL"),
        "expected E_UNTYPED_INT_LITERAL, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_range_violation_for_assignment_literal() {
    let schema_src = r#"
mod app {
  #[range(1..=8)]
  option channel: u32 = 1;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::channel = 9;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code == "E_RANGE_VIOLATION"),
        "expected E_RANGE_VIOLATION, got: {diagnostics:#?}"
    );
}

#[test]
fn range_violation_diagnostic_includes_args() {
    let schema_src = r#"
mod app {
  #[range(1..=8)]
  option channel: u32 = 1;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::channel = 9;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    let diag = diagnostics
        .iter()
        .find(|diag| diag.code == "E_RANGE_VIOLATION")
        .expect("expected E_RANGE_VIOLATION");
    assert_eq!(diag.args.get("actual"), Some(&DiagnosticArgValue::Int(9)));
    assert_eq!(diag.args.get("min"), Some(&DiagnosticArgValue::Int(1)));
    assert_eq!(diag.args.get("max"), Some(&DiagnosticArgValue::Int(8)));
    assert_eq!(
        diag.args.get("inclusive"),
        Some(&DiagnosticArgValue::Bool(true))
    );
}

#[test]
fn range_violation_redacts_secret_actual_arg() {
    let schema_src = r#"
mod app {
  #[secret]
  #[range(1..=8)]
  option token: u32 = 1;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::token = 9;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    let diag = diagnostics
        .iter()
        .find(|diag| diag.code == "E_RANGE_VIOLATION")
        .expect("expected E_RANGE_VIOLATION");
    assert_eq!(
        diag.args.get("actual"),
        Some(&DiagnosticArgValue::String("[redacted]".to_string()))
    );
    assert_eq!(diag.args.get("min"), Some(&DiagnosticArgValue::Int(1)));
    assert_eq!(diag.args.get("max"), Some(&DiagnosticArgValue::Int(8)));
}

#[test]
fn accepts_assignment_within_range() {
    let schema_src = r#"
mod app {
  #[range(1..=8)]
  option channel: u32 = 1;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::channel = 8;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code != "E_RANGE_VIOLATION"),
        "unexpected E_RANGE_VIOLATION diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn warns_require_missing_msg_attribute() {
    let src = r#"
constraint {
  require!(true);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "L_REQUIRE_MISSING_MSG"),
        "expected L_REQUIRE_MISSING_MSG, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn require_with_msg_attribute_suppresses_lint() {
    let src = r#"
constraint {
  #[msg("app.require.ok")]
  require!(true);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "L_REQUIRE_MISSING_MSG"),
        "unexpected L_REQUIRE_MISSING_MSG diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn warns_option_missing_doc_comment() {
    let src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "L_MISSING_DOC"),
        "expected L_MISSING_DOC, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn documented_option_suppresses_missing_doc_lint() {
    let src = r#"
mod app {
  /// whether app is enabled
  option enabled: bool = false;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "L_MISSING_DOC"),
        "unexpected L_MISSING_DOC diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn warns_inactive_assignment_for_conditional_option() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
  when enabled {
    option hidden: u32;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::hidden = 42;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code == "W_INACTIVE_ASSIGNMENT"),
        "expected W_INACTIVE_ASSIGNMENT, got: {diagnostics:#?}"
    );
}

#[test]
fn reports_inactive_value_reference_in_when_condition() {
    let src = r#"
mod app {
  option enabled: bool = false;

  when enabled {
    option level: u32 = 1;
  }

  when level > 0 {
    option extra: bool = false;
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_INACTIVE_VALUE_REFERENCE"),
        "expected E_INACTIVE_VALUE_REFERENCE, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_circular_activation_dependencies() {
    let src = r#"
mod app {
  when b {
    option a: bool = false;
  }

  when a {
    option b: bool = false;
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    let cycle_diags = report
        .diagnostics
        .iter()
        .filter(|diag| diag.code == "E_CIRCULAR_ACTIVATION")
        .collect::<Vec<_>>();
    assert!(
        !cycle_diags.is_empty(),
        "expected E_CIRCULAR_ACTIVATION, got: {:#?}",
        report.diagnostics
    );
    assert!(
        cycle_diags
            .iter()
            .any(|diag| diag.path.as_deref() == Some("app::a")),
        "expected cycle diagnostic path app::a, got: {:#?}",
        cycle_diags
    );
    assert!(
        cycle_diags
            .iter()
            .any(|diag| diag.path.as_deref() == Some("app::b")),
        "expected cycle diagnostic path app::b, got: {:#?}",
        cycle_diags
    );
}

#[test]
fn does_not_report_missing_context_value_when_not_referenced() {
    let schema_src = r#"
mod ctx {
  option arch: string;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code != "E_MISSING_CONTEXT_VALUE"),
        "unexpected E_MISSING_CONTEXT_VALUE diagnostics: {diagnostics:#?}"
    );
}

#[test]
fn reports_missing_context_value_when_referenced() {
    let schema_src = r#"
mod ctx {
  option arch: string;
}

mod app {
  when ctx::arch == "arm" {
    option enabled: bool = false;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code == "E_MISSING_CONTEXT_VALUE"),
        "expected E_MISSING_CONTEXT_VALUE, got: {diagnostics:#?}"
    );
}

#[test]
fn strict_mode_upgrades_require_missing_msg_to_error() {
    let src = r#"
constraint {
  require!(true);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema_strict(&file);
    let diag = report
        .diagnostics
        .iter()
        .find(|diag| diag.code == "E_REQUIRE_MISSING_MSG")
        .expect("expected E_REQUIRE_MISSING_MSG");
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn strict_mode_upgrades_inactive_assignment_to_error() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
  when enabled {
    option hidden: u32;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::hidden = 42;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values_strict(&values, &symbols);
    let diag = diagnostics
        .iter()
        .find(|diag| diag.code == "E_INACTIVE_ASSIGNMENT")
        .expect("expected E_INACTIVE_ASSIGNMENT");
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn strict_mode_upgrades_duplicate_assignment_to_error() {
    let schema_src = r#"
mod app {
  option retries: u32 = 1;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::retries = 2;
app::retries = 3;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values_strict(&values, &symbols);
    let diag = diagnostics
        .iter()
        .find(|diag| diag.code == "E_DUPLICATE_ASSIGNMENT")
        .expect("expected E_DUPLICATE_ASSIGNMENT");
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn strict_mode_upgrades_non_exhaustive_match_to_error() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema_strict(&file);
    let diag = report
        .diagnostics
        .iter()
        .find(|diag| diag.code == "E_NON_EXHAUSTIVE_MATCH")
        .expect("expected E_NON_EXHAUSTIVE_MATCH");
    assert_eq!(diag.severity, Severity::Error);
}

#[test]
fn warns_unused_enum_variant() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "L_UNUSED_ENUM_VARIANT"),
        "expected L_UNUSED_ENUM_VARIANT, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn no_unused_enum_variant_when_all_referenced() {
    let src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;

  match mode {
    case Mode::off => { }
    case Mode::on => { }
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "L_UNUSED_ENUM_VARIANT"),
        "unexpected L_UNUSED_ENUM_VARIANT diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn reports_require_failed_for_statically_false_expr() {
    let src = r#"
constraint {
  require!(true && false);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_REQUIRE_FAILED"),
        "expected E_REQUIRE_FAILED, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn require_failure_includes_generated_message_key() {
    let src = r#"
mod app {
  constraint {
    require!(false);
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    let diag = report
        .diagnostics
        .iter()
        .find(|diag| diag.code == "E_REQUIRE_FAILED")
        .expect("expected E_REQUIRE_FAILED");
    assert_eq!(diag.message_key.as_deref(), Some("main.app.require.1"));
}

#[test]
fn require_failure_prefers_msg_attribute_as_message_key() {
    let src = r#"
mod app {
  constraint {
    #[msg("demo.app.require.custom")]
    require!(false);
  }
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    let diag = report
        .diagnostics
        .iter()
        .find(|diag| diag.code == "E_REQUIRE_FAILED")
        .expect("expected E_REQUIRE_FAILED");
    assert_eq!(diag.message_key.as_deref(), Some("demo.app.require.custom"));
}

#[test]
fn runtime_require_failure_includes_generated_message_key() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
  require!(enabled);
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let diagnostics = analyze_values(&values, &symbols);
    let diag = diagnostics
        .iter()
        .find(|diag| diag.code == "E_REQUIRE_FAILED")
        .expect("expected E_REQUIRE_FAILED");
    assert_eq!(diag.message_key.as_deref(), Some("main.app.require.1"));
}

#[test]
fn does_not_report_require_failed_for_statically_true_expr() {
    let src = r#"
constraint {
  require!(true || false);
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .all(|diag| diag.code != "E_REQUIRE_FAILED"),
        "unexpected E_REQUIRE_FAILED diagnostics: {:#?}",
        report.diagnostics
    );
}

#[test]
fn warns_secret_not_exported_without_flag() {
    let schema_src = r#"
mod app {
  #[secret]
  option token: string = "abc";
}
"#;
    let symbols = symbols_from(schema_src);

    let (planned, diagnostics) = plan_c_header_exports(&symbols, false);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code == "W_SECRET_NOT_EXPORTED"),
        "expected W_SECRET_NOT_EXPORTED, got: {diagnostics:#?}"
    );
    assert!(
        planned.iter().all(|entry| entry.path != "app::token"),
        "secret option should not be exported by default: {planned:#?}"
    );
}

#[test]
fn includes_secret_export_when_flag_enabled() {
    let schema_src = r#"
mod app {
  #[secret]
  option token: string = "abc";
}
"#;
    let symbols = symbols_from(schema_src);

    let (planned, diagnostics) = plan_c_header_exports(&symbols, true);
    assert!(
        diagnostics
            .iter()
            .all(|diag| diag.code != "W_SECRET_NOT_EXPORTED"),
        "unexpected W_SECRET_NOT_EXPORTED diagnostics: {diagnostics:#?}"
    );
    assert!(
        planned.iter().any(|entry| entry.path == "app::token"),
        "secret option should be exported when enabled: {planned:#?}"
    );
}

#[test]
fn reports_export_name_collision() {
    let schema_src = r#"
mod a_b {
  option c: bool = false;
}

mod a {
  option b_c: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let (_planned, diagnostics) = plan_c_header_exports(&symbols, true);
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag.code == "E_EXPORT_NAME_COLLISION"),
        "expected E_EXPORT_NAME_COLLISION, got: {diagnostics:#?}"
    );
}

#[test]
fn generates_c_and_cmake_exports_for_active_values() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
  option retries: u32 = 3;
  enum Mode { off, on }
  option mode: Mode = off;
}
"#;
    let symbols = symbols_from(schema_src);

    let values_src = r#"
app::enabled = true;
app::retries = 7;
app::mode = on;
"#;
    let (values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(&symbols, &resolved, &ExportOptions::default());

    assert!(
        exports
            .c_header
            .starts_with("#pragma once\n#ifndef RCFG_CONFIG_H\n#define RCFG_CONFIG_H\n/* Auto-generated by rcfg — do not edit */\n\n"),
        "expected c_header prelude, got: {}",
        exports.c_header
    );
    assert!(
        exports.c_header.ends_with("\n#endif /* RCFG_CONFIG_H */"),
        "expected c_header include guard tail, got: {}",
        exports.c_header
    );

    assert!(
        exports.c_header.contains("#define CONFIG_APP_ENABLED 1"),
        "expected bool define in c_header, got: {}",
        exports.c_header
    );
    assert!(
        exports.c_header.contains("#define CONFIG_APP_RETRIES 7"),
        "expected int define in c_header, got: {}",
        exports.c_header
    );
    assert!(
        exports.c_header.contains("#define CONFIG_APP_MODE_OFF 0")
            && exports.c_header.contains("#define CONFIG_APP_MODE_ON 1"),
        "expected enum one-hot macros in c_header, got: {}",
        exports.c_header
    );

    let expected_cmake_prelude = format!(
        "# Auto-generated by rcfg {} — do not edit\n\n",
        env!("CARGO_PKG_VERSION")
    );
    assert!(
        exports.cmake.starts_with(&expected_cmake_prelude),
        "expected cmake prelude, got: {}",
        exports.cmake
    );
    assert!(
        exports.cmake.contains("set(CFG_APP_ENABLED ON)"),
        "expected bool cmake export, got: {}",
        exports.cmake
    );
    assert!(
        exports.cmake.contains("set(CFG_APP_RETRIES 7)"),
        "expected int cmake export, got: {}",
        exports.cmake
    );
    assert!(
        exports
            .cmake
            .contains("set(CFG_APP_MODE \"app::Mode::on\")"),
        "expected enum cmake export, got: {}",
        exports.cmake
    );
}

#[test]
fn skips_inactive_option_in_exports() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
  when enabled {
    option hidden: u32 = 9;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("app::enabled = false;");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(&symbols, &resolved, &ExportOptions::default());
    assert!(
        !exports.c_header.contains("CONFIG_APP_HIDDEN"),
        "inactive option should not be exported in c_header: {}",
        exports.c_header
    );
    assert!(
        !exports.cmake.contains("CFG_APP_HIDDEN"),
        "inactive option should not be exported in cmake: {}",
        exports.cmake
    );
}

#[test]
fn exports_context_options_only_when_enabled() {
    let schema_src = r#"
mod ctx {
  option arch: string;
}

mod app {
  when ctx::arch == "arm" {
    option optimized: bool = true;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let mut context = std::collections::HashMap::new();
    context.insert(
        "ctx::arch".to_string(),
        rcfg_lang::ResolvedValue::String("arm".to_string()),
    );

    let resolved = rcfg_lang::resolve_values_with_context(&values, &symbols, &context);

    let exports_default = generate_exports(&symbols, &resolved, &ExportOptions::default());
    assert!(
        !exports_default.c_header.contains("CONFIG_CTX_ARCH"),
        "ctx options should be omitted by default: {}",
        exports_default.c_header
    );
    assert!(
        exports_default.c_header.contains("#define CONFIG_APP_OPTIMIZED 1"),
        "non-ctx options should still be exported: {}",
        exports_default.c_header
    );

    let exports_with_context = generate_exports(
        &symbols,
        &resolved,
        &ExportOptions {
            include_context: true,
            ..ExportOptions::default()
        },
    );

    assert!(
        exports_with_context
            .c_header
            .contains("#define CONFIG_CTX_ARCH \"arm\""),
        "expected ctx export in c_header: {}",
        exports_with_context.c_header
    );
    assert!(
        exports_with_context
            .cmake
            .contains("set(CFG_CTX_ARCH \"arm\")"),
        "expected ctx export in cmake: {}",
        exports_with_context.cmake
    );
}

#[test]
fn supports_custom_export_prefixes() {
    let schema_src = r#"
mod app {
  option enabled: bool = true;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(
        &symbols,
        &resolved,
        &ExportOptions {
            include_secrets: false,
            include_context: false,
            c_prefix: "MYCFG_".to_string(),
            cmake_prefix: "MY_".to_string(),
            bool_false_style: BoolFalseExportStyle::Omit,
            enum_export_style: EnumExportStyle::OneHot,
            int_export_format: IntExportFormat::Decimal,
            export_name_rule: ExportNameRule::PkgPath,
        },
    );

    assert!(
        exports.c_header.contains("#define MYCFG_APP_ENABLED 1"),
        "expected custom c prefix, got: {}",
        exports.c_header
    );
    assert!(
        exports.cmake.contains("set(MY_APP_ENABLED ON)"),
        "expected custom cmake prefix, got: {}",
        exports.cmake
    );
}

#[test]
fn exports_name_rule_path_only_strips_leading_segment() {
    let schema_src = r#"
mod demo {
  mod app {
    option enabled: bool = true;
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(
        &symbols,
        &resolved,
        &ExportOptions {
            export_name_rule: ExportNameRule::PathOnly,
            ..ExportOptions::default()
        },
    );

    assert!(
        exports.c_header.contains("#define CONFIG_APP_ENABLED 1"),
        "expected path-only c export name, got: {}",
        exports.c_header
    );
    assert!(
        !exports.c_header.contains("CONFIG_DEMO_APP_ENABLED"),
        "did not expect package-prefixed c export name, got: {}",
        exports.c_header
    );
    assert!(
        exports.cmake.contains("set(CFG_APP_ENABLED ON)"),
        "expected path-only cmake export name, got: {}",
        exports.cmake
    );
}

#[test]
fn exports_integer_as_hex_when_configured() {
    let schema_src = r#"
mod app {
  option retries: u32 = 31;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(
        &symbols,
        &resolved,
        &ExportOptions {
            int_export_format: IntExportFormat::Hex,
            ..ExportOptions::default()
        },
    );

    assert!(
        exports.c_header.contains("#define CONFIG_APP_RETRIES 0x1F"),
        "expected hex int define, got: {}",
        exports.c_header
    );
    assert!(
        exports.cmake.contains("set(CFG_APP_RETRIES 0x1F)"),
        "expected hex int cmake export, got: {}",
        exports.cmake
    );
}

#[test]
fn exports_enum_as_string_when_configured() {
    let schema_src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = Mode::off;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("app::mode = on;");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(
        &symbols,
        &resolved,
        &ExportOptions {
            enum_export_style: EnumExportStyle::String,
            ..ExportOptions::default()
        },
    );

    assert!(
        exports
            .c_header
            .contains("#define CONFIG_APP_MODE \"app::Mode::on\""),
        "expected enum string define, got: {}",
        exports.c_header
    );
    assert!(
        !exports.c_header.contains("CONFIG_APP_MODE_OFF")
            && !exports.c_header.contains("CONFIG_APP_MODE_ON"),
        "did not expect one-hot enum defines, got: {}",
        exports.c_header
    );
    assert!(
        exports
            .cmake
            .contains("set(CFG_APP_MODE \"app::Mode::on\")"),
        "expected enum cmake export, got: {}",
        exports.cmake
    );
}

#[test]
fn exports_bool_false_as_define_zero_when_configured() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let exports = generate_exports(
        &symbols,
        &resolved,
        &ExportOptions {
            bool_false_style: BoolFalseExportStyle::DefineZero,
            ..ExportOptions::default()
        },
    );

    assert!(
        exports.c_header.contains("#define CONFIG_APP_ENABLED 0"),
        "expected bool false define, got: {}",
        exports.c_header
    );
    assert!(
        exports.cmake.contains("set(CFG_APP_ENABLED OFF)"),
        "expected bool false cmake export, got: {}",
        exports.cmake
    );
}

#[test]
fn patch_default_overrides_schema_default_with_patch_source() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}

patch app {
  default enabled = true;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let option = resolved
        .options
        .iter()
        .find(|option| option.path == "app::enabled")
        .expect("expected app::enabled in resolved options");

    assert!(option.active, "expected app::enabled to be active");
    assert_eq!(
        option.value.as_ref(),
        Some(&rcfg_lang::ResolvedValue::Bool(true))
    );
    assert_eq!(option.source, Some(rcfg_lang::ValueSource::Patch));
}

#[test]
fn user_value_takes_precedence_over_patch_default() {
    let schema_src = r#"
mod app {
  option enabled: bool = false;
}

patch app {
  default enabled = true;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("app::enabled = false;");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let option = resolved
        .options
        .iter()
        .find(|option| option.path == "app::enabled")
        .expect("expected app::enabled in resolved options");

    assert!(option.active, "expected app::enabled to be active");
    assert_eq!(
        option.value.as_ref(),
        Some(&rcfg_lang::ResolvedValue::Bool(false))
    );
    assert_eq!(option.source, Some(rcfg_lang::ValueSource::User));
}

#[test]
fn patch_default_respects_when_activation() {
    let schema_src = r#"
mod app {
  option gate: bool = false;
  option enabled: bool = false;

  when gate {
    patch app {
      default enabled = true;
    }
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values_off, values_off_diags) = parse_values_with_diagnostics("");
    assert!(
        values_off_diags.is_empty(),
        "values parse diagnostics: {values_off_diags:#?}"
    );
    let resolved_off = resolve_values(&values_off, &symbols);
    let enabled_off = resolved_off
        .options
        .iter()
        .find(|option| option.path == "app::enabled")
        .expect("expected app::enabled");
    assert_eq!(
        enabled_off.value.as_ref(),
        Some(&rcfg_lang::ResolvedValue::Bool(false))
    );
    assert_eq!(enabled_off.source, Some(rcfg_lang::ValueSource::Default));

    let (values_on, values_on_diags) = parse_values_with_diagnostics("app::gate = true;");
    assert!(
        values_on_diags.is_empty(),
        "values parse diagnostics: {values_on_diags:#?}"
    );
    let resolved_on = resolve_values(&values_on, &symbols);
    let enabled_on = resolved_on
        .options
        .iter()
        .find(|option| option.path == "app::enabled")
        .expect("expected app::enabled");
    assert_eq!(
        enabled_on.value.as_ref(),
        Some(&rcfg_lang::ResolvedValue::Bool(true))
    );
    assert_eq!(enabled_on.source, Some(rcfg_lang::ValueSource::Patch));
}

#[test]
fn patch_default_respects_match_activation() {
    let schema_src = r#"
mod app {
  enum Mode { off, on }
  option mode: Mode = off;
  option retries: u32 = 1;

  match mode {
    case Mode::on => {
      patch app {
        default retries = 8;
      }
    }
    case _ => { }
  }
}
"#;
    let symbols = symbols_from(schema_src);

    let (values_off, values_off_diags) = parse_values_with_diagnostics("");
    assert!(
        values_off_diags.is_empty(),
        "values parse diagnostics: {values_off_diags:#?}"
    );
    let resolved_off = resolve_values(&values_off, &symbols);
    let retries_off = resolved_off
        .options
        .iter()
        .find(|option| option.path == "app::retries")
        .expect("expected app::retries");
    assert_eq!(
        retries_off.value.as_ref(),
        Some(&rcfg_lang::ResolvedValue::Int(1))
    );
    assert_eq!(retries_off.source, Some(rcfg_lang::ValueSource::Default));

    let (values_on, values_on_diags) = parse_values_with_diagnostics("app::mode = on;");
    assert!(
        values_on_diags.is_empty(),
        "values parse diagnostics: {values_on_diags:#?}"
    );
    let resolved_on = resolve_values(&values_on, &symbols);
    let retries_on = resolved_on
        .options
        .iter()
        .find(|option| option.path == "app::retries")
        .expect("expected app::retries");
    assert_eq!(
        retries_on.value.as_ref(),
        Some(&rcfg_lang::ResolvedValue::Int(8))
    );
    assert_eq!(retries_on.source, Some(rcfg_lang::ValueSource::Patch));
}

#[test]
fn patch_default_type_mismatch_reports_error() {
    let src = r#"
mod app {
  option retries: u32 = 1;
}

patch app {
  default retries = "oops";
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report
            .diagnostics
            .iter()
            .any(|diag| diag.code == "E_PATCH_TYPE_MISMATCH"),
        "expected E_PATCH_TYPE_MISMATCH, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn patch_default_target_not_found_reports_error() {
    let src = r#"
mod app {
  option enabled: bool = false;
}

patch app {
  default missing = true;
}
"#;
    let (file, parse_diags) = parse_schema_with_diagnostics(src);
    assert!(
        parse_diags.is_empty(),
        "parse diagnostics: {parse_diags:#?}"
    );

    let report = analyze_schema(&file);
    assert!(
        report.diagnostics.iter().any(|diag| {
            diag.code == "E_SYMBOL_NOT_FOUND"
                && diag
                    .message
                    .contains("patch default target `missing` cannot be resolved")
        }),
        "expected patch default target not found diagnostics, got: {:#?}",
        report.diagnostics
    );
}

#[test]
fn rust_exporter_renders_typed_constants_and_enum() {
    let schema_src = r#"
mod app {
  option enabled: bool = true;
  option retries: u8 = 7;
  option offset: i32 = -2;
  option name: string = "demo";
  enum Mode { off, on }
  option mode: Mode = on;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let rust_exporter = rcfg_lang::create_builtin_exporter("rust").expect("rust exporter");
    let rendered = rust_exporter.render(&symbols, &resolved, &ExportOptions::default());

    assert_eq!(rendered.format, "rust");
    assert!(
        rendered.diagnostics.is_empty(),
        "expected no render diagnostics, got: {:#?}",
        rendered.diagnostics
    );
    assert!(
        rendered.content.starts_with("// Auto-generated by rcfg"),
        "expected rust prelude, got: {}",
        rendered.content
    );
    assert!(
        rendered.content.contains("pub enum AppMode {"),
        "expected rust enum declaration, got: {}",
        rendered.content
    );
    assert!(
        rendered.content.contains("    Off,") && rendered.content.contains("    On,"),
        "expected rust enum variants, got: {}",
        rendered.content
    );
    assert!(
        rendered
            .content
            .contains("pub const CONFIG_APP_ENABLED: bool = true;"),
        "expected bool constant, got: {}",
        rendered.content
    );
    assert!(
        rendered
            .content
            .contains("pub const CONFIG_APP_RETRIES: u8 = 7;"),
        "expected u8 constant, got: {}",
        rendered.content
    );
    assert!(
        rendered
            .content
            .contains("pub const CONFIG_APP_OFFSET: i32 = -2;"),
        "expected i32 constant, got: {}",
        rendered.content
    );
    assert!(
        rendered
            .content
            .contains("pub const CONFIG_APP_NAME: &str = \"demo\";"),
        "expected string constant, got: {}",
        rendered.content
    );
    assert!(
        rendered
            .content
            .contains("pub const CONFIG_APP_MODE: AppMode = AppMode::On;"),
        "expected enum constant, got: {}",
        rendered.content
    );
}

#[test]
fn python_exporter_renders_bool_and_enum_as_constants() {
    let schema_src = r#"
mod app {
  option enabled: bool = true;
  option retries: u16 = 42;
  option name: string = "alpha";
  enum Mode { off, on }
  option mode: Mode = on;
}
"#;
    let symbols = symbols_from(schema_src);

    let (values, values_diags) = parse_values_with_diagnostics("");
    assert!(
        values_diags.is_empty(),
        "values parse diagnostics: {values_diags:#?}"
    );

    let resolved = resolve_values(&values, &symbols);
    let python_exporter = rcfg_lang::create_builtin_exporter("python").expect("python exporter");
    let rendered = python_exporter.render(&symbols, &resolved, &ExportOptions::default());

    assert_eq!(rendered.format, "python");
    assert!(
        rendered.diagnostics.is_empty(),
        "expected no render diagnostics, got: {:#?}",
        rendered.diagnostics
    );
    assert!(
        rendered.content.starts_with("# Auto-generated by rcfg"),
        "expected python prelude, got: {}",
        rendered.content
    );
    assert!(
        rendered.content.contains("CONFIG_APP_ENABLED = True"),
        "expected bool constant, got: {}",
        rendered.content
    );
    assert!(
        rendered.content.contains("CONFIG_APP_RETRIES = 42"),
        "expected int constant, got: {}",
        rendered.content
    );
    assert!(
        rendered.content.contains("CONFIG_APP_NAME = \"alpha\""),
        "expected string constant, got: {}",
        rendered.content
    );
    assert!(
        rendered
            .content
            .contains("CONFIG_APP_MODE = \"app::Mode::on\""),
        "expected enum string constant, got: {}",
        rendered.content
    );
}

#[test]
fn builtin_exporter_registry_lists_c_cmake_python_and_rust() {
    let mut names = rcfg_lang::builtin_exporter_names();
    names.sort();
    assert_eq!(names, vec!["c-header", "cmake", "python", "rust"]);

    let c = rcfg_lang::create_builtin_exporter("c-header").expect("c exporter should exist");
    assert_eq!(c.format_name(), "c-header");

    let cmake = rcfg_lang::create_builtin_exporter("cmake").expect("cmake exporter should exist");
    assert_eq!(cmake.format_name(), "cmake");

    let python =
        rcfg_lang::create_builtin_exporter("python").expect("python exporter should exist");
    assert_eq!(python.format_name(), "python");

    let rust = rcfg_lang::create_builtin_exporter("rust").expect("rust exporter should exist");
    assert_eq!(rust.format_name(), "rust");

    assert!(rcfg_lang::create_builtin_exporter("unknown").is_none());
}
