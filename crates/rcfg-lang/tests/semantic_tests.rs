use std::path::PathBuf;

use rcfg_lang::parser::{parse_schema_with_diagnostics, parse_values_with_diagnostics};
use rcfg_lang::{
    analyze_schema, analyze_schema_files, analyze_schema_strict, analyze_values,
    analyze_values_from_path, analyze_values_from_path_report, analyze_values_strict,
    expand_values_includes_from_path, expand_values_includes_with_origins, generate_exports,
    plan_c_header_exports, resolve_values, DiagnosticArgValue, ExportOptions, Severity, SymbolKind,
    SymbolTable,
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
            .starts_with("#pragma once\n/* Auto-generated by rcfg — do not edit */\n\n"),
        "expected c_header prelude, got: {}",
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
            c_prefix: "MYCFG_".to_string(),
            cmake_prefix: "MY_".to_string(),
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
