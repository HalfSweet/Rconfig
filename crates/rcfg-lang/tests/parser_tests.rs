use rcfg_lang::ast::{BinaryOp, ConstraintItem};
use rcfg_lang::{
    Expr, Item, MatchPat, parse_schema_with_diagnostics, parse_values_with_diagnostics,
};

#[test]
fn parses_schema_with_when_match_and_require() {
    let src = r#"
mod uart {
  option enable: bool = false;
  enum Mode { off, on }
  option mode: Mode = off;

  when enable {
    option baud: u32 = 115200 {
      #[msg("uart.baud.valid")]
      require!(self in 1200..=3000000);
    };
  }

  match mode {
    case on => {
      require!(enable == true);
    }
    case _ => { }
  }
}
"#;

    let (file, diags) = parse_schema_with_diagnostics(src);
    assert!(diags.is_empty(), "expected no diagnostics, got: {diags:#?}");
    assert_eq!(file.items.len(), 1);

    let Item::Mod(module) = &file.items[0] else {
        panic!("expected module");
    };
    assert_eq!(module.name.value, "uart");
    assert_eq!(module.items.len(), 5);

    let Item::When(when_block) = &module.items[3] else {
        panic!("expected when block");
    };
    let Expr::Path(path) = &when_block.condition else {
        panic!("expected path condition");
    };
    assert_eq!(path.to_string(), "enable");

    let Item::Match(match_block) = &module.items[4] else {
        panic!("expected match block");
    };
    assert_eq!(match_block.cases.len(), 2);
    assert!(matches!(
        match_block.cases[1].pattern,
        MatchPat::Wildcard(_)
    ));
}

#[test]
fn parses_values_with_include_use_and_assignments() {
    let src = r#"
include "base.rcfgv";
use uart_pkg::uart as uart;
uart::enable = true;
uart::baud = env("UART_BAUD");
uart::mode = rtu;
"#;
    let (values, diags) = parse_values_with_diagnostics(src);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");
    assert_eq!(values.stmts.len(), 5);
}

#[test]
fn parses_env_with_fallback_without_feature_gate_error() {
    let src = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let (_file, diags) = parse_schema_with_diagnostics(src);
    assert!(diags.is_empty(), "schema parse diagnostics: {diags:#?}");

    let values_src = r#"app::enabled = env("RCFG_ENABLE", "false");"#;
    let (_values, values_diags) = parse_values_with_diagnostics(values_src);
    assert!(
        values_diags
            .iter()
            .all(|diag| diag.code != "E_FEATURE_NOT_SUPPORTED"),
        "env fallback should be supported, got: {values_diags:#?}"
    );
}

#[test]
fn parses_cfg_attr_and_export_block_without_feature_gate_error() {
    let src = r#"
#[cfg(true)]
mod app {
  option enabled: bool = false;
}

export c "config.h" {
  prefix = "CONFIG_";
}
"#;
    let (file, diags) = parse_schema_with_diagnostics(src);
    assert!(
        diags.iter().all(|d| d.code != "E_FEATURE_NOT_SUPPORTED"),
        "cfg/export should not be blocked by feature gate, got: {diags:#?}"
    );
    assert_eq!(file.items.len(), 2);

    let Item::Mod(module) = &file.items[0] else {
        panic!("expected mod item");
    };
    assert!(
        module
            .meta
            .attrs
            .iter()
            .any(|attr| matches!(attr.kind, rcfg_lang::AttrKind::Cfg(_))),
        "expected cfg attribute in module metadata"
    );

    let Item::Export(export) = &file.items[1] else {
        panic!("expected export item");
    };
    assert_eq!(export.stmts.len(), 1);
}

#[test]
fn parses_expression_precedence() {
    let src = r#"require!(!a && b || c == d);"#;
    let wrapped = format!("constraint {{ {src} }}");
    let (file, diags) = parse_schema_with_diagnostics(&wrapped);
    assert!(diags.is_empty(), "unexpected diagnostics: {diags:#?}");

    let Item::Constraint(block) = &file.items[0] else {
        panic!("expected constraint");
    };
    let ConstraintItem::Require(require) = &block.items[0] else {
        panic!("expected require");
    };

    let Expr::Binary { op, .. } = &require.expr else {
        panic!("expected binary expression");
    };
    assert_eq!(op, &BinaryOp::Or);
}

#[test]
fn reports_integer_literal_out_of_range() {
    let src = r#"
mod app {
  option huge: i32 = 170141183460469231731687303715884105728;
}
"#;
    let (_file, diags) = parse_schema_with_diagnostics(src);
    assert!(
        diags
            .iter()
            .any(|diag| diag.code == "E_INT_LITERAL_OUT_OF_RANGE"),
        "expected E_INT_LITERAL_OUT_OF_RANGE, got: {diags:#?}"
    );
}

#[test]
fn parses_patch_reserved_grammar_without_feature_gate_error() {
    let src = r#"
patch foo { default x = true; }
"#;
    let (file, diags) = parse_schema_with_diagnostics(src);
    assert!(
        diags
            .iter()
            .all(|diag| diag.code != "E_FEATURE_NOT_SUPPORTED"),
        "patch should not be blocked by feature gate, got: {diags:#?}"
    );
    assert_eq!(file.items.len(), 1);

    let Item::Patch(patch) = &file.items[0] else {
        panic!("expected patch item");
    };
    assert_eq!(patch.target.to_string(), "foo");
    assert_eq!(patch.stmts.len(), 1);
}

#[test]
fn parse_expected_token_diagnostic_contains_fixit_note() {
    let src = r#"
mod app {
  option enabled: bool = false
}
"#;

    let (_file, diags) = parse_schema_with_diagnostics(src);
    let diag = diags
        .iter()
        .find(|diag| diag.code == "E_PARSE_EXPECTED_TOKEN")
        .expect("expected E_PARSE_EXPECTED_TOKEN");

    assert!(
        diag.note
            .as_deref()
            .is_some_and(|note| note.contains("fix-it:")),
        "expected fix-it note in parser diagnostic: {diag:#?}"
    );
}

#[test]
fn reports_parse_nesting_too_deep_for_when_blocks() {
    let src = nested_when_schema(300);
    let (_file, diags) = parse_schema_with_diagnostics(&src);
    assert!(
        diags
            .iter()
            .any(|diag| diag.code == "E_PARSE_NESTING_TOO_DEEP"),
        "expected E_PARSE_NESTING_TOO_DEEP, got: {diags:#?}"
    );
}

#[test]
fn reports_parse_nesting_too_deep_for_group_expr() {
    let src = format!(
        "constraint {{ require!({}true{}); }}",
        "(".repeat(300),
        ")".repeat(300)
    );
    let (_file, diags) = parse_schema_with_diagnostics(&src);
    assert!(
        diags
            .iter()
            .any(|diag| diag.code == "E_PARSE_NESTING_TOO_DEEP"),
        "expected E_PARSE_NESTING_TOO_DEEP, got: {diags:#?}"
    );
}

#[test]
fn parser_nesting_within_limit_does_not_trigger_depth_error() {
    let src = nested_when_schema(32);
    let (_file, diags) = parse_schema_with_diagnostics(&src);
    assert!(
        diags
            .iter()
            .all(|diag| diag.code != "E_PARSE_NESTING_TOO_DEEP"),
        "unexpected E_PARSE_NESTING_TOO_DEEP: {diags:#?}"
    );
}

#[test]
fn parser_handles_replacement_char_without_panicking() {
    let src = format!(
        "mod app {{\n  option enabled: bool = true;\n  {}\n}}\n",
        '\u{FFFD}'
    );
    let (_file, diags) = parse_schema_with_diagnostics(&src);
    assert!(
        diags
            .iter()
            .any(|diag| diag.code == "E_PARSE_UNEXPECTED_TOKEN"),
        "expected E_PARSE_UNEXPECTED_TOKEN, got: {diags:#?}"
    );
}

#[test]
fn parser_handles_multibyte_unsupported_escape_without_panicking() {
    let src = format!(
        "mod app {{\n  option value: string = \"\\{}\";\n}}\n",
        '\u{FFFD}'
    );
    let (_file, diags) = parse_schema_with_diagnostics(&src);
    assert!(
        diags
            .iter()
            .any(|diag| diag.code == "E_PARSE_INVALID_LITERAL"),
        "expected E_PARSE_INVALID_LITERAL, got: {diags:#?}"
    );
}

fn nested_when_schema(depth: usize) -> String {
    let mut src = String::from("mod app {\n  option enabled: bool = false;\n");
    for _ in 0..depth {
        src.push_str("  when true {\n");
    }
    src.push_str("  option nested: bool = true;\n");
    for _ in 0..depth {
        src.push_str("  }\n");
    }
    src.push_str("}\n");
    src
}
