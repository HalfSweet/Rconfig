use rcfg_lang::{
    format_schema, format_values, parse_schema_with_diagnostics, parse_values_with_diagnostics,
};

fn assert_no_parse_errors(diags: &[rcfg_lang::Diagnostic]) {
    let has_error = diags
        .iter()
        .any(|diag| diag.severity == rcfg_lang::Severity::Error);
    assert!(!has_error, "unexpected parse diagnostics: {diags:#?}");
}

#[test]
fn format_schema_normalizes_layout_and_is_idempotent() {
    let source = r#"
mod app{
option enabled:bool=true;
constraint{
require!(enabled==true);
}
match enabled{
case _=>{option retries:u8=3;}
}
}
"#;
    let (file, diags) = parse_schema_with_diagnostics(source);
    assert_no_parse_errors(&diags);

    let formatted = format_schema(source, &file);
    let expected = r#"mod app {
    option enabled: bool = true;

    constraint {
        require!(enabled == true);
    }

    match enabled {
        case _ => {
            option retries: u8 = 3;
        }
    }
}
"#;
    assert_eq!(formatted, expected);

    let (again_file, again_diags) = parse_schema_with_diagnostics(&formatted);
    assert_no_parse_errors(&again_diags);
    let reformatted = format_schema(&formatted, &again_file);
    assert_eq!(
        reformatted, formatted,
        "schema formatter must be idempotent"
    );
}

#[test]
fn format_schema_preserves_line_and_trailing_comments() {
    let source = r#"
// top
mod app {
  option enabled: bool = true; // trailing
  // comment before when
  when enabled {
    // inside
    require!(enabled == true);
  }
}
"#;
    let (file, diags) = parse_schema_with_diagnostics(source);
    assert_no_parse_errors(&diags);

    let formatted = format_schema(source, &file);
    assert!(
        formatted.contains("// top"),
        "top comment should be preserved: {formatted}"
    );
    assert!(
        formatted.contains("option enabled: bool = true; // trailing"),
        "trailing comment should stay inline: {formatted}"
    );
    assert!(
        formatted.contains("// comment before when"),
        "line comment between items should be preserved: {formatted}"
    );
    assert!(
        formatted.contains("// inside"),
        "line comment inside block should be preserved: {formatted}"
    );

    let (again_file, again_diags) = parse_schema_with_diagnostics(&formatted);
    assert_no_parse_errors(&again_diags);
    let reformatted = format_schema(&formatted, &again_file);
    assert_eq!(
        reformatted, formatted,
        "schema formatter must be idempotent"
    );
}

#[test]
fn format_schema_preserves_export_header_shape() {
    let source = r#"
export cmake "demo.cmake" {
prefix="CFG_";
}
"#;
    let (file, diags) = parse_schema_with_diagnostics(source);
    assert_no_parse_errors(&diags);

    let formatted = format_schema(source, &file);
    let expected = r#"export cmake "demo.cmake" {
    prefix = "CFG_";
}
"#;
    assert_eq!(formatted, expected);
}

#[test]
fn format_values_applies_group_spacing_and_is_idempotent() {
    let source = r#"
use app as cfg;
use foo as foo_cfg;
include "base.rcfgv";
include "extra.rcfgv";
cfg::enabled=true;
foo_cfg::mode=pro;
"#;
    let (values, diags) = parse_values_with_diagnostics(source);
    assert_no_parse_errors(&diags);

    let formatted = format_values(source, &values);
    let expected = r#"use app as cfg;
use foo as foo_cfg;

include "base.rcfgv";
include "extra.rcfgv";

cfg::enabled = true;
foo_cfg::mode = pro;
"#;
    assert_eq!(formatted, expected);

    let (again_values, again_diags) = parse_values_with_diagnostics(&formatted);
    assert_no_parse_errors(&again_diags);
    let reformatted = format_values(&formatted, &again_values);
    assert_eq!(
        reformatted, formatted,
        "values formatter must be idempotent"
    );
}
