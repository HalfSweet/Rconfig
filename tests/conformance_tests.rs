use std::fs;
use std::path::{Path, PathBuf};

use rcfg_lang::{
    ExportOptions, Severity, analyze_schema, analyze_values, generate_exports,
    parse_schema_with_diagnostics, parse_values_with_diagnostics, resolve_values,
};

fn collect_case_dirs(root: &Path) -> Vec<PathBuf> {
    let mut dirs = fs::read_dir(root)
        .unwrap_or_else(|err| {
            panic!(
                "failed to read conformance root `{}`: {err}",
                root.display()
            )
        })
        .filter_map(|entry| entry.ok().map(|item| item.path()))
        .filter(|path| path.is_dir())
        .collect::<Vec<_>>();
    dirs.sort();
    dirs
}

fn assert_no_error_diagnostics(diags: &[rcfg_lang::Diagnostic], context: &str) {
    assert!(
        diags.iter().all(|diag| diag.severity != Severity::Error),
        "{context}: expected no error diagnostics, got: {diags:#?}"
    );
}

fn normalize_newlines(text: String) -> String {
    text.replace("\r\n", "\n").trim_end().to_string()
}

#[test]
fn conformance_ok_cases_pass() {
    for case_dir in collect_case_dirs(Path::new("tests/conformance/ok")) {
        let case_name = case_dir
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        let schema_src = fs::read_to_string(case_dir.join("schema.rcfg"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read schema.rcfg: {err}"));
        let values_src = fs::read_to_string(case_dir.join("values.rcfgv"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read values.rcfgv: {err}"));

        let (schema_file, schema_parse_diags) = parse_schema_with_diagnostics(&schema_src);
        assert_no_error_diagnostics(&schema_parse_diags, case_name);

        let schema_report = analyze_schema(&schema_file);
        let rcfg_lang::SemanticReport {
            symbols,
            diagnostics: schema_diags,
        } = schema_report;
        assert_no_error_diagnostics(&schema_diags, case_name);

        let (values, values_parse_diags) = parse_values_with_diagnostics(&values_src);
        assert_no_error_diagnostics(&values_parse_diags, case_name);

        let values_diags = analyze_values(&values, &symbols);
        assert_no_error_diagnostics(&values_diags, case_name);
    }
}

#[test]
fn conformance_fail_cases_match_expected_codes() {
    for case_dir in collect_case_dirs(Path::new("tests/conformance/fail")) {
        let case_name = case_dir
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        let schema_src = fs::read_to_string(case_dir.join("schema.rcfg"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read schema.rcfg: {err}"));
        let values_src = fs::read_to_string(case_dir.join("values.rcfgv"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read values.rcfgv: {err}"));
        let expected_src = fs::read_to_string(case_dir.join("expected.json"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read expected.json: {err}"));

        let expected: serde_json::Value = serde_json::from_str(&expected_src)
            .unwrap_or_else(|err| panic!("case `{case_name}` has invalid expected.json: {err}"));
        let expected_codes = expected
            .get("error_codes")
            .and_then(serde_json::Value::as_array)
            .unwrap_or_else(|| {
                panic!("case `{case_name}` expected.json must contain `error_codes` array")
            });

        let (schema_file, mut diagnostics) = parse_schema_with_diagnostics(&schema_src);
        let schema_report = analyze_schema(&schema_file);
        let rcfg_lang::SemanticReport {
            symbols,
            diagnostics: mut schema_diags,
        } = schema_report;
        diagnostics.append(&mut schema_diags);

        let (values, mut values_parse_diags) = parse_values_with_diagnostics(&values_src);
        diagnostics.append(&mut values_parse_diags);
        diagnostics.extend(analyze_values(&values, &symbols));

        for expected_code in expected_codes {
            let expected_code = expected_code
                .as_str()
                .unwrap_or_else(|| panic!("case `{case_name}` has non-string error code"));
            assert!(
                diagnostics.iter().any(|diag| diag.code == expected_code),
                "case `{case_name}` missing expected code `{expected_code}`; diagnostics: {diagnostics:#?}"
            );
        }
    }
}

#[test]
fn conformance_golden_cases_match_exports() {
    for case_dir in collect_case_dirs(Path::new("tests/conformance/golden")) {
        let case_name = case_dir
            .file_name()
            .and_then(|name| name.to_str())
            .unwrap_or("<unknown>");
        let schema_src = fs::read_to_string(case_dir.join("schema.rcfg"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read schema.rcfg: {err}"));
        let values_src = fs::read_to_string(case_dir.join("values.rcfgv"))
            .unwrap_or_else(|err| panic!("case `{case_name}` failed to read values.rcfgv: {err}"));

        let (schema_file, schema_parse_diags) = parse_schema_with_diagnostics(&schema_src);
        assert_no_error_diagnostics(&schema_parse_diags, case_name);
        let schema_report = analyze_schema(&schema_file);
        let rcfg_lang::SemanticReport {
            symbols,
            diagnostics: schema_diags,
        } = schema_report;
        assert_no_error_diagnostics(&schema_diags, case_name);

        let (values, values_parse_diags) = parse_values_with_diagnostics(&values_src);
        assert_no_error_diagnostics(&values_parse_diags, case_name);
        let values_diags = analyze_values(&values, &symbols);
        assert_no_error_diagnostics(&values_diags, case_name);

        let resolved = resolve_values(&values, &symbols);
        let exports = generate_exports(&symbols, &resolved, &ExportOptions::default());
        assert_no_error_diagnostics(&exports.diagnostics, case_name);

        let mut matched_any = false;
        let expected_h = case_dir.join("expected.h");
        if expected_h.exists() {
            matched_any = true;
            let expected =
                normalize_newlines(fs::read_to_string(&expected_h).unwrap_or_else(|err| {
                    panic!(
                        "case `{case_name}` failed to read {}: {err}",
                        expected_h.display()
                    )
                }));
            assert_eq!(
                normalize_newlines(exports.c_header.clone()),
                expected,
                "case `{case_name}` c-header mismatch"
            );
        }

        let expected_cmake = case_dir.join("expected.cmake");
        if expected_cmake.exists() {
            matched_any = true;
            let expected =
                normalize_newlines(fs::read_to_string(&expected_cmake).unwrap_or_else(|err| {
                    panic!(
                        "case `{case_name}` failed to read {}: {err}",
                        expected_cmake.display()
                    )
                }));
            assert_eq!(
                normalize_newlines(exports.cmake.clone()),
                expected,
                "case `{case_name}` cmake mismatch"
            );
        }

        assert!(
            matched_any,
            "case `{case_name}` must contain expected.h and/or expected.cmake"
        );
    }
}
