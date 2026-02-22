use std::fs;
use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base.join("target").join("tmp-tests").join(name)
}

fn normalize_newlines(text: String) -> String {
    text.replace("\r\n", "\n").trim_end().to_string()
}

#[test]
fn dump_json_snapshots_match_cpp_cmake_example() {
    let root = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let manifest = root.join("examples/cpp-cmake/Config.toml");
    let values = root.join("examples/cpp-cmake/profiles/dev.rcfgv");

    let snapshot_dir = root.join("tests/snapshots/dump");
    let out_schema_ir = fixture_path("dump_snapshot/schema_ir.json");
    let out_resolved = fixture_path("dump_snapshot/resolved.json");
    let out_diagnostics = fixture_path("dump_snapshot/diagnostics.json");

    if let Some(parent) = out_schema_ir.parent() {
        let _ = fs::create_dir_all(parent);
    }

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"))
        .args([
            "dump",
            "--manifest",
            manifest.to_str().expect("manifest path"),
            "--values",
            values.to_str().expect("values path"),
            "--out",
            out_resolved.to_str().expect("resolved path"),
            "--out-schema-ir",
            out_schema_ir.to_str().expect("schema_ir path"),
            "--out-diagnostics",
            out_diagnostics.to_str().expect("diagnostics path"),
        ])
        .status()
        .expect("run rcfg dump");
    assert!(status.success(), "rcfg dump should succeed");

    let actual_schema_ir = normalize_newlines(
        fs::read_to_string(&out_schema_ir).expect("read generated schema_ir snapshot"),
    );
    let actual_resolved = normalize_newlines(
        fs::read_to_string(&out_resolved).expect("read generated resolved snapshot"),
    );
    let actual_diagnostics = normalize_newlines(
        fs::read_to_string(&out_diagnostics).expect("read generated diagnostics snapshot"),
    );

    let expected_schema_ir = normalize_newlines(
        fs::read_to_string(snapshot_dir.join("schema_ir.json"))
            .expect("read expected schema_ir snapshot"),
    );
    let expected_resolved = normalize_newlines(
        fs::read_to_string(snapshot_dir.join("resolved.json"))
            .expect("read expected resolved snapshot"),
    );
    let expected_diagnostics = normalize_newlines(
        fs::read_to_string(snapshot_dir.join("diagnostics.json"))
            .expect("read expected diagnostics snapshot"),
    );

    assert_eq!(
        actual_schema_ir, expected_schema_ir,
        "schema_ir.json snapshot mismatch"
    );
    assert_eq!(
        actual_resolved, expected_resolved,
        "resolved.json snapshot mismatch"
    );
    assert_eq!(
        actual_diagnostics, expected_diagnostics,
        "diagnostics.json snapshot mismatch"
    );
}
