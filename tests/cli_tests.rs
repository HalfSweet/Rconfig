use std::fs;
use std::path::PathBuf;

fn fixture_path(name: &str) -> PathBuf {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base.join("target").join("tmp-tests").join(name)
}

fn write_file(path: &PathBuf, text: &str) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    fs::write(path, text).expect("write file");
}

#[test]
fn rcfg_dump_redacts_secret_by_default() {
    let schema = fixture_path("cli/schema.rcfg");
    let values = fixture_path("cli/values.rcfgv");
    let dump = fixture_path("cli/resolved.json");

    write_file(
        &schema,
        r#"
mod app {
  #[secret]
  option token: string = "abc";
}
"#,
    );
    write_file(&values, "");

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"))
        .args([
            "dump",
            "--schema",
            schema.to_str().expect("schema path"),
            "--values",
            values.to_str().expect("values path"),
            "--out",
            dump.to_str().expect("dump path"),
        ])
        .status()
        .expect("run rcfg dump");
    assert!(status.success(), "rcfg dump should succeed");

    let text = fs::read_to_string(&dump).expect("read dump");
    assert!(
        text.contains("\"redacted\": true"),
        "secret should be redacted by default: {text}"
    );
}

#[test]
fn rcfg_export_generates_files() {
    let schema = fixture_path("cli_export/schema.rcfg");
    let values = fixture_path("cli_export/values.rcfgv");
    let out_h = fixture_path("cli_export/config.h");
    let out_cmake = fixture_path("cli_export/config.cmake");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
  option retries: u32 = 3;
}
"#,
    );
    write_file(
        &values,
        r#"
app::enabled = true;
app::retries = 8;
"#,
    );

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"))
        .args([
            "export",
            "--schema",
            schema.to_str().expect("schema path"),
            "--values",
            values.to_str().expect("values path"),
            "--out-h",
            out_h.to_str().expect("header path"),
            "--out-cmake",
            out_cmake.to_str().expect("cmake path"),
        ])
        .status()
        .expect("run rcfg export");
    assert!(status.success(), "rcfg export should succeed");

    let header = fs::read_to_string(&out_h).expect("read header");
    let cmake = fs::read_to_string(&out_cmake).expect("read cmake");
    assert!(header.contains("#define CONFIG_APP_ENABLED 1"), "{header}");
    assert!(header.contains("#define CONFIG_APP_RETRIES 8"), "{header}");
    assert!(cmake.contains("set(CFG_APP_ENABLED ON)"), "{cmake}");
    assert!(cmake.contains("set(CFG_APP_RETRIES 8)"), "{cmake}");
}

#[test]
fn rcfg_dump_writes_schema_ir_and_diagnostics_json() {
    let schema = fixture_path("cli_dump/schema.rcfg");
    let values = fixture_path("cli_dump/values.rcfgv");
    let dump = fixture_path("cli_dump/resolved.json");
    let schema_ir = fixture_path("cli_dump/schema_ir.json");
    let diagnostics = fixture_path("cli_dump/diagnostics.json");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
}
"#,
    );
    write_file(&values, "app::enabled = true;");

    let status = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"))
        .args([
            "dump",
            "--schema",
            schema.to_str().expect("schema path"),
            "--values",
            values.to_str().expect("values path"),
            "--out",
            dump.to_str().expect("dump path"),
            "--out-schema-ir",
            schema_ir.to_str().expect("schema_ir path"),
            "--out-diagnostics",
            diagnostics.to_str().expect("diagnostics path"),
        ])
        .status()
        .expect("run rcfg dump");
    assert!(status.success(), "rcfg dump should succeed");

    let schema_ir_text = fs::read_to_string(&schema_ir).expect("read schema_ir");
    let diagnostics_text = fs::read_to_string(&diagnostics).expect("read diagnostics");
    assert!(schema_ir_text.contains("\"symbols\""), "{schema_ir_text}");
    assert!(diagnostics_text.contains("L_MISSING_DOC"), "{diagnostics_text}");
}

#[test]
fn rcfg_check_supports_manifest_entry_schema() {
    let root = fixture_path("cli_manifest");
    let schema = root.join("src/schema.rcfg");
    let manifest = root.join("Config.toml");
    let values = root.join("values.rcfgv");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
}
"#,
    );
    write_file(
        &manifest,
        r#"
[package]
name = "demo"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
    );
    write_file(&values, "app::enabled = true;");

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"))
        .args([
            "check",
            "--manifest",
            manifest.to_str().expect("manifest path"),
            "--values",
            values.to_str().expect("values path"),
            "--format",
            "json",
        ])
        .output()
        .expect("run rcfg check");
    assert!(output.status.success(), "rcfg check should succeed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains("L_MISSING_DOC"), "{stdout}");
}
