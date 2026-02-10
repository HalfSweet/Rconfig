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
fn menuconfig_script_outputs_final_json_only() {
    let schema = fixture_path("menuconfig_script/schema.rcfg");
    let script = fixture_path("menuconfig_script/script.txt");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
}
"#,
    );
    write_file(
        &script,
        r#"
enter
down
"#,
    );

    let output = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"))
        .args([
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--script",
            script.to_str().expect("script path"),
        ])
        .output()
        .expect("run rcfg menuconfig script");

    assert!(
        output.status.success(),
        "menuconfig script should succeed: stderr={}",
        String::from_utf8_lossy(&output.stderr)
    );

    let stdout = String::from_utf8_lossy(&output.stdout);
    let payload: serde_json::Value =
        serde_json::from_str(&stdout).expect("stdout should be pure json payload");

    assert!(payload.get("selected_path").is_some(), "{payload}");
    assert!(payload.get("user_values").is_some(), "{payload}");
    assert!(payload.get("active_paths").is_some(), "{payload}");
    assert!(payload.get("diagnostics").is_some(), "{payload}");
}
