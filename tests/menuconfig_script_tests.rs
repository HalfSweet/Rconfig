use std::fs;
use std::path::{Path, PathBuf};

fn fixture_dir(name: &str) -> PathBuf {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let dir = base.join("target").join("tmp-tests").join(name);
    let _ = fs::create_dir_all(&dir);
    dir
}

fn fixture_path(dir: &str, file: &str) -> PathBuf {
    fixture_dir(dir).join(file)
}

fn write_file(path: &Path, text: &str) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    fs::write(path, text).expect("write file");
}

fn run_menuconfig(args: &[&str], cwd: Option<&Path>) -> std::process::Output {
    let mut command = std::process::Command::new(env!("CARGO_BIN_EXE_rcfg"));
    command.args(args);
    if let Some(cwd) = cwd {
        command.current_dir(cwd);
    }
    command.output().expect("run rcfg menuconfig")
}

fn assert_success(output: &std::process::Output) {
    assert!(
        output.status.success(),
        "menuconfig should succeed: stderr={}",
        String::from_utf8_lossy(&output.stderr)
    );
}

#[test]
fn menuconfig_script_outputs_final_json_only() {
    let schema = fixture_path("menuconfig_script_json", "schema.rcfg");
    let script = fixture_path("menuconfig_script_json", "script.txt");

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

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let stdout = String::from_utf8_lossy(&output.stdout);
    let payload: serde_json::Value =
        serde_json::from_str(&stdout).expect("stdout should be pure json payload");

    assert!(payload.get("selected_path").is_some(), "{payload}");
    assert!(payload.get("user_values").is_some(), "{payload}");
    assert!(payload.get("active_paths").is_some(), "{payload}");
    assert!(payload.get("diagnostics").is_some(), "{payload}");
}

#[test]
fn menuconfig_save_writes_minimal_diff_to_values_path() {
    let schema = fixture_path("menuconfig_save_values", "schema.rcfg");
    let script = fixture_path("menuconfig_save_values", "script.txt");
    let values = fixture_path("menuconfig_save_values", "profile.rcfgv");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
  option note: string = "default";
}
"#,
    );
    write_file(
        &script,
        r#"
enter
down
space
save
enter
"#,
    );

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--values",
            values.to_str().expect("values path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let saved = fs::read_to_string(&values).expect("read saved values");
    assert_eq!(saved, "app::enabled = true;\n");
}

#[test]
fn menuconfig_save_defaults_to_dot_config_without_values() {
    let test_dir = fixture_dir("menuconfig_save_default");
    let schema = test_dir.join("schema.rcfg");
    let script = test_dir.join("script.txt");
    let default_values = test_dir.join(".config.rcfgv");

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
space
save
enter
"#,
    );

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        Some(&test_dir),
    );

    assert_success(&output);

    let saved = fs::read_to_string(&default_values).expect("read default save file");
    assert_eq!(saved, "app::enabled = true;\n");
}

#[test]
fn menuconfig_save_prefers_out_path_over_values_path() {
    let schema = fixture_path("menuconfig_save_out_priority", "schema.rcfg");
    let script = fixture_path("menuconfig_save_out_priority", "script.txt");
    let values = fixture_path("menuconfig_save_out_priority", "profile.rcfgv");
    let out = fixture_path("menuconfig_save_out_priority", "final.rcfgv");

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
space
save
enter
"#,
    );

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--values",
            values.to_str().expect("values path"),
            "--out",
            out.to_str().expect("out path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let saved = fs::read_to_string(&out).expect("read out save file");
    assert_eq!(saved, "app::enabled = true;\n");
    assert!(!values.exists(), "values path should not be written");
}

#[test]
fn menuconfig_save_escapes_string_values() {
    let schema = fixture_path("menuconfig_save_escape", "schema.rcfg");
    let script = fixture_path("menuconfig_save_escape", "script.txt");
    let values = fixture_path("menuconfig_save_escape", "profile.rcfgv");

    write_file(
        &schema,
        r#"
mod app {
  option note: string = "default";
}
"#,
    );
    write_file(
        &script,
        "\nenter\ndown\nchars a\"b\\c\nsave\nenter\n",
    );

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--values",
            values.to_str().expect("values path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let saved = fs::read_to_string(&values).expect("read saved values");
    assert_eq!(saved, "app::note = \"a\\\"b\\\\c\";\n");
}

#[test]
fn menuconfig_script_summary_keeps_active_source_value_consistent() {
    let schema = fixture_path("menuconfig_script_consistency", "schema.rcfg");
    let script = fixture_path("menuconfig_script_consistency", "script.txt");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
  when enabled {
    option baud: u32 = 115200;
  }
}
"#,
    );
    write_file(
        &script,
        r#"
enter
down
space
"#,
    );

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let payload: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("script summary json");

    assert_eq!(payload["user_values"]["app::enabled"], serde_json::json!(true));

    let active_paths = payload["active_paths"]
        .as_array()
        .expect("active paths array")
        .iter()
        .filter_map(|item| item.as_str())
        .collect::<Vec<_>>();
    assert!(active_paths.contains(&"app::enabled"));
    assert!(active_paths.contains(&"app::baud"));

    let resolved = payload["resolved_options"]
        .as_array()
        .expect("resolved options array");

    let enabled = resolved
        .iter()
        .find(|item| item["path"] == "app::enabled")
        .expect("resolved enabled option");
    assert_eq!(enabled["active"], serde_json::json!(true));
    assert_eq!(enabled["source"], serde_json::json!("user"));
    assert_eq!(enabled["value"], serde_json::json!(true));

    let baud = resolved
        .iter()
        .find(|item| item["path"] == "app::baud")
        .expect("resolved baud option");
    assert_eq!(baud["active"], serde_json::json!(true));
    assert_eq!(baud["source"], serde_json::json!("default"));
    assert_eq!(baud["value"], serde_json::json!(115200));
}
