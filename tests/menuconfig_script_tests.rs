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

    let _ = fs::remove_file(&values);

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

    let _ = fs::remove_file(&default_values);

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

    let _ = fs::remove_file(&values);
    let _ = fs::remove_file(&out);

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

    let _ = fs::remove_file(&values);

    write_file(
        &schema,
        r#"
mod app {
  option note: string = "default";
}
"#,
    );
    write_file(&script, "\nenter\ndown\nchars a\"b\\c\nsave\nenter\n");

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

    assert_eq!(
        payload["user_values"]["app::enabled"],
        serde_json::json!(true)
    );

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

#[test]
fn menuconfig_script_supports_int_edit_and_range_validation() {
    let schema = fixture_path("menuconfig_script_int_edit", "schema.rcfg");
    let script = fixture_path("menuconfig_script_int_edit", "script.txt");

    write_file(
        &schema,
        r#"
mod app {
  #[range(1200..=115200)]
  option baud: u32 = 9600;
}
"#,
    );
    write_file(
        &script,
        r#"
enter
down
chars 19200
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

    assert_eq!(
        payload["user_values"]["app::baud"],
        serde_json::json!(19200)
    );

    let resolved = payload["resolved_options"]
        .as_array()
        .expect("resolved options array");
    let baud = resolved
        .iter()
        .find(|item| item["path"] == "app::baud")
        .expect("resolved baud option");
    assert_eq!(baud["value"], serde_json::json!(19200));
    assert_eq!(baud["source"], serde_json::json!("user"));

    let diagnostics = payload["diagnostics"]
        .as_array()
        .expect("diagnostics array");
    assert!(
        diagnostics
            .iter()
            .all(|diag| diag["code"] != "E_RANGE_VIOLATION"),
        "did not expect E_RANGE_VIOLATION for in-range int edit: {payload}"
    );
}

#[test]
fn menuconfig_script_supports_enum_selection() {
    let schema = fixture_path("menuconfig_script_enum_edit", "schema.rcfg");
    let script = fixture_path("menuconfig_script_enum_edit", "script.txt");

    write_file(
        &schema,
        r#"
mod app {
  enum Mode { rtu, ascii }
  option mode: Mode = Mode::rtu;
}
"#,
    );
    write_file(
        &script,
        r#"
enter
down
down
chars ascii
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

    assert_eq!(
        payload["user_values"]["app::mode"],
        serde_json::json!("ascii")
    );

    let resolved = payload["resolved_options"]
        .as_array()
        .expect("resolved options array");
    let mode = resolved
        .iter()
        .find(|item| item["path"] == "app::mode")
        .expect("resolved mode option");
    assert_eq!(mode["value"], serde_json::json!("app::Mode::ascii"));
    assert_eq!(mode["source"], serde_json::json!("user"));
}

#[test]
fn menuconfig_script_blocks_save_when_errors_present() {
    let schema = fixture_path("menuconfig_script_save_blocked", "schema.rcfg");
    let script = fixture_path("menuconfig_script_save_blocked", "script.txt");
    let values = fixture_path("menuconfig_script_save_blocked", "profile.rcfgv");

    let _ = fs::remove_file(&values);

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;
  #[msg("demo.app.require.enabled")]
  require!(enabled == true);
}
"#,
    );
    write_file(
        &script,
        r#"
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
    assert!(
        !values.exists(),
        "save should be blocked when errors are present"
    );

    let payload: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("script summary json");
    let diagnostics = payload["diagnostics"]
        .as_array()
        .expect("diagnostics array");
    assert!(
        diagnostics
            .iter()
            .any(|diag| diag["code"] == "E_REQUIRE_FAILED"),
        "expected E_REQUIRE_FAILED in diagnostics: {payload}"
    );
}

#[test]
fn menuconfig_script_reset_clears_selected_override() {
    let schema = fixture_path("menuconfig_script_reset", "schema.rcfg");
    let script = fixture_path("menuconfig_script_reset", "script.txt");

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
reset
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
    let user_values = payload["user_values"]
        .as_object()
        .expect("user values object");
    assert!(
        !user_values.contains_key("app::enabled"),
        "reset should remove selected user override: {payload}"
    );
}

#[test]
fn menuconfig_script_applies_context_to_activation() {
    let schema = fixture_path("menuconfig_script_context", "schema.rcfg");
    let script = fixture_path("menuconfig_script_context", "script.txt");
    let context = fixture_path("menuconfig_script_context", "context.json");

    write_file(
        &schema,
        r#"
mod ctx {
  option arch: string;
}

mod app {
  option enabled: bool = false;
  when ctx::arch == "arm" {
    option arm_only: bool = true;
  }
}
"#,
    );
    write_file(&script, "\nenter\n");
    write_file(&context, "{\"ctx::arch\": \"arm\"}\n");

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--context",
            context.to_str().expect("context path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let payload: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("script summary json");
    let active_paths = payload["active_paths"]
        .as_array()
        .expect("active paths array")
        .iter()
        .filter_map(|item| item.as_str())
        .collect::<Vec<_>>();

    assert!(
        active_paths.contains(&"app::arm_only"),
        "ctx-driven option should be active with --context: {payload}"
    );
}

#[test]
fn menuconfig_script_localizes_diagnostics_via_i18n() {
    let schema = fixture_path("menuconfig_script_i18n", "schema.rcfg");
    let script = fixture_path("menuconfig_script_i18n", "script.txt");
    let i18n = fixture_path("menuconfig_script_i18n", "zh-CN.toml");

    write_file(
        &schema,
        r#"
mod app {
  option enabled: bool = false;

  #[msg("demo.app.require.fail")]
  require!(enabled == true);
}
"#,
    );
    write_file(&script, "\nenter\n");
    write_file(
        &i18n,
        r#"
locale = "zh-CN"

[strings]
"demo.app.require.fail" = "必须启用 app::enabled"
"#,
    );

    let output = run_menuconfig(
        &[
            "menuconfig",
            "--schema",
            schema.to_str().expect("schema path"),
            "--i18n",
            i18n.to_str().expect("i18n path"),
            "--script",
            script.to_str().expect("script path"),
        ],
        None,
    );

    assert_success(&output);

    let payload: serde_json::Value =
        serde_json::from_slice(&output.stdout).expect("script summary json");
    let diagnostics = payload["diagnostics"]
        .as_array()
        .expect("diagnostics array");
    let require = diagnostics
        .iter()
        .find(|diag| diag["code"] == "E_REQUIRE_FAILED")
        .expect("E_REQUIRE_FAILED diagnostic");

    assert_eq!(
        require["message"],
        serde_json::json!("必须启用 app::enabled")
    );
}
