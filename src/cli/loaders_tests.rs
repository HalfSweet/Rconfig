use std::fs;
use std::path::PathBuf;

use super::{load_manifest_graph, resolve_schema_path};

fn fixture_root(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("target")
        .join("tmp-tests")
        .join(name)
}

fn write_file(path: &PathBuf, text: &str) {
    if let Some(parent) = path.parent() {
        let _ = fs::create_dir_all(parent);
    }
    fs::write(path, text).expect("write file");
}

#[test]
fn manifest_graph_loads_path_dependencies_depth_first() {
    let root = fixture_root("manifest_graph_depth_first");
    let root_manifest = root.join("app/Config.toml");
    let root_schema = root.join("app/src/schema.rcfg");

    let dep_uart_manifest = root.join("deps/hal_uart/Config.toml");
    let dep_uart_schema = root.join("deps/hal_uart/src/schema.rcfg");

    let dep_gpio_manifest = root.join("deps/hal_gpio/Config.toml");
    let dep_gpio_schema = root.join("deps/hal_gpio/src/schema.rcfg");

    write_file(&root_schema, "mod app { option enabled: bool = false; }");
    write_file(&dep_uart_schema, "mod hal_uart { option on: bool = true; }");
    write_file(&dep_gpio_schema, "mod hal_gpio { option on: bool = true; }");

    write_file(
        &dep_uart_manifest,
        r#"
[package]
name = "hal_uart"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
    );
    write_file(
        &dep_gpio_manifest,
        r#"
[package]
name = "hal_gpio"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"
"#,
    );
    write_file(
        &root_manifest,
        r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
hal_uart = "../deps/hal_uart"
hal_gpio = "../deps/hal_gpio"
"#,
    );

    let graph = load_manifest_graph(Some(&root_manifest))
        .expect("load graph")
        .expect("manifest graph");
    let package_names = graph
        .packages_depth_first
        .iter()
        .map(|package| package.package_name.clone())
        .collect::<Vec<_>>();
    assert_eq!(
        package_names,
        vec![
            "hal_gpio".to_string(),
            "hal_uart".to_string(),
            "app".to_string()
        ]
    );

    let schema = resolve_schema_path(None, Some(&graph.root)).expect("resolve schema");
    assert!(schema.ends_with("app/src/schema.rcfg"), "{}", schema.display());
}

#[test]
fn manifest_graph_reports_dependency_cycle() {
    let root = fixture_root("manifest_graph_cycle");

    let app_manifest = root.join("app/Config.toml");
    let app_schema = root.join("app/src/schema.rcfg");

    let dep_a_manifest = root.join("deps/dep_a/Config.toml");
    let dep_a_schema = root.join("deps/dep_a/src/schema.rcfg");

    let dep_b_manifest = root.join("deps/dep_b/Config.toml");
    let dep_b_schema = root.join("deps/dep_b/src/schema.rcfg");

    write_file(&app_schema, "mod app { option enabled: bool = false; }");
    write_file(&dep_a_schema, "mod dep_a { option enabled: bool = false; }");
    write_file(&dep_b_schema, "mod dep_b { option enabled: bool = false; }");

    write_file(
        &app_manifest,
        r#"
[package]
name = "app"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_a = "../deps/dep_a"
"#,
    );
    write_file(
        &dep_a_manifest,
        r#"
[package]
name = "dep_a"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_b = "../dep_b"
"#,
    );
    write_file(
        &dep_b_manifest,
        r#"
[package]
name = "dep_b"
version = "0.1.0"

[entry]
schema = "src/schema.rcfg"

[dependencies]
dep_a = "../dep_a"
"#,
    );

    let error = load_manifest_graph(Some(&app_manifest)).expect_err("should fail with cycle");
    assert!(error.contains("E_PACKAGE_CYCLE"), "{error}");
}
