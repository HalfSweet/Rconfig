use std::path::PathBuf;
use std::time::Duration;

use futures_util::StreamExt;
use serde_json::{Value, json};
use tower_lsp::LspService;
use tower_lsp::jsonrpc::Request;
use tower_service::Service as TowerService;

use crate::server::Backend;

async fn call_service(
    service: &mut LspService<Backend>,
    method: &str,
    id: i32,
    params: Value,
) -> Value {
    let request = Request::build(method.to_string())
        .id(id as i64)
        .params(params)
        .finish();

    let response = TowerService::call(service, request)
        .await
        .expect("service call should succeed")
        .expect("response should exist");
    serde_json::to_value(response).expect("response to json")
}

async fn notify_service(service: &mut LspService<Backend>, method: &str, params: Value) {
    let request = Request::build(method.to_string()).params(params).finish();
    let _ = TowerService::call(service, request)
        .await
        .expect("service call should succeed");
}

async fn initialize(service: &mut LspService<Backend>) {
    let response = call_service(service, "initialize", 1, json!({ "capabilities": {} })).await;
    assert!(response.get("result").is_some(), "{response:#?}");
}

async fn wait_for_publish(socket: &mut tower_lsp::ClientSocket) -> Value {
    let request = tokio::time::timeout(Duration::from_secs(2), async {
        while let Some(message) = socket.next().await {
            if message.method() == "textDocument/publishDiagnostics" {
                return Some(message);
            }
        }
        None
    })
    .await
    .expect("timed out waiting diagnostics")
    .expect("publish diagnostics missing");

    serde_json::to_value(request).expect("request to json")
}

fn as_file_uri(path: &PathBuf) -> String {
    tower_lsp::lsp_types::Url::from_file_path(path)
        .expect("file uri")
        .to_string()
}

fn fixture_root(name: &str) -> PathBuf {
    let base = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    base.join("..")
        .join("..")
        .join("target")
        .join("tmp-tests")
        .join(name)
}

fn write_file(path: &PathBuf, text: &str) {
    if let Some(parent) = path.parent() {
        let _ = std::fs::create_dir_all(parent);
    }
    std::fs::write(path, text).expect("write file");
}

#[tokio::test]
async fn initialize_reports_capabilities() {
    let (mut service, _) = LspService::new(Backend::new);
    let response = call_service(&mut service, "initialize", 1, json!({ "capabilities": {} })).await;

    let capabilities = &response["result"]["capabilities"];
    assert_eq!(capabilities["textDocumentSync"], json!(1));
    assert_eq!(capabilities["hoverProvider"], json!(true));
    assert_eq!(capabilities["definitionProvider"], json!(true));
    assert_eq!(capabilities["documentSymbolProvider"], json!(true));
}

#[tokio::test]
async fn publishes_diagnostics_on_open_and_change() {
    let root = fixture_root("lsp_diagnostics_flow");
    let schema_path = root.join("schema.rcfg");

    write_file(
        &schema_path,
        r#"
mod app {
  option enabled bool = false;
}
"#,
    );

    let bad_text = std::fs::read_to_string(&schema_path).expect("read schema");
    let good_text = r#"
mod app {
  option enabled: bool = false;
}
"#;
    let schema_uri = as_file_uri(&schema_path);

    let (mut service, mut socket) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "languageId": "rcfg",
                "version": 1,
                "text": bad_text,
            }
        }),
    )
    .await;

    let first_publish = wait_for_publish(&mut socket).await;
    let first_diags = first_publish["params"]["diagnostics"]
        .as_array()
        .cloned()
        .unwrap_or_default();
    assert!(!first_diags.is_empty(), "{first_publish:#?}");

    notify_service(
        &mut service,
        "textDocument/didChange",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "version": 2
            },
            "contentChanges": [
                { "text": good_text }
            ]
        }),
    )
    .await;

    let mut saw_no_error = false;
    for _ in 0..4 {
        let publish = wait_for_publish(&mut socket).await;
        let diagnostics = publish["params"]["diagnostics"]
            .as_array()
            .cloned()
            .unwrap_or_default();
        let has_error = diagnostics.iter().any(|diag| diag["severity"] == 1);
        if !has_error {
            saw_no_error = true;
            break;
        }
    }
    assert!(
        saw_no_error,
        "expected parse error to disappear after didChange"
    );

    notify_service(
        &mut service,
        "textDocument/didSave",
        json!({
            "textDocument": {
                "uri": schema_uri
            }
        }),
    )
    .await;

    let save_publish = wait_for_publish(&mut socket).await;
    let save_diags = save_publish["params"]["diagnostics"]
        .as_array()
        .cloned()
        .unwrap_or_default();
    let save_has_error = save_diags.iter().any(|diag| diag["severity"] == 1);
    assert!(
        !save_has_error,
        "didSave should not reintroduce parse errors: {save_publish:#?}"
    );
}

#[tokio::test]
async fn hover_goto_completion_and_symbols_work() {
    let root = fixture_root("lsp_features");
    let schema_path = root.join("schema.rcfg");

    write_file(
        &schema_path,
        r#"
mod app {
  /// Option doc.
  option enabled: bool = false;

  enum Mode { off, on }
  option mode: Mode = off;

  use app::enabled as enabled_alias;

  when enabled {
    require!(enabled == true);
  }
}
"#,
    );

    let schema_text = std::fs::read_to_string(&schema_path).expect("read schema");
    let schema_uri = as_file_uri(&schema_path);

    let (mut service, mut socket) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "languageId": "rcfg",
                "version": 1,
                "text": schema_text,
            }
        }),
    )
    .await;

    let _ = wait_for_publish(&mut socket).await;

    let hover_response = call_service(
        &mut service,
        "textDocument/hover",
        2,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 3, "character": 10}
        }),
    )
    .await;

    let hover_text = hover_response["result"]["contents"]["value"]
        .as_str()
        .or_else(|| hover_response["result"]["contents"].as_str())
        .unwrap_or_default();
    assert!(
        hover_text.contains("option") || hover_text.contains("enabled"),
        "{hover_response:#?}"
    );

    let goto_response = call_service(
        &mut service,
        "textDocument/definition",
        3,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 10, "character": 11}
        }),
    )
    .await;

    assert!(goto_response["result"].is_object() || goto_response["result"].is_array());

    let completion_response = call_service(
        &mut service,
        "textDocument/completion",
        4,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 10, "character": 13}
        }),
    )
    .await;

    assert!(completion_response["result"].is_array() || completion_response["result"].is_object());

    let symbols_response = call_service(
        &mut service,
        "textDocument/documentSymbol",
        5,
        json!({
            "textDocument": {"uri": schema_uri}
        }),
    )
    .await;

    assert!(
        symbols_response["result"].is_array(),
        "{symbols_response:#?}"
    );
}

#[tokio::test]
async fn values_without_schema_returns_response() {
    let root = fixture_root("lsp_values_parse_only");
    let values_path = root.join("values.rcfgv");

    write_file(
        &values_path,
        r#"
include ;
"#,
    );

    let values_text = std::fs::read_to_string(&values_path).expect("read values");
    let values_uri = as_file_uri(&values_path);

    let (mut service, _) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": values_uri,
                "languageId": "rcfgv",
                "version": 1,
                "text": values_text,
            }
        }),
    )
    .await;
}

#[tokio::test]
async fn hover_and_goto_definition_support_use_alias() {
    let root = fixture_root("lsp_alias_hover_goto");
    let schema_path = root.join("schema.rcfg");

    write_file(
        &schema_path,
        r#"mod app {
  option enabled: bool = false;
  use enabled as enabled_alias;
  when enabled_alias {
    require!(enabled_alias == true);
  }
}
"#,
    );

    let schema_text = std::fs::read_to_string(&schema_path).expect("read schema");
    let schema_uri = as_file_uri(&schema_path);

    let (mut service, mut socket) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "languageId": "rcfg",
                "version": 1,
                "text": schema_text,
            }
        }),
    )
    .await;

    let _ = wait_for_publish(&mut socket).await;

    let hover_response = call_service(
        &mut service,
        "textDocument/hover",
        21,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 2, "character": 22}
        }),
    )
    .await;

    let hover_text = hover_response["result"]["contents"]["value"]
        .as_str()
        .or_else(|| hover_response["result"]["contents"].as_str())
        .unwrap_or_default();
    assert!(hover_text.contains("use alias"), "{hover_response:#?}");
    assert!(
        hover_text.contains("raw path: `enabled`"),
        "{hover_response:#?}"
    );
    assert!(
        hover_text.contains("target: `app::enabled`"),
        "{hover_response:#?}"
    );

    let goto_response = call_service(
        &mut service,
        "textDocument/definition",
        22,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 2, "character": 22}
        }),
    )
    .await;

    assert_eq!(
        goto_response["result"]["range"]["start"]["line"],
        json!(1),
        "{goto_response:#?}"
    );
}

fn completion_items(response: &Value) -> Vec<Value> {
    if let Some(items) = response["result"].as_array() {
        return items.to_vec();
    }

    response["result"]["items"]
        .as_array()
        .cloned()
        .unwrap_or_default()
}

#[tokio::test]
async fn completion_for_values_lhs_returns_option_paths_with_incremental_insert() {
    let root = fixture_root("lsp_completion_values_lhs");
    let schema_path = root.join("schema.rcfg");
    let values_path = root.join("values.rcfgv");

    write_file(
        &schema_path,
        r#"mod app {
  option enabled: bool = false;
  option mode: bool = false;
}
"#,
    );
    write_file(&values_path, "app::en = true;\n");

    let schema_text = std::fs::read_to_string(&schema_path).expect("read schema");
    let values_text = std::fs::read_to_string(&values_path).expect("read values");
    let schema_uri = as_file_uri(&schema_path);
    let values_uri = as_file_uri(&values_path);

    let (mut service, mut socket) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "languageId": "rcfg",
                "version": 1,
                "text": schema_text,
            }
        }),
    )
    .await;
    let _ = wait_for_publish(&mut socket).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": values_uri,
                "languageId": "rcfgv",
                "version": 1,
                "text": values_text,
            }
        }),
    )
    .await;
    let _ = wait_for_publish(&mut socket).await;

    let completion_response = call_service(
        &mut service,
        "textDocument/completion",
        31,
        json!({
            "textDocument": {"uri": values_uri},
            "position": {"line": 0, "character": 7}
        }),
    )
    .await;

    let items = completion_items(&completion_response);
    let enabled_item = items
        .iter()
        .find(|item| item["detail"].as_str() == Some("app::enabled"))
        .expect("should contain app::enabled candidate");

    assert_eq!(enabled_item["kind"], json!(5));
    assert_eq!(enabled_item["insertText"], json!("abled"));
}

#[tokio::test]
async fn completion_respects_scope_and_filters_unreachable_symbols() {
    let root = fixture_root("lsp_completion_scope_filter");
    let schema_path = root.join("schema.rcfg");

    write_file(
        &schema_path,
        r#"mod app {
  option enabled: bool = false;

  when en {
    require!(en == true);
  }
}

mod other {
  option enabled: bool = false;
}
"#,
    );

    let schema_text = std::fs::read_to_string(&schema_path).expect("read schema");
    let schema_uri = as_file_uri(&schema_path);

    let (mut service, mut socket) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "languageId": "rcfg",
                "version": 1,
                "text": schema_text,
            }
        }),
    )
    .await;
    let _ = wait_for_publish(&mut socket).await;

    let completion_response = call_service(
        &mut service,
        "textDocument/completion",
        32,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 3, "character": 9}
        }),
    )
    .await;

    let items = completion_items(&completion_response);
    let app_enabled = items
        .iter()
        .find(|item| item["detail"].as_str() == Some("app::enabled"))
        .expect("should include app::enabled in app scope");

    assert_eq!(app_enabled["insertText"], json!("abled"));
    assert!(
        items
            .iter()
            .all(|item| item["detail"].as_str() != Some("other::enabled")),
        "other::enabled should not be visible in app scope"
    );
}

#[tokio::test]
async fn completion_falls_back_to_heuristic_for_incomplete_use_line() {
    let root = fixture_root("lsp_completion_use_heuristic");
    let schema_path = root.join("schema.rcfg");

    write_file(
        &schema_path,
        r#"mod app {
  option enabled: bool = false;
}

use app::en
"#,
    );

    let schema_text = std::fs::read_to_string(&schema_path).expect("read schema");
    let schema_uri = as_file_uri(&schema_path);

    let (mut service, mut socket) = LspService::new(Backend::new);
    initialize(&mut service).await;

    notify_service(
        &mut service,
        "textDocument/didOpen",
        json!({
            "textDocument": {
                "uri": schema_uri,
                "languageId": "rcfg",
                "version": 1,
                "text": schema_text,
            }
        }),
    )
    .await;
    let _ = wait_for_publish(&mut socket).await;

    let completion_response = call_service(
        &mut service,
        "textDocument/completion",
        33,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 4, "character": 11}
        }),
    )
    .await;

    let items = completion_items(&completion_response);
    let enabled = items
        .iter()
        .find(|item| item["detail"].as_str() == Some("app::enabled"))
        .expect("heuristic use completion should still return app::enabled");

    assert_eq!(enabled["insertText"], json!("abled"));
}
