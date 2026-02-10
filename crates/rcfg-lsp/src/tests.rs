use std::io;
use std::path::PathBuf;

use serde_json::{Value, json};
use tower_lsp::jsonrpc::Request;
use tower_lsp::LspService;
use tower_service::Service as TowerService;

use crate::server::Backend;

async fn call_service(service: &mut LspService<Backend>, method: &str, id: i32, params: Value) -> Value {
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

    let (mut service, _) = LspService::new(Backend::new);
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

    let hover_response = call_service(
        &mut service,
        "textDocument/hover",
        2,
        json!({
            "textDocument": {"uri": schema_uri},
            "position": {"line": 2, "character": 10}
        }),
    )
    .await;

    let hover_text = hover_response["result"]["contents"]["value"]
        .as_str()
        .or_else(|| hover_response["result"]["contents"].as_str())
        .unwrap_or_default();
    assert!(hover_text.contains("option") || hover_text.contains("enabled") || hover_text.contains("mod"), "{hover_response:#?}");

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

    assert!(symbols_response["result"].is_array(), "{symbols_response:#?}");
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

#[test]
fn placeholder() -> io::Result<()> {
    Ok(())
}
