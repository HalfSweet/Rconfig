# rcfg-lang

`rcfg-lang` 是 Rconfig 的语言核心库，负责：

- `.rcfg` / `.rcfgv` 解析
- schema 与 values 语义分析
- 运行时求解（激活、缺值、require）
- 导出规划与生成（C / CMake）
- 结构化诊断（含 code、message_key、args、related）

该库面向上层工具复用（CLI / TUI / LSP / 插件等）。

## 与项目文档的关系

- 项目总览：[`../../README.md`](../../README.md)
- 快速上手：[`../../docs/quickstart.md`](../../docs/quickstart.md)
- DSL 说明：[`../../docs/dsl.md`](../../docs/dsl.md)
- 架构文档：[`../../docs/architecture.md`](../../docs/architecture.md)

## 对外 API 分组

> 本轮重构保持 API 兼容，主要是内部模块工程化拆分。

### 解析 API

- `parse_schema`
- `parse_schema_with_diagnostics`
- `parse_values`
- `parse_values_with_diagnostics`

### schema 语义 API

- `analyze_schema`
- `analyze_schema_strict`
- `analyze_schema_files`
- `analyze_schema_files_strict`

### values 语义 API

- `analyze_values`
- `analyze_values_strict`
- `analyze_values_with_context`
- `analyze_values_with_context_strict`
- `analyze_values_from_path*`
- `analyze_values_from_path_report*`
- `expand_values_includes_*`

### 求解与导出 API

- `resolve_values`
- `resolve_values_with_context`
- `plan_c_header_exports*`
- `generate_exports`

### 核心类型

- `SymbolTable` / `SymbolKind` / `SymbolInfo`
- `ResolvedConfig` / `ResolvedOption` / `ResolvedValue` / `ValueSource`
- `Diagnostic` / `Severity` / `RelatedInfo`
- `ExportOptions` / `GeneratedExports` / `PlannedExport`

## 模块结构（重构后）

- `src/semantic/mod.rs`：语义门面（re-export）
- `src/semantic/types.rs`：核心类型定义
- `src/semantic/exports.rs`：导出规划与生成
- `src/semantic/schema_check.rs`：schema 符号与类型检查
- `src/semantic/values_check.rs`：values/include 检查
- `src/semantic/runtime.rs`：运行时状态与 require 校验
- `src/semantic/eval.rs`：表达式运行时求值
- `src/semantic/helpers.rs`：通用辅助逻辑

> 当前采用“facade + 分文件实现”的方式，优先保证行为不变与 API 兼容。

## 示例

```rust
use rcfg_lang::{
    analyze_schema,
    analyze_values_from_path_report,
    parse_schema,
};

let schema = parse_schema(r#"
mod app {
  option enabled: bool = false;
}
"#)?;

let schema_report = analyze_schema(&schema);
let values_report = analyze_values_from_path_report(
    std::path::Path::new("profiles/dev.rcfgv"),
    &schema_report.symbols,
);

for diag in values_report.diagnostics {
    eprintln!("{}: {}", diag.code, diag.message);
}
# Ok::<(), Vec<rcfg_lang::Diagnostic>>(())
```

## 开发建议

- 重构优先保持行为等价（测试先绿再拆）
- 诊断 code 视为稳定契约
- 新增语义规则请同步补 `semantic_tests.rs`
