# rcfg-lang

`rcfg-lang` 是 Rconfig 的核心语言库，负责：

- `.rcfg`（schema）语法解析
- `.rcfgv`（values）语法解析
- schema / values 语义检查
- 结构化诊断（含 code + span + 来源链路）

该库不直接处理 CLI/TUI/LSP 交互，供上层工具复用。

## 当前实现范围（v0.1 进行中）

- 词法：关键字、路径、字符串、整数（含 `0x` 与 `_`）、`///` 文档注释、属性标记
- schema 语法：`use/mod/enum/option/require!/constraint/when/match`
- values 语法：`include/use/path = value;`
- 表达式：`! && || == != < <= > >= in`、函数调用、分组
- schema 语义：符号收集、重定义/冲突、表达式基础类型检查、`match` 覆盖检查
- values 语义：目标解析、`use alias`、`ctx` 赋值限制、`env` 校验、RHS enum-path 规则、重复赋值警告
- include 展开：`E_INCLUDE_NOT_FOUND`、`E_INCLUDE_CYCLE`

## 快速使用

```rust
use rcfg_lang::{
    analyze_schema, analyze_values_from_path_report, parse_schema,
};

let schema = parse_schema(r#"
mod app {
  option enabled: bool = false;
}
"#)?;

let schema_report = analyze_schema(&schema);
let symbols = schema_report.symbols;

let values_report = analyze_values_from_path_report(
    std::path::Path::new("profiles/dev.rcfgv"),
    &symbols,
);

for diag in values_report.diagnostics {
    eprintln!("{}: {}", diag.code, diag.message);
}
```

## 诊断与溯源

- `Diagnostic.source`：诊断对应的文件来源（若可确定）
- `Diagnostic.include_chain`：include 链（根文件 -> 当前文件）
- `ValuesAnalysisReport.stmt_origins`：展开后每条 values 语句的来源映射
- `ValuesAnalysisReport.diagnostic_stmt_indexes`：诊断到展开语句索引的映射（与 `diagnostics` 同长度）

## 模块结构

- `src/lexer.rs`：词法分析
- `src/parser.rs`：递归下降 + 表达式优先级解析
- `src/semantic.rs`：schema/values 语义分析
- `src/ast.rs`：schema/values AST
- `src/error.rs`：诊断结构
- `src/span.rs`：位置信息
