# rcfg-lang

`rcfg-lang` 是 Rconfig 的核心语言库，负责：

- `.rcfg`（schema）语法解析
- `.rcfgv`（values）语法解析
- 统一 AST 表达
- 结构化语法诊断（含 code + span）

该库不直接处理 CLI/TUI/LSP 交互，供上层工具复用。

## 当前实现范围（v0.1 parser 基础）

- 词法：关键字、路径、字符串、整数（含 `0x` 与 `_`）、`///` 文档注释、属性标记
- schema 语法：`use/mod/enum/option/require!/constraint/when/match`
- values 语法：`include/use/path = value;`
- 表达式：`! && || == != < <= > >= in`、函数调用、分组
- 预留语法诊断：`patch/export`、`#[cfg(...)]` 报 `E_FEATURE_NOT_SUPPORTED`
- 条件块中 `enum` 声明诊断：`E_ILLEGAL_TYPE_DECL_IN_CONDITIONAL`

## 快速使用

```rust
use rcfg_lang::{parse_schema, parse_values};

let schema = parse_schema(r#"
mod usart1 {
  option enable: bool = false;
}
"#)?;

let values = parse_values(r#"
usart1::enable = true;
"#)?;
```

如需拿到非阻断告警/错误全集，可用：

- `parse_schema_with_diagnostics`
- `parse_values_with_diagnostics`

## 模块结构

- `src/lexer.rs`：词法分析
- `src/parser.rs`：递归下降 + 表达式优先级解析
- `src/ast.rs`：schema/values AST
- `src/error.rs`：诊断结构
- `src/span.rs`：位置信息
