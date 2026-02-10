# 架构与模块职责

## 总体数据流

1. `parser` 解析 `.rcfg` / `.rcfgv`
2. `semantic` 收集符号并做静态检查
3. `resolve` 计算运行时激活与最终值
4. `export` 生成 C/CMake/Rust/Python 等产物
5. `cli` / `tui` 渲染诊断、状态与交互

## 分层结构（v0.7）

- `rcfg`：命令入口与参数解析（`check/export/dump/i18n/menuconfig`）
- `rcfg-app`：共享加载与分析层（schema/manifest/context/i18n + analyze/resolve）
- `rcfg-tui`：纯 TUI 引擎层（状态机、渲染、事件循环、保存）

## CLI 模块

- `src/main.rs`：进程入口
- `src/cli/args.rs`：CLI 参数定义
- `src/cli/mod.rs`：命令分发与总流程
- `src/cli/commands/*`：各子命令执行
- `src/cli/diagnostics.rs`：诊断渲染
- `src/cli/render.rs`：schema_ir/resolved 渲染
- `src/cli/i18n_extract.rs`：i18n 模板提取

## `rcfg-app` 模块

- `crates/rcfg-app/src/lib.rs`
  - `AppLoadOptions` / `AppSession`
  - `load_session()`：统一加载 schema/manifest/context/i18n
  - `analyze_values_from_path()` / `analyze_values()` / `resolve()`
  - 诊断本地化与 manifest 图构建

## `rcfg-tui` 模块

- `crates/rcfg-tui/src/lib.rs`：`TuiConfig` + `run()` 入口
- `crates/rcfg-tui/src/model.rs`：配置树（声明顺序 + 类型注入）
- `crates/rcfg-tui/src/state.rs`：UI 状态、导航、编辑、保存弹窗、帮助面板
- `crates/rcfg-tui/src/runtime.rs`：`crossterm` 事件循环与脚本模式
- `crates/rcfg-tui/src/render.rs`：`ratatui` 布局渲染
- `crates/rcfg-tui/src/save.rs`：baseline/minimal-diff 渲染

## `rcfg-lang` 模块

- `src/ast.rs`：语法树
- `src/lexer.rs`：词法分析
- `src/parser.rs`：递归下降语法解析
- `src/semantic/mod.rs`：语义门面
- `src/semantic/types.rs`：核心类型定义
- `src/semantic/exports.rs`：导出规划与生成
- `src/semantic/schema_check.rs`：schema 检查
- `src/semantic/values_check.rs`：values/include 检查
- `src/semantic/runtime.rs`：运行时求解与 require 校验
- `src/semantic/eval.rs`：表达式运行时求值
- `src/semantic/helpers.rs`：公共辅助逻辑

## 设计原则

- 先稳定行为，再拆分职责
- 公开 API 优先兼容
- 诊断 code 稳定可机器消费
- JSON 输出字段保持向后兼容（以追加为主）
