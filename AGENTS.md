# Rconfig Agent Brief

## 项目概述

Rconfig 是一个面向配置场景的声明式 DSL 工具链，目标是提供类似 Kconfig 的配置建模体验，并保持更清晰的语义、可解释的诊断和可预测的导出结果。

- 主要 DSL 文件：
  - `*.rcfg`：Schema（声明 option/enum/激活规则/约束/导出元信息）
  - `*.rcfgv`：Values（include/use/赋值）
- 核心语义：`when`/`match` 激活、`require!` 约束、`#[range]` 范围约束、`#[secret]` 脱敏与受控导出、i18n key 提取。

## 当前实现状态（基于仓库现状）

- 主链路已完成：词法/语法/语义分析、运行时求解、多格式导出、CLI 与 i18n 提取。
- 包模型能力已落地：`Config.toml`、path 依赖、跨包加载与命名空间隔离。
- CLI 已提供：`check`、`export`、`dump`、`i18n extract`、`menuconfig`。
- 导出已支持：`c-header`、`cmake`、`rust`、`python`。
- 仓库已包含 `rcfg-lsp` 与 `rcfg-tui` 两个 crate，并具有可运行的初版能力（诊断/跳转/补全与交互式配置编辑）。

> 规划与勾选状态以 `_prd/todo.md`、`_prd/stream.md` 为准；若代码与清单不一致，需优先补齐文档同步。

## 仓库结构速览

- `src/`：`rcfg` CLI 入口与命令实现。
- `crates/rcfg-lang/`：语言核心（lexer/parser/semantic/runtime/export）。
- `crates/rcfg-app/`：CLI/TUI 共享的加载与分析编排层。
- `crates/rcfg-lsp/`：LSP Server（诊断、Hover、GoToDefinition、补全、文档符号）。
- `crates/rcfg-tui/`：menuconfig 终端交互引擎。
- `docs/`：用户与开发文档。
- `_prd/`：产品定义、工程化约束、路线图与待办。
- `tests/`：CLI/TUI 集成测试。

## 推荐阅读顺序（开始改动前）

1. `README.md`（定位与命令入口）
2. `docs/dsl.md`（语法与语义最小定义）
3. `docs/architecture.md`（模块边界）
4. `_prd/todo.md`（进度与阶段目标）
5. `_prd/stream.md`（能力依赖关系）

## 开发与提交流程约定

- 采用小步迭代，优先最小可验证改动。
- **每完成一个最小粒度改动后立即提交 git commit**。
- 提交信息遵循 Conventional Commits（如 `feat:` / `fix:` / `refactor:` / `test:` / `docs:` / `chore:`）。
- 代码行为变更需同步更新 `docs/` 与必要的 `_prd/` 状态。
- 涉及 Rust 代码改动时，至少执行：
  - `cargo check --workspace --all-targets`
  - `cargo test --workspace`

## 当前阶段重点（建议）

- 对齐 `_prd/todo.md` 与真实代码进度，避免“实现已完成但清单未更新”。
- 继续增强 LSP 稳定性与协议测试覆盖。
- 推进编辑器插件与格式化能力。
- 完善 TUI 交互细节、可用性与保存流程。
