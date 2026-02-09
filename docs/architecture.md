# 架构与模块职责

## 总体数据流

1. `parser` 解析 `.rcfg` / `.rcfgv`
2. `semantic` 收集符号并做静态检查
3. `resolve` 计算运行时激活与最终值
4. `export` 生成 C/CMake
5. `cli` 渲染 human/json 诊断与 dump/i18n 输出

## CLI 模块

- `src/main.rs`：进程入口
- `src/cli/args.rs`：CLI 参数定义
- `src/cli/mod.rs`：调度与总流程
- `src/cli/commands/*`：各子命令执行
- `src/cli/loaders.rs`：manifest/context/i18n 加载
- `src/cli/diagnostics.rs`：诊断渲染与本地化 fallback
- `src/cli/render.rs`：schema_ir/resolved 渲染
- `src/cli/i18n_extract.rs`：i18n 模板提取

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
