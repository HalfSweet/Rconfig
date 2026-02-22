# 开发与贡献指南

## 环境要求

- Rust stable（建议与仓库当前 toolchain 一致）
- 可执行 `cargo`

## 常用命令

```bash
cargo check --workspace --all-targets
cargo test --workspace
```

## 提交流程

建议采用小步提交（Conventional Commits）：

- `feat:` 新能力
- `fix:` 修复
- `refactor:` 结构调整（不改行为）
- `test:` 测试
- `docs:` 文档
- `chore:` 构建/元数据

## 代码组织约定

- CLI 与语言库分层清晰，避免跨层直接依赖内部细节
- 单文件过大时优先拆模块，不做无关行为改动
- 高风险重构必须先保证测试覆盖，再做迁移

## 质量门禁

合并前最低要求：

1. `cargo check --workspace --all-targets` 通过
2. `cargo test --workspace` 通过
3. 对外行为不变的重构需说明“行为等价”

## Fuzz 测试（Parser）

当前仓库提供两个 fuzz target：

- `fuzz_parse_schema`
- `fuzz_parse_values`

首次使用前请安装 `cargo-fuzz`：

```bash
cargo install cargo-fuzz
```

运行 5 分钟 smoke fuzz：

```bash
cargo fuzz run fuzz_parse_schema --manifest-path fuzz/Cargo.toml -- -max_total_time=300
cargo fuzz run fuzz_parse_values --manifest-path fuzz/Cargo.toml -- -max_total_time=300
```

初始语料库位于：

- `fuzz/corpus/fuzz_parse_schema/`
- `fuzz/corpus/fuzz_parse_values/`

## 文档约定

- 用户文档与开发文档统一放 `docs/`
- 当前阶段文档语言为简体中文
- 重要行为变更需同步更新对应文档页面
