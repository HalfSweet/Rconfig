# CLI 命令手册

## 全局参数

- `--schema <PATH>`：schema 文件路径
- `--manifest <PATH>`：`Config.toml` 路径（可提供 `entry.schema`）
- `--strict`：严格模式（部分 warning 升级为 error）
- `--context <PATH>`：上下文注入 JSON（`ctx`）
- `--i18n <PATH>`：语言包 TOML（用于诊断消息 fallback）

## `rcfg check`

校验 schema + values 并输出诊断。

```bash
rcfg check --schema schema.rcfg --values profile.rcfgv
```

关键参数：

- `--values <PATH>`（必填）
- `--format human|json`（默认 `human`）

## `rcfg export`

生成配置导出文件。

```bash
# 兼容旧方式：一次同时导出 C header + CMake
rcfg export   --schema schema.rcfg   --values profile.rcfgv   --out-h build/config.h   --out-cmake build/config.cmake

# 新方式：按格式与输出路径成对指定（可重复）
rcfg export   --schema schema.rcfg   --values profile.rcfgv   --format rust   --out build/config.rs   --format python --out build/config.py
```

关键参数：

- `--format c-header|cmake|rust|python` + `--out <PATH>`：导出目标（可重复，按顺序一一对应）
- `--out-h <PATH>` + `--out-cmake <PATH>`：旧版导出参数，保留兼容
- `--export-secrets`：允许导出 `#[secret]`
- `--c-prefix <STR>`：C 宏前缀，默认 `CONFIG_`
- `--cmake-prefix <STR>`：CMake 变量前缀，默认 `CFG_`
- `--bool-false-style omit|define-0`：bool=false 的 C 导出策略（默认 `omit`）
- `--enum-export-style one-hot|string`：enum 的 C 导出策略（默认 `one-hot`）
- `--int-export-format decimal|hex`：整数导出格式（默认 `decimal`）
- `--export-name-rule pkg-path|path-only`：导出名规则（默认 `pkg-path`）
- `--diag-format human|json`：导出阶段诊断输出格式（默认 `human`）
- 导出阶段会比较产物内容哈希；内容不变时不会重写文件（便于减少无效重编译）
- 构建系统集成示例：`docs/cmake.md`

## `rcfg dump`

输出求解结果和可选的 schema/diagnostics JSON。

```bash
rcfg dump \
  --schema schema.rcfg \
  --values profile.rcfgv \
  --out build/resolved.json
```

关键参数：

- `--out <PATH>`（必填）
- `--out-schema-ir <PATH>`：输出 schema IR
- `--out-diagnostics <PATH>`：输出 diagnostics JSON
- `--include-secrets`：dump 中包含 secret 真实值

## `rcfg i18n extract`

提取语言包模板。

```bash
rcfg i18n extract --schema schema.rcfg --out i18n/zh-CN.toml --locale zh-CN
```

关键参数：

- `--out <PATH>`（必填）
- `--locale <STR>`（默认 `en`）

## 返回码约定

- `0`：成功（即使有 warning）
- `1`：执行失败（含 error 诊断或 I/O 错误）
