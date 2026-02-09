# 5 分钟上手

本文演示如何从零开始运行一套最小配置流程。

## 1. 准备 schema

新建 `schema.rcfg`：

```rcfg
mod app {
  /// 是否启用功能
  option enabled: bool = false;

  /// 重试次数
  option retries: u32 = 3;
}
```

## 2. 准备 values

新建 `profile.rcfgv`：

```rcfg
app::enabled = true;
app::retries = 5;
```

## 3. 校验

```bash
rcfg check --schema schema.rcfg --values profile.rcfgv
```

如果无错误，命令返回码为 `0`。

## 4. 导出

```bash
rcfg export \
  --schema schema.rcfg \
  --values profile.rcfgv \
  --out-h build/config.h \
  --out-cmake build/config.cmake
```

输出：

- `build/config.h`
- `build/config.cmake`

## 5. 查看求解与诊断 JSON

```bash
rcfg dump \
  --schema schema.rcfg \
  --values profile.rcfgv \
  --out build/resolved.json \
  --out-schema-ir build/schema_ir.json \
  --out-diagnostics build/diagnostics.json
```

适用场景：

- UI/TUI 调试配置状态
- CI 分析诊断 code 与定位
- 下游工具消费稳定 JSON 结构

## 6. 生成 i18n 模板

```bash
rcfg i18n extract \
  --schema schema.rcfg \
  --locale zh-CN \
  --out i18n/zh-CN.toml
```

模板结构：

- `locale = "zh-CN"`
- `[strings]` 下为 `key = fallback`
