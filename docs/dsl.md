# DSL 语法与语义（v0.1）

## 基本对象

- `mod`：命名空间
- `enum`：枚举类型
- `option`：配置项
- `require!`：约束表达式
- `when` / `match`：条件激活
- `constraint`：约束块

## schema 示例

```rcfg
mod app {
  enum mode { Fast, Safe }

  option enabled: bool = false;
  option retries: u32 = 3;
  option selected: mode = mode::Fast;

  when enabled {
    require!(retries >= 1 && retries <= 8);
  }
}
```

## values 示例

```rcfg
include "base.rcfgv";
use app as cfg;

cfg::enabled = true;
cfg::retries = 5;
```

## 关键语义

### 激活

- `when` 条件为真时，块内项激活。
- `match` 采用“首个命中 case”规则。
- 失活项在运行时求解中不会参与最终导出。

### 类型

- bool / u8 / u16 / u32 / i32 / string / enum
- values 赋值必须满足目标类型
- 整数会执行范围/上下界检查

### require

- 可在 `mod`、`constraint`、option 附着约束中出现
- 可使用 `#[msg("...")]` 指定稳定 i18n key
- 未指定时自动生成 `<pkg>.<scope>.require.<n>`

### secret

- `#[secret]` 默认不导出
- dump 默认脱敏（除非 `--include-secrets`）
- 诊断 args 中的 secret 值会脱敏

### i18n

- `///` 第一段为 summary，后续段为 help
- 自动生成 label/help key
- `rcfg i18n extract` 输出 `i18n/<locale>.toml` 模板
