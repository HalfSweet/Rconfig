# Rconfig 项目进度与待办

> 最后更新：2026-02-10
>
> **项目统计**：128 commits · 127 tests · ~14,000 行 Rust 代码 · v0.1–v0.4 已完成

---

## v0.1 — 已完成（基础工具链）

核心能力已就绪：词法/语法/语义分析、运行时求解、C header + CMake 导出、CLI 四命令、i18n 提取。

<details>
<summary>v0.1 完成清单（点击展开）</summary>

- Lexer：75 种 token、关键字/保留字、文档注释、字符串转义、整数字面量
- Parser：递归下降 + Pratt parsing、schema/values 完整语法、错误恢复
- AST：Schema + Values 完整节点、Span 贯穿
- 语义分析 Schema：符号收集、类型检查、match 穷尽性/重叠/不可达、激活追踪、循环检测、lint
- 语义分析 Values：赋值解析、use 别名、类型/范围校验、缺值检测、env() 校验
- Include：递归展开、循环检测、来源追踪
- 运行时求解：迭代激活集合、默认值应用、require 运行时校验、值来源追踪
- 导出：C header + CMake、enum one-hot、secret 过滤、前缀可配置
- CLI：check / export / dump / i18n extract、--strict / --format json / --context / --manifest
- IR/JSON：schema_ir.json / resolved.json / diagnostics.json
- 整数类型：U8/U16/U32/I32 拆分
- Diagnostic：args / message_key / note / related
- i18n：key 自动生成、extract 命令、语言包 fallback
- 测试：127 个（3 loaders + 7 parser + 93 semantic + 24 CLI）

</details>

---

## v0.2 — 已完成（包模型与 Manifest 强化）

Config.toml 解析、包名命名空间、path 依赖递归加载与循环检测、多包符号合并与跨包引用、env() fallback。

<details>
<summary>v0.2 完成清单（点击展开）</summary>

### 2.1 Config.toml 完整解析

- [x] `[package]` 段：name / version / entry（schema 入口）
- [x] `[dependencies]` 段：包名 → path 路径映射
- [x] manifest 校验：缺失必填字段报错、entry 文件不存在报错

### 2.2 包名命名空间

- [x] 包名作为符号路径隐式前缀（IR 层 `pkg::mod::option`）
- [x] 同包内引用可省略包名前缀
- [x] schema_ir.json / resolved.json 输出携带包名前缀

### 2.3 path 依赖解析

- [x] 按 `[dependencies]` 中的 path 定位依赖包的 Config.toml
- [x] 递归加载依赖包的 schema（深度优先）
- [x] 依赖循环检测（E_PACKAGE_CYCLE）

### 2.4 多包加载与合并

- [x] 多包符号表合并（包名前缀隔离）
- [x] 跨包符号冲突检测
- [x] 跨包 `use` 路径引用（`use hal_uart::usart1`）
- [x] 跨包 when/match/require 条件引用

### 2.5 env() fallback 支持

- [x] 解除 `env("NAME", "fallback")` 的 E_FEATURE_NOT_SUPPORTED 限制
- [x] fallback 值类型检查（与目标 option 类型一致）

### 2.6 测试

- [x] 多包加载集成测试
- [x] 跨包引用语义测试
- [x] 依赖循环检测测试
- [x] env() fallback 测试

</details>

---

## v0.3 — 已完成（C/CMake 导出增强）

导出可配置性（bool false 策略、enum 导出策略、十六进制整数、导出名规则）、include guard、CMake 文件头注释、构建系统集成示例、hash 变更检测、patch 语法全链路。

<details>
<summary>v0.3 完成清单（点击展开）</summary>

### 3.1 导出可配置性

- [x] bool false 导出策略可选：不定义（Kconfig 默认） vs `#define X 0`
- [x] enum 导出策略可选：one-hot 宏（默认） vs 字符串宏
- [x] 整数导出格式可选：十进制（默认） vs 十六进制
- [x] 导出名规则可配置：pkg+path（默认） vs 仅 path

### 3.2 导出稳定性

- [x] C header 添加 `#ifndef` / `#define` / `#endif` 传统 include guard（与 `#pragma once` 并存）
- [x] CMake 添加文件头注释（工具版本 + 生成时间戳可选）
- [x] 导出产物 diff 友好：确定性排序、无多余空行

### 3.3 构建系统集成

- [x] CMake `find_package(Rconfig)` 模块或 `add_custom_command` 示例
- [x] 导出产物变更检测（hash 比较，避免无变更时触发重编译）

### 3.4 patch 语法（default 覆盖）

- [x] parser 解除 `patch` 的 E_FEATURE_NOT_SUPPORTED 限制
- [x] patch 语义：只能修改默认值与附加约束，不能改变类型
- [x] patch 受 when/match 激活语义影响
- [x] 值来源优先级：user > patch > default

### 3.5 测试

- [x] 导出格式可配置性测试
- [x] patch 语义测试
- [x] 构建系统集成示例验证

</details>

---

## v0.4 — 已完成（多语言 Wrapper）

ConfigExporter trait 统一导出接口、C header / CMake / Rust / Python 四种内建导出器、CLI `--format`/`--out` 多目标导出。

<details>
<summary>v0.4 完成清单（点击展开）</summary>

### 4.1 Wrapper 接口抽象

- [x] 定义通用导出 trait/接口（输入 ResolvedConfig，输出文件内容）
- [x] 将现有 C header / CMake 导出重构为 trait 实现
- [x] 导出器注册机制（按名称选择导出器）

### 4.2 Rust 导出

- [x] 生成 `config.rs`：`pub const CONFIG_X: u32 = 123;`
- [x] bool → `pub const`、enum → `pub enum` + `pub const SELECTED: Type = ...`
- [x] 类型精确映射（u8/u16/u32/i32/bool/&str）

### 4.3 Python 导出

- [x] 生成 `config.py`：`CONFIG_X = 123`
- [x] bool → `True`/`False`、enum → 字符串常量

### 4.4 CLI 扩展

- [x] `rcfg export --format rust --out config.rs`
- [x] `rcfg export --format python --out config.py`
- [x] `--format` 支持多值（同时导出多种格式）

### 4.5 测试

- [x] Rust 导出正确性测试
- [x] Python 导出正确性测试
- [x] 多格式同时导出测试

</details>

---

## 阶段 0 — rcfg-lang 增强层（v0.5–v0.7 共同前置）

> 所有后续 Stream 的基础依赖。不影响现有公共 API 兼容性，纯增量添加。
> 详见 [stream.md](stream.md) 依赖关系图。

### 0.1 Span 行列转换

- [x] 实现 byte offset → line:column 转换工具（基于源文本换行索引）
- [x] 为 LSP `Position` / TUI 光标定位提供统一接口

### 0.2 反向符号索引

- [x] 构建 position → symbol 反向查找结构（interval tree 或排序 vec）
- [x] 支持 hover / goto-definition 按光标位置定位符号

### 0.3 补全声明 Span

- [x] 为 mod 声明添加 span 记录（当前仅 option / enum variant 有 span）
- [x] 为 enum 声明本身添加 span 记录

### 0.4 文档注释关联

- [x] 将 `///` 文档注释关联到 SymbolTable 对应符号
- [x] hover 时可展示文档内容

### 0.5 引用追踪（可延后至 v0.5 Step 3）

- [x] 记录每个符号被引用的所有位置
- [x] 支持 find-all-references

---

## v0.5 — Stream A：LSP Server

> 目标：实时诊断、补全、跳转、Hover、DocumentSymbol 最小可用链路。
> 依赖阶段 0 完成。新建 `crates/rcfg-lsp` workspace member。
> 详细设计见 [lsp-plan.md](lsp-plan.md)。

### 5.1 Step 1 — Crate 脚手架与文档同步

- [x] 创建 `crates/rcfg-lsp`，加入 workspace members
- [x] `main.rs`：tower-lsp stdio server 启动入口
- [x] `server.rs`：`LanguageServer` trait 骨架（initialize 返回 capabilities）
- [x] `document.rs`：`DocumentState`（uri / version / text / kind / is_open）
- [ ] `document.rs`：`ProjectSnapshot`（project_key / schema_docs / uri_base_offsets / analysis / doc_indexes）
- [ ] `project.rs`：Config.toml 向上查找 + manifest 解析（轻量副本）
- [ ] `project.rs`：`.rcfgv` schema 归属三层回退（manifest → 同目录 .rcfg → parse-only）
- [ ] `didOpen / didChange / didClose / didSave` 文档同步
- [ ] `didChange` 150ms 防抖（tokio::time::sleep + version 检查）
- [ ] `didClose` 诊断清理：schema 文件触发重分析后决定保留/清空；values 文件立即清空

### 5.2 Step 1 — UTF-16 位置系统

- [ ] `position.rs`：`offset_to_lsp_position`（byte offset → UTF-16 line:character）
- [ ] `position.rs`：`lsp_position_to_offset`（UTF-16 Position → byte offset）
- [ ] `position.rs`：`span_to_lsp_range`（Span → LSP Range）
- [ ] 不复用 `SourceIndex::offset_to_position` 的 character 结果（byte-based ≠ UTF-16）
- [ ] 所有偏移 clamp，越界安全回退

### 5.3 Step 1 — 诊断推送

- [ ] `providers/diagnostics.rs`：schema 诊断流水线
  - [ ] 三步合并 schema 集合：manifest 图基线 → open 文档覆盖 → 补充非 manifest open .rcfg
  - [ ] 每文件分配 base_offset，AST span rebasing（local + base）
  - [ ] `analyze_schema_files` 统一语义分析
  - [ ] parse + semantic 诊断合并、去重
  - [ ] global offset → uri + local span → UTF-16 Range 映射
  - [ ] `textDocument/publishDiagnostics` 发布
- [ ] `providers/diagnostics.rs`：values 诊断流水线
  - [ ] 有 schema 上下文时走 include-aware 语义分析
  - [ ] 无 schema 时仅 parse 诊断
- [ ] 诊断字段映射：severity / code / source="rcfg-lang" / note / related
- [ ] stale 诊断清理：对不再有诊断的 URI publish 空数组
- [ ] **交付物**：编辑器内实时红色波浪线 + 错误详情

### 5.4 Step 2 — Hover

- [ ] LSP Position → byte offset（UTF-16 转换）→ global offset 定位链路
- [ ] `SymbolPositionIndex::find_symbol_at_offset` 命中符号
- [ ] `providers/hover.rs`：option hover（kind + 类型 + 默认值 + `///` docs）
- [ ] `providers/hover.rs`：enum hover（kind + variant 列表 + `///` docs）
- [ ] `providers/hover.rs`：mod hover（kind + `///` docs）
- [ ] `providers/hover.rs`：`use` 别名 hover（原始路径 + 目标符号信息）
- [ ] 输出格式：Markdown

### 5.5 Step 2 — GoToDefinition

- [ ] `providers/goto_def.rs`：option 引用 → option 声明位置
- [ ] `providers/goto_def.rs`：enum variant 引用 → variant 声明位置
- [ ] `providers/goto_def.rs`：`use` 别名 → 原始路径声明位置
- [ ] `ProjectSnapshot` 中建立 `alias_span → target_symbol_path` 索引
- [ ] 返回 `Location`（支持跨文件跳转）

### 5.6 Step 2 — DocumentSymbol

- [ ] `providers/document_symbol.rs`：返回嵌套树结构（`DocumentSymbol[]`）
- [ ] 节点映射：mod→Module / enum→Enum / variant→EnumMember / option→Field
- [ ] 遍历 `schema_items()` 递归构建，range 取声明整体 span，selectionRange 取名称 span

### 5.7 Step 3 — 符号补全

- [ ] `providers/completion.rs`：`::` 路径补全（schema 中 option / enum / variant）
- [ ] `providers/completion.rs`：`use` 语句包路径补全
- [ ] `providers/completion.rs`：values 赋值目标补全（`.rcfgv` 左侧路径）
- [ ] 上下文判定：优先 AST/span，不完整时回退词法片段启发式（行内前缀 + `::` 分隔符）
- [ ] scope 过滤：按当前 module/use 上下文筛可见路径（复刻 `build_candidate_paths`）
- [ ] item kind 映射：mod→MODULE / enum→ENUM / option→FIELD / variant→ENUM_MEMBER
- [ ] 插入策略：仅插入剩余 segment，不覆盖已输入前缀
- [ ] **交付物**：智能补全

### 5.8 测试

- [ ] 单元测试：UTF-16 转换（ASCII / 中文 / emoji / 跨行）
- [ ] 单元测试：诊断映射（severity / code / source / range / related）
- [ ] 单元测试：防抖版本排序（旧版本不覆盖新版本）
- [ ] 单元测试：alias 索引（`use ... as ...` 命中跳转）
- [ ] 集成测试：`didOpen / didChange / didSave` 诊断推送
- [ ] 集成测试：多文件 schema 联合诊断落到正确 URI
- [ ] 集成测试：Hover（option / enum / mod）内容正确
- [ ] 集成测试：GoToDefinition（option / variant / use alias）跳转正确
- [ ] 集成测试：Completion 三场景可触发且候选符合 scope
- [ ] 集成测试：DocumentSymbol 返回嵌套层级正确
- [ ] 回归：`cargo check --workspace --all-targets`
- [ ] 回归：`cargo test --workspace && cargo test -p rcfg-lsp`

---

## v0.6 — Stream B：VSCode 插件 + Stream D：格式化

> 目标：基于 LSP 的 VSCode 集成与调试体验闭环。
> Stream B（B.1–B.3）**零依赖**，可与阶段 0 / Stream A 并行启动。

### 6.1 TextMate Grammar（Stream B.1，零依赖）

- [ ] `.rcfg` schema 语法高亮规则（`rcfg-schema.tmLanguage.json`）
- [ ] `.rcfgv` values 语法高亮规则（`rcfg-values.tmLanguage.json`）
- [ ] 覆盖：关键字、类型、字面量、注释、路径、属性

### 6.2 Extension 脚手架（Stream B.2，零依赖）

- [ ] `package.json`：语言注册（`.rcfg` / `.rcfgv` 文件关联）
- [ ] 扩展激活入口（`extension.ts`）
- [ ] 图标、README、marketplace 元数据

### 6.3 Snippets（Stream B.3，零依赖）

- [ ] `option` / `mod` / `enum` / `when` / `match` / `patch` 代码片段模板

### 6.4 LSP Client 集成（Stream B.4，依赖 v0.5 Step 1）

- [ ] 配置 `LanguageClient` 自动启动 `rcfg-lsp` binary
- [ ] 内联诊断（红色波浪线 + hover 显示错误详情）

### 6.5 `rcfg fmt` 格式化（Stream D，独立）

- [ ] AST pretty-printer 实现
- [ ] schema 文件格式化（缩进、对齐、空行规范化）
- [ ] values 文件格式化
- [ ] CLI `rcfg fmt` 命令
- [ ] LSP `textDocument/formatting` 支持（依赖 v0.5）

### 6.6 Zed 扩展（可选）

- [ ] Zed extension 基础框架
- [ ] LSP 集成

### 6.7 测试

- [ ] TextMate grammar 高亮覆盖测试
- [ ] 格式化 round-trip 测试

---

## v0.7 — Stream C：TUI（menuconfig）

> 目标：交互式配置浏览/编辑与约束即时反馈。
> **不依赖 LSP**，仅依赖 `rcfg-lang` 公共 API。阶段 0 完成后可与 Stream A 并行。
> 采用三层架构：rcfg → rcfg-app（共享加载/分析）→ rcfg-tui（纯 TUI 引擎）。
> 详细设计见 [tui-plan.md](tui-plan.md)。

### Phase 0 — rcfg-app 架构重排（前置）

- [ ] 新建 `crates/rcfg-app`，加入 workspace members
- [ ] 定义 `AppLoadOptions` / `AppSession` 公开类型
- [ ] 实现 `load_session()`：schema/manifest/context/i18n 加载 + parse + analyze 全链路
- [ ] 实现 `AppSession::analyze_values_from_path()`（从文件路径分析 values）
- [ ] 实现 `AppSession::analyze_values()`（从内存 ValuesFile 分析，TUI 用）
- [ ] 实现 `AppSession::resolve()`（求解 ResolvedConfig）
- [ ] 迁移 `I18nCatalog` / `ManifestModel` / `ManifestGraph` 类型到 rcfg-app
- [ ] 迁移 `load_schema_with_dependencies` / `namespace_schema_items_for_package` 到 rcfg-app
- [ ] 迁移 `localize_diagnostic_message` 到 rcfg-app（CLI/TUI 共用）
- [ ] `src/cli/mod.rs` 改为调用 rcfg-app API，删除原 loaders 实现
- [ ] 门禁：`cargo test --workspace` 全部通过，check/export/dump/i18n 行为不变

### 7.1 Phase 1 — TUI 框架（C1）

- [ ] 新建 `crates/rcfg-tui`，加入 workspace members
- [ ] 定义 `TuiConfig` / `TuiError` / `pub fn run()` 入口
- [ ] CLI 接线：`src/cli/args.rs` 新增 menuconfig 子命令（继承全局参数 + --values + --script）
- [ ] CLI 接线：`src/cli/commands/menuconfig.rs` 调用 rcfg-app load_session 后传入 rcfg-tui
- [ ] `model.rs`：以 AST Item 层级构建 ConfigTree/ConfigNode（保留声明顺序）
- [ ] `model.rs`：从 SymbolTable 注入类型/文档/约束到节点
- [ ] `render.rs`：左树右详情 + 底栏状态/诊断 双栏布局
- [ ] `event.rs`：键盘事件分发 + Event::Resize 处理
- [ ] `state.rs`：选中项、展开/折叠状态、导航（Up/Down/Enter/Esc）
- [ ] `state.rs`：失活节点灰显、禁止编辑、可导航可查看
- [ ] `runtime.rs`：event loop 主循环（crossterm raw mode + alternate screen）

### 7.2 Phase 1 — 值编辑（C2）

- [ ] bool：Space 切换
- [ ] int/string：编辑态输入 + Enter 提交 + Esc 取消 + 范围提示
- [ ] enum：选择态列表 + Enter 提交
- [ ] `state.rs`：维护 user_values: BTreeMap<String, ResolvedValue>
- [ ] overrides 桥接：合成 ValuesFile（AssignStmt + 全路径 + 排序稳定 + Span::default()）
- [ ] d 键清除当前项用户覆盖（回到默认链路）

### 7.3 Phase 1 — 实时校验（C3）

- [ ] 每次提交编辑后调用 session.analyze_values() + session.resolve()
- [ ] 更新 active/inactive 状态、底栏诊断摘要、错误路径高亮
- [ ] 失活项状态同步：编辑其他项导致当前项失活时正确刷新

### 7.4 Phase 1 — 最简保存

- [ ] Ctrl+S 触发保存，直接写入 --values 路径（无弹窗）
- [ ] 若未指定 --values，保存到 ./.config.rcfgv
- [ ] 保存前校验：有 error 则阻止并在底栏提示
- [ ] 输出格式：扁平全路径赋值、按 path 排序、文件末尾换行
- [ ] 最小 diff：仅输出 source == User 且 value != baseline 的赋值
- [ ] 脏状态退出：q 首次提示，再按 q 确认退出

### 7.5 Phase 2 — 帮助与文档（C4）

- [ ] F1 打开帮助面板，展示 summary/help（doc 注释分段）
- [ ] 文案优先 i18n key，缺失回退 doc fallback（通过 session.localize_diagnostic_message）

### 7.6 Phase 2 — 完整保存（C5，替换最简保存）

- [ ] 路径弹窗输入，默认值优先级：--out > --values > ./.config.rcfgv
- [ ] 保存前强校验：存在 error 则阻止保存并展示诊断
- [ ] baseline 计算：以空 ValuesFile + context 求解（默认/patch/context 基线）

### 7.7 --script 测试支持

- [ ] 解析逐行脚本命令（up/down/enter/esc/space/f1/save/quit/reset/chars <text>）
- [ ] 脚本执行完毕后 JSON 输出到 stdout（无混杂日志，仅最终 JSON）
- [ ] JSON 字段：selected_path / user_values / active_paths / diagnostics

### 7.8 测试

- [ ] rcfg-app 单测：load_session 正确加载、analyze/resolve 结果一致
- [ ] rcfg-tui 状态机单测：导航/编辑/失活不可编辑/脏状态退出确认
- [ ] 脚本驱动集成测试：编辑后重算、诊断高亮、JSON 断言
- [ ] 保存测试：最小 diff、字符串转义、有/无 --values 两种路径行为
- [ ] 一致性测试：求解结果与渲染状态一致（active/source/value 同步）
- [ ] 回归：`cargo check --workspace --all-targets && cargo test --workspace`

---

## 未排期（v0.2+ 按需插入）

- [x] `#[cfg(...)]` 条件属性（解除 reserved grammar 限制）
- [x] `export` 语法块（schema 内声明导出规则）
- [x] `@root/...` include 路径（相对工程根）
- [x] `--export-context` 导出 ctx 项
- [x] 显式 i18n key 覆盖（`#[label_key]` / `#[help_key]`）
- [x] fix-it 自动修复建议

---

## 架构决策记录

| 决策 | 结论 | 理由 |
|------|------|------|
| Parser 实现方式 | 手写递归下降 | 错误恢复好、诊断可控、语法简单无需 LR、LSP 友好 |
| 路径分隔符 | `::` | 已在 parser 中固定 |
| 整数类型 | `ValueType::Int(IntType)` | 已拆分，支持 U8/U16/U32/I32 宽度语义 |
| 内部容器 | `BTreeMap` / `BTreeSet` | 确定性遍历顺序，导出/诊断输出稳定 |
| 路径 newtype | `SymbolPath` / `OptionPath` / `VariantPath` / `EnumPath` | 类型安全，防止路径混淆 |
| regex 依赖 | 接受 `regex` crate | `matches()` 内建函数需要正则语义 |
| 导出接口 | `ConfigExporter` trait | 统一输入输出契约，新语言只需实现 trait |
| 诊断去重 | 结构化 dedup key | 避免 `format!` 字符串拼接的脆弱性 |
| schema_check 拆分 | collector.rs + type_checker.rs | 原 monolith 超 1000 行，职责分离 |
| 包依赖加载 | 深度优先 + 已访问集合 | 简单可靠，天然支持循环检测 |
