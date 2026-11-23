# 变更日志 (Changelog)

本项目所有重要变更均记录于此。格式参考 [Keep a Changelog](https://keepachangelog.com/zh-CN/1.1.0/)，版本号遵循语义化版本。

## [0.1.0] - 2026-11-23

首个正式发布版本：包含规则驱动的翻译核心、统一构建系统、RV32 ELF 工具链与寄存器分配器。

### 新增 (Added)

- 核心翻译引擎：trie 规则索引、翻译工具函数集、翻译插件、源处理插件与多阶段翻译系统（`src/trie.rkt`、`src/translate-*.rkt`、`src/source-processors.rkt`）
- 插件系统：允许用 Racket 编写自定义语义扩展
- 统一命令行入口 `type.rkt`：`--version/-v` 输出 `TYPE v0.1.0`、`--help/-h` 显示帮助、无参数启动 REPL（#1）
- 版本号统一管理：`info.rkt` 作为唯一来源，翻译核心 `*type-version*` 从同一处读取
- 构建系统：`type` 作为统一入口，读取声明式「规则 + 工序」清单（`-b/--build`、`--verbose`）
- 内置工序：translate / link / encode / wrap-elf，统一 `(env options) -> env` 契约
- ELF32/ELF64 包装器：支持可执行文件与可重定位目标文件，link 工序产出符号表与重定位项（#8 / #9）
- 寄存器分配器 `src/register-allocator.rkt`：活跃区间 + 线性扫描 + 调用感知，跨调用活跃值使用被调用者保存寄存器；模块为纯机制，寄存器池、符号前缀、调用/控制/返回标签等约定全部由规则文件显式传入（#7）
- 示例：简单翻译示例 `example/simple`；RV32 计算 1 至 100 之和的汇编与 ELF 示例（现统一为 `example/rv32_elf`）
- 文档：README、CONTRIBUTING、ROADMAP

### 变更 (Changed)

- 重构翻译系统：拆分前处理、后处理与 apply 逻辑，便于单元测试（#3 / #2）
- 全面适配 Racket 7.5
- `example/rv32_elf` 规则集中 `reg-bytes` 修正为 4，与 RV32 字长一致

### 修复 (Fixed)

- 寄存器冲突：移除 `example/rv32_elf/rules-0.scm` 中针对变量 `end` 的 `a2` 硬编码，改由分配器统一处理跨调用活跃值（#7）
- sum100 示例：修正末尾参数打包行为与参数匹配逻辑（#11）

[0.1.0]: https://github.com/fanguangping/type/releases/tag/v0.1.0
