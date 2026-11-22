# TYPE - 计算机程序翻译系统

**TYPE is translate your programs easily**

[![Racket](https://img.shields.io/badge/Racket-7.5-blue.svg)](https://racket-lang.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)

TYPE 是一个基于规则的、语义驱动的计算机程序翻译系统。它将结构化的计算机程序（以 S 表达式形式表示）通过可扩展的规则集，对等或不对等地翻译为另一个结构化的程序。系统采用 Racket 语言实现，核心翻译引擎稳定可靠，规则可灵活扩展，支持多阶段流水线翻译，并可最终生成二进制可执行文件。

---

## ✨ 核心特性

- **语义驱动翻译**：基于九种核心翻译语义（顺序、字典、求值、指代、转换、升级、消除、位置、索引），实现灵活强大的程序转换
- **工序清单构建**：`type.rkt` 作为统一入口，把源码与「规则 + 工序」清单交给构建器，串联翻译、链接、编码与包装
- **多阶段流水线**：支持通过规则文件串联多个翻译阶段，每个阶段独立配置
- **可逆/不可逆翻译**：支持对等（可逆）与不对等（不可逆）的翻译策略
- **S 表达式原生支持**：翻译源与目标均为 S 表达式，完美适配 Lisp 系语言生态
- **ELF 文件包装**：依据 `ruleset/elf/elf-file.scm` 布局规则，生成 ELF32/ELF64 可执行文件（ET_EXEC）与可重定位目标文件（ET_REL），支持符号解析与 `.rela.text` 重定位项，入口点默认为 `_start`
- **高度可扩展**：翻译系统核心稳定，翻译规则完全可定制

---

## 🚀 快速开始

### 安装

1. **克隆仓库**
   ```bash
   git clone https://github.com/fanguangping/type.git
   cd type
   ```

2. **安装 Racket**
    - 访问 [Racket 官网](https://racket-lang.org/) 下载并安装（版本 7.5 或更高）
    - 或使用包管理器：
      ```bash
      # macOS (Homebrew)
      brew install racket
      
      # Ubuntu/Debian
      sudo apt-get install racket
      ```

3. **安装依赖**
   ```bash
   raco pkg install --auto
   ```

### 基本使用

1. **准备翻译源文件** (`example.scm`)
   ```scheme
   ;; 一个简单的 S 表达式程序
	(:program
	 (*** 简单的求和程序 ***)
	 (:code
	  (let x 3)
	  (let y 5)
	  (print (add x y))))
   ```

2. **编写规则文件** (例如 `rules-0.scm`)
   ```scheme
	(rules
	 (processor
	  (*** 后处理器 ***)
	  (post
	   (upgrade (lambda (x) x) :<=)))
	 (*** 字典映射 ***)
	 (mapping
	  (add '+)
	  (print 'displayln))
	 (*** 宏转换 ***)
	 (macro
	  (let (x num)
		(define x num))))
   ```

3. **执行翻译**
   ```scheme
   ;; 在 Racket REPL 中
   (require "type.rkt")
   
   ;; 直接翻译一个 S 表达式程序
   (translate source '() rules)

   ;; 或按工序清单构建最终成品
   (build-file "example/rv32_elf/build.scm")
   ```

4. **翻译结果**
   ```scheme
   '(:program
	 (:code
	  (define x 3)
	  (define y 5)
	  ('displayln ('+ x y))))
   ```

### 版本与帮助

TYPE 提供统一的命令行入口 `type.rkt`，可查看版本、帮助，或进入带欢迎信息的 REPL：

```bash
# 查看版本号,输出形如 TYPE v0.1.0
racket type.rkt --version
racket type.rkt -v

# 查看帮助
racket type.rkt --help
racket type.rkt -h

# 进入 REPL(启动时显示项目名称、版本号和简要使用提示)
racket type.rkt

# 根据工序清单构建最终成品
racket type.rkt --build example/rv32_elf/build.scm
racket type.rkt -b example/rv32_elf/build.scm --verbose
```

版本号统一存放在仓库根目录的 `info.rkt` 中；`raco pkg` 元数据直接读取它，翻译核心中的 `*type-version*` 通过基础 `read` 解析同一处定义，发布时只需修改一处。

---

## 🧠 工作原理

### 核心思想

TYPE 将**所有结构化程序抽象为 S 表达式**，视为“单词（原子）”与“句子（列表）”的组合。句子可以嵌套或排列，翻译过程由**九种语义**驱动：

| 语义 | 描述 | 示例                                                |
|------|------|---------------------------------------------------|
| **顺序语义** | 默认自上而下、由内向外翻译，可通过算符调整顺序 | -                                                 |
| **字典语义** | 将单词翻译为另一个单词或句子 | `(mapping + add)`                                 |
| **求值语义** | 将句子翻译为其实际运算值 | `(:= (* 3 4))` → `12`                             |
| **指代语义** | 使用新符号表示已有表达式 | `(: x (* 3 4))`                                   |
| **转换语义** | 将句子转换为另一个句子（宏） | `(macro (when cond body) (if cond (begin body)))` |
| **升级语义** | 提升句子的嵌套层级到父级 | `(:<= ((x 1) (y 2)))` → `(x 1) (y 2)`             |
| **消除语义** | 去掉无意义的单词或句子 | -                                                 |
| **位置语义** | 调整句子或单词的顺序 | -                                                 |
| **索引语义** | 通过绝对/相对数值找到符号要链接的语句地址 | -                                                 |

### 构建流水线

TYPE 把「从源码到成品」看作一条构建链：

- **翻译器**是一种工序：把 S 表达式按规则变换为另一种 S 表达式；
- **链接器**是一种工序：解析符号与重定位，连接多个翻译成果；
- **包装器**是一种工序：把代码、符号和重定位信息序列化为最终文件格式；
- **构建器**不是工序，而是工序的编排者：读取工序清单、传递中间产物、输出成品。

```
example.scm + build.scm
        │
        ├─ translate(rules-0.scm) ──► translate(rules-1.scm) ──► ...
        ├─ link(rules-link.scm)      ──► symbols / relocs
        ├─ encode                    ──► bytes
        └─ wrap-elf                  ──► sum100.o / sum100.elf
```

每个阶段独立配置规则与选项，`build.scm` 中的相对路径以清单文件所在目录为基准。

### 工序清单示例

`example/rv32_elf/build.scm` 是一份纯数据的工序清单：

```scheme
(build
 (source "example.scm")
 (dump-dir "build")
 (stages
  (translate (rules "rules-0.scm"))
  (translate (rules "rules-1.scm"))
  (translate (rules "rules-2.scm"))
  (link      (rules "rules-link.scm") (emit symbols relocs))
  (translate (rules "rules-bin.scm"))
  (encode    (from source) (to code))
  (wrap-elf  (kind object)     (output "sum100.o")   (class 32))
  (wrap-elf  (kind executable) (output "sum100.elf") (class 32)
             (entry _start) (load-addr #x80000000))))
```

`link` 工序把符号表与重定位项写入构建环境，后续 `wrap-elf` 只负责序列化，包装器不需要反向解析源码。

### 翻译引擎架构

```
┌─────────────────────────────────────────────────────┐
│                   输入 S 表达式                     │
└─────────────────────┬───────────────────────────────┘
                      ▼
┌─────────────────────────────────────────────────────┐
│              语义解析插件 (Semantic Parser)         │
│  识别并应用九种翻译语义的规则                       │
└─────────────────────┬───────────────────────────────┘
                      ▼
┌─────────────────────────────────────────────────────┐
│              规则引擎 (Rule Engine)                 │
│  加载规则 → 匹配模式 → 应用转换                     │
└─────────────────────┬───────────────────────────────┘
                      ▼
┌─────────────────────────────────────────────────────┐
│            翻译执行器 (Translator)                  │
│  按顺序语义递归遍历并转换 S 表达式                  │
└─────────────────────┬───────────────────────────────┘
                      ▼
┌─────────────────────────────────────────────────────┐
│              输出 S 表达式                          │
└─────────────────────────────────────────────────────┘
```

### 寄存器分配

寄存器是有限资源，函数调用又会改写一部分寄存器。若把跨调用仍然有用的值留在易失寄存器里，
调用返回后这个值就被覆盖了。`example/rv32_elf` 早期用「把 `end` 硬搬到 `a2`」绕过该问题，
这是不可扩展的补丁；现在由 `src/register-allocator.rkt` 统一解决。

**为什么要入栈/出栈**

- 调用指令会改写返回地址寄存器 `ra`，函数若不保存它就无法返回；
- 被调用者保存寄存器 `s*` 的约定是「被调用者负责恢复」，因此函数使用了哪些 `s*`，
  就必须在序言 `push-stack` 保存、在尾声 `pop-stack` 恢复；
- 序言还负责把入口实参寄存器 `a*` 中的跨调用活跃值搬到分配到的 `s*`；
- 调用方则把实参搬到 `a*`、把调用后仍需要的 `t*` 暂存到栈上，再执行 `call`。
  这就是「为何出入栈」：保存调用链上会被改写、但后续仍要用的值。

**算法是纯机制，约定来自规则文件**

`src/register-allocator.rkt` 不预设任何目标平台或规则文件的约定。以下内容全部由规则文件的
`mapping` 定义，并通过参数显式传给分配器（以 `example/rv32_elf/rules-0.scm` 为例）：

- `reg-pools`：各角色寄存器表及分配优先级（`arg` / `temp` / `saved`）；
- `reg-prefixes`：物理寄存器符号的首字母（`"sfta"`）；
- `call-tags`：哪些标签代表调用（`:f` / `:s` / `call`）；
- `control-tags`：哪些标签是控制结构（`!for` / `!while` / `!do-while` / `!if` / `!if-else`）；
- `return-tag`：`return` 语句的标签。

因此换一套 ABI 或标签约定只需改规则文件，分配器源码无需改动。

**分配算法（调用感知的线性扫描）**

1. `live-intervals` 为每个形参计算活跃区间 `[入口, 最后使用]`，并标记是否跨过调用。
   控制结构头部的变量会在节点末尾补记一次使用，避免漏掉「循环每轮都要重新判断」的循环变量。
2. `allocate-registers` 按区间起点扫描：
   - 不跨调用的变量保留入口实参寄存器（易失，但调用前已用完）；
   - 跨调用的变量只能分配到规则文件提供的被调用者保存寄存器池；
   - 保存寄存器不足时返回溢出槽位（spill），当前规则层直接报错，避免生成错误代码。
3. `rules-0.scm` 用分配结果替换函数体中的形参，插入入口搬移，并把实际使用的保存寄存器加入序言保存列表。
   返回值寄存器不加入序言保存列表，以免把刚写好的返回值覆盖掉。
   `signature` 从替换后的函数体提取，记录真实的 ABI 实参/返回寄存器。

分配器辅助过程通过 `src/translate-utils.rkt` 重新导出，规则文件可直接调用
`register-pool` / `live-intervals` / `allocate-registers` 等过程。
相关测试见 `test/register-allocator-test.rkt`，其中包含「不再出现 `end -> a2` 硬编码」的回归验证，
以及使用自定义池/前缀/标签（非 RV32）验证分配器无硬编码的用例。

---

## 📚 项目结构

```
type/
├── README.md                 # 项目说明文档
├── LICENSE                   # 开源协议
├── CONTRIBUTING.md           # 贡献指南
├── ROADMAP.md                # 路线图
├── info.rkt                  # 版本号唯一来源 + raco pkg 元数据
├── type.rkt                  # 统一命令行 / REPL / 构建入口
├── example/                  # 示例程序
│   ├── simple/
│   ├── sum100_rv32/
│   └── rv32_elf/             # 声明式工序清单 + 生成的 .elf/.o
│       ├── build.scm         # 「规则 + 工序」清单
│       └── rv32-elf-builder.rkt
├── ruleset/                  # 规则库
│   └── elf/
│       └── elf-file.scm      # ELF32/64 文件布局规则
├── src/                      # 翻译系统与构建工序
│   ├── build-system.rkt      # 构建器:读取工序清单并依次执行
│   ├── rule-loader.rkt       # 规则/源码 S 表达式加载
│   ├── trie.rkt
│   ├── register-allocator.rkt # 调用感知的线性扫描寄存器分配器
│   ├── translate-utils.rkt    # 同时重新导出分配器辅助过程
│   ├── translate-plugins.rkt
│   ├── source-processors.rkt
│   ├── translate-system.rkt  # 默认翻译工序的核心实现
│   ├── process/              # 各阶段工序适配器
│   │   ├── process-utils.rkt
│   │   ├── symbol-tools.rkt
│   │   ├── translate-process.rkt
│   │   ├── link-process.rkt
│   │   ├── encode-process.rkt
│   │   └── elf-process.rkt
│   └── wrapper/              # 文件包装器
│       └── elf-file-wrapper.rkt
├── test/                     # 测试套件
│   ├── trie-test.rkt
│   ├── translate-utils-test.rkt
│   ├── translate-plugins-test.rkt
│   ├── source-processors-test.rkt
│   ├── translate-system-test.rkt
│   ├── version-test.rkt
│   ├── elf-file-wrapper-test.rkt
│   ├── build-system-test.rkt
│   ├── process-test.rkt
│   └── register-allocator-test.rkt # 分配器单测 + end -> a2 硬编码回归
└── doc/                      # 详细文档
    └── reference/api.md      # 算符/关键字参考
```

---

## 🤝 贡献指南

我们热烈欢迎所有形式的贡献！无论是报告 Bug、提出新特性，还是提交代码，都请参考以下流程：

### 报告问题

请在 [GitHub Issues](https://github.com/fanguangping/type/issues) 中提交问题，并遵循模板：

```markdown
**描述问题**：清晰简洁地描述问题
**复现步骤**：
1. 使用规则文件 '...'
2. 运行命令 '...'
3. 看到错误 '...'
**预期行为**：应该发生什么？
**环境**：Racket 版本、操作系统等
```

### 提交代码

1. **Fork 仓库** 并克隆到本地
2. **创建特性分支**
   ```bash
   git checkout -b feature/amazing-feature
   ```
3. **编写代码并测试**
   ```bash
   raco test test/
   ```
4. **提交更改**（遵循[约定式提交](https://www.conventionalcommits.org/zh-hans/v1.0.0/)规范）
   ```bash
   git commit -m "feat: 添加新的翻译语义插件"
   ```
5. **推送并创建 Pull Request**

### 代码规范

- 遵循 [Racket Style Guide](https://docs.racket-lang.org/style/)
- 为新功能添加测试用例
- 更新相关文档
- 确保 CI 通过

### 开发路线图

查看 [ROADMAP.md](ROADMAP.md) 了解项目未来规划。

---

## 📄 开源协议

本项目采用 **MIT 许可证**。您可以自由使用、修改、分发本软件，但需保留版权声明和许可声明。详见 [LICENSE](LICENSE) 文件。

---

## 🌟 为什么选择 TYPE？

- **灵活性**：通过组合九种语义，可以表达从简单词法替换到复杂程序变换的任何翻译
- **可逆性**：支持对等翻译，便于实现程序反编译或双向转换
- **教育价值**：清晰的语义划分使 TYPE 成为学习程序语言理论和编译原理的优秀教学工具
- **工业潜力**：可用于代码迁移、领域特定语言（DSL）转换、遗留系统现代化等场景

---

## 📖 文档与资源

- [完整 API 文档](doc/api-reference.md)
- [九种语义详解](doc/semantics.md)
- [规则编写指南](doc/rules-guide.md)
- [示例程序集](example/)
- [常见问题解答](doc/faq.md)

---

## 🗣️ 社区与支持

- **GitHub Issue**：[提问与讨论](https://github.com/fanguangping/type/issues)
- **GitHub Wiki**：[文档](https://github.com/fanguangping/type/wiki)
- **QQ群**：[111086164](111086164)

---

## 🙏 致谢

感谢所有贡献者的付出！特别感谢以下人员（按字母顺序）：

- [贡献者列表]

---

## 📊 项目状态

![GitHub last commit](https://img.shields.io/github/last-commit/fanguangping/type)
![GitHub issues](https://img.shields.io/github/issues/fanguangping/type)
![GitHub stars](https://img.shields.io/github/stars/fanguangping/type)

---

**⭐ 如果这个项目对您有帮助，请给我们一个 Star！**

