# TYPE - 计算机程序翻译系统

**TYPE is translate your programs easily**

[![Racket](https://img.shields.io/badge/Racket-7.5-blue.svg)](https://racket-lang.org/)
[![License](https://img.shields.io/badge/License-MIT-green.svg)](LICENSE)
[![PRs Welcome](https://img.shields.io/badge/PRs-welcome-brightgreen.svg)](CONTRIBUTING.md)

TYPE 是一个基于规则的、语义驱动的计算机程序翻译系统。它将结构化的计算机程序（以 S 表达式形式表示）通过可扩展的规则集，对等或不对等地翻译为另一个结构化的程序。系统采用 Racket 语言实现，核心翻译引擎稳定可靠，规则可灵活扩展，支持多阶段流水线翻译，并可最终生成二进制可执行文件。

---

## ✨ 核心特性

- **语义驱动翻译**：基于九种核心翻译语义（顺序、字典、求值、指代、转换、升级、消除、位置、索引），实现灵活强大的程序转换
- **多阶段流水线**：支持通过规则文件串联多个翻译阶段，每个阶段独立配置
- **可逆/不可逆翻译**：支持对等（可逆）与不对等（不可逆）的翻译策略
- **S 表达式原生支持**：翻译源与目标均为 S 表达式，完美适配 Lisp 系语言生态
- **可执行文件生成**：提供包装器、链接器、加载器，可将规整的四元组列表等目标程序编译为二进制文件
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
   (require "translate-system.rkt")
   
   ;; 多阶段翻译
   (translate-program "./"
					  "./build/"
					  "simple-program.scm"
					  '("rules-0.scm"))
   ```

4. **翻译结果**
   ```scheme
   '(:program
	 (:code
	  (define x 3)
	  (define y 5)
	  ('displayln ('+ x y))))
   ```

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

### 翻译流水线

```
example.scm + rules-0.scm --[stage1]--> tmpSource1.scm
tmpSource1.scm + rules-1.scm --[stage2]--> tmpSource2.scm
tmpSource2.scm + rules-2.scm --[stage3]--> dest.scm
```

每个阶段独立配置规则文件，实现关注点分离和规则复用。

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

---

## 📚 项目结构

```
type/
├── README.md                 # 项目说明文档
├── LICENSE                   # 开源协议
├── CONTRIBUTING.md           # 贡献指南
├── example/                  # 示例程序
│   ├── example-0.scm
│   ├── example-1.scm
│   └── example-2.scm
├── src/                      # 翻译系统
│   ├── trie.rkt
│   ├── translate-utils.rkt
│   └── ...
├── test/                     # 测试套件
│   ├── trie-test.rkt
│   ├── translate-utils-test.rkt
│   └── ...
└── doc/                      # 详细文档
    ├── semantics.md          # 语义详解
    └── api-reference.md      # API 参考
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

