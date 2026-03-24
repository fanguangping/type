# 贡献指南

感谢你考虑为 TYPE 项目做出贡献！我们欢迎任何形式的贡献，包括报告问题、改进文档、提交代码或分享使用经验。

请花几分钟阅读以下指南，确保贡献流程顺畅高效。

---

## 📋 目录

- [行为准则](#行为准则)
- [如何报告问题](#如何报告问题)
- [如何提交代码](#如何提交代码)
  - [准备工作](#准备工作)
  - [开发流程](#开发流程)
  - [代码风格](#代码风格)
  - [测试](#测试)
  - [提交信息规范](#提交信息规范)
  - [建议](#建议)
- [开发环境设置](#开发环境设置)
- [获得帮助](#获得帮助)

---

## 行为准则

本项目遵循 [贡献者公约](https://www.contributor-covenant.org/zh-cn/version/2/0/code_of_conduct/) 行为准则。我们期望所有参与者都能尊重彼此，保持友善和包容的氛围。任何不可接受的行为请向项目维护者举报。

---

## 如何报告问题

如果你遇到 Bug、有功能建议，或对文档有疑问，请在 [GitHub Issues](https://github.com/fanguangping/type/issues) 中提交。

### 提交 Issue 时请包含以下信息

- **标题**：简洁描述问题（如：“翻译规则中的字典语义无法处理嵌套列表”）
- **描述**：详细说明问题，包括：
  - 预期行为与实际行为的差异
  - 复现步骤（提供最小化的示例代码和规则文件）
  - 使用的 Racket 版本（运行 `racket -v`）
  - 操作系统信息
- **相关文件**：如有，请附上 `example.scm`、`rules-*.scm` 等
- **错误日志**：粘贴完整的错误输出（使用代码块）

### 标签说明

维护者会根据 Issue 类型添加标签（如 `bug`、`enhancement`、`documentation`），方便分类处理。

---

## 如何提交代码

我们欢迎各种形式的代码贡献，包括新功能、Bug 修复、性能优化和测试补充。

### 准备工作

1. **Fork 仓库**：点击 GitHub 页面右上角的 “Fork” 按钮。
2. **克隆到本地**：
   ```bash
   git clone https://github.com/fanguangping/type.git
   cd type
   ```
3. **添加上游仓库**（可选，用于同步）：
   ```bash
   git remote add upstream https://github.com/原仓库/type.git
   ```

### 开发流程

1. **创建分支**（从 `main` 分支切出）：
   ```bash
   git checkout -b feature/your-feature-name
   # 或 bugfix/issue-number
   ```
   分支命名建议：`feature/`、`bugfix/`、`doc/`、`test/` 等。

2. **编写代码**：
   - 遵循代码风格（见下文）。
   - 为新功能添加测试用例（放在 `test/` 目录）。
   - 更新相关文档（如 `README.md`、`doc/` 下的文件）。

3. **本地测试**：
   ```bash
   raco test test/
   ```
   确保所有测试通过。

4. **提交更改**：
   ```bash
   git add .
   git commit -m "类型: 简洁描述"
   ```
   提交信息遵循 [规范](#提交信息规范)。

5. **同步上游代码**（避免冲突）：
   ```bash
   git fetch upstream
   git merge upstream/main
   ```

6. **推送到你的远程仓库**：
   ```bash
   git push origin feature/your-feature-name
   ```

7. **创建 Pull Request (PR)**：
   - 前往你 Fork 的仓库，点击 “Compare & pull request”。
   - 填写 PR 标题和描述，说明改动内容、动机和相关 Issue（如有）。
   - 等待 CI 检查通过，并邀请项目维护者审阅。

### 代码风格

本项目使用 Racket 语言开发，请遵循 [Racket Style Guide](https://docs.racket-lang.org/style/) 的规范。主要要求：

- 使用 **2 个空格** 缩进（不使用 Tab）。
- 函数名使用 **kebab-case**（如 `translate-program`）。
- 为公开函数添加文档字符串（`;;` 注释），说明参数、返回值和副作用。
- 保持代码简洁，避免过长的行（建议 ≤ 80 字符）。
- 使用 `define` 定义变量和函数，避免 `set!` 除非必要。
- 为复杂逻辑添加注释。

**示例**：
```racket
;; 对输入 S 表达式应用字典语义
;; input: 待翻译的 S 表达式
;; dict:  单词到单词/句子的映射表
;; 返回转换后的 S 表达式
(define (apply-dict-semantics input dict)
  (if (pair? input)
      (cons (apply-dict-semantics (car input) dict)
            (apply-dict-semantics (cdr input) dict))
      (or (hash-ref dict input #f) input)))
```

### 测试

- 所有新增功能或 Bug 修复必须包含测试用例。
- 测试文件放在 `test/` 目录下，命名格式为 `*-test.rkt`。
- 使用 `raco test` 运行测试套件。
- 测试应覆盖正常情况、边界情况和错误情况。

### 提交信息规范

我们鼓励使用 [约定式提交](https://www.conventionalcommits.org/zh-hans/v1.0.0/) 规范，使提交历史清晰易读。

格式：
```
<类型>(<范围>): <简短描述>

<详细描述>

<脚注>
```

**类型**：
- `feat`：新功能
- `fix`：Bug 修复
- `docs`：文档更新
- `style`：代码格式（不影响逻辑）
- `refactor`：重构（不新增功能，不修复 Bug）
- `test`：添加或修改测试
- `chore`：构建过程、工具链等杂项

**示例**：
```
feat(translator): 添加指代语义支持

- 新增 `referential` 规则类型，支持使用新符号表示表达式
- 更新规则解析器以识别指代语义
- 添加单元测试 tests/test-referential.rkt

Closes #42
```

### 建议

为了保证代码审查的效率、降低引入缺陷的风险，我们强烈建议：

1. **控制 PR 规模**
    - 每个 PR 应聚焦于**单一功能点或 Bug 修复**，避免在一个 PR 中混杂多个不相关的改动。
    - 如果改动涉及多个独立部分，请拆分为多个 PR。
    - 推荐 PR 的代码变更行数在 **200 行以内**（文档、测试文件除外），若超出此范围，请在 PR 描述中说明拆分理由。

2. **合并前测试**
    - **自动化测试**：确保所有 CI 检查（单元测试、语法检查、测试覆盖率）均通过。
    - **手动验证**：如果改动涉及核心翻译逻辑或规则解析，请在本地运行相关示例程序，验证输出符合预期。
    - **向后兼容性**：如果修改了公开 API 或规则格式，请确保旧版规则文件仍能正常工作（或提供迁移指南）。

3. **合并后观察**
    - PR 合并后，请关注 CI 流水线是否在 `main` 分支上顺利执行，避免因合并冲突导致意外失败。
    - 若合并后发现异常，请立即提交修复 PR 或通知维护者回滚。

4. **代码审查配合**
    - 积极响应审查意见，及时修正问题。
    - 如果审查过程中需要大规模重构，建议关闭原 PR，重新提交更清晰的代码变更。

遵循这些建议，可以帮助我们保持代码库的健康，并让每次贡献都更顺畅地被接受。感谢您的理解与配合！

---

## 开发环境设置

1. **安装 Racket**（版本 9.14 或更高）：
   - 官网下载：https://racket-lang.org/download/
   - 或使用包管理器：
     - macOS: `brew install racket`
     - Ubuntu: `sudo apt install racket`

2. **克隆项目并安装依赖**：
   ```bash
   git clone https://github.com/fanguangping/type.git
   cd type
   raco pkg install --auto
   ```

3. **运行测试**：
   ```bash
   raco test test/
   ```

4. **构建文档**（可选）：
   ```bash
   raco doc
   ```

5. **运行示例**：
   ```bash
   racket example/simple/simple-program.scm
   ```

---

## 获得帮助

如果你在贡献过程中遇到任何问题，可以通过以下方式寻求帮助：

- **GitHub Discussions**：[提问与讨论](https://github.com/fanguangping/type/discussions)
- **QQ群**：[111086164](111086164)
- **邮件**：fanguangping@163.com

我们期待你的参与！✨