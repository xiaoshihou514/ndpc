# Ndpc 项目上下文文档

## 项目概述

Ndpc 是一个用于单类谓词形式逻辑的证明器，专注于正确性、可维护性和形式化验证。其代码风格与手写证明相仿，支持以下功能：

- **证明检查**：验证逻辑证明的正确性
- **Lean4 证明生成**：生成对应的 Lean4 证明
- **多格式导出**：将证明导出为 HTML、LaTeX 和 Typst

**主要技术栈**：
- **编程语言**：Scala 3.8.2
- **构建工具**：sbt（Scala Build Tool）
- **前端框架**：Laminar（Scala.js）
- **构建系统**：Vite（Web 前端）
- **解析器**：Parsley 5.0.0-M16
- **函数式编程**：Cats Effect 3.7.0

## 架构

项目采用多模块架构，支持多个目标平台：

### 模块结构

1. **core**（核心模块）
   - 包含共享的业务逻辑和解析器
   - 位于 `core/shared/src/main/scala`
   - 依赖：Parsley（解析器库）

2. **cli**（命令行接口）
   - 基于 JVM 的命令行工具
   - 依赖：Decline（命令行解析）、Cats Effect、os-lib
   - 可生成可执行 JAR 文件：`ndpc.jar`
   - 支持 GraalVM Native Image 编译

3. **cli-native**（原生命令行接口）
   - 使用 Scala Native 编译的原生二进制
   - 输出文件：`ndpc-native`
   - 避免 JVM TASTy 与本地构建的 Scala 补丁版本混合

4. **web**（Web 界面）
   - 基于 Scala.js 和 Laminar 的前端应用
   - 使用 Vite 进行构建和开发服务器
   - 入口文件：`index.html`、`main.js`、`style.css`

### 构建输出

- `ndpc.jar`：可执行的 JAR 文件
- `ndpc-graal`：GraalVM Native Image 二进制
- `ndpc-native`：Scala Native 二进制
- Web 静态文件：位于 `web/target` 目录

## 构建和运行

### 前提条件

- Java Development Kit（JDK）
- sbt（Scala Build Tool）
- Node.js 和 npm（用于 Web 开发）
- GraalVM Native Image（可选，用于原生编译）
- Scala Native 工具链（可选）

### 常用命令

#### 使用 Makefile（推荐）

```bash
# 代码格式化
make format

# 编译项目
make compile

# 运行测试
make test

# 运行 CLI（传递参数）
make run ARGS="your-arguments"

# 构建所有发布版本
make release

# 仅构建 JAR
make jar

# 仅构建 GraalVM 原生镜像
make graal

# 仅构建 Scala Native 二进制
make native

# 清理构建文件
make clean
```

#### 使用 sbt 直接命令

```bash
# 编译所有模块
sbt compile

# 运行测试
sbt test

# 代码格式化
sbt scalafmtAll

# 构建 CLI JAR
sbt cli/assembly

# 构建 GraalVM 原生镜像
sbt cli/graalNativeImage

# 构建 Scala Native 二进制
sbt cliNative/rootNativeLink

# 运行 Web 开发服务器
cd web && npm run dev
```

#### Web 开发命令

```bash
# 进入 web 目录
cd web

# 启动开发服务器
npm run dev

# 构建生产版本
npm run build

# 预览生产构建
npm run preview
```

## 开发约定

### 代码风格

- **缩进**：4 个空格（Scala 文件）
- **行宽**：最大 100 字符
- **格式化工具**：scalafmt（版本 3.6.1）
- **配置文件**：`.scalafmt.conf`
- **编辑器配置**：`.editorconfig`

### 项目结构约定

1. **共享代码**：位于 `core/shared/src/main/scala`
2. **平台特定代码**：分别位于各模块的 `src/main/scala`
3. **测试文件**：位于 `test` 目录，按功能分类
   - `checker/`：证明检查器测试
   - `formatter/`：格式化测试
   - `latex/`、`lean/`、`typst/`：导出格式测试
4. **构建配置**：`build.sbt` 定义所有模块和依赖

### 依赖管理

- **主要依赖**：在 `build.sbt` 中定义
- **Web 依赖**：在 `package.json` 中定义
- **解析器**：Parsley 用于语法解析
- **前端**：Laminar 用于响应式 UI
- **命令行**：Decline 用于参数解析

### 测试实践

- **测试框架**：ScalaTest
- **测试目录**：`test/` 包含多种测试类型
- **测试数据**：按格式分类（LaTeX、Lean、Typst）
- **测试执行**：`sbt test` 或 `make test`

## 项目状态

### 当前分支
- Git 分支：`web`
- 最新提交：cf59e1a "merge: sbt-ai-migration"

### 构建状态
- Scala 版本：3.8.2
- sbt 版本：通过 `build.properties` 定义
- 项目版本：0.1.0-SNAPSHOT

## 相关项目

- [ndp.vim](https://github.com/xiaoshihou514/ndp.vim)：NeoVim/Vim 对 ndp 文件的支持
- [aristotle](https://github.com/xiaoshihou514/aristotle)：Ndpc 的图形界面前端
- [boxproof](https://github.com/YunkaiZhang233/boxproof)：LaTeX 后端支持库
- [boxproof-typst](https://github.com/xiaoshihou514/boxproof-typst)：Typst 后端支持库

## 故障排除

### 常见问题

1. **Native Image 构建失败**
   - 确保已安装 GraalVM 和 Native Image 组件
   - 检查 Java 版本兼容性

2. **Scala Native 构建问题**
   - 确认 Scala Native 工具链已正确安装
   - 检查 Scala 版本一致性（3.8.2）

3. **Web 开发服务器问题**
   - 确保在 `web` 目录中运行 `npm run dev`
   - 检查 Node.js 版本兼容性

4. **测试失败**
   - 检查 `test` 目录中的测试文件
   - 确认环境变量 `ndpc.repoRoot` 已正确设置

### 获取帮助

- [GitHub Issues](https://github.com/xiaoshihou514/ndpc/issues/new)
- [GitHub Discussions](https://github.com/xiaoshihou514/ndpc/discussions)

---

*本文档最后更新于 2026年3月18日，基于项目当前状态生成。*