<div align="center">

# Ndpc

<img src="https://github.com/user-attachments/assets/a6a3fd99-1a96-40c5-805b-235e43202112" alt="logo" width="30%" />

适用于单类谓词形式逻辑的证明器

[入门](https://xiaoshihou514.github.io/ndpc/getting-started.html) •
[教程](https://xiaoshihou514.github.io/ndpc/tutorial.html) •
[手册](https://xiaoshihou514.github.io/ndpc/syntax.html)

</div>

ndpc专注于正确性、可维护性和形式化验证，其代码风格与手写证明相仿。ndpc可以：

- 证明检查
- 生成对应的Lean4证明
- 将证明导出为HTML、Latex和Typst

## Monorepo 结构

- `core/`：共享的语法树、解析器、检查器与格式化核心逻辑
- `cli/`：命令行入口、后端生成器与发布打包
- `web/`：基于 Laminar + Scala.js 的前端脚手架，并已接入共享核心逻辑

## 构建与测试

需要先安装 [sbt](https://www.scala-sbt.org/) 和 JDK。

```bash
sbt coreJVM/test
sbt cli/test
sbt web/fastLinkJS
sbt "cli/run -- check example.ndp"
```

## 发布构建

构建可运行 jar：

```bash
sbt cli/assembly
```

产物文件为仓库根目录下的 `ndpc.jar`。

使用 GraalVM `native-image` 构建原生可执行文件：

```bash
sbt cli/graalNativeImage
```

产物文件为仓库根目录下的 `ndpc-graal`。

使用 Scala Native 构建原生可执行文件：

```bash
sbt cliNative/rootNativeLink
```

产物文件为仓库根目录下的 `ndpc-native`。

## 入门

关于安装和基本使用的详细信息，请访问我们的[入门指南页面](https://xiaoshihou514.github.io/ndpc/getting-started.html)。

推荐阅读我们的[入门指南](https://xiaoshihou514.github.io/ndpc/tutorial.html)，亦可参考[语法手册](https://xiaoshihou514.github.io/ndpc/syntax.html)。

## 答疑

[Github工单](https://github.com/xiaoshihou514/ndpc/issues/new)或[Github论坛](https://github.com/xiaoshihou514/ndpc/discussions)

## 友链

- [ndp.vim](https://github.com/xiaoshihou514/ndp.vim)：ndp文件(Neo)Vim支持
- [aristotle](https://github.com/xiaoshihou514/aristotle)：ndpc图形界面
- [boxproof](https://github.com/YunkaiZhang233/boxproof)：Latex后端依赖
- [boxproof-typst](https://github.com/xiaoshihou514/boxproof-typst): Typst后端依赖
