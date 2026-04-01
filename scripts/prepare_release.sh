#! /usr/bin/env bash

info() {
    echo -e "\033[0;32m[INFO] $1\033[0m"
}

err() {
    echo -e "\033[0;31m[ERROR] $1\033[0m"
}

set -ex

# make clean
#
# info "Test compile..."
# if sbt compile; then
#     info "Compile ok"
# else
#     err "Compile check failed"
# fi
#
# info "Run core tests"
# if sbt "core/test"; then
#     info "Core test ok"
# else
#     err "Core test failed"
# fi
#
# info "Run cli tests"
# if sbt "cli/test"; then
#     info "Cli test ok"
# else
#     err "Cli test failed"
# fi
#
# info "Check web build"
# if npm run build; then
#     info "Web build ok"
# else
#     err "Web build failed"
# fi
#
# info "Check jar build"
# if make jar; then
#     info "Jar build ok"
# else
#     err "Jar build failed"
# fi
#
# info "Check graal build"
# if make graal; then
#     info "Graal build ok"
# else
#     err "Graal build failed"
# fi
#
# info "Check native build"
# if make native; then
#     info "Native build ok"
# else
#     err "Native build failed"
# fi
#
# info "Start package"
# mv ndpc-graal ndpc-graal-linux-x86_64
# mv ndpc-native ndpc-native-linux-x86_64

# 获取所有tag并按版本号排序
tags=$(git tag -l | sort -V)

# 获取最新的两个tag
latest_tag=$(echo "$tags" | tail -n 1)
second_latest_tag=$(echo "$tags" | tail -n 2 | head -n 1)

# 检查是否成功获取到tag
if [ -z "$latest_tag" ] || [ -z "$second_latest_tag" ]; then
    err "错误：无法获取到足够的tag"
    exit 1
fi

set +ex
echo "================================"

echo "# $latest_tag"
echo

# 生成英文总结并保存到变量
en_summary=$(git log "$second_latest_tag..$latest_tag" --pretty=medium --no-merges \
    | qwen "Summarize the important, non trivial changes in English, keep it very concise, no additional outputs, minimal markdown formatting")

# 输出英文总结
echo "$en_summary"

echo

# 将英文总结翻译为中文（确保内容一致）
zh_summary=$(echo "$en_summary" | qwen "Translate the above text to Chinese, keep it concise, no additional outputs")
echo "$zh_summary"

echo
echo "## Downloads"
echo "- [Universal jar](https://github.com/xiaoshihou514/ndpc/releases/download/$latest_tag/ndpc.jar)"
echo "- [Linux graal build](https://github.com/xiaoshihou514/ndpc/releases/download/$latest_tag/ndpc-graal-linux-x86_64)"
echo "- [Linux native build](https://github.com/xiaoshihou514/ndpc/releases/download/$latest_tag/ndpc-native-linux-x86_64)"

echo
echo "## 下载"
echo '- [jar（全平台）](https://github.com/xiaoshihou514/ndpc/releases/download/'$latest_tag'/ndpc.jar)'
echo "- [Graal VM可执行文件（Linux x86_64）](https://github.com/xiaoshihou514/ndpc/releases/download/$latest_tag/ndpc-graal-linux-x86_64)"
echo "- [原生可执行文件（Linux x86_64）](https://github.com/xiaoshihou514/ndpc/releases/download/$latest_tag/ndpc-native-linux-x86_64)"
