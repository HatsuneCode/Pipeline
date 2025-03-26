#!/bin/bash

# 检查参数数量是否正确
if [ $# -ne 2 ]; then
    echo "使用方法: $0 <输入文件.txt> <输出目录>"
    echo "示例: $0 SRR_Acc_List.txt datasets"
    exit 1
fi

input_file="$1"
output_dir="$2"

# 检查输入文件是否存在
if [ ! -f "$input_file" ]; then
    echo "错误: 输入文件 $input_file 不存在"
    exit 1
fi

# 创建输出目录（如果不存在）
mkdir -p "$output_dir" || {
    echo "错误: 无法创建输出目录 $output_dir"
    exit 1
}

# 运行prefetch命令
echo "正在下载数据..."
prefetch -p -f no --option-file "$input_file" -O "$output_dir"

# 检查命令执行是否成功
if [ $? -eq 0 ]; then
    echo "下载完成！数据保存在: $output_dir"
else
    echo "下载过程中出现错误"
    exit 1
fi
