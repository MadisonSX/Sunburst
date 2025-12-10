# 项目设置指南

## 项目概述
这是一个R语言项目，专门用于创建和可视化旭日图（Sunburst Charts）。

## 快速开始

### 1. 安装R和必要的包
```R
# 在R或RStudio中运行
install.packages(c("plotly", "dplyr", "tidyr", "viridis", "htmlwidgets"))
```

### 2. 运行示例
```R
# 设置工作目录
setwd("D:/Software-file/VS_code/Sunburst")

# 运行示例脚本
source("scripts/example_sunburst.R")
```

### 3. 使用自己的数据
- 将CSV数据放在 `data/` 目录中
- 在R脚本中使用 `data_preparation.R` 中的函数处理数据
- 使用 `utils.R` 中的 `create_sunburst()` 函数生成旭日图

## 文件说明

### scripts/ 目录
- **example_sunburst.R** - 两个完整的旭日图示例
- **data_preparation.R** - 数据处理和转换函数
- **utils.R** - 通用工具函数

### data/ 目录
- **sample_sales.csv** - 示例销售数据

## 旭日图结构说明

旭日图需要以下列：
- **labels** - 节点标签名称
- **parents** - 父节点标签（根节点为空字符串""）
- **values** - 节点值（通常是数值）

## 常见操作

### 创建简单旭日图
```R
# 准备数据
data <- data.frame(
  labels = c("总计", "类别A", "类别B", "项目A1", "项目B1"),
  parents = c("", "总计", "总计", "类别A", "类别B"),
  values = c(NA, 100, 80, 60, 50)
)

# 创建并保存
create_sunburst(data, 
                title = "我的旭日图",
                output_file = "output/my_sunburst.html")
```

### 从CSV读取数据并创建旭日图
```R
source("scripts/data_preparation.R")
source("scripts/utils.R")

# 读取数据
df <- read.csv("data/your_data.csv", encoding = "UTF-8")

# 准备为分级结构
hierarchical_df <- prepare_hierarchical_data(
  df,
  category_col = "category",
  subcategory_col = "subcategory",
  value_col = "value"
)

# 创建旭日图
create_sunburst(hierarchical_df, 
                title = "数据旭日图",
                output_file = "output/data_sunburst.html")
```

## 输出

所有生成的HTML文件将保存在 `output/` 目录中，可以用浏览器打开查看交互式图表。

## 推荐工具

- **RStudio** - R的IDE环境（推荐）
- **VS Code** - 轻量级编辑器
- **R Console** - 命令行运行

## 许可证
本项目为开源项目，可自由使用和修改。
