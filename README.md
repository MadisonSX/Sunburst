# 旭日图项目 (Sunburst Chart Project)

## 项目简介
这是一个使用R语言制作旭日图（Sunburst Chart）的项目。旭日图是一种层级化的可视化图表，用于展示多层级结构数据的组成比例。

## 项目结构
```
Sunburst/
├── data/              # 数据文件目录
├── scripts/           # R脚本目录
├── output/            # 输出图表目录
├── README.md          # 项目说明
└── requirements.txt   # R包依赖
```

## 所需的R包
- plotly - 交互式图表库
- ggplot2 - 优雅的图形语法库
- dplyr - 数据处理库
- tidyr - 数据整理库

## 安装依赖

在R控制台中运行：
```R
install.packages(c("plotly", "ggplot2", "dplyr", "tidyr"))
```

## 使用说明

1. 在 `data/` 目录中放置数据文件（CSV格式）
2. 在 `scripts/` 目录中编写R脚本
3. 运行脚本生成旭日图
4. 输出图表将保存在 `output/` 目录中

## 示例

详见 `scripts/example_sunburst.R` 文件

## 开发工具
- R语言 (≥ 3.6.0)
- RStudio (推荐)
- VS Code (可选)
