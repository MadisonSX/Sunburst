# nolint: start (Pylance 无法识别 dplyr NSE 的非标准求值语言特性，产生虚假警告)
# 加载必要的包
library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(stringr)
library(scales)
library(geomtextpath)

# 规范化文本：替换特殊字符和 Unicode 字符，确保兼容性
normalize_text <- function(text) {
  if (is.na(text) || text == "") return(text)
  
  # 替换 Unicode 罗马数字为 ASCII 等价
  replacements <- c(
    "Ⅰ" = "I", "ⅰ" = "i",
    "Ⅱ" = "II", "ⅱ" = "ii",
    "Ⅲ" = "III", "ⅲ" = "iii",
    "Ⅳ" = "IV", "ⅳ" = "iv",
    "Ⅴ" = "V", "ⅴ" = "v",
    "Ⅵ" = "VI", "ⅵ" = "vi",
    "Ⅶ" = "VII", "ⅶ" = "vii",
    "Ⅷ" = "VIII", "ⅷ" = "viii",
    "Ⅸ" = "IX", "ⅸ" = "ix",
    "Ⅹ" = "X", "ⅹ" = "x"
  )
  
  for (k in names(replacements)) {
    text <- stringr::str_replace_all(text, fixed(k), replacements[[k]])
  }
  
  # 替换特殊短划线为标准短划线
  text <- stringr::str_replace_all(text, "–", "-")  # en dash
  text <- stringr::str_replace_all(text, "—", "-")  # em dash
  text <- stringr::str_replace_all(text, "−", "-")  # 数学减号
  
  return(text)
}

# 创建output文件夹（如果不存在）
if (!dir.exists("output")) dir.create("output", recursive = TRUE, showWarnings = FALSE)

# 读取数据：增加第4列 count（数值），并指定 col_types 以减少解析错误
# Excel 表格第一行为数据（没有表头），因此使用 col_names 来命名列
data <- read_excel("data/旭日图数据.xlsx", 
                   col_names = c("category", "subcategory", "therapy", "count"),
                   col_types = c("text", "text", "text", "numeric"))

# 数据预处理
data_clean <- data %>%
  # 去除完全空白的行
  filter(!(is.na(category) & is.na(subcategory) & is.na(therapy) & is.na(count))) %>%
  # 去除category为空的行
  filter(!is.na(category) & category != "") %>%
  # 创建标识：第二列是否为空
  mutate(
    has_subcategory = !is.na(subcategory) & subcategory != "",
    # 如果第二列为空，则第二圈留空（明确字符类型）
    subcategory = ifelse(has_subcategory, subcategory, NA_character_),
    # 确保therapy不为空：优先使用 therapy，本身为空则使用 subcategory，否则默认 "疗法"
    therapy = ifelse(is.na(therapy) | therapy == "", dplyr::coalesce(subcategory, "疗法"), therapy),
    # 确保 count 为数值，缺失或 <=0 的行默认赋值 1（表示至少一项）
    count = as.numeric(count),
    count = ifelse(is.na(count) | count <= 0, 1, count)
  ) %>%
  # 创建唯一标识和文本规范化
  mutate(
    id = row_number(),
    category = as.character(category),
    subcategory = as.character(subcategory),
    therapy = as.character(therapy),
    has_subcategory = as.logical(has_subcategory)
  ) %>%
  # 规范化所有文本（替换特殊字符）
  mutate(
    category = sapply(category, normalize_text, USE.NAMES = FALSE),
    subcategory = sapply(subcategory, normalize_text, USE.NAMES = FALSE),
    therapy = sapply(therapy, normalize_text, USE.NAMES = FALSE)
  )

# 智能换行函数 - 不拆分单词
smart_wrap <- function(text, max_chars = 10) {
  if (is.na(text) || text == "") return(text)

  # 如果是单个长词，折半插入换行
  if (!grepl(" ", text) && nchar(text) > max_chars) {
    mid <- ceiling(nchar(text) / 2)
    return(paste0(substr(text, 1, mid), "\n", substr(text, mid + 1, nchar(text))))
  }

  # 使用 stringr 的分词换行，尽量不拆分单词
  wrapped <- stringr::str_wrap(text, width = max_chars)
  return(wrapped)
}

# 准备旭日图数据（改为使用 count 作为权重）
prepare_sunburst_data <- function(data) {
  # 第一层：分类（使用 count 的和来表示权重）
  level1 <- data %>%
    group_by(category) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      ymax = cumsum(count),
      ymin = c(0, head(ymax, n = -1)),
      xmin = 2.5,
      xmax = 4.0,
      label = sapply(category, smart_wrap, max_chars = 8),  # 应用智能换行
      level = 1,
      label_x = 3.25,
      label_y = (ymin + ymax) / 2
    )
  
  # 第二层：亚分类（只有当有内容时才显示分块，否则显示空白环块）
  level2_with_sub <- data %>%
    filter(has_subcategory) %>%
    group_by(category, subcategory) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    left_join(level1 %>% select(category, cat_ymin = ymin, cat_ymax = ymax), 
              by = "category")
  
  # 创建空白环块数据（用于没有亚分类的部分）
  level2_empty <- data %>%
    filter(!has_subcategory) %>%
    group_by(category) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    left_join(level1 %>% select(category, cat_ymin = ymin, cat_ymax = ymax), 
              by = "category") %>%
    group_by(category) %>%
    mutate(
      ymax = cat_ymax,
      ymin = cat_ymin,
      xmin = 4.0,
      xmax = 5.1,
      label = "",  # 空白标签
      level = 2,
      label_x = 4.55,
      label_y = (ymin + ymax) / 2
    )
  
  # 如果有亚分类数据
  if (nrow(level2_with_sub) > 0) {
    level2_with_sub <- level2_with_sub %>%
      group_by(category) %>%
      mutate(
        prop = count / sum(count),
        ymax = cat_ymin + cumsum(prop) * (cat_ymax - cat_ymin),
        ymin = cat_ymin + c(0, head(cumsum(prop), n = -1)) * (cat_ymax - cat_ymin),
        ymin = ifelse(is.na(ymin), cat_ymin, ymin),
        xmin = 4.0,
        xmax = 5.1,
        label = sapply(subcategory, smart_wrap, max_chars = 10),  # 应用智能换行
        level = 2,
        label_x = 4.55,
        label_y = (ymin + ymax) / 2
      )
    
    # 合并有亚分类和空白环块的数据
    level2 <- bind_rows(
      level2_with_sub %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y),
      level2_empty %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y)
    )
  } else {
    # 如果没有亚分类数据，只使用空白环块
    level2 <- level2_empty %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y)
  }
  
  # 第三层：疗法（均按 count 权重分配）
  # 需要区分有亚分类和没有亚分类的数据
  
  # 处理有亚分类的数据
  data_with_sub <- data %>%
    filter(has_subcategory)
  
  if (nrow(data_with_sub) > 0) {
    level3_with_sub <- data_with_sub %>%
      group_by(category, subcategory, therapy) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
      left_join(level2_with_sub %>% select(category, subcategory, sub_ymin = ymin, sub_ymax = ymax), 
                by = c("category", "subcategory")) %>%
      group_by(category, subcategory) %>%
      mutate(
        prop = count / sum(count),
        ymax = sub_ymin + cumsum(prop) * (sub_ymax - sub_ymin),
        ymin = sub_ymin + c(0, head(cumsum(prop), n = -1)) * (sub_ymax - sub_ymin),
        ymin = ifelse(is.na(ymin), sub_ymin, ymin),
        xmin = 5.1,
        xmax = 5.9,
        label = sapply(therapy, smart_wrap, max_chars = 8),  # 应用智能换行
        level = 3,
        label_x = 5.5,
        label_y = (ymin + ymax) / 2
      ) %>%
      select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y)
  } else {
    level3_with_sub <- tibble()
  }
  
  # 处理没有亚分类的数据
  data_without_sub <- data %>%
    filter(!has_subcategory)
  
  if (nrow(data_without_sub) > 0) {
    level3_without_sub <- data_without_sub %>%
      group_by(category, therapy) %>%
      summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
      left_join(level1 %>% select(category, cat_ymin = ymin, cat_ymax = ymax), 
                by = "category") %>%
      group_by(category) %>%
      mutate(
        prop = count / sum(count),
        ymax = cat_ymin + cumsum(prop) * (cat_ymax - cat_ymin),
        ymin = cat_ymin + c(0, head(cumsum(prop), n = -1)) * (cat_ymax - cat_ymin),
        ymin = ifelse(is.na(ymin), cat_ymin, ymin),
        xmin = 5.1,  # 第三圈位置
        xmax = 5.9,
        label = sapply(therapy, smart_wrap, max_chars = 8),  # 应用智能换行
        level = 3,
        label_x = 5.5,
        label_y = (ymin + ymax) / 2
      ) %>%
      select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y)
  } else {
    level3_without_sub <- tibble()
  }
  
  # 合并所有数据
  sunburst_data <- bind_rows(
    level1 %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y),
    level2,
    level3_with_sub,
    level3_without_sub
  ) %>%
    arrange(level, ymin)
  
  return(sunburst_data)
}

# 准备数据
sunburst_data <- prepare_sunburst_data(data_clean)

# 检查数据结构
cat("数据层次结构:\n")
cat("第一层(分类)记录数:", nrow(filter(sunburst_data, level == 1)), "\n")
cat("第二层(亚分类/空白)记录数:", nrow(filter(sunburst_data, level == 2)), "\n")
cat("第三层(疗法)记录数:", nrow(filter(sunburst_data, level == 3)), "\n")

# 设置颜色（更鲜艳的调色板）
n_categories <- dplyr::n_distinct(data_clean$category)

# 使用高对比度色系
if (n_categories <= 8) {
  color_palette <- brewer.pal(max(3, n_categories), "Dark2")
} else if (n_categories <= 12) {
  color_palette <- brewer.pal(n_categories, "Paired")
} else {
  color_palette <- colorRampPalette(brewer.pal(12, "Paired"))(n_categories)
}

category_colors <- setNames(color_palette[seq_len(n_categories)], unique(data_clean$category))

# 创建环状旭日图（优化美观性）
sunburst_plot <- ggplot(sunburst_data) +
  # 绘制矩形（增加边框宽度和对比度）
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
                fill = category),
            color = "white", linewidth = 1.3, alpha = 0.96) +
  # 转换为极坐标
  coord_polar(theta = "y", start = 0, clip = "off") +
  # 最小化外周留白：调整 x 轴范围使其紧凑
  xlim(0.2, 6.1) +
  # 设置颜色（更鲜艳）
  scale_fill_manual(values = category_colors) +
  # 改进主题：现代简洁风格
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#f8f9fa", color = NA),
    panel.background = element_rect(fill = "#f8f9fa", color = NA),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)  # 无外周留白
  )

# 标签添加函数 - 简单有效的版本
add_textpath_labels <- function(plot, data, levels = 1:3, min_sector_deg = 6) {
  max_y <- max(data$ymax, na.rm = TRUE)

  for (lvl in levels) {
    level_data <- data %>%
      filter(level == lvl) %>%
      filter(label != "") %>%
      mutate(
        sector_angle = (ymax - ymin) / max_y * 360,
        center_y = (ymin + ymax) / 2
      ) %>%
      filter(sector_angle > min_sector_deg)

    if (nrow(level_data) == 0) next

    # 为每个标签生成一条弧路径（在极坐标下，y 映射为角度）
    n_points <- 160
    path_list <- lapply(seq_len(nrow(level_data)), function(i) {
      row <- level_data[i, ]
      yseq <- seq(row$ymin, row$ymax, length.out = n_points)
      data.frame(
        x = rep(row$label_x, n_points),
        y = yseq,
        label = rep(as.character(row$label), n_points),
        id = rep(paste0("L", lvl, "_", i), n_points),
        stringsAsFactors = FALSE
      )
    })

    path_df <- do.call(rbind, path_list)

    # 分别设置各层文字大小和颜色
    size_map <- c(`1` = 8, `2` = 6, `3` = 5)
    txt_size <- ifelse(as.character(lvl) %in% names(size_map), size_map[as.character(lvl)], 5)

    plot <- plot +
      geom_textpath(
        data = path_df,
        aes(x = x, y = y, label = label, group = id),
        linetype = 0,
        size = txt_size,
        color = "white",
        fontface = "bold",
        upright = TRUE,
        # 避免文本彼此重叠过多
        vjust = 0.5
      )
  }

  return(plot)
}

# 使用 geomtextpath 沿弧添加标签（更接近 example.R 的做法）
sunburst_plot <- add_textpath_labels(sunburst_plot, sunburst_data, levels = 1:3)

# 显示图形
print(sunburst_plot)

# 保存图形（高质量输出，紧凑尺寸）
output_file <- "output/旭日图.png"
ggsave(output_file, sunburst_plot,
       width = 14, height = 14, dpi = 300, bg = "#f8f9fa",
       limitsize = FALSE)

cat("\n═══════════════════════════════════════════════════════════\n")
cat("✓ 旭日图已成功生成！\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("📁 保存路径：", output_file, "\n")
cat("📊 图像尺寸：14×14 英寸 (300 DPI)，圆形紧凑\n")
cat("🎨 样式：优化色系 + 白色文本标签\n")
cat("📝 标签：沿圆弧排列，上半部分向圆心，下半部分向外\n")
cat("═══════════════════════════════════════════════════════════\n")

# 输出换行统计
cat("\n换行统计:\n")
for (lvl in 1:3) {
  level_data <- sunburst_data %>% filter(level == lvl, label != "")
  if (nrow(level_data) == 0) next

  original_labels <- switch(as.character(lvl),
                            '1' = unique(na.omit(data_clean$category)),
                            '2' = unique(na.omit(data_clean$subcategory)),
                            '3' = unique(na.omit(data_clean$therapy)))

  wrapped_labels <- level_data$label
  wrapped_count <- sum(str_detect(wrapped_labels, "\n"))
  total_count <- nrow(level_data)

  pct <- if (total_count > 0) wrapped_count / total_count * 100 else 0
  cat(sprintf("第%d层: %d 个标签，其中 %d 个被换行显示 (%.1f%%)\n", lvl, total_count, wrapped_count, pct))
}
# nolint: end