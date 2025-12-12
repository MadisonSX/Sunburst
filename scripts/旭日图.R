# nolint: start (Pylance 无法识别 dplyr NSE 的非标准求值语言特性，产生虚假警告)

# ============================================================================
# 配置参数
# ============================================================================
CONFIG <- list(
  # 文件路径
  data_file = "data/旭日图数据.xlsx",
  output_file = "output/旭日图.png",
  
  # 图形参数
  plot_width = 14,
  plot_height = 14,
  plot_dpi = 300,
  plot_bg = "#f8f9fa",
  
  # 层级半径（内到外）
  radius = list(
    inner = 0.2,      # 中心空白
    level1_min = 2.5,
    level1_max = 4.0,
    level2_min = 4.0,
    level2_max = 5.1,
    level3_min = 5.1,
    level3_max = 5.9,
    outer = 6.1       # 外围边界
  ),
  
  # 标签参数
  label = list(
    max_chars = c(8, 20, 8),  # 各层最大字符数
    sizes = c(8, 6, 5),       # 各层字体大小
    min_sector_deg = 6        # 最小扇区角度（度）
  ),
  
  # 边框和透明度
  border_width = 1.3,
  alpha = 0.96
)

# ============================================================================
# 加载必要的包
# ============================================================================
required_packages <- c("readxl", "ggplot2", "dplyr", "RColorBrewer", 
                       "stringr", "scales", "geomtextpath")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    stop(sprintf("缺少必需的包：%s。请运行 install.packages('%s')", pkg, pkg))
  }
}

# ============================================================================
# 工具函数
# ============================================================================

#' 规范化文本：替换特殊字符和 Unicode 字符，确保兼容性
#' @param text 输入文本字符串或字符向量
#' @return 规范化后的文本
normalize_text <- function(text) {
  # 批量替换 Unicode 罗马数字为 ASCII 等价
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
    "Ⅹ" = "X", "ⅹ" = "x",
    "–" = "-",  # en dash
    "—" = "-",  # em dash
    "−" = "-"   # 数学减号
  )
  
  # 逐个替换以确保向量化正确
  for (pattern in names(replacements)) {
    text <- stringr::str_replace_all(text, stringr::fixed(pattern), replacements[pattern])
  }
  
  return(text)
}

#' 智能换行函数 - 不拆分单词
#' @param text 输入文本
#' @param max_chars 每行最大字符数
#' @return 换行后的文本
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

#' 验证数据完整性
#' @param data 数据框
#' @return 逻辑值，TRUE 表示通过验证
validate_data <- function(data) {
  if (nrow(data) == 0) {
    stop("数据文件为空")
  }
  
  required_cols <- c("category", "subcategory", "therapy", "count")
  if (!all(required_cols %in% names(data))) {
    stop("数据缺少必需的列")
  }
  
  if (all(is.na(data$category))) {
    stop("所有分类都为空")
  }
  
  return(TRUE)
}

# 数据预处理
cat("正在清洗数据...\n")
data_clean <- data %>%
  # 去除完全空白的行
  filter(!(is.na(category) & is.na(subcategory) & is.na(therapy) & is.na(count))) %>%
  # 去除category为空的行
  filter(!is.na(category) & category != "") %>%
  # 将空值转换为空白字符串，确保 count 为数值
  mutate(
    category = as.character(category),
    subcategory = ifelse(is.na(subcategory) | subcategory == "", "", as.character(subcategory)),
    therapy = ifelse(is.na(therapy) | therapy == "", "", as.character(therapy)),
    count = as.numeric(count),
    count = ifelse(is.na(count) | count <= 0, 1, count),
    # 规范化文本（现在 normalize_text 支持向量化操作）
    category = normalize_text(category),
    subcategory = normalize_text(subcategory),
    therapy = normalize_text(therapy)
  )

# 获取分类的原始顺序
category_order <- unique(data_clean$category)
cat("分类数量:", length(category_order), "\n")
cat("分类列表:", paste(category_order, collapse = ", "), "\n")

# ============================================================================
# 数据加载和预处理
# ============================================================================

# 创建输出文件夹
if (!dir.exists("output")) dir.create("output", recursive = TRUE, showWarnings = FALSE)

# 检查数据文件是否存在
if (!file.exists(CONFIG$data_file)) {
  stop(sprintf("数据文件不存在：%s", CONFIG$data_file))
}

# 读取数据
cat("正在读取数据...\n")
data <- tryCatch({
  read_excel(CONFIG$data_file, 
             col_names = c("category", "subcategory", "therapy", "count"),
             col_types = c("text", "text", "text", "numeric"))
}, error = function(e) {
  stop(sprintf("读取数据文件失败：%s", e$message))
})

# 验证数据
validate_data(data)

# ============================================================================
# 准备旭日图数据
# ============================================================================

#' 准备旭日图数据（保持原始顺序）
#' @param data 清洗后的数据
#' @param category_order 分类的原始顺序
#' @return 旭日图数据框
prepare_sunburst_data <- function(data, category_order) {
  cfg <- CONFIG$radius
  label_cfg <- CONFIG$label
  
  # 第一层：分类（使用 count 的和来表示权重），按照原始顺序
  level1 <- data %>%
    group_by(category) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    # 按照原始顺序排序
    mutate(category = factor(category, levels = category_order)) %>%
    arrange(category) %>%
    mutate(
      ymax = cumsum(count),
      ymin = c(0, head(ymax, n = -1)),
      xmin = cfg$level1_min,
      xmax = cfg$level1_max,
      label = sapply(category, smart_wrap, max_chars = label_cfg$max_chars[1], USE.NAMES = FALSE),
      level = 1,
      label_x = (cfg$level1_min + cfg$level1_max) / 2,
      label_y = (ymin + ymax) / 2,
      category = as.character(category)
    )
  
  # 第二层：亚分类（包括空值，保留为空块），保持原始顺序
  level2 <- data %>%
    group_by(category, subcategory) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    mutate(category = factor(category, levels = category_order)) %>%
    arrange(category) %>%
    left_join(level1 %>% select(category, cat_ymin = ymin, cat_ymax = ymax), 
              by = "category") %>%
    group_by(category) %>%
    mutate(
      prop = count / sum(count),
      ymax = cat_ymin + cumsum(prop) * (cat_ymax - cat_ymin),
      ymin = cat_ymin + c(0, head(cumsum(prop), n = -1)) * (cat_ymax - cat_ymin),
      ymin = ifelse(is.na(ymin), cat_ymin, ymin),
      xmin = cfg$level2_min,
      xmax = cfg$level2_max,
      # 空值亚分类不显示标签，但保留颜色块
      label = ifelse(subcategory == "", "", sapply(subcategory, smart_wrap, max_chars = label_cfg$max_chars[2], USE.NAMES = FALSE)),
      level = 2,
      label_x = (cfg$level2_min + cfg$level2_max) / 2,
      label_y = (ymin + ymax) / 2,
      category = as.character(category)
    ) %>%
    select(category, subcategory, label, xmin, xmax, ymin, ymax, level, label_x, label_y)
  
  # 第三层：疗法（包括空值，保留为空块），保持原始顺序
  level3 <- data %>%
    group_by(category, subcategory, therapy) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    mutate(category = factor(category, levels = category_order)) %>%
    arrange(category) %>%
    left_join(level2 %>% select(category, subcategory, sub_ymin = ymin, sub_ymax = ymax), 
              by = c("category", "subcategory")) %>%
    group_by(category, subcategory) %>%
    mutate(
      prop = count / sum(count),
      ymax = sub_ymin + cumsum(prop) * (sub_ymax - sub_ymin),
      ymin = sub_ymin + c(0, head(cumsum(prop), n = -1)) * (sub_ymax - sub_ymin),
      ymin = ifelse(is.na(ymin), sub_ymin, ymin),
      xmin = cfg$level3_min,
      xmax = cfg$level3_max,
      # 空值疗法不显示标签，但保留颜色块
      label = ifelse(therapy == "", "", sapply(therapy, smart_wrap, max_chars = label_cfg$max_chars[3], USE.NAMES = FALSE)),
      level = 3,
      label_x = (cfg$level3_min + cfg$level3_max) / 2,
      label_y = (ymin + ymax) / 2,
      category = as.character(category)
    ) %>%
    select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y)
  
  # 合并所有数据
  sunburst_data <- bind_rows(
    level1 %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y),
    level2 %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y),
    level3
  ) %>%
    arrange(level, ymin)
  
  return(sunburst_data)
}

# 准备数据
cat("正在准备旭日图数据...\n")
sunburst_data <- prepare_sunburst_data(data_clean, category_order)

# 数据统计
cat("\n数据层次结构:\n")
cat("├─ 第一层(分类):", nrow(filter(sunburst_data, level == 1)), "项\n")
cat("├─ 第二层(亚分类):", nrow(filter(sunburst_data, level == 2)), "项\n")
cat("└─ 第三层(疗法):", nrow(filter(sunburst_data, level == 3)), "项\n")

therapy_empty_count <- nrow(data_clean %>% filter(therapy == ""))
if (therapy_empty_count > 0) {
  cat("注: 疗法为空的记录数:", therapy_empty_count, "（已保留为空白块）\n")
}

# ============================================================================
# 设置颜色方案
# ============================================================================
cat("\n正在设置颜色方案...\n")
n_categories <- length(category_order)

# 使用高对比度色系
if (n_categories <= 8) {
  color_palette <- brewer.pal(max(3, n_categories), "Dark2")
} else if (n_categories <= 12) {
  color_palette <- brewer.pal(n_categories, "Paired")
} else {
  color_palette <- colorRampPalette(brewer.pal(12, "Paired"))(n_categories)
}

# 按照原始顺序分配颜色
category_colors <- setNames(color_palette[seq_len(n_categories)], category_order)

# ============================================================================
# 创建环状旭日图
# ============================================================================
cat("正在生成旭日图...\n")

sunburst_plot <- ggplot(sunburst_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = category),
            color = "white", linewidth = CONFIG$border_width, alpha = CONFIG$alpha) +
  coord_polar(theta = "y", start = 0, clip = "off") +
  xlim(CONFIG$radius$inner, CONFIG$radius$outer) +
  scale_fill_manual(values = category_colors) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = CONFIG$plot_bg, color = NA),
    panel.background = element_rect(fill = CONFIG$plot_bg, color = NA),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

#' 标签添加函数 - 沿弧路径绘制文本
#' @param plot ggplot对象
#' @param data 旭日图数据
#' @param levels 要添加标签的层级
#' @param min_sector_deg 最小扇区角度（度）
#' @return 添加了标签的ggplot对象
add_textpath_labels <- function(plot, data, levels = 1:3, 
                                min_sector_deg = CONFIG$label$min_sector_deg) {
  max_y <- max(data$ymax, na.rm = TRUE)
  label_sizes <- CONFIG$label$sizes
  
  for (lvl in levels) {
    level_data <- data %>%
      filter(level == lvl, label != "") %>%
      mutate(
        sector_angle = (ymax - ymin) / max_y * 360,
        center_y = (ymin + ymax) / 2
      ) %>%
      filter(sector_angle > min_sector_deg)
    
    if (nrow(level_data) == 0) next
    
    # 为每个标签生成一条弧路径
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
    
    # 获取该层的字体大小
    txt_size <- if (lvl <= length(label_sizes)) label_sizes[lvl] else 5
    
    plot <- plot +
      geom_textpath(
        data = path_df,
        aes(x = x, y = y, label = label, group = id),
        linetype = 0,
        size = txt_size,
        color = "white",
        fontface = "bold",
        upright = TRUE,
        vjust = 0.5
      )
  }
  
  return(plot)
}

# 添加标签
sunburst_plot <- add_textpath_labels(sunburst_plot, sunburst_data, levels = 1:3)

# ============================================================================
# 保存和输出
# ============================================================================
cat("正在保存图形...\n")

# 保存图形
ggsave(CONFIG$output_file, sunburst_plot,
       width = CONFIG$plot_width, 
       height = CONFIG$plot_height, 
       dpi = CONFIG$plot_dpi, 
       bg = CONFIG$plot_bg,
       limitsize = FALSE)

# 显示图形
print(sunburst_plot)

# 输出摘要信息
cat("\n")
cat("═══════════════════════════════════════════════════════════\n")
cat("✓ 旭日图已成功生成！\n")
cat("═══════════════════════════════════════════════════════════\n")
cat(sprintf("📁 保存路径: %s\n", CONFIG$output_file))
cat(sprintf("📊 图像尺寸: %d×%d 英寸 (%d DPI)\n", 
            CONFIG$plot_width, CONFIG$plot_height, CONFIG$plot_dpi))
cat(sprintf("🎨 配色方案: %s (%d种颜色)\n", 
            ifelse(n_categories <= 8, "Dark2", 
                   ifelse(n_categories <= 12, "Paired", "扩展Paired")),
            n_categories))
cat("📝 特性: 沿弧排列标签，智能换行，保持原始顺序\n")
cat("═══════════════════════════════════════════════════════════\n")

# 输出标签换行统计
cat("\n标签换行统计:\n")
for (lvl in 1:3) {
  level_data <- sunburst_data %>% filter(level == lvl, label != "")
  if (nrow(level_data) == 0) next
  
  wrapped_count <- sum(str_detect(level_data$label, "\n"))
  total_count <- nrow(level_data)
  pct <- if (total_count > 0) wrapped_count / total_count * 100 else 0
  
  cat(sprintf("├─ 第%d层: %d/%d 标签换行 (%.1f%%)\n", 
              lvl, wrapped_count, total_count, pct))
}

cat("\n处理完成！\n")
# nolint: end