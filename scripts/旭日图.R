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
  plot_bg = "#FFFFFF",
  
  # 层级半径（内到外）
  radius = list(
    inner = 0.2,      # 中心空白
    level1_min = 1.5,
    level1_max = 3.5,
    level2_min = 3.5,
    level2_max = 4.8,
    level3_min = 4.8,
    level3_max = 5.9,
    outer = 6.1       # 外围边界
  ),
  
  # 标签参数
  label = list(
    max_chars = c(8, 20, 8),  # 各层最大字符数
    sizes = c(7, 6, 5),       # 各层字体大小
    min_sector_deg = 6        # 最小扇区角度（度）
  ),
  
  # 边框和透明度
  border_width = 1.3,
  alpha = 0.96,
  # 图例排序："none" 不排序，"alpha" 按解释文字 A–Z 排序
  legend_sort = "alpha"
)

# ============================================================================
# 加载必要的包
# ============================================================================
required_packages <- c("readxl", "ggplot2", "dplyr", "RColorBrewer", 
                       "stringr", "scales", "geomtextpath", "cowplot", "grid")

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

# 添加highlight列（如果不存在）
if (!"highlight" %in% names(data_clean)) {
  data_clean$highlight <- "Yes"
} else {
  data_clean <- data_clean %>%
    mutate(highlight = ifelse(is.na(highlight) | highlight == "", "Yes", as.character(highlight)))
}

# 添加rotation列（如果不存在），规范化为小写
if (!"rotation" %in% names(data_clean)) {
  data_clean$rotation <- "up"
} else {
  data_clean <- data_clean %>%
    mutate(rotation = tolower(ifelse(is.na(rotation) | rotation == "", "up", as.character(rotation))),
           rotation = ifelse(rotation == "down", "down", "up"))  # 只允许"up"或"down"
}

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
  # 先读取前4列，然后检查是否有第5列
  data_raw <- read_excel(CONFIG$data_file, col_names = FALSE)
  n_cols <- ncol(data_raw)
  
  if (n_cols >= 6) {
    # 有6列或更多，读取前6列
    read_excel(CONFIG$data_file, 
               col_names = c("category", "subcategory", "therapy", "count", "highlight", "rotation"),
               col_types = c("text", "text", "text", "numeric", "text", "text"))
  } else if (n_cols >= 5) {
    # 有5列，读取前5列，第6列默认为up
    data_temp <- read_excel(CONFIG$data_file, 
               col_names = c("category", "subcategory", "therapy", "count", "highlight"),
               col_types = c("text", "text", "text", "numeric", "text"))
    data_temp$rotation <- "up"
    data_temp
  } else {
    # 只有4列，读取后添加highlight和rotation列
    data_temp <- read_excel(CONFIG$data_file, 
                            col_names = c("category", "subcategory", "therapy", "count"),
                            col_types = c("text", "text", "text", "numeric"))
    data_temp$highlight <- "Yes"
    data_temp$rotation <- "up"
    data_temp
  }
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
  # 判断该分类下是否所有行都是No
  category_highlight <- data %>%
    group_by(category) %>%
    summarise(is_highlighted = !all(highlight == "No"), .groups = "drop")
  
  # 获取分类的旋转状态（取该分类下的第一个值）
  category_rotation <- data %>%
    group_by(category) %>%
    summarise(rotation = first(rotation), .groups = "drop")
  
  level1 <- data %>%
    group_by(category) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = "drop") %>%
    # 按照原始顺序排序
    mutate(category = factor(category, levels = category_order)) %>%
    arrange(category) %>%
    left_join(category_highlight, by = "category") %>%
    left_join(category_rotation, by = "category") %>%
    mutate(
      ymax = cumsum(count),
      ymin = c(0, head(ymax, n = -1)),
      xmin = cfg$level1_min,
      xmax = cfg$level1_max,
      label = sapply(category, smart_wrap, max_chars = label_cfg$max_chars[1], USE.NAMES = FALSE),
      
      level = 1,
      label_x = (cfg$level1_min + cfg$level1_max) / 2,
      label_y = (ymin + ymax) / 2,
      rotation = ifelse(is.na(rotation), "up", rotation),
      category = as.character(category)
    )
  
  # 第二层：亚分类（包括空值，保留为空块），保持原始顺序
  # 判断该亚分类下是否所有行都是No
  subcategory_highlight <- data %>%
    group_by(category, subcategory) %>%
    summarise(is_highlighted = !all(highlight == "No"), .groups = "drop")
  
  # 获取亚分类的旋转状态（取该亚分类下的第一个值）
  subcategory_rotation <- data %>%
    group_by(category, subcategory) %>%
    summarise(rotation = first(rotation), .groups = "drop")
  
  level2 <- data %>%
    group_by(category, subcategory) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    mutate(category = factor(category, levels = category_order)) %>%
    arrange(category) %>%
    left_join(subcategory_highlight, by = c("category", "subcategory")) %>%
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
    ungroup() %>%
    left_join(subcategory_rotation %>% select(category, subcategory, rotation), 
              by = c("category", "subcategory")) %>%
    mutate(rotation = ifelse(is.na(rotation), "up", rotation)) %>%
    select(category, subcategory, label, xmin, xmax, ymin, ymax, level, label_x, label_y, is_highlighted, rotation)
  
  # 第三层：疗法（包括空值，保留为空块），保持原始顺序
  # 获取每个疗法的highlight状态
  therapy_highlight <- data %>%
    group_by(category, subcategory, therapy) %>%
    summarise(is_highlighted = !all(highlight == "No"), .groups = "drop")
  
  # 获取疗法的旋转状态（取该疗法下的第一个值）
  therapy_rotation <- data %>%
    group_by(category, subcategory, therapy) %>%
    summarise(rotation = first(rotation), .groups = "drop")
  
  level3 <- data %>%
    group_by(category, subcategory, therapy) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%
    mutate(category = factor(category, levels = category_order)) %>%
    arrange(category) %>%
    left_join(therapy_highlight, by = c("category", "subcategory", "therapy")) %>%
    left_join(level2 %>% select(category, subcategory, sub_ymin = ymin, sub_ymax = ymax), 
              by = c("category", "subcategory")) %>%
    # 不再需要 total_y，移除该连接
    # left_join(level1 %>% select(category, total_y) %>% distinct(), by = "category") %>%
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
    ungroup() %>%
    left_join(therapy_rotation %>% select(category, subcategory, therapy, rotation), 
              by = c("category", "subcategory", "therapy")) %>%
    mutate(rotation = ifelse(is.na(rotation), "up", rotation)) %>%
    select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y, is_highlighted, rotation)
  
  # 合并所有数据
  sunburst_data <- bind_rows(
    level1 %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y, is_highlighted, rotation),
    level2 %>% select(category, label, xmin, xmax, ymin, ymax, level, label_x, label_y, is_highlighted, rotation),
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

# 按照原始顺序分配颜色（深色）
category_colors <- setNames(color_palette[seq_len(n_categories)], category_order)

# 灰色（用于highlight为No的块）
gray_color <- "#CCCCCC"

# 为每个块分配颜色（根据is_highlighted）
sunburst_data <- sunburst_data %>%
  mutate(
    color = ifelse(is_highlighted, 
                   category_colors[category], 
                   gray_color)
  )

# ============================================================================
# 创建环状旭日图
# ============================================================================
cat("正在生成旭日图...\n")

sunburst_plot <- ggplot(sunburst_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color),
            color = "white", linewidth = CONFIG$border_width, alpha = CONFIG$alpha) +
  coord_polar(theta = "y", start = 0, clip = "off") +
  xlim(CONFIG$radius$inner, 7.5) +  # 扩大右边界以容纳图示
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = CONFIG$plot_bg, color = NA),
    panel.background = element_rect(fill = CONFIG$plot_bg, color = NA),
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0)
  )

#' 标签添加函数 - 沿弧路径绘制文本，支持up/down旋转
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
        center_y = (ymin + ymax) / 2,
        rotation = ifelse(is.na(rotation), "up", rotation)
      ) %>%
      filter(sector_angle > min_sector_deg)
    
    if (nrow(level_data) == 0) next
    
    # 为每个标签生成一条弧路径
    n_points <- 160
    path_list <- lapply(seq_len(nrow(level_data)), function(i) {
      row <- level_data[i, ]
      yseq <- seq(row$ymin, row$ymax, length.out = n_points)
      
      # 如果rotation为"down"，反转yseq顺序以实现180度旋转
      if (!is.na(row$rotation) && row$rotation == "down") {
        yseq <- rev(yseq)
      }
      
      data.frame(
        x = rep(row$label_x, n_points),
        y = yseq,
        label = rep(as.character(row$label), n_points),
        id = rep(paste0("L", lvl, "_", i), n_points),
        rotation = rep(row$rotation, n_points),
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
# 添加图示（Legend）在右下角
# ============================================================================
cat("正在生成图示...\n")

# 创建图示数据（可按解释文字 A–Z 排序，灰色 Not included 固定最后）
legend_categories <- category_order
legend_colors <- category_colors[legend_categories]
legend_df <- data.frame(
  label = legend_categories,
  color = legend_colors,
  stringsAsFactors = FALSE
)

# 按配置排序图例（不影响图中颜色，仅影响图例顺序）
if (!is.null(CONFIG$legend_sort) && CONFIG$legend_sort == "alpha") {
  legend_df <- legend_df %>% arrange(label)
}

# 追加灰色项到末尾
legend_df <- rbind(legend_df, data.frame(label = "Not included", color = gray_color))

# 计算总项数
n_items <- nrow(legend_df)

# 使用 cowplot 和 grid 来添加图例
# 先用 ggdraw 创建画布
sunburst_plot_with_legend <- cowplot::ggdraw(sunburst_plot)

# 添加图例矩形框和文本（在图片的右下角）
legend_x_start <- 0.75  # 调整这个值来移动图例左右位置（0-1，越大越靠右）
legend_y_start <- 0.28
legend_box_width <- 0.28
legend_box_height <- 0.02
legend_spacing <- 0.025

# 添加图例背景框（无边框）
sunburst_plot_with_legend <- sunburst_plot_with_legend +
  cowplot::draw_grob(
    grid::rectGrob(
      x = legend_x_start + legend_box_width / 2,
      y = legend_y_start - n_items * legend_spacing / 2,
      width = legend_box_width,
      height = n_items * legend_spacing + 0.01,
      gp = grid::gpar(fill = "white", col = NA)  # col = NA 移除边框线
    ),
    x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0
  )

# 添加每项图例
for (i in seq_len(n_items)) {
  y_pos <- legend_y_start - (i - 1) * legend_spacing
  
  # 颜色块
  sunburst_plot_with_legend <- sunburst_plot_with_legend +
    cowplot::draw_grob(
      grid::rectGrob(
        x = legend_x_start + 0.01,
        y = y_pos,
        width = 0.04,
        height = 0.018,
        gp = grid::gpar(fill = legend_df$color[i], col = "black", lwd = 0.5)
      ),
      x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0
    )
  
  # 标签文本
  sunburst_plot_with_legend <- sunburst_plot_with_legend +
    cowplot::draw_grob(
      grid::textGrob(
        legend_df$label[i],
        x = legend_x_start + 0.04,
        y = y_pos,
        just = c("left", "center"),
        gp = grid::gpar(fontsize = 14, col = "black")
      ),
      x = 0, y = 0, width = 1, height = 1, hjust = 0, vjust = 0
    )
}

sunburst_plot <- sunburst_plot_with_legend

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