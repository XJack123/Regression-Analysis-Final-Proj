# ==============================================================================
# 脚本名称: 03 可视化分析.R
# 功能描述: 对应 Python 代码的 FDI 时间序列可视化
#           包含两幅图: 1. 原始 FDI  2. Log(FDI)
#           输出: 显示图像并保存至 Figure 文件夹
# 依赖关系: 必须先运行或存在 "01 数据处理.R"
# ==============================================================================

# 1. 环境初始化与数据加载
rm(list = ls()) # 清空环境变量
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
source("01 数据处理.R", encoding = "UTF-8") # 加载数据、包和 theme_academic

# 补充加载扩展包
library(RColorBrewer) 

# ==================== 字体设置 (解决 Mac 中文乱码) ====================
# Mac 系统下 "serif" 字体可能无法正常显示中文，需指定系统自带中文字体
if (Sys.info()["sysname"] == "Darwin") {
  # Mac 系统：首选 "Songti SC" (宋体) 以保持学术风格，或者 "PingFang SC" (黑体)
  my_font_family <- "Arial Unicode MS" 
} else {
  # Windows/Linux 系统：默认 "serif" 通常能映射到宋体
  my_font_family <- "serif"
}

# ==================== 数据准备 ====================

plot_df <- df %>%
  mutate(
    Year_num = as.numeric(as.character(Year)),
    FDI = as.numeric(FDI),
    # 计算 Log(FDI)
    log_FDI = log(as.numeric(FDI))
  ) %>%
  filter(!is.na(Year_num) & !is.na(FDI) & !is.na(log_FDI) & !is.na(Region)) %>%
  # 剔除异常值 (保持与回归模型一致)
  filter(!(Year == "2021" & Province == "海南")) %>% 
  arrange(Region, Year_num)

# ==================== 通用绘图元素构建 ====================

# 1. 构造背景条数据 (模拟 Python 中的 axvspan 灰白相间效果)
years <- unique(plot_df$Year_num)
years_sorted <- sort(years)
bg_rects <- data.frame(
  xmin = years_sorted - 0.5,
  xmax = years_sorted + 0.5,
  ymin = -Inf,
  ymax = Inf,
  fill_group = seq_along(years_sorted) %% 2 == 0 
)
bg_rects <- subset(bg_rects, fill_group == TRUE)

# 2. 定义绘图函数 (避免代码重复)
create_fdi_plot <- function(data, y_var, title_str, y_label) {
  ggplot(data = data, aes(x = Year_num, y = .data[[y_var]], group = Region)) +
    
    # 背景条
    geom_rect(data = bg_rects, 
              aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
              inherit.aes = FALSE,
              fill = "#f2f2f2", alpha = 0.6) +
    
    # 线性拟合
    geom_smooth(aes(color = Region), method = "lm", formula = y ~ x, se = FALSE, linewidth = 1.2) +
    
    # 散点图
    geom_jitter(aes(fill = Region), 
                color = "black",
                shape = 21,
                size = 3,
                stroke = 0.3,
                width = 0.2,
                alpha = 0.8) +
    
    # 配色
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1") +
    
    # 坐标轴
    scale_x_continuous(breaks = years_sorted) +
    
    # 标签
    labs(
      title = title_str,
      x = "年份",
      y = y_label,
      color = "区域",
      fill = "区域"
    ) +
    
    # 主题
    theme_academic() +
    
    # 字体与布局微调 (针对保存图片优化字号和对齐)
    theme(
      text = element_text(family = my_font_family),
      
      # --- 修改点：标题居中 (hjust=0.5) 且 增大字号 (size=18) ---
      plot.title = element_text(family = my_font_family, face = "bold", size = 18, hjust = 0.5),
      
      # --- 修改点：增大坐标轴标题和刻度数字字号 ---
      axis.title = element_text(family = my_font_family, size = 14),
      axis.text = element_text(family = my_font_family, size = 12),
      
      # --- 修改点：增大图例文字字号 ---
      legend.text = element_text(family = my_font_family, size = 12),
      legend.title = element_text(family = my_font_family, size = 13),
      
      legend.position = c(0.01, 0.99),
      legend.justification = c(0, 1),
      legend.background = element_rect(fill = "white", color = "black", linewidth = 0.2),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank()
    )
}

# ==================== 生成两幅图 ====================

# 图 1: 原始 FDI
p1 <- create_fdi_plot(plot_df, "FDI", "FDI 随时间变化（分区域）", "FDI")

# 图 2: Log(FDI)
p2 <- create_fdi_plot(plot_df, "log_FDI", "Log(FDI) 随时间变化（分区域）", "Log(FDI)")

# ==================== 输出显示与保存 ====================

# 1. 检查并创建 Figure 文件夹
output_dir <- "Figure"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat(sprintf("已创建保存目录: %s/\n", output_dir))
}

# 2. 依次打印并保存图表
# 注意：在 RStudio 中，后一张图会覆盖前一张图，请使用 "Plots" 面板的左右箭头查看历史图片

# --- 处理图 1 ---
cat("\n正在绘制并保存图 1: 原始 FDI ...\n")
print(p1)
ggsave(file.path(output_dir, "FDI 随时间变化（分区域）.png"), p1, width = 10, height = 6, dpi = 300)
cat(sprintf("-> 图 1 已保存至: %s/FDI_Origin.png\n", output_dir))

# --- 处理图 2 ---
cat("\n正在绘制并保存图 2: Log(FDI) ...\n")
print(p2)
ggsave(file.path(output_dir, "Log(FDI) 随时间变化（分区域）.png"), p2, width = 10, height = 6, dpi = 300)
cat(sprintf("-> 图 2 已保存至: %s/FDI_Log.png\n", output_dir))