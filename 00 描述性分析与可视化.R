setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01 数据处理.R", encoding = "UTF-8")
color_original <- "#AD0B08" # 原始分布颜色（深红）
color_log <- "#237B9F" # 对数分布颜色（深蓝）


# ===========  单变量分布对比  =====================

# 定义连续变量
vars <- c("FDI", "PATENT", "TER_GDP")

# 批量生成所有变量的原始 vs 对数对比图（网格展示）
plot_list <- list()
for (i in seq_along(vars)) {
    var <- vars[i]

    # 原始分布（使用 df）
    p_left <- ggplot(df, aes(x = .data[[var]])) +
        geom_histogram(aes(y = after_stat(density)),
            bins = 30,
            fill = color_original, alpha = 0.6, color = color_original
        ) +
        geom_density(color = "#2c3e50", linewidth = 0.6) +
        labs(x = var, y = "Density") +
        theme_academic() +
        theme(axis.title = element_text(size = 9))

    # 对数分布（直接对 df 中的变量取 log）
    p_right <- ggplot(df, aes(x = log(.data[[var]]))) +
        geom_histogram(aes(y = after_stat(density)),
            bins = 30,
            fill = color_log, alpha = 0.6, color = color_log
        ) +
        geom_density(color = "#2c3e50", linewidth = 0.6) +
        labs(x = paste0("log(", var, ")"), y = "Density") +
        theme_academic() +
        theme(axis.title = element_text(size = 9))

    plot_list[[2 * i - 1]] <- p_left
    plot_list[[2 * i]] <- p_right
}

# 展示所有变量（6行x4列，或根据需要调整）
p_dist <- do.call(grid.arrange, c(plot_list, ncol = 2))

# 保存分布对比图
ggsave("Figure/1.1 变量分布直方图对比.png", plot = p_dist, width = 8, height = 8, dpi = 300)


# ============= 地图：2021年各省FDI分布 ========================

library(sf)
library(stringr)

# 准备数据：筛选2021年各省FDI
df_2021_FDI <- df %>%
    filter(Year == "2021") %>%
    select(Province, FDI) %>%
    mutate(Province = as.character(Province))

# 获取中国地图数据（含南海诸岛）
map_url <- "https://geo.datav.aliyun.com/areas_v3/bound/100000_full.json"
china_map <- read_sf(map_url)

# 获取九段线数据
nine_dash_url <- "https://geo.datav.aliyun.com/areas_v3/bound/100000.json"
nine_dash <- tryCatch(read_sf(nine_dash_url), error = function(e) NULL)

# 数据清洗与连接
china_map_cleaned <- china_map %>%
    mutate(short_name = name %>%
        str_remove("省|市|维吾尔自治区|回族自治区|壮族自治区|特别行政区|自治区")) %>%
    filter(!is.na(short_name))

map_data_joined <- china_map_cleaned %>%
    left_join(df_2021_FDI, by = c("short_name" = "Province"))

# 绘制地图
ggplot() +
    # 绑定省份数据
    geom_sf(
        data = map_data_joined,
        aes(fill = FDI),
        color = "white",
        linewidth = 0.2
    ) +

    # 添加国界线轮廓
    {
        if (!is.null(nine_dash)) geom_sf(data = nine_dash, fill = NA, color = "grey30", linewidth = 0.3)
    } +

  
    # 配色
    scale_fill_gradientn(
      # 颜色渐变
      colors = c("#F5F5F5", "#DB7093", "#6f63cb", "#483D8B"),
      # colors = c("#FCFDBF", "#FEC98D", "#FD9567", "#F1605D", "#B73779", "#721F81", "#2C115F"),
      
      na.value = "#EEEEEE",
      name = "FDI",
  
    ) +

    # Lambert等角圆锥投影（中国官方地图常用）
    coord_sf(
        crs = "+proj=lcc +lat_1=25 +lat_2=47 +lat_0=35 +lon_0=105 +datum=WGS84"
    ) +

    # 学术风格主题（基于 theme_academic 微调）
    theme_academic() +
    theme(
        axis.text = element_text(size = 8, color = "grey40"),
        axis.ticks = element_line(color = "grey40", linewidth = 0.3),
        axis.title = element_blank(),
        legend.position.inside = c(0.13, 0.25),
        legend.position = "inside",
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.2, "cm")
    )

# 保存地图
ggsave("Figure/1.2 2021年分省FDI热力地图.png", width = 8, height = 6, dpi = 300)
    

# ===========  自变量相关性   ===================

# 计算相关系数矩阵
cor_vars <- c("FDI", "GDP", "PGDP", "TER_GDP", "APC", "WAGE", "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns")
cor_mat <- cor(df[, cor_vars], use = "complete.obs")

# 转换为长格式
cor_long <- cor_mat %>%
    as.data.frame() %>%
    mutate(Var1 = rownames(cor_mat)) %>%
    pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation")

# 设置因子顺序
cor_long$Var1 <- factor(cor_long$Var1, levels = cor_vars)
cor_long$Var2 <- factor(cor_long$Var2, levels = rev(cor_vars))

# 绑制相关系数热图
p_cor <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 0.5) +
    
    # 添加相关系数文字
    geom_text(aes(label = sprintf("%.2f", Correlation)), 
              color = "black", size = 2.8, family = "serif") +
    
    # 配色：蓝-白-红渐变
    scale_fill_gradient2(
        low = color_log,
        mid = "white",
        high = color_original,
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlation"
    ) +
    
    # 学术风格主题
    theme_academic() +
    theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
        axis.text.y = element_text(size = 9),
        axis.title = element_blank(),
        legend.position.inside = c(1.15, 0.5),
        legend.position = "inside",
        legend.key.width = unit(0.4, "cm"),
        legend.key.height = unit(1.2, "cm")
    ) +
    
    coord_fixed()

print(p_cor)

# 保存相关系数图
ggsave("Figure/1.3 相关系数矩阵.png", plot = p_cor, width = 9, height = 6, dpi = 300)


# ===========  散点图矩阵   ===================

library(GGally)
df_raw$Region <- factor(df_raw$Region, levels = c("西部", "东北", "中部", "东部"))
cols_npg <- c("#54a377ff", "#DB7093", "#6f63cb", "#b6b859ff")
p_pairs <- ggpairs(df_raw, 
        columns = c("FDI","STHC", "TRANS", "PGDP", "TER_GDP","WAGE", "Region"), 
        mapping = aes(color = Region, fill = Region), 
        lower = list(continuous = wrap("points", shape = 21, size = 2, stroke = 0.2, alpha = 0.7)),
        diag = list(continuous = wrap("densityDiag", alpha = 0.7)),
        upper = list(combo = wrap("box_no_facet", fill = NA,lwd = 0.8, outlier.size = 1))
) +
  theme_academic() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  scale_fill_manual(values = cols_npg) +
  scale_color_manual(values = cols_npg)

ggsave("Figure/1.4 散点图矩阵.png", plot = p_pairs, width = 13, height = 10, dpi = 300)


# # ===========  热图：2021年各省份指标  ===================
# 
# library(pheatmap)
# 
# # 准备2021年数据矩阵
# df_2021_heat <- df %>%
#     filter(Year == "2021") %>%
#     select(Province, GDP, PGDP, TER_GDP, APC, WAGE, PATENT, OPEN, TRANS, RD, STHC, STHC_ns)
# 
# mat <- df_2021_heat %>%
#     select(-Province) %>%
#     as.matrix()
# rownames(mat) <- df_2021_heat$Province
# 
# # 标准化矩阵
# mat_scaled <- scale(mat)
# # 转换为长格式用于ggplot2
# df_heat_long <- mat_scaled %>%
#     as.data.frame() %>%
#     mutate(Province = rownames(mat_scaled)) %>%
#     pivot_longer(-Province, names_to = "Variable", values_to = "Value")
# 
# # 聚类排序
# hc_row <- hclust(dist(mat_scaled))
# hc_col <- hclust(dist(t(mat_scaled)))
# df_heat_long$Province <- factor(df_heat_long$Province, levels = rownames(mat_scaled)[hc_row$order])
# df_heat_long$Variable <- factor(df_heat_long$Variable, levels = colnames(mat_scaled)[hc_col$order])
# 
# # 绘制热图（ggplot2 学术风格）
# p_heat <- ggplot(df_heat_long, aes(x = Variable, y = Province, fill = Value)) +
#     geom_tile(color = "white", linewidth = 0.3) +
# 
#     # 配色：蓝-白-红渐变
#     scale_fill_gradient2(
#         low = color_log,
#         mid = "white",
#         high = color_original,
#         midpoint = 0,
#         name = "Z-score"
#     ) +
# 
#     # 学术风格主题
#     theme_academic() +
#     theme(
#         axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
#         axis.text.y = element_text(size = 8),
#         axis.title = element_blank(),
#         legend.position.inside = c(1.12, 0.5),
#         legend.position = "inside",
#         legend.key.width = unit(0.4, "cm"),
#         legend.key.height = unit(1.5, "cm")
#     ) +
# 
#     coord_fixed(ratio = 0.4)
# 
# print(p_heat)
# 
# # 保存热图
# ggsave("Figure/1.X heatmap_2021.png", plot = p_heat, width = 10, height = 12, dpi = 300)
