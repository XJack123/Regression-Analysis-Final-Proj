setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00 数据处理.R", encoding = "UTF-8")

# ---------------------------------------------------------
# 1. 数据导入与预处理
# ---------------------------------------------------------

# 删除某 （year-province）的行
df <- df[!(df$Year == "2021" & df$Province == "海南"), ]

# A. 因变量：仅取对数，不标准化
df_final <- data.frame(log_FDI = log(df$FDI))

# B. 自变量：不取对数，直接标准化 (Z-score)
x_vars <- c("GDP", "PGDP", "TER_GDP", "APC", "WAGE", "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns")
x_scaled <- as.data.frame(scale(df[, x_vars]))

# ---------------------------------------------------------
# 2. 分组 PCA 降维 (基于标准化后的自变量)
# ---------------------------------------------------------

# --- 组1：知识与创新能级 (Innovation_Power) ---
pca_factor <- prcomp(x_scaled[, c("PATENT", "RD", "STHC", "STHC_ns")])
df_final$Innovation_Power <- pca_factor$x[, 1]

# --- 组2：市场联通度 (Market_Scale) ---
pca_market <- prcomp(x_scaled[, c("TRANS", "OPEN")])
df_final$Market_Scale <- pca_market$x[, 1]

# --- 组3：经济质量与市场深度 (Econ_Quality) ---
pca_quality <- prcomp(x_scaled[, c("PGDP", "APC", "GDP")])
df_final$Econ_Quality <- pca_quality$x[, 1]

# ---------------------------------------------------------
# 2.1 主成分分析可视化 - 碎石图
# ---------------------------------------------------------

var_explained_1 <- (pca_factor$sdev[1]^2) / sum(pca_factor$sdev^2)
var_explained_2 <- (pca_market$sdev[1]^2) / sum(pca_market$sdev^2)
var_explained_3 <- (pca_quality$sdev[1]^2) / sum(pca_quality$sdev^2)

# Function to create scree plot
create_scree_plot <- function(pca_obj, color, var_pct, label) {
    var_prop <- (pca_obj$sdev^2 / sum(pca_obj$sdev^2)) * 100
    df_plot <- data.frame(PC = 1:length(var_prop), Variance = var_prop)

    ggplot(df_plot, aes(x = PC, y = Variance)) +
        geom_line(linewidth = 1.2, color = color, alpha = 0.7) +
        geom_point(size = 4, shape = 21, fill = color, color = "white", stroke = 0.8, alpha = 0.9) +
        geom_hline(
            yintercept = 100 / length(var_prop), linetype = "dashed",
            color = "#999999", linewidth = 0.5
        ) +
        annotate("text",
            x = max(df_plot$PC) * 0.7, y = max(df_plot$Variance) * 0.85,
            label = sprintf("PC1: %.1f%%", var_pct),
            size = 3.8, fontface = "bold", family = "serif", color = "#333333"
        ) +
        scale_x_continuous(breaks = 1:length(var_prop)) +
        labs(
            x = "Principal Component",
            y = "Variance Explained (%)",
            caption = label
        ) +
        theme_academic() +
        theme(
            plot.caption = element_text(hjust = 0.5, size = 12, face = "bold"),
            axis.text = element_text(color = "black", size = 10)
        )
}

p1 <- create_scree_plot(pca_factor, "#cc36ae", var_explained_1 * 100, "(a)")
p2 <- create_scree_plot(pca_market, "#6f63cb", var_explained_2 * 100, "(b)")
p3 <- create_scree_plot(pca_quality, "#294cc9", var_explained_3 * 100, "(c)")

combined_plot <- grid.arrange(p1, p2, p3, ncol = 3)

ggsave("Figure/4.1 构造新变量PCA碎石图.png", combined_plot, width = 12, height = 4, dpi = 300)

# --- 组4 & 5：产业结构与成本 (直接使用标准化后的变量) ---
df_final$Ind_Structure <- x_scaled$TER_GDP
df_final$Labor_Cost <- x_scaled$WAGE

# ---------------------------------------------------------
# 3. 用新变量做回归分析
# ---------------------------------------------------------
df_final$Region <- df$Region
df_final$Province <- df$Province
df_final$Year <- df$Year

# 注意：此时因变量是取了对数的 log_FDI
model_p <- lm(log_FDI ~ Innovation_Power + Market_Scale + Econ_Quality + Ind_Structure + Labor_Cost + Region, data = df_final)

# 输出回归摘要
summary(model_p)

png("Figure/4.2 新变量回归诊断图.png", width = 1200, height = 1000, res = 120)
par(mfrow = c(2, 2))
plot(model_p, which = 1:4)
dev.off()

# VIF、异方差、自相关、异常值检验
test_vif(model_p)
test_heteroscedasticity(model_p)
test_outliers(model_p, df_final)
ac_out <- test_autocorrelation(model_p)
ggsave("Figure/4.3 新变量回归的自相关图.png", ac_out$plot, width = 10, height = 8, dpi = 300)


# # --------------------------------
# # 4. 考虑时间效应 引入时间变量
# # --------------------------------
# df_final$Year <- df$Year
# df_final$Region <- df$Region
# df_final$Province <- df$Province
#
# # 建立模型
# model_time <- lm(log_FDI ~ Innovation_Power + Market_Scale + Econ_Quality + Ind_Structure + Labor_Cost + Region + Year, data = df_final)
# summary(model_time)
#
# png("Figure/4.2 新变量+时间回归诊断图.png", width = 1200, height = 1000, res = 120)
# par(mfrow = c(2, 2))
# plot(model_time, which = 1:4)
# dev.off()
