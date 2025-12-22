
library(readxl)
library(dplyr)
library(FactoMineR) # 用于 PCA
library(fixest)     # 用于高效的聚类标准误回归 (feols)
library(lmtest)     # 用于异方差检验
library(sandwich)   # 用于稳健方差估计
library(ggplot2)
library(patchwork)  # 用于合并图表

# 导入数据
df <- read_excel("/Users/xujialing/Desktop/回归分析数据.xlsx")

# 数据清洗
df <- df %>%
  mutate(across(c(FDI, GDP, PGDP, TER_GDP, APC, WAGE, RD, OPEN, TRANS, STHC, STHC_ns, PATENT, Year), 
                ~ as.numeric(as.character(.)))) %>%
  mutate(log_FDI = log(FDI + 1),
         Province = trimws(Province)) %>%
  # 剔除海南 2021
  filter(!(Province == "海南" & Year == 2021)) %>%
  mutate(prov_id = as.factor(Province)) %>%
  arrange(prov_id, Year)

# --- PCA 指标生成 ---
# 注意：Stata 的 predict score 默认提取第一主成分
get_pca_score <- function(data, vars) {
  res_pca <- PCA(data[, vars], ncp = 1, graph = FALSE)
  return(res_pca$ind$coord[, 1])
}

df$Innovation_Power <- get_pca_score(df, c("PATENT", "RD", "STHC", "STHC_ns"))
df$Market_Scale     <- get_pca_score(df, c("TRANS", "OPEN"))
df$Econ_Quality     <- get_pca_score(df, c("PGDP", "APC", "GDP"))
df$Ind_Structure    <- as.vector(scale(df$TER_GDP))
df$Labor_Cost       <- as.vector(scale(df$WAGE))

# 区域定义
df <- df %>%
  mutate(
    East = as.numeric(Region == "东部"),
    West = as.numeric(Region == "西部"),
    # 基准组：中部或东北 (reg_group=3)
    reg_group = case_when(
      Region == "东部" ~ 1,
      Region == "西部" ~ 2,
      Region %in% c("中部", "东北") ~ 3
    )
  )

# --- 生成一阶差分项 ---
# 使用 group_by 确保在省份内部进行差分
df <- df %>%
  group_by(prov_id) %>%
  mutate(
    d_log_FDI    = log_FDI - lag(log_FDI),
    d_Innovation = Innovation_Power - lag(Innovation_Power),
    d_Market     = Market_Scale - lag(Market_Scale),
    d_Econ       = Econ_Quality - lag(Econ_Quality),
    d_Ind        = Ind_Structure - lag(Ind_Structure),
    d_Labor      = Labor_Cost - lag(Labor_Cost)
  ) %>%
  ungroup() %>%
  filter(!is.na(d_log_FDI)) # 剔除差分产生的缺失值

# =========================================================
# 2. 统一计算 WLS 权重
# =========================================================
# 定义右侧变量（不含虚拟变量的部分）
rhs_vars <- c("d_Innovation", "d_Market", "d_Econ", "d_Ind", "d_Labor", "as.factor(Year)")

# 全样本权重 w_all
formula_all <- as.formula(paste("d_log_FDI ~", paste(c(rhs_vars, "East", "West"), collapse = " + ")))
reg_all <- lm(formula_all, data = df)
df$res_all <- residuals(reg_all)
df$w_all <- 1 / (df$res_all^2)
df$w_all[is.na(df$w_all) | df$w_all <= 0] <- 0.0001

# 各分组独立权重
df$w_group <- NA
for (i in 1:3) {
  sub_data <- df[df$reg_group == i, ]
  formula_sub <- as.formula(paste("d_log_FDI ~", paste(rhs_vars, collapse = " + ")))
  m_sub <- lm(formula_sub, data = sub_data)
  res_sub <- residuals(m_sub)
  df$w_group[df$reg_group == i] <- 1 / (res_sub^2)
}
df$w_group[is.na(df$w_group) | df$w_group <= 0] <- 0.0001

# =========================================================
# 3. 核心回归与诊断程序
# =========================================================

run_cluster_wls <- function(data, mname, weight_var, use_dummy = FALSE) {
  
  # 准备变量
  vars <- if(use_dummy) c(rhs_vars, "East", "West") else rhs_vars
  formula <- as.formula(paste("d_log_FDI ~", paste(vars, collapse = " + ")))
  
  # A. 临时回归用于诊断 (普通权重 lm)
  m_diag <- lm(formula, data = data, weights = data[[weight_var]])
  
  # 1. BP 检验
  bp_test <- bptest(m_diag)
  bp_p <- bp_test$p.value
  
  # 2. 手动计算 DW (考虑面板结构)
  res <- residuals(m_diag)
  # 注意：差分后数据已按省份/年份排序
  # 此处简化计算，真实 DW 在面板中应注意跨省份断点
  diff_res <- diff(res)
  dw_val <- sum(diff_res^2) / sum(res^2)
  
  # 3. Spearman 秩相关
  fitted_vals <- fitted(m_diag)
  sp_test <- cor.test(res, fitted_vals, method = "spearman", exact = FALSE)
  sp_p <- sp_test$p.value
  
  # 4. 绘图
  # 异方差诊断图
  p1 <- ggplot(data.frame(res, fitted_vals), aes(x = fitted_vals, y = res)) +
    geom_point(color = "grey", alpha = 0.5) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(title = paste(mname, ": 残差-拟合值图"), x = "拟合值", y = "残差") +
    theme_minimal()
  
  # 自相关诊断图
  res_df <- data.frame(r_t = res[2:length(res)], r_t_1 = res[1:(length(res)-1)])
  p2 <- ggplot(res_df, aes(x = r_t_1, y = r_t)) +
    geom_point(color = "blue", alpha = 0.3) +
    geom_smooth(method = "lm", color = "red", se = FALSE) +
    labs(title = paste(mname, ": 残差自相关图"), x = "残差(t-1)", y = "残差(t)") +
    theme_minimal()
  
  # B. 正式回归：带聚类标准误的 WLS (使用 fixest 包)
  # weights 在 fixest 中直接使用
  m_final <- feols(formula, data = data, weights = data[[weight_var]], cluster = ~prov_id)
  
  return(list(model = m_final, bp_p = bp_p, dw_val = dw_val, sp_p = sp_p, plots = list(p1, p2)))
}

# =========================================================
# 4. 运行模型并展示
# =========================================================

res_full  <- run_cluster_wls(df, "Full", "w_all", use_dummy = TRUE)
res_east  <- run_cluster_wls(df[df$reg_group == 1, ], "East", "w_group")
res_west  <- run_cluster_wls(df[df$reg_group == 2, ], "West", "w_group")
res_cenne <- run_cluster_wls(df[df$reg_group == 3, ], "CenNE", "w_group")

# 5. 展示结果汇总
library(modelsummary) # 用于美化表格

models <- list(
  "Full Sample" = res_full$model,
  "East" = res_east$model,
  "West" = res_west$model,
  "Central/NE" = res_cenne$model
)

# 打印诊断指标汇总
cat("\n=== 模型诊断统计量汇总 ===\n")
diag_summary <- data.frame(
  Model = c("Full", "East", "West", "CenNE"),
  DW = c(res_full$dw_val, res_east$dw_val, res_west$dw_val, res_cenne$dw_val),
  BP_p = c(res_full$bp_p, res_east$bp_p, res_west$bp_p, res_cenne$bp_p),
  Spearman_p = c(res_full$sp_p, res_east$sp_p, res_west$sp_p, res_cenne$sp_p)
)
print(diag_summary)

# 6. 绘图展示
# 全样本诊断图
(res_full$plots[[1]] + res_full$plots[[2]]) + plot_annotation(title = "图1：全样本模型诊断图")

# 分地区诊断图 (3x2 布局)
(res_east$plots[[1]] + res_east$plots[[2]]) /
  (res_west$plots[[1]] + res_west$plots[[2]]) /
  (res_cenne$plots[[1]] + res_cenne$plots[[2]]) + 
  plot_annotation(title = "图2：分地区回归模型诊断图", 
                  subtitle = "顺序：东部、西部、中部+东北")