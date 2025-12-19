# ==============================================================================
# 脚本名称: 变量选择与惩罚回归.R
# 功能描述: Python sklearn 惩罚回归代码的 R 语言实现
# 包含方法: Lasso, Adaptive Lasso, SCAD, MCP, Elastic Net
# 新增功能: 最优模型的后选推断 (Refitted OLS) 与回归诊断
# ==============================================================================

# 清空工作空间
rm(list = ls())
source("01 数据处理.R", encoding = "UTF-8")
# 设置工作路径
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}

# =============================== 1. 加载包 ====================================
# 检查并加载所需的包
packages <- c("readxl", "dplyr", "glmnet", "ncvreg", "caret", "lmtest", "car", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

library(readxl)
library(dplyr)
library(glmnet)  # 用于 Lasso, Elastic Net, Adaptive Lasso
library(ncvreg)  # 用于 SCAD, MCP
library(lmtest)  # 用于 BP test, DW test
library(car)     # 用于 VIF
library(ggplot2) # 用于绘图

# =============================== 2. 数据处理与特征工程 ========================
# (复用 01 数据处理.R 的逻辑)
if (file.exists("回归分析数据.xlsx")) {
  df <- read_excel("回归分析数据.xlsx")
} else if (file.exists("回归分析数据.xlsx - Sheet1.csv")) {
  df <- read.csv("回归分析数据.xlsx - Sheet1.csv", fileEncoding = "UTF-8")
} else {
  stop("未找到数据文件！")
}

# 基础因子化
df$Region <- as.factor(df$Region)
df$Province <- as.factor(df$Province)
df$Year <- as.factor(df$Year)

# --- 修改：区域合并与基准组设定 ---
# 需求：将 "中部" 和 "东北" 合并为 "中部及东北" 并设为基准
df <- df %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Region = case_when(
    Region %in% c("中部", "东北") ~ "中部及东北",
    TRUE ~ Region 
  )) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = relevel(Region, ref = "中部及东北"))

cat("\n[数据处理] 已合并区域并设定 '中部及东北' 为基准组。\n")

# --- 对数变换 (Log-Log 模型准备) ---
log_vars <- c(
  "FDI", "GDP", "PGDP", "APC", "WAGE",
  "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns"
)

cols_to_keep <- c("Year", "Province", "Region", "TER_GDP")
df_log <- df[, cols_to_keep[cols_to_keep %in% colnames(df)]]

for (var in log_vars) {
  if (var %in% colnames(df)) {
    df_log[[paste0("log_", var)]] <- log(df[[var]])
  }
}
df_log <- df_log[!(df_log$Year == "2021" & df_log$Province == "海南"), ]

cat("[数据处理] 已完成对数变换 (Log-Log)。\n")

# =============================== 3. 准备矩阵 (X 和 y) =========================
# 建立全模型 OLS 用于获取设计矩阵
full_formula <- log_FDI ~ . - Year - Province
model_ols <- lm(full_formula, data = df_log)

# 提取 X 矩阵 (自动处理因子变量生成虚拟变量)
X <- model.matrix(full_formula, data = df_log)[, -1] 
y <- df_log$log_FDI

n <- nrow(X)
p <- ncol(X)
mse_full <- summary(model_ols)$sigma^2
y_mean <- mean(y)

# 初始化结果列表 (包含 R2)
results_all <- data.frame(
  Method = character(),
  Alpha = numeric(),
  N_Selected = integer(),
  AIC = numeric(),
  BIC = numeric(),
  Cp = numeric(),
  MSE = numeric(),
  R2 = numeric(), # 确保 R2 存在
  stringsAsFactors = FALSE
)

# 定义要保存的列名，确保 R2 被选中
cols_to_save <- c("Method", "Alpha", "N_Selected", "AIC", "BIC", "Cp", "MSE", "R2")

# 存储详细模型结果的列表 (用于后续提取系数)
models_detailed <- list()

# 指标计算函数
calc_metrics <- function(method_name, y_true, y_pred, coefs, lambda_val, alpha_val = NA) {
  residuals <- y_true - y_pred
  mse_val <- mean(residuals^2)
  sse_val <- sum(residuals^2)
  
  # 变量选择数 (非零系数，不含截距)
  n_selected <- sum(as.vector(coefs)[-1] != 0) 
  
  aic_val <- n * log(mse_val) + 2 * (n_selected + 1)
  bic_val <- n * log(mse_val) + log(n) * (n_selected + 1)
  cp_val <- if(mse_full > 0) (sse_val / mse_full - n + 2 * (n_selected + 1)) else Inf
  r2_val <- 1 - mse_val / var(y_true)
  
  return(list(
    Method = method_name,
    Alpha = alpha_val, 
    Lambda = lambda_val,
    N_Selected = n_selected,
    AIC = aic_val,
    BIC = bic_val,
    Cp = cp_val,
    MSE = mse_val,
    R2 = r2_val,
    Coefs = coefs # 保存系数对象
  ))
}

# =============================== 4. Lasso 回归 ================================
cat("\n正在运行 Lasso...\n")
set.seed(123)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 5)
best_lambda_lasso <- cv_lasso$lambda.min
coef_lasso <- coef(cv_lasso, s = "lambda.min")
y_pred_lasso <- predict(cv_lasso, newx = X, s = "lambda.min")

res_lasso <- calc_metrics("Lasso", y, y_pred_lasso, coef_lasso, best_lambda_lasso, alpha_val = 1)
# 修正：显式选择包含 R2 的列
results_all <- rbind(results_all, as.data.frame(res_lasso[cols_to_save]))
models_detailed[["Lasso"]] <- res_lasso

# =========================== 5. 自适应 Lasso ==================================
cat("正在运行 Adaptive Lasso...\n")
ols_coefs <- coef(model_ols)[-1]
weights <- 1 / (abs(ols_coefs) + 1e-6)

set.seed(123)
cv_adalasso <- cv.glmnet(X, y, alpha = 1, penalty.factor = weights, nfolds = 5)
best_lambda_ada <- cv_adalasso$lambda.min
coef_ada <- coef(cv_adalasso, s = "lambda.min")
y_pred_ada <- predict(cv_adalasso, newx = X, s = "lambda.min")

res_ada <- calc_metrics("Adaptive Lasso", y, y_pred_ada, coef_ada, best_lambda_ada, alpha_val = 1)
results_all <- rbind(results_all, as.data.frame(res_ada[cols_to_save]))
models_detailed[["Adaptive Lasso"]] <- res_ada

# =============================== 6. SCAD ======================================
cat("正在运行 SCAD...\n")
set.seed(123)
cv_scad <- cv.ncvreg(X, y, penalty = "SCAD", nfolds = 5)
best_lambda_scad <- cv_scad$lambda.min
coef_scad <- coef(cv_scad, s = "lambda.min")
y_pred_scad <- predict(cv_scad, X, s = "lambda.min")

res_scad <- calc_metrics("SCAD", y, y_pred_scad, coef_scad, best_lambda_scad)
results_all <- rbind(results_all, as.data.frame(res_scad[cols_to_save]))
models_detailed[["SCAD"]] <- res_scad

# =============================== 7. MCP =======================================
cat("正在运行 MCP...\n")
set.seed(123)
cv_mcp <- cv.ncvreg(X, y, penalty = "MCP", nfolds = 5)
best_lambda_mcp <- cv_mcp$lambda.min
coef_mcp <- coef(cv_mcp, s = "lambda.min")
y_pred_mcp <- predict(cv_mcp, X, s = "lambda.min")

res_mcp <- calc_metrics("MCP", y, y_pred_mcp, coef_mcp, best_lambda_mcp)
results_all <- rbind(results_all, as.data.frame(res_mcp[cols_to_save]))
models_detailed[["MCP"]] <- res_mcp

# =========================== 8. 弹性网 (Elastic Net) ==========================
cat("正在运行 Elastic Net...\n")
l1_ratios <- c(0.1, 0.5, 0.9, 0.95, 0.99)
best_aic_en <- Inf
best_model_en <- NULL

for (alpha in l1_ratios) {
  set.seed(123)
  cv_temp <- cv.glmnet(X, y, alpha = alpha, nfolds = 5)
  y_pred_temp <- predict(cv_temp, newx = X, s = "lambda.min")
  mse_temp <- mean((y - y_pred_temp)^2)
  coef_temp <- coef(cv_temp, s = "lambda.min")
  k_temp <- sum(as.vector(coef_temp)[-1] != 0)
  aic_temp <- n * log(mse_temp) + 2 * (k_temp + 1)
  
  if (aic_temp < best_aic_en) {
    best_aic_en <- aic_temp
    best_model_en <- list(model = cv_temp, alpha = alpha, pred = y_pred_temp, coef = coef_temp, lambda = cv_temp$lambda.min)
  }
}

res_en <- calc_metrics("Elastic Net", y, best_model_en$pred, best_model_en$coef, best_model_en$lambda, alpha_val = best_model_en$alpha)
results_all <- rbind(results_all, as.data.frame(res_en[cols_to_save]))
models_detailed[["Elastic Net"]] <- res_en

# =============================== 9. 结果对比 ==================================
cat("\n", paste0(rep("=", 80), collapse = ""), "\n", sep="")
cat("所有方法对比（按AIC排序）：\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

comparison_df <- results_all[order(results_all$AIC), ]
# 格式化 R2 显示保留4位小数
comparison_df$R2 <- round(comparison_df$R2, 4)
print(comparison_df, row.names = FALSE)

best_aic_idx <- which.min(comparison_df$AIC)
best_method_name <- comparison_df$Method[best_aic_idx]

cat(sprintf("\n综合最佳模型（基于AIC）: %s (R2 = %.4f)\n", best_method_name, comparison_df$R2[best_aic_idx]))

# ================= 10. 最优模型的后选推断与诊断 (Refitted OLS) =================
cat("\n", paste0(rep("=", 80), collapse = ""), "\n", sep="")
cat("步骤 10：最优模型的 Refitted OLS 回归与诊断\n")
cat("逻辑: 提取最优模型选出的变量，重新拟合 OLS 以获得 Summary 表和诊断指标。\n")
cat(paste0(rep("=", 80), collapse = ""), "\n")

# 1. 提取非零系数
best_model_res <- models_detailed[[best_method_name]]
best_coefs <- best_model_res$Coefs

# 将稀疏矩阵转换为普通向量
if (is.list(best_coefs) || is(best_coefs, "sparseMatrix")) {
  best_coefs <- as.vector(best_coefs)
  # 注意：glmnet/ncvreg 的 coef names 可能在 dimnames 中
  if (best_method_name %in% c("Lasso", "Adaptive Lasso", "Elastic Net")) {
    coef_names <- rownames(best_model_res$Coefs)
  } else {
    coef_names <- names(best_model_res$Coefs)
  }
} else {
  coef_names <- names(best_coefs)
}

# 找出非零变量名 (排除 Intercept)
nonzero_indices <- which(best_coefs != 0)
selected_vars <- coef_names[nonzero_indices]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]

if (length(selected_vars) == 0) {
  stop("最优模型未选择任何变量，无法进行 OLS 回归。")
}

cat("最优模型选择的变量:\n")
print(selected_vars)

# 2. 构建 Refitted OLS 数据集
# 直接从 X 矩阵中提取对应的列
X_selected <- X[, selected_vars, drop = FALSE]
refit_data <- data.frame(log_FDI = y, X_selected)

# 3. 拟合 OLS 模型
refit_model <- lm(log_FDI ~ ., data = refit_data)

cat("\n--- Refitted OLS Summary 表 ---\n")
print(summary(refit_model))

# 4. 回归诊断

# (A) 异方差检验 (Breusch-Pagan)
cat("\n--- 1. 异方差检验 (Breusch-Pagan Test) ---\n")
bp_res <- bptest(refit_model)
print(bp_res)
if(bp_res$p.value < 0.05) cat("-> 存在显著异方差 (P < 0.05)\n") else cat("-> 未检测到显著异方差\n")

# (B) 自相关检验 (Durbin-Watson)
cat("\n--- 2. 自相关检验 (Durbin-Watson Test) ---\n")
dw_res <- dwtest(refit_model)
print(dw_res)

# (C) 共线性检验 (VIF)
test_vif(refit_model)

# (D) 绘图：残差图
cat("\n--- 4. 生成诊断图 (Residuals vs Fitted) ---\n")
# 创建绘图数据
diag_data <- data.frame(
  Fitted = fitted(refit_model),
  Residuals = residuals(refit_model)
)

# 使用 ggplot2 绘制
p_resid <- ggplot(diag_data, aes(x = Fitted, y = Residuals)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "loess", se = FALSE, color = "darkred", size = 0.5) +
  labs(
    title = paste("Residuals vs Fitted (Refitted", best_method_name, ")"),
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_bw()

print(p_resid)
cat("诊断图已生成。\n")