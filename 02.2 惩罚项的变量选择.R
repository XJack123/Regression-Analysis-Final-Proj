# ==============================================================================
# 脚本名称: 变量选择与惩罚回归.R
# 功能描述: 惩罚回归 (Lasso/SCAD/MCP/ElasticNet) 及 后选推断 (Refitted OLS)
# ==============================================================================

# 1. 环境设置与包加载
rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
if (file.exists("01 数据处理.R")) source("01 数据处理.R", encoding = "UTF-8")

packages <- c("readxl", "dplyr", "glmnet", "ncvreg", "caret", "lmtest", "car", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# 2. 数据读取与预处理
if (file.exists("回归分析数据.xlsx")) {
  df <- read_excel("回归分析数据.xlsx")
} else if (file.exists("回归分析数据.xlsx - Sheet1.csv")) {
  df <- read.csv("回归分析数据.xlsx - Sheet1.csv", fileEncoding = "UTF-8")
} else {
  stop("未找到数据文件！")
}

# 因子化与滞后项处理
df$Region <- as.factor(df$Region)
df$Province <- as.factor(df$Province)
df$Year <- as.factor(df$Year)
if ("FDI_lag" %in% colnames(df)) df <- df[, !colnames(df) %in% c("FDI_lag")]

# 区域合并与基准组设定
df <- df %>%
  mutate(Region = as.character(Region)) %>%
  mutate(Region = case_when(
    Region %in% c("中部", "东北") ~ "中部及东北",
    TRUE ~ Region 
  )) %>%
  mutate(Region = as.factor(Region)) %>%
  mutate(Region = relevel(Region, ref = "中部及东北"))

cat("\n[数据处理] 区域已合并，基准组设为 '中部及东北'。\n")

# 对数变换
log_vars <- c("FDI", "GDP", "PGDP", "APC", "WAGE", "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns")
cols_to_keep <- c("Year", "Province", "Region", "TER_GDP")
df_log <- df[, cols_to_keep[cols_to_keep %in% colnames(df)]]

for (var in log_vars) {
  if (var %in% colnames(df)) df_log[[paste0("log_", var)]] <- log(df[[var]])
}

# 剔除特定样本 (海南 2021)
df_log <- df_log[!(df_log$Year == "2021" & df_log$Province == "海南"), ]
cat("[数据处理] 已完成对数变换并剔除海南2021样本。\n")

# 3. 准备矩阵 (X 和 y)
full_formula <- log_FDI ~ . - Year - Province
model_ols <- lm(full_formula, data = df_log)
X <- model.matrix(full_formula, data = df_log)[, -1] 
y <- df_log$log_FDI

n <- nrow(X); p <- ncol(X)
mse_full <- summary(model_ols)$sigma^2

# 结果存储初始化
results_all <- data.frame()
models_detailed <- list()
cols_to_save <- c("Method", "Alpha", "N_Selected", "AIC", "BIC", "Cp", "MSE", "R2")

# 指标计算辅助函数
calc_metrics <- function(method_name, y_true, y_pred, coefs, lambda_val, alpha_val = NA) {
  residuals <- y_true - y_pred
  mse_val <- mean(residuals^2)
  sse_val <- sum(residuals^2)
  n_selected <- sum(as.vector(coefs)[-1] != 0) 
  
  list(
    Method = method_name, Alpha = alpha_val, Lambda = lambda_val, N_Selected = n_selected,
    AIC = n * log(mse_val) + 2 * (n_selected + 1),
    BIC = n * log(mse_val) + log(n) * (n_selected + 1),
    Cp = if(mse_full > 0) (sse_val / mse_full - n + 2 * (n_selected + 1)) else Inf,
    MSE = mse_val, R2 = 1 - mse_val / var(y_true),
    Coefs = coefs
  )
}

# 4. 模型运行
cat("\n正在运行惩罚回归模型...\n")

# (1) Lasso
set.seed(123)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 5)
res_lasso <- calc_metrics("Lasso", y, predict(cv_lasso, X, s="lambda.min"), coef(cv_lasso, s="lambda.min"), cv_lasso$lambda.min, 1)
results_all <- rbind(results_all, as.data.frame(res_lasso[cols_to_save]))
models_detailed[["Lasso"]] <- res_lasso

# (2) Adaptive Lasso
ols_coefs <- coef(model_ols)[-1]
weights <- 1 / (abs(ols_coefs) + 1e-6)
set.seed(123)
cv_ada <- cv.glmnet(X, y, alpha = 1, penalty.factor = weights, nfolds = 5)
res_ada <- calc_metrics("Adaptive Lasso", y, predict(cv_ada, X, s="lambda.min"), coef(cv_ada, s="lambda.min"), cv_ada$lambda.min, 1)
results_all <- rbind(results_all, as.data.frame(res_ada[cols_to_save]))
models_detailed[["Adaptive Lasso"]] <- res_ada

# (3) SCAD
set.seed(123)
cv_scad <- cv.ncvreg(X, y, penalty = "SCAD", nfolds = 5)
res_scad <- calc_metrics("SCAD", y, predict(cv_scad, X, s="lambda.min"), coef(cv_scad, s="lambda.min"), cv_scad$lambda.min)
results_all <- rbind(results_all, as.data.frame(res_scad[cols_to_save]))
models_detailed[["SCAD"]] <- res_scad

# (4) MCP
set.seed(123)
cv_mcp <- cv.ncvreg(X, y, penalty = "MCP", nfolds = 5)
res_mcp <- calc_metrics("MCP", y, predict(cv_mcp, X, s="lambda.min"), coef(cv_mcp, s="lambda.min"), cv_mcp$lambda.min)
results_all <- rbind(results_all, as.data.frame(res_mcp[cols_to_save]))
models_detailed[["MCP"]] <- res_mcp

# (5) Elastic Net
l1_ratios <- c(0.1, 0.5, 0.9, 0.95, 0.99); best_aic_en <- Inf; best_model_en <- NULL
for (alpha in l1_ratios) {
  set.seed(123)
  cv_tmp <- cv.glmnet(X, y, alpha = alpha, nfolds = 5)
  y_pred <- predict(cv_tmp, X, s="lambda.min")
  mse <- mean((y - y_pred)^2); k <- sum(as.vector(coef(cv_tmp, s="lambda.min"))[-1] != 0)
  aic <- n * log(mse) + 2 * (k + 1)
  if (aic < best_aic_en) best_model_en <- list(model=cv_tmp, alpha=alpha, pred=y_pred, coef=coef(cv_tmp, s="lambda.min"), lambda=cv_tmp$lambda.min)
}
res_en <- calc_metrics("Elastic Net", y, best_model_en$pred, best_model_en$coef, best_model_en$lambda, best_model_en$alpha)
results_all <- rbind(results_all, as.data.frame(res_en[cols_to_save]))
models_detailed[["Elastic Net"]] <- res_en

# 5. 结果对比
cat("\n=== 模型对比 (按 AIC 排序) ===\n")
comparison_df <- results_all[order(results_all$AIC), ]
comparison_df$R2 <- round(comparison_df$R2, 4)
print(comparison_df, row.names = FALSE)

best_idx <- which.min(comparison_df$AIC)
best_method <- comparison_df$Method[best_idx]
cat(sprintf("\n综合最佳模型: %s (R2 = %.4f)\n", best_method, comparison_df$R2[best_idx]))

# 6. 最优模型后选推断 (Refitted OLS)
cat("\n=== 步骤 10: Refitted OLS 回归与诊断 ===\n")

# 提取变量
best_coefs <- models_detailed[[best_method]]$Coefs
if (is.list(best_coefs) || is(best_coefs, "sparseMatrix")) best_coefs <- as.vector(best_coefs)
coef_names <- if(best_method %in% c("Lasso","Adaptive Lasso","Elastic Net")) rownames(models_detailed[[best_method]]$Coefs) else names(models_detailed[[best_method]]$Coefs)
selected_vars <- coef_names[which(best_coefs != 0)]
selected_vars <- selected_vars[selected_vars != "(Intercept)"]

if (length(selected_vars) == 0) stop("未选择变量，无法 Refit OLS。")
cat("选择变量:", paste(selected_vars, collapse=", "), "\n")

# 拟合 OLS
refit_data <- data.frame(log_FDI = y, X[, selected_vars, drop=FALSE])
refit_model <- lm(log_FDI ~ ., data = refit_data)
print(summary(refit_model))

# 7. 回归诊断
cat("\n(A) 异方差检验 (BP Test): ")
print(bptest(refit_model))

cat("\n(B) 自相关检验 (DW Test): ")
print(dwtest(refit_model))

cat("\n(C) 共线性检验 (VIF):\n")
test_vif(refit_model)

cat("\n(D) 生成残差图...\n")
p_resid <- ggplot(data.frame(Fitted=fitted(refit_model), Residuals=residuals(refit_model)), aes(x=Fitted, y=Residuals)) +
  geom_hline(yintercept=0, linetype="dashed", color="red") +
  geom_point(alpha=0.6, color="steelblue") +
  geom_smooth(method="loess", se=FALSE, color="darkred", size=0.5) +
  labs(title=paste("Residuals vs Fitted (Refitted", best_method, ")"), x="Fitted Values", y="Residuals") + theme_bw()
print(p_resid)