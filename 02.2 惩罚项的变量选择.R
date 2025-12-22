# ==============================================================================
# 脚本名称: 惩罚回归分析.R
# 功能描述: 使用 glmnet 和 ncvreg 进行惩罚回归分析
#           包括 Ridge、Lasso、Elastic Net、SCAD、MCP
# ==============================================================================

# 1. 环境设置
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("00 数据处理.R", encoding = "UTF-8")

# 2. 加载必要的包
if (!require(glmnet)) install.packages("glmnet")
if (!require(ncvreg)) install.packages("ncvreg")
library(glmnet)
library(ncvreg)

# 3. 数据准备
invisible(capture.output(source("02 多元线性回归.R", encoding = "UTF-8")))

# 准备矩阵
full_formula <- log_FDI ~ . - Year - Province
X <- model.matrix(full_formula, data = df_log)[, -1]
y <- df_log$log_FDI

cat("\n样本量:", nrow(X), "\n变量数:", ncol(X), "\n")

# ============ Ridge 回归 (α = 0) ============
cat("\n\n========== Ridge 回归 ==========\n")
set.seed(123)
cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = 10)

cat("最优 Lambda:", cv_ridge$lambda.min, "\n")
cat("最优 Lambda (1se):", cv_ridge$lambda.1se, "\n")

# 提取系数
coef_ridge <- coef(cv_ridge, s = "lambda.min")
cat("\nRidge 系数:\n")
print(round(as.matrix(coef_ridge), 4))

# 预测和评估
pred_ridge <- predict(cv_ridge, X, s = "lambda.min")
mse_ridge <- mean((y - pred_ridge)^2)
r2_ridge <- 1 - mse_ridge / var(y)
cat("\nMSE:", round(mse_ridge, 4), "\n")
cat("R²:", round(r2_ridge, 4), "\n")

# ============ Lasso 回归 (α = 1) ============
cat("\n\n========== Lasso 回归 ==========\n")
set.seed(123)
cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)

cat("最优 Lambda:", cv_lasso$lambda.min, "\n")

# 提取系数
coef_lasso <- coef(cv_lasso, s = "lambda.min")
n_selected <- sum(as.vector(coef_lasso)[-1] != 0)
cat("选择变量数:", n_selected, "\n")

cat("\nLasso 系数 (非零):\n")
nonzero_idx <- which(as.vector(coef_lasso) != 0)
print(round(as.matrix(coef_lasso[nonzero_idx, ]), 4))

# 预测和评估
pred_lasso <- predict(cv_lasso, X, s = "lambda.min")
mse_lasso <- mean((y - pred_lasso)^2)
r2_lasso <- 1 - mse_lasso / var(y)
cat("\nMSE:", round(mse_lasso, 4), "\n")
cat("R²:", round(r2_lasso, 4), "\n")

# ============ Elastic Net (寻找最优 α) ============
cat("\n\n========== Elastic Net ==========\n")
alpha_values <- seq(0.1, 1, by = 0.1)
cv_results <- data.frame()

for (alpha in alpha_values) {
    set.seed(123)
    cv_tmp <- cv.glmnet(X, y, alpha = alpha, nfolds = 10)
    cv_results <- rbind(cv_results, data.frame(
        alpha = alpha,
        lambda = cv_tmp$lambda.min,
        mse = min(cv_tmp$cvm)
    ))
}

best_alpha <- cv_results$alpha[which.min(cv_results$mse)]
cat("最优 α:", best_alpha, "\n")

# 使用最优 α 拟合模型
set.seed(123)
cv_enet <- cv.glmnet(X, y, alpha = best_alpha, nfolds = 10)
cat("最优 Lambda:", cv_enet$lambda.min, "\n")

# 提取系数
coef_enet <- coef(cv_enet, s = "lambda.min")
n_selected <- sum(as.vector(coef_enet)[-1] != 0)
cat("选择变量数:", n_selected, "\n")

cat("\nElastic Net 系数 (非零):\n")
nonzero_idx <- which(as.vector(coef_enet) != 0)
print(round(as.matrix(coef_enet[nonzero_idx, ]), 4))

# 预测和评估
pred_enet <- predict(cv_enet, X, s = "lambda.min")
mse_enet <- mean((y - pred_enet)^2)
r2_enet <- 1 - mse_enet / var(y)
cat("\nMSE:", round(mse_enet, 4), "\n")
cat("R²:", round(r2_enet, 4), "\n")

# ============ SCAD ============
cat("\n\n========== SCAD ==========\n")
set.seed(123)
cv_scad <- cv.ncvreg(X, y, penalty = "SCAD", nfolds = 10)

cat("最优 Lambda:", cv_scad$lambda.min, "\n")

# 提取系数
coef_scad <- coef(cv_scad, lambda = cv_scad$lambda.min)
n_selected <- sum(coef_scad[-1] != 0)
cat("选择变量数:", n_selected, "\n")

cat("\nSCAD 系数 (非零):\n")
nonzero_idx <- which(coef_scad != 0)
nonzero_coef <- coef_scad[nonzero_idx]
names(nonzero_coef) <- names(coef_scad)[nonzero_idx]
print(round(nonzero_coef, 4))

# 预测和评估
pred_scad <- predict(cv_scad, X, lambda = cv_scad$lambda.min)
mse_scad <- mean((y - pred_scad)^2)
r2_scad <- 1 - mse_scad / var(y)
cat("\nMSE:", round(mse_scad, 4), "\n")
cat("R²:", round(r2_scad, 4), "\n")

# ============ MCP ============
cat("\n\n========== MCP ==========\n")
set.seed(123)
cv_mcp <- cv.ncvreg(X, y, penalty = "MCP", nfolds = 10)

cat("最优 Lambda:", cv_mcp$lambda.min, "\n")

# 提取系数
coef_mcp <- coef(cv_mcp, lambda = cv_mcp$lambda.min)
n_selected <- sum(coef_mcp[-1] != 0)
cat("选择变量数:", n_selected, "\n")

cat("\nMCP 系数 (非零):\n")
nonzero_idx <- which(coef_mcp != 0)
nonzero_coef <- coef_mcp[nonzero_idx]
names(nonzero_coef) <- names(coef_mcp)[nonzero_idx]
print(round(nonzero_coef, 4))

# 预测和评估
pred_mcp <- predict(cv_mcp, X, lambda = cv_mcp$lambda.min)
mse_mcp <- mean((y - pred_mcp)^2)
r2_mcp <- 1 - mse_mcp / var(y)
cat("\nMSE:", round(mse_mcp, 4), "\n")
cat("R²:", round(r2_mcp, 4), "\n")

# ============ 模型比较 ============
cat("\n\n========== 模型比较 ==========\n")
comparison <- data.frame(
    Model = c("Ridge", "Lasso", "Elastic Net", "SCAD", "MCP"),
    MSE = c(mse_ridge, mse_lasso, mse_enet, mse_scad, mse_mcp),
    R2 = c(r2_ridge, r2_lasso, r2_enet, r2_scad, r2_mcp),
    N_Vars = c(
        ncol(X),
        sum(as.vector(coef_lasso)[-1] != 0),
        sum(as.vector(coef_enet)[-1] != 0),
        sum(coef_scad[-1] != 0),
        sum(coef_mcp[-1] != 0)
    )
)

comparison <- comparison[order(comparison$MSE), ]
print(comparison, row.names = FALSE)

# 可视化交叉验证结果
par(mfrow = c(2, 3))
plot(cv_ridge, main = "Ridge")
plot(cv_lasso, main = "Lasso")
plot(cv_enet, main = "Elastic Net")
plot(cv_scad, main = "SCAD")
plot(cv_mcp, main = "MCP")
par(mfrow = c(1, 1))

cat("\n分析完成！\n")
