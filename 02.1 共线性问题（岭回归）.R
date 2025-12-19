setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01 数据处理.R", encoding = "UTF-8")
if (!require(glmnet)) install.packages("glmnet"); library(glmnet)

# ============ 1. 数据准备 (合并区域/Log转换/去异常) ============
df <- df %>%
  mutate(Region = relevel(as.factor(if_else(Region %in% c("中部", "东北"), "中部及东北", as.character(Region))), ref = "中部及东北"))

cat("\n[区域分布]", table(df$Region), "\n")

# 批量 Log 转换并筛选数据
log_vars <- c("FDI", "GDP", "PGDP", "APC", "WAGE", "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns")
df_log <- df %>%
  mutate(across(all_of(log_vars), log, .names = "log_{.col}")) %>%
  select(Year, Province, Region, TER_GDP, starts_with("log_")) %>%
  filter(!(Year == "2021" & Province == "海南")) # 剔除异常值

# ============ 2. OLS 模型与 VIF 检测 ============
cat("\n=== OLS 模型基准 ===\n")
model_ols <- lm(log_FDI ~ . - Year - Province, data = df_log)
summary(model_ols)

# --- 使用 01 文件定义的函数进行 VIF 检测 (仅展示) ---
cat("\n--- VIF 多重共线性检验 (基于 01 数据处理.R) ---\n")
# 这里 test_vif 会打印 GVIF, Df, Adjusted GVIF
vif_matrix <- test_vif(model_ols)

# ============ 3. 岭回归 (Ridge Regression) ============
cat("\n=== 岭回归分析 ===\n")
x <- model.matrix(log_FDI ~ . - Year - Province, data = df_log)[, -1] # 自变量矩阵
y <- df_log$log_FDI

set.seed(123)
cv <- cv.glmnet(x, y, alpha = 0, nfolds = 10) # 交叉验证
best_lambda <- cv$lambda.min
ridge_model <- glmnet(x, y, alpha = 0, lambda = best_lambda) # 最终模型
cat("最佳 Lambda:", best_lambda, "\n")

# ============ 4. 全能汇总表 (系数/VIF/拟合优度) ============
# (1) 提取系数
coef_ols <- coef(model_ols)
coef_rid <- setNames(as.vector(coef(ridge_model, s = best_lambda)), rownames(coef(ridge_model)))

# (2) 计算 VIF
# --- Ridge VIF (矩阵法计算有效VIF) ---
R <- cor(x);  I <- diag(ncol(x))
inv_k <- solve(R + 2 * best_lambda * I)
vif_rid <- c("(Intercept)" = NA, diag(inv_k %*% R %*% inv_k)) 

# --- OLS VIF (直接使用 test_vif 的 GVIF 列) ---
# 修改说明：直接提取第一列 (GVIF)，确保表格数据与控制台打印的 'GVIF' 列完全一致
# 注意：对于连续变量，GVIF 等于 VIF；对于因子变量，这是 Generalized VIF
ols_vif_vals <- vif_matrix[, 1] 

# 将 VIF 值映射回系数 (包括哑变量)
vif_ols_mapped <- rep(NA, length(coef_ols)) 
names(vif_ols_mapped) <- names(coef_ols)

for (var_name in rownames(vif_matrix)) {
  # 匹配系数名 (例如变量 "Region" 会匹配 "Region中部及东北" 等哑变量)
  idx <- grep(paste0("^", var_name), names(coef_ols))
  if (length(idx) > 0) {
    vif_ols_mapped[idx] <- ols_vif_vals[var_name]
  }
}

# (3) 计算拟合指标 (R2 & RMSE)
pred_rid <- predict(ridge_model, s = best_lambda, newx = x)
r2_rid <- 1 - sum((y - pred_rid)^2) / sum((y - mean(y))^2)
rmse_rid <- sqrt(mean((y - pred_rid)^2))
r2_ols <- summary(model_ols)$r.squared
rmse_ols <- sqrt(mean(residuals(model_ols)^2))

# (4) 整合表格
final_tab <- data.frame(Variable = names(coef_ols), OLS_Coef = coef_ols, stringsAsFactors = F) %>%
  left_join(data.frame(Variable = names(coef_rid), Ridge_Coef = coef_rid), by = "Variable") %>%
  left_join(data.frame(Variable = names(vif_ols_mapped), OLS_调整后GVIF = vif_ols_mapped), by = "Variable") %>% # 列名改为 OLS_调整后GVIF 以示准确
  left_join(data.frame(Variable = names(vif_rid), Ridge_GVIF = vif_rid), by = "Variable")

# (5) 追加统计量行
stats_row <- data.frame(
  Variable = c("R-squared", "RMSE"),
  OLS_Coef = c(r2_ols, rmse_ols), Ridge_Coef = c(r2_rid, rmse_rid),
  OLS_调整后GVIF = NA, Ridge_GVIF = NA
)
final_tab <- rbind(final_tab, stats_row)

# (6) 格式化输出
final_tab[, 2:5] <- round(final_tab[, 2:5], 4)
cat("\n[OLS vs Ridge 完整对比表]\n")
print(final_tab, row.names = FALSE, na.print = "-")