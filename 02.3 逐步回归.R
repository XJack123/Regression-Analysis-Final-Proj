# ==============================================================================
# 脚本名称: 逐步回归分析.R
# 功能描述: 基于 AIC、BIC 和 F检验(P值) 三种准则进行逐步回归变量筛选
#           并对比各模型的系数 (Coefficients) 和 共线性 (VIF)
# ==============================================================================

# 1. 环境设置与包加载
rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
if (file.exists("01 数据处理.R")) source("01 数据处理.R", encoding = "UTF-8")

# 加载必要的包
packages <- c("readxl", "dplyr", "lmtest", "car", "ggplot2", "olsrr", "tibble")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# =============================== 2. 数据读取与预处理 ==========================
# (严格保持与之前的逻辑一致)
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

# =============================== 辅助函数 =====================================
# 安全获取 VIF 的函数 (处理因子变量产生的矩阵输出)
get_safe_vif <- function(model) {
  if(length(coef(model)) < 2) return(NA)
  tryCatch({
    v <- vif(model)
    # 如果是矩阵(含因子变量的GVIF)，取最后一列(GVIF^(1/(2*Df)))作为参考指标
    if(is.matrix(v)) {
      val <- v[, ncol(v)] 
    } else {
      val <- v
    }
    return(val)
  }, error = function(e) return(NA))
}

# =============================== 3. 建立全模型 ================================
cat("\n================ 1. 全模型 (Full Model) ================\n")
full_formula <- log_FDI ~ . - Year - Province
full_model <- lm(full_formula, data = df_log)

print(summary(full_model))
cat("AIC:", AIC(full_model), "| BIC:", BIC(full_model), "\n")

cat("\n[全模型 VIF]:\n")
vif_full <- get_safe_vif(full_model)
print(vif_full)

# =============================== 4. 逐步回归筛选 ==============================

# --- 方法 1: 基于 AIC 准则 ---
cat("\n\n================ 2. 逐步回归 (AIC准则) ================\n")
step_aic_model <- step(full_model, direction = "both", trace = 0) 

print(summary(step_aic_model))
cat("AIC:", AIC(step_aic_model), "\n")

cat("\n[AIC模型 VIF]:\n")
vif_aic <- get_safe_vif(step_aic_model)
print(vif_aic)


# --- 方法 2: 基于 BIC 准则 ---
cat("\n\n================ 3. 逐步回归 (BIC准则) ================\n")
n_obs <- nrow(df_log)
step_bic_model <- step(full_model, direction = "both", k = log(n_obs), trace = 0)

print(summary(step_bic_model))
cat("BIC:", BIC(step_bic_model), "\n")

cat("\n[BIC模型 VIF]:\n")
vif_bic <- get_safe_vif(step_bic_model)
print(vif_bic)


# --- 方法 3: 基于 F检验/P值 准则 ---
cat("\n\n================ 4. 逐步回归 (F检验/P值准则) ================\n")
cat("准则: 进入 P < 0.05, 剔除 P > 0.10\n")
step_f_res <- ols_step_both_p(full_model, pent = 0.05, prem = 0.10, details = FALSE)
step_f_model <- step_f_res$model

print(summary(step_f_model))

cat("\n[F检验模型 VIF]:\n")
vif_f <- get_safe_vif(step_f_model)
print(vif_f)


# =============================== 5. 结果汇总与对比 ============================

# 定义模型列表
models_list <- list(
  "Full Model" = full_model,
  "Stepwise (AIC)" = step_aic_model,
  "Stepwise (BIC)" = step_bic_model,
  "Stepwise (F-test)" = step_f_model
)

# --- (1) 拟合效果对比 ---
cat("\n\n################################################################\n")
cat("### A. 拟合效果对比 (Model Fit Comparison) ###\n")
cat("################################################################\n")

fit_comparison <- data.frame(
  Model = names(models_list),
  R2 = sapply(models_list, function(m) summary(m)$r.squared),
  Adj_R2 = sapply(models_list, function(m) summary(m)$adj.r.squared),
  AIC = sapply(models_list, AIC),
  BIC = sapply(models_list, BIC),
  Num_Vars = sapply(models_list, function(m) length(coef(m)) - 1)
)
fit_comparison$R2 <- round(fit_comparison$R2, 4)
fit_comparison$Adj_R2 <- round(fit_comparison$Adj_R2, 4)
fit_comparison$AIC <- round(fit_comparison$AIC, 2)
fit_comparison$BIC <- round(fit_comparison$BIC, 2)

print(fit_comparison)


# --- (2) 系数对比 (Coefficients Comparison) ---
cat("\n\n################################################################\n")
cat("### B. 回归系数对比 (Coefficients Comparison) ###\n")
cat("################################################################\n")

# 提取系数并合并的函数
extract_coefs <- function(model_name, model) {
  coefs <- coef(model)
  data.frame(Variable = names(coefs), Coefficient = as.numeric(coefs)) %>%
    rename(!!model_name := Coefficient)
}

# 循环合并所有模型的系数
coef_df <- extract_coefs(names(models_list)[1], models_list[[1]])
for (i in 2:length(models_list)) {
  coef_df <- full_join(coef_df, extract_coefs(names(models_list)[i], models_list[[i]]), by = "Variable")
}

# 格式化并打印 (保留4位小数，NA显示为空白以便阅读)
print(coef_df %>% mutate(across(where(is.numeric), ~ round(., 4))), row.names = FALSE)


# --- (3) VIF 对比 (VIF Comparison) ---
cat("\n\n################################################################\n")
cat("### C. VIF 对比 (VIF Comparison) ###\n")
cat("################################################################\n")

# 提取 VIF 并合并的函数
extract_vifs_df <- function(model_name, model) {
  vifs <- get_safe_vif(model)
  # 如果 VIF 计算失败或没有变量，返回空 DF
  if (all(is.na(vifs))) return(data.frame(Variable = character(), Value = numeric()))
  
  # 处理 VIF 名称
  vif_names <- names(vifs)
  if (is.null(vif_names)) vif_names <- rownames(vifs) # 如果是矩阵可能在 rownames
  
  data.frame(Variable = vif_names, Value = as.numeric(vifs)) %>%
    rename(!!model_name := Value)
}

# 循环合并所有模型的 VIF
vif_df <- extract_vifs_df(names(models_list)[1], models_list[[1]])
for (i in 2:length(models_list)) {
  # 注意：某些模型可能因为变量太少没有 VIF，full_join 会处理这种情况
  next_vif <- extract_vifs_df(names(models_list)[i], models_list[[i]])
  if (nrow(next_vif) > 0) {
    if (nrow(vif_df) == 0) {
      vif_df <- next_vif
    } else {
      vif_df <- full_join(vif_df, next_vif, by = "Variable")
    }
  }
}

# 格式化并打印
if (nrow(vif_df) > 0) {
  print(vif_df %>% mutate(across(where(is.numeric), ~ round(., 4))), row.names = FALSE)
  cat("\n注: 对于因子变量(如Region)，展示的是调整后的广义VIF (GVIF^(1/(2*Df)))。\n")
} else {
  cat("无法计算或对比 VIF (可能变量过少)。\n")
}