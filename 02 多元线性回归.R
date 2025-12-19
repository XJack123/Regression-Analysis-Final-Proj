setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01 数据处理.R", encoding = "UTF-8")

# ============ 原模型 和 log-log 模型的建立 ============
model <- lm(FDI ~ . - Year - Province, data = df)
test_heteroscedasticity(model)

# 数据对数转换
log_vars <- c("FDI", "GDP", "PGDP", "APC", "WAGE", "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns")
df_log <- df %>%
  mutate(across(all_of(log_vars), log, .names = "log_{.col}")) %>%
  select(Year, Province, TER_GDP, starts_with("log_"), Region)

# log-log 模型
model_log <- lm(log_FDI ~ . - Year - Province, data = df_log)

# 异方差检验消除
test_heteroscedasticity(model_log)
# 检测异常值
outlier_results <- test_outliers(model_log, df_log)
# 删除 Y 变量的异常值
df_log <- df_log[!(df_log$Year == "2021" & df_log$Province == "海南"), ]

# 重建 log-log 模型
model_log <- lm(log_FDI ~ . - Year - Province, data = df_log)
summary(model_log)
par(mfrow = c(2, 2))
plot(model_log, which = 1:4)

# VIF 检验
test_vif(model_log)
