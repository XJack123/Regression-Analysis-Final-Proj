setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01 数据处理.R", encoding = "UTF-8")


# ============ 原模型 和 log-log 模型的建立 ============
model <- lm(FDI ~ . - Year - Province, data = df)
test_heteroscedasticity(model)

log_vars <- c(
  "FDI", "GDP", "PGDP", "APC", "WAGE",
  "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns"
)
df_log <- df[, c("Year", "Province", "Region", "TER_GDP")]

df_log$Region <- as.character(df_log$Region)
target_regions <- c("中部", "东北") 
df_log$Region[df_log$Region %in% target_regions] <- "中部及东北"
df_log$Region <- as.factor(df_log$Region)

df_log$Region <- relevel(df_log$Region, ref = "中部及东北")
print("当前的基准组是：")
print(levels(df_log$Region)[1])

print("合并后的区域分布：")
print(table(df_log$Region))


for (var in log_vars) {
  df_log[[paste0("log_", var)]] <- log(df[[var]])
}

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
