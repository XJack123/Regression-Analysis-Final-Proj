<<<<<<< HEAD
# 清空工作空间 并设置文件路径为当前脚本所在目录
rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
getwd()


library(readxl)

df <- read_excel("回归分析数据.xlsx")

colnames(df)

# 定义符合你要求的新列名
colnames(df) <- c(
  "Year", # 年份
  "Province", # 省份
  "FDI", # 外商直接投资
  "GDP", # 国内生产总值
  "PGDP", # 人均GDP
  "TER_GDP", # 第三产业占比
  "APC", # 平均消费倾向
  "WAGE", # 工资
  "PATENT", # 专利申请数
  "IP&OP", # 进出口总额
  "TRANS", # 交通里程
  "R&D", # 研发投入
  "STHC", # 学生人力资本
  "Region" # 区域分组
)

# 将 Region 和 Province 转换为因子型
df$Region <- as.factor(df$Region)
df$Province <- as.factor(df$Province)

# 调整变量顺序
df <- df[, c("Year", "Province", "Region", "FDI", "GDP", "PGDP", "TER_GDP",
             "APC", "WAGE", "PATENT", "IP&OP", "TRANS", "R&D", "STHC")]


summary(df)

# # 批量检查哪些变量需要取对数
# library(moments)
# numeric_vars <- df[, 3:13]
# skew_df <- data.frame(
#   Variable = names(numeric_vars),
#   Skewness = sapply(numeric_vars, skewness, na.rm = TRUE),
#   Decision = ifelse(sapply(numeric_vars, skewness, na.rm = TRUE) > 1,
#     "建议取log", "可不取log"
#   )
# )
# print(skew_df)

# ===========  对数转换  =====================

# 对所有偏度>1的数值变量取对数
log_vars <- c(
  "FDI", "GDP", "PGDP", "TER_GDP", "APC", "WAGE",
  "PATENT", "IP&OP", "TRANS", "R&D", "STHC"
)

# 创建新数据框，只包含分类变量和对数变量
df_log <- df[, c("Year", "Province", "Region")]

# 添加对数变量
for (var in log_vars) {
  df_log[[paste0("log_", var)]] <- log(df[[var]])
}


# # 检查转换后的偏度
# numeric_vars_log <- df_log[, grep("^log_", names(df_log))]
# skew_df_log <- data.frame(
#   Variable = names(numeric_vars_log),
#   Skewness = sapply(numeric_vars_log, skewness, na.rm = TRUE)
# )
# print(skew_df_log)

model <- lm(log_FDI ~ . - Year - Province, data = df_log)

summary(model)

colnames(df_log)
colnames(df)

# 只保留 df 和 df_log
rm(list = setdiff(ls(), c("df", "df_log", "model")))

=======
# 清空工作空间 并设置文件路径为当前脚本所在目录
rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
getwd()


library(readxl)

df <- read_excel("回归分析数据.xlsx")

colnames(df)

# 定义符合你要求的新列名
colnames(df) <- c(
  "Year", # 年份
  "Province", # 省份
  "FDI", # 外商直接投资
  "GDP", # 国内生产总值
  "PGDP", # 人均GDP
  "TER_GDP", # 第三产业占比
  "APC", # 平均消费倾向
  "WAGE", # 工资
  "PATENT", # 专利申请数
  "IP&OP", # 进出口总额
  "TRANS", # 交通里程
  "R&D", # 研发投入
  "STHC", # 学生人力资本
  "Region" # 区域分组
)

# 将 Region 和 Province 转换为因子型
df$Region <- as.factor(df$Region)
df$Province <- as.factor(df$Province)

# 调整变量顺序
df <- df[, c("Year", "Province", "Region", "FDI", "GDP", "PGDP", "TER_GDP",
             "APC", "WAGE", "PATENT", "IP&OP", "TRANS", "R&D", "STHC")]


summary(df)

# # 批量检查哪些变量需要取对数
# library(moments)
# numeric_vars <- df[, 3:13]
# skew_df <- data.frame(
#   Variable = names(numeric_vars),
#   Skewness = sapply(numeric_vars, skewness, na.rm = TRUE),
#   Decision = ifelse(sapply(numeric_vars, skewness, na.rm = TRUE) > 1,
#     "建议取log", "可不取log"
#   )
# )
# print(skew_df)

# ===========  对数转换  =====================

# 对所有偏度>1的数值变量取对数
log_vars <- c(
  "FDI", "GDP", "PGDP", "TER_GDP", "APC", "WAGE",
  "PATENT", "IP&OP", "TRANS", "R&D", "STHC"
)

# 创建新数据框，只包含分类变量和对数变量
df_log <- df[, c("Year", "Province", "Region")]

# 添加对数变量
for (var in log_vars) {
  df_log[[paste0("log_", var)]] <- log(df[[var]])
}


# # 检查转换后的偏度
# numeric_vars_log <- df_log[, grep("^log_", names(df_log))]
# skew_df_log <- data.frame(
#   Variable = names(numeric_vars_log),
#   Skewness = sapply(numeric_vars_log, skewness, na.rm = TRUE)
# )
# print(skew_df_log)

model <- lm(log_FDI ~ . - Year - Province, data = df_log)

summary(model)

colnames(df_log)
colnames(df)

# 只保留 df 和 df_log
rm(list = setdiff(ls(), c("df", "df_log", "model")))

>>>>>>> a98194459f636eb055c4c7faf1142e1137e4c6e5
