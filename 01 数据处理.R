# 清空工作空间 并设置文件路径为当前脚本所在目录
rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
getwd()

library(readxl)

df <- read_excel("回归分析数据.xlsx")

colnames(df)

# 将 Region 和 Province 转换为因子型
df$Region <- as.factor(df$Region)
df$Province <- as.factor(df$Province)
df$Year <- as.factor(df$Year)

summary(df)
