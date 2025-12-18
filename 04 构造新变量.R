setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01 数据处理.R", encoding = "UTF-8")

# ---------------------------------------------------------
# 1. 数据导入与预处理
# ---------------------------------------------------------

# 删除某 （year-province）的行
df <- df[!(df$Year == "2021" & df$Province == "海南"), ]

# A. 因变量：仅取对数，不标准化
df_final <- data.frame(log_FDI = log(df$FDI))

# B. 自变量：不取对数，直接标准化 (Z-score)
x_vars <- c("GDP", "PGDP", "TER_GDP", "APC", "WAGE", "PATENT", "OPEN", "TRANS", "RD", "STHC", "STHC_ns")
x_scaled <- as.data.frame(scale(df[, x_vars]))

# ---------------------------------------------------------
# 2. 分组 PCA 降维 (基于标准化后的自变量)
# ---------------------------------------------------------

# --- 组1：知识与创新能级 (Innovation_Power) ---
pca_factor <- prcomp(x_scaled[, c("PATENT", "RD", "STHC", "STHC_ns")])
df_final$Innovation_Power <- pca_factor$x[, 1]

# --- 组2：市场联通度 (Market_Scale) ---
pca_market <- prcomp(x_scaled[, c("TRANS", "OPEN")])
df_final$Market_Scale <- pca_market$x[, 1]

# --- 组3：经济质量与市场深度 (Econ_Quality) ---
pca_quality <- prcomp(x_scaled[, c("PGDP", "APC", "GDP")])
df_final$Econ_Quality <- pca_quality$x[, 1]

# --- 组4 & 5：产业结构与成本 (直接使用标准化后的变量) ---
df_final$Ind_Structure <- x_scaled$TER_GDP
df_final$Labor_Cost <- x_scaled$WAGE

# ---------------------------------------------------------
# 3. 用新变量做回归分析
# ---------------------------------------------------------
df_final$Region <- df$Region
df_final$Province <- df$Province
df_final$Year <- df$Year

# 注意：此时因变量是取了对数的 log_FDI
model_p <- lm(log_FDI ~ Innovation_Power + Market_Scale + Econ_Quality + Ind_Structure + Labor_Cost + Region, data = df_final)

# 输出回归摘要
summary(model_p)
par(mfrow = c(2, 2))
plot(model_p, which = 1:4)

# 输出 VIF 检验
test_vif(model_p)

# --------------------------------
# 4. 考虑时间效应 引入时间变量
# --------------------------------
df_final$Year <- df$Year
df_final$Region <- df$Region
df_final$Province <- df$Province

model_time <- lm(log_FDI ~ Innovation_Power + Market_Scale + Econ_Quality + Ind_Structure + Labor_Cost + Region + Year, data = df_final)
summary(model_time)

par(mfrow = c(2, 2))
plot(model_time, which = 1:4)

test_vif(model_time)
test_heteroscedasticity(model_time)
test_outliers(model_time, df_final)
test_autocorrelation(model_time)



