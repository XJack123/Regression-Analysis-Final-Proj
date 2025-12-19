setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("01 数据处理.R", encoding = "UTF-8")

# 删除某 （year-province）的行
df <- df[!(df$Year == "2021" & df$Province == "海南"), ]

# ============ 1. 主成分分析 ============

# 数值变量并标准化
numeric_vars <- c(
  "GDP", "PGDP", "TER_GDP", "APC", "WAGE", "RD",
  "OPEN", "TRANS", "STHC", "STHC_ns", "PATENT"
)
X_scaled <- scale(df[, numeric_vars])

# PCA分析
pca_result <- principal(X_scaled, nfactors = ncol(X_scaled), rotate = "none")
eigenvalues <- pca_result$values

# 方差贡献率表
variance_pct <- eigenvalues / sum(eigenvalues) * 100
variance_table <- data.frame(
  PC = paste0("PC", 1:length(eigenvalues)),
  特征值 = round(eigenvalues, 3),
  方差贡献率 = paste0(round(variance_pct, 2), "%"),
  累计贡献率 = paste0(round(cumsum(variance_pct), 2), "%")
)
print(variance_table)

# 确定主成分数量
n_pc <- sum(eigenvalues > 1) # Kaiser准则
cat("\n保留", n_pc, "个主成分（特征值>1）\n")

# 构建主成分数据集
df_pca <- data.frame(
  Year = df$Year,
  Province = df$Province,
  Region = df$Region,
  FDI = df$FDI,
  pca_result$scores[, 1:n_pc]
)
colnames(df_pca)[5:(4 + n_pc)] <- paste0("PC", 1:n_pc)

# ============ 2. 主成分回归（PCR） ============

pc_formula <- as.formula(paste("log(FDI) ~ ", paste0("PC", 1:n_pc, collapse = " + "), " + Region"))
model_PCR <- lm(pc_formula, data = df_pca)
summary(model_PCR)

png("Figure/3.1 主成分回归诊断图.png", width = 1200, height = 1000, res = 120)
par(mfrow = c(2, 2))
plot(model_PCR, which = 1:4)
dev.off()

# VIF 检验
test_vif(model_PCR)

# 异方差检验
test_heteroscedasticity(model_PCR)

# 异常值检测
outlier_results <- test_outliers(model_PCR, df_pca)

# 自相关
test_autocorrelation(model_PCR)

# ============ 3. 还原原始系数 ============

# 1. 提取主成分回归的系数（只提取主成分的系数，不包括 Region 和截距）
pc_names <- paste0("PC", 1:n_pc)
beta_pc <- coef(model_PCR)[pc_names]

# 2. 提取 PCA 的权重矩阵 (Weights)
# 注意：只提取被保留的那 n_pc 个主成分的权重
# psych 包的 principal 结果中，weights 矩阵就是我们要的变换矩阵
W <- pca_result$weights[, 1:n_pc]

# 3. 计算针对“标准化数据”的回归系数
# 矩阵乘法：[变量数 x n_pc] * [n_pc x 1] = [变量数 x 1]
beta_scaled <- W %*% beta_pc

# 4. 还原到原始量纲的系数
# 需要除以各变量的标准差
sd_vars <- apply(df[, numeric_vars], 2, sd)
beta_original <- beta_scaled / sd_vars

# 整理结果展示
coef_summary <- data.frame(
  Scaled_Coef = round(beta_scaled, 5), # 标准化后的系数（通常用于比较影响力大小）
  Original_Coef = round(beta_original, 5) # 原始量纲下的系数
)

print(coef_summary)
