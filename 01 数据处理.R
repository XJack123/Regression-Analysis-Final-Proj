# 清空工作空间 并设置文件路径为当前脚本所在目录
rm(list = ls())
if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
}
getwd()

# =============== 加载包 ===============
library(readxl)
library(dplyr)
library(tidyr)
library(car)
library(plm)
library(lmtest)
library(psych)
library(ggplot2)
library(glmnet)
library(ncvreg)
library(caret)
library(olsrr)
library(RColorBrewer)
library(gridExtra)

# =================== 数据处理 ===================
df_raw <- read_excel("回归分析数据.xlsx")

colnames(df_raw)

# 保存 FDI_lag 变量
FDI_lag_temp <- df_raw$FDI_lag

# 合并 Region 变量，并将 3 个变量转换为因子型
df <- df_raw %>%
  mutate(
    Region = ifelse(Region %in% c("东北", "中部"), "中部及东北", Region),
    Region = factor(Region, levels = c("中部及东北", "东部", "西部")),
    Province = as.factor(Province),
    Year = as.factor(Year)
  ) %>%
  select(-FDI_lag)

summary(df)

# ==================== 全局审美定义 (Design System) ====================

# 1. 定义高级配色 (从奶油色到深砖红)
color_low <- "#FEE8C8"
color_high <- "#B30000"

# 2. 定义学术风主题 (无网格线版)
theme_academic <- function() {
  theme_bw(base_size = 12, base_family = "serif") + # 核心：衬线字体
    theme(
      # 面板与网格：去除所有网格线
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # 边框与背景
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.background = element_rect(fill = "white", color = NA),

      # 文字样式
      axis.text = element_text(color = "black", size = 10, family = "serif"),
      axis.title = element_text(face = "bold", size = 11, family = "serif"),

      # 图例样式
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(face = "bold", size = 10, family = "serif"),

      # 底部标签样式 ((a), (b))
      plot.caption = element_text(
        hjust = 0.5, size = 13, face = "bold",
        margin = margin(t = 12), family = "serif"
      )
    )
}

# 1. 定义高级配色 (从奶油色到深砖红)
color_low <- "#FEE8C8"
color_high <- "#B30000"

# 2. 定义学术风主题 (无网格线版)
theme_academic <- function() {
  theme_bw(base_size = 12, base_family = "serif") + # 核心：衬线字体
    theme(
      # 面板与网格：去除所有网格线
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),

      # 边框与背景
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
      plot.background = element_rect(fill = "white", color = NA),

      # 文字样式
      axis.text = element_text(color = "black", size = 10, family = "serif"),
      axis.title = element_text(face = "bold", size = 11, family = "serif"),

      # 图例样式
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.title = element_text(face = "bold", size = 10, family = "serif"),

      # 底部标签样式 ((a), (b))
      plot.caption = element_text(
        hjust = 0.5, size = 13, face = "bold",
        margin = margin(t = 12), family = "serif"
      )
    )
}


# ============ 回归诊断函数 ============

# --- Spearman秩相关检验异方差 ---
test_heteroscedasticity <- function(model) {
  # Spearman秩相关检验（拟合值 vs 残差绝对值）
  spearman_test <- cor.test(fitted(model), abs(residuals(model)), method = "spearman")

  cat("\nSpearman秩相关检验:\n")
  print(spearman_test)

  cat(ifelse(spearman_test$p.value < 0.05,
    paste("\n结论: p =", round(spearman_test$p.value, 4), "< 0.05，存在异方差"),
    paste("\n结论: p =", round(spearman_test$p.value, 4), "> 0.05，不存在显著异方差")
  ))
  cat("\n秩相关系数 rho =", round(spearman_test$estimate, 4), "\n")

  return(spearman_test)
}


# --- 异常值检测函数 ---
test_outliers <- function(model, data) {
  cat("\n========== 异常值检测 ==========\n")
  cat("样本总数:", nrow(data), "\n")

  # 获取模型使用的实际数据行号
  model_data <- model.frame(model)
  actual_indices <- as.numeric(rownames(model_data))

  # 1. 删除学生化残差
  deleted_stud_resid <- rstudent(model)
  outliers_resid <- which(abs(deleted_stud_resid) > 3)

  cat("\n(1) 删除学生化残差检验（|删除学生化残差| > 3）:\n")
  if (length(outliers_resid) > 0) {
    cat("发现", length(outliers_resid), "个异常观测值:\n\n")
    # 使用实际的行索引
    actual_rows <- actual_indices[outliers_resid]
    result_df <- data.frame(
      序号 = actual_rows,
      年份 = as.character(data$Year[actual_rows]),
      省份 = as.character(data$Province[actual_rows]),
      删除学生化残差 = round(deleted_stud_resid[outliers_resid], 4)
    )
    print(result_df, row.names = FALSE)
  }

  # 2. Cook距离
  cooksd <- cooks.distance(model)
  n <- length(actual_indices)
  threshold_cook <- 4 / n
  influential_cook <- which(cooksd > threshold_cook)

  cat("\n(2) Cook距离检验（Cook's D > 4/n =", round(threshold_cook, 4), "）:\n")
  if (length(influential_cook) > 0) {
    cat("发现", length(influential_cook), "个高影响力观测值（显示前20个）:\n\n")
    show_n <- min(20, length(influential_cook))
    actual_rows <- actual_indices[influential_cook[1:show_n]]
    result_df <- data.frame(
      序号 = actual_rows,
      年份 = as.character(data$Year[actual_rows]),
      省份 = as.character(data$Province[actual_rows]),
      Cook距离 = round(cooksd[influential_cook[1:show_n]], 6)
    )
    print(result_df, row.names = FALSE)
  } else {
    cat("未发现高影响力观测值\n")
  }

  # 3. 杠杆值
  hatvalues_val <- hatvalues(model)
  p <- length(coef(model)) - 1
  threshold_lev <- 3 * (p + 1) / n
  high_leverage <- which(hatvalues_val > threshold_lev)

  cat("\n(3) 杠杆值检验（Leverage > 3(p+1)/n =", round(threshold_lev, 4), "）:\n")
  if (length(high_leverage) > 0) {
    cat("发现", length(high_leverage), "个高杠杆观测值（显示前20个）:\n\n")
    show_n <- min(20, length(high_leverage))
    actual_rows <- actual_indices[high_leverage[1:show_n]]
    result_df <- data.frame(
      序号 = actual_rows,
      年份 = as.character(data$Year[actual_rows]),
      省份 = as.character(data$Province[actual_rows]),
      杠杆值 = round(hatvalues_val[high_leverage[1:show_n]], 4)
    )
    print(result_df, row.names = FALSE)
  } else {
    cat("未发现高杠杆观测值\n")
  }

  return(list(
    outliers = outliers_resid,
    influential = influential_cook,
    high_leverage = high_leverage
  ))
}

# --- 多重共线性 VIF 检验函数 ---

test_vif <- function(model) {
  cat("\n========== 多重共线性检验（VIF） ==========\n")

  # 计算VIF
  vif_result <- vif(model)

  # 判断是否为GVIF（包含因子变量）
  if (is.matrix(vif_result)) {
    # 有因子变量，使用GVIF^(1/(2*Df))
    result_df <- data.frame(
      变量 = rownames(vif_result),
      GVIF = round(vif_result[, "GVIF"], 4),
      Df = vif_result[, "Df"],
      调整GVIF = round(vif_result[, "GVIF^(1/(2*Df))"], 4)
    )
    cat("\n注意: 模型包含因子变量，使用GVIF和调整后的GVIF\n")
    cat("诊断标准: 调整GVIF > 2 表示存在共线性问题\n\n")
    print(result_df, row.names = FALSE)

    # 诊断
    problem_vars <- result_df$变量[result_df$调整GVIF > 2]
    if (length(problem_vars) > 0) {
      cat("\n结论: 以下变量存在共线性问题:\n")
      cat(paste(problem_vars, collapse = ", "), "\n")
    } else {
      cat("\n结论: 未发现显著的多重共线性问题\n")
    }
  } else {
    # 纯数值变量，使用VIF
    result_df <- data.frame(
      变量 = names(vif_result),
      VIF = round(vif_result, 4)
    )
    cat("\n诊断标准: VIF > 10 表示严重共线性，VIF > 5 表示中等共线性\n\n")
    print(result_df, row.names = FALSE)

    # 诊断
    severe_vars <- result_df$变量[result_df$VIF > 10]
    moderate_vars <- result_df$变量[result_df$VIF > 5 & result_df$VIF <= 10]

    if (length(severe_vars) > 0) {
      cat("\n严重共线性变量 (VIF > 10):\n")
      cat(paste(severe_vars, collapse = ", "), "\n")
    }
    if (length(moderate_vars) > 0) {
      cat("\n中等共线性变量 (5 < VIF ≤ 10):\n")
      cat(paste(moderate_vars, collapse = ", "), "\n")
    }
    if (length(severe_vars) == 0 && length(moderate_vars) == 0) {
      cat("\n结论: 未发现显著的多重共线性问题\n")
    }
  }

  return(invisible(vif_result))
}

# --- 自相关检验函数 ---
test_autocorrelation <- function(model, plot = TRUE) {
  cat("\n========== 自相关检验 ==========\n")

  # 1. Durbin-Watson检验
  cat("\n(1) Durbin-Watson检验:\n")
  cat("H0: 无一阶自相关; H1: 存在一阶自相关\n")
  cat("DW值在2附近说明无自相关，接近0说明正自相关，接近4说明负自相关\n\n")

  dw_test <- dwtest(model)
  print(dw_test)

  if (dw_test$p.value < 0.05) {
    cat("\n结论: p =", round(dw_test$p.value, 4), "< 0.05，存在显著自相关\n")
    cat("DW统计量 =", round(dw_test$statistic, 4), "\n")
    if (dw_test$statistic < 1.5) {
      cat("提示: DW < 1.5，可能存在正自相关\n")
    } else if (dw_test$statistic > 2.5) {
      cat("提示: DW > 2.5，可能存在负自相关\n")
    }
    cat("建议: 使用HAC标准误或考虑动态模型\n")
  } else {
    cat("\n结论: p =", round(dw_test$p.value, 4), "> 0.05，不存在显著自相关\n")
  }

  # 2. 绘制 ei vs ei-1 图
  p <- NULL
  cor_coef <- NULL
  if (plot) {
    cat("\n\n(2) 绘制残差自相关图 (ei vs ei-1):\n")

    resid <- residuals(model)
    n <- length(resid)

    # 创建滞后残差
    resid_lag1 <- resid[1:(n - 1)]
    resid_current <- resid[2:n]

    # 计算相关系数
    cor_coef <- cor(resid_lag1, resid_current)

    # 创建数据框用于 ggplot2
    plot_data <- data.frame(
      ei_lag = resid_lag1,
      ei = resid_current
    )

    p <- ggplot(plot_data, aes(x = ei_lag, y = ei)) +
      geom_vline(xintercept = 0, linetype = "longdash", color = "gray70", linewidth = 0.4) +
      geom_hline(yintercept = 0, linetype = "longdash", color = "gray70", linewidth = 0.4) +
      geom_point(
        shape = 21, fill = "#87b1e2ff", color = "gray30",
        size = 3.5, stroke = 0.25, alpha = 0.85
      ) +
      labs(
        x = expression(paste(e[i - 1])),
        y = expression(paste(e[i])),
        title = NULL,
        caption = bquote(bold(hat(rho) == .(round(cor_coef, 4))))
      ) +
      theme_academic() +
      theme(
        plot.caption = element_text(
          hjust = 0.5, size = 12, face = "bold",
          margin = margin(t = 10)
        )
      )
    print(p)
    cat("相关系数 (ei, ei-1) =", round(cor_coef, 4), "\n")
  }

  return(list(
    dw_test = dw_test,
    plot = p,
    cor_coef = cor_coef
  ))
}
