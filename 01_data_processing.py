import os

import numpy as np
import pandas as pd
import statsmodels.api as sm


def main():
    # 清空工作空间并将工作目录设置为当前脚本所在目录（尽量模拟 R 中的行为）
    if "__file__" in globals():
        script_dir = os.path.dirname(os.path.abspath(__file__))
        os.chdir(script_dir)

    # 打印当前工作目录（对应 R 中的 getwd()）
    print(os.getcwd())

    # 读取 Excel 数据
    df = pd.read_excel("回归分析数据.xlsx")

    # 定义新的列名（与 R 脚本保持一致）
    df.columns = [
        "Year",  # 年份
        "Province",  # 省份
        "FDI",  # 外商直接投资
        "GDP",  # 国内生产总值
        "PGDP",  # 人均GDP
        "TER_GDP",  # 第三产业占比
        "APC",  # 平均消费倾向
        "WAGE",  # 工资
        "PATENT",  # 专利申请数
        "IP&OP",  # 进出口总额
        "TRANS",  # 交通里程
        "R&D",  # 研发投入
        "STHC",  # 学生人力资本
        "Region",  # 区域分组
    ]

    # 将 Region 和 Province 转换为分类变量（因子）
    df["Region"] = df["Region"].astype("category")
    df["Province"] = df["Province"].astype("category")

    # 调整变量顺序
    df = df[
        [
            "Year",
            "Province",
            "Region",
            "FDI",
            "GDP",
            "PGDP",
            "TER_GDP",
            "APC",
            "WAGE",
            "PATENT",
            "IP&OP",
            "TRANS",
            "R&D",
            "STHC",
        ]
    ]

    # 输出数据概览（对应 R 中的 summary(df)）
    print(df.describe(include="all"))

    # 需要取对数的变量
    log_vars = [
        "FDI",
        "GDP",
        "PGDP",
        "TER_GDP",
        "APC",
        "WAGE",
        "PATENT",
        "IP&OP",
        "TRANS",
        "R&D",
        "STHC",
    ]

    # 创建新数据框，只包含分类变量和对数变量
    df_log = df[["Year", "Province", "Region"]].copy()

    # 添加对数变量（与 R 中 log(df[[var]]) 对应；假定数据为正）
    for var in log_vars:
        df_log[f"log_{var}"] = np.log(df[var])

    # ====== 回归模型：log_FDI ~ . - Year - Province ======
    # 为了避免列名中包含特殊字符（如 &、& 等）导致公式解析错误，
    # 这里不使用公式接口，而是手动构造设计矩阵：
    #   - 因变量：log_FDI
    #   - 自变量：除 Year、Province、log_FDI 外的所有变量，
    #             其中 Region 作为分类变量展开虚拟变量（dummy）
    y = df_log["log_FDI"]
    X = df_log.drop(columns=["log_FDI", "Year", "Province"])

    # 将分类变量（如 Region）转成虚拟变量，丢弃一个基准类别以避免完全共线
    X = pd.get_dummies(X, drop_first=True)

    # 确保所有自变量和因变量都是纯数值类型，并丢弃含有缺失的行
    X = X.apply(pd.to_numeric, errors="coerce")
    y = pd.to_numeric(y, errors="coerce")
    valid_mask = X.notna().all(axis=1) & y.notna()
    X = X[valid_mask]
    y = y[valid_mask]

    # 确保仍是带列名的 DataFrame/Series，但元素均为 float
    X = X.astype(float)
    y = y.astype(float)

    # 添加截距项（与 R 中 lm 的默认截距一致）
    X = sm.add_constant(X)

    # 拟合线性回归模型（保留列名，便于与 R 输出对比）
    model = sm.OLS(y, X).fit()

    # 输出模型摘要（对应 R 中的 summary(model)）
    print(model.summary())

    # 输出列名（对应 R 中的 colnames(df_log) 和 colnames(df)）
    print("df_log columns:", list(df_log.columns))
    print("df columns:", list(df.columns))

    # 模拟只保留 df, df_log, model 变量（在脚本中仅作为注释说明）
    # 在 Python 脚本执行完后，局部变量会自动释放，
    # 如果在交互式环境中需要，可手动保留这三个变量。
    return df, df_log, model


if __name__ == "__main__":
    main()


