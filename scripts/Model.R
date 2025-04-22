library(dplyr)
library(corrplot)

# Step 1: 选择数值型变量进行相关性分析
cor_vars <- clean_data %>%
  select(
    VehicleSearchedIndicator,
    CustodialArrestIndicator,
    TowedIndicator,
    ContrabandIndicator,
    InterventionDurationCode,
    SubjectAge,
    ResidentIndicator,
    TownRecidentIndicator
    # 你也可以添加更多 0/1 变量
  )

# Step 2: 转换为 numeric（防止是 character/factor）
cor_vars <- mutate_all(cor_vars, as.numeric)

# Step 3: 计算相关系数矩阵
cor_matrix <- cor(cor_vars, use = "complete.obs")

# Step 4: 可视化相关性矩阵
corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")

library(ggcorrplot)

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

clean_data$InterventionDispositionCode

