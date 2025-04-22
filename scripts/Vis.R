library(tidyverse)
library(here)

# Load cleaned dataset
df <- readRDS(here("dataset-ignore", "clean_selected_data.rds"))

# Check frequency of key fields
df %>% count(InterventionReasonCode, sort = TRUE)
df %>% count(InterventionTechniqueCode, sort = TRUE)
df %>% count(InterventionDispositionCode, sort = TRUE)

df %>%
  map(~ unique(.x))
# 先统计组合占比
heatmap_data <- df %>%
  group_by(InterventionReasonCode, InterventionTechniqueCode) %>%
  summarise(
    arrest_rate = mean(InterventionDispositionCode == "U"),
    count = n()
  )

# 热力图
ggplot(heatmap_data, aes(x = InterventionReasonCode, y = InterventionTechniqueCode, fill = arrest_rate)) +
  geom_tile(color = "white") +
  geom_text(aes(label = scales::percent(arrest_rate, accuracy = 0.1)), color = "black") +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Arrest Rate by Reason and Technique",
       x = "Reason",
       y = "Technique",
       fill = "Arrest Rate") +
  theme_minimal()


df_model <- df %>%
  filter(InterventionDispositionCode %in% c("U", "V", "W", "I", "M")) %>%
  mutate(arrest = ifelse(InterventionDispositionCode == "U", 1, 0))

model <- glm(arrest ~ InterventionReasonCode + InterventionTechniqueCode,
             data = df_model, family = binomial)

summary(model)

df %>%
  map(~ unique(.x))


drop_vars <- c(
  "SubjectAge",
  "Month",
  "InterventionDateTime",
  "InterventionDate",
  "SubjectEthnicityCode",
  "TownRecidentIndicator",
  "ResidentIndicator",
  "InterventionLocationName",
  "StatuteReason",
  "SearchAuthorizationCode",
  "OrganizationIdentificationID",
  "Department Name"
)

# 去除指定变量，生成新数据框
reduced_data <- df %>%
  select(-all_of(drop_vars))


reduced_data %>%
  map(~ unique(.x))

reduced_data
#推荐一键转换代码（生成一个干净可分析数据集） -------
library(dplyr)

# 先复制你正在用的数据集（例如 clean_data 或 reduced_data）
data_ready <- reduced_data %>%
  mutate(
    # 转为 0/1 的逻辑变量
    CustodialArrestIndicator = as.numeric(CustodialArrestIndicator),
    TowedIndicator = as.numeric(TowedIndicator),
    ContrabandIndicator = as.numeric(ContrabandIndicator),
    VehicleSearchedIndicator = as.numeric(VehicleSearchedIndicator),
    
    # 性别编码为 0/1
    SubjectSexCode = ifelse(SubjectSexCode == "M", 1, 0)
  )

# One-hot encoding 处理字符型分类变量（多类的）
data_ready <- data_ready %>%
  mutate(across(c(
    SubjectRaceCode,
    InterventionDispositionCode,
    InterventionReasonCode,
    InterventionTechniqueCode
  ), as.factor))

# 生成 dummy 变量（不包含原始分类列）
library(fastDummies)
data_ready <- dummy_cols(
  data_ready,
  select_columns = c("SubjectRaceCode", "InterventionDispositionCode",
                     "InterventionReasonCode", "InterventionTechniqueCode"),
  remove_selected_columns = TRUE
)
data_ready






library(FactoMineR)
library(factoextra)

pca_res <- PCA(data_ready %>% select(where(is.numeric)), graph = FALSE)

fviz_pca_var(pca_res,
             col.var = "contrib", # 用贡献度上色
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE) +
  ggtitle("Correlation Circle of Variables")

# -- ---------
model <- glm(VehicleSearchedIndicator ~ 
               SubjectRaceCode_B + SubjectRaceCode_I + SubjectRaceCode_A +
               SubjectSexCode + InterventionReasonCode_V + 
               InterventionTechniqueCode_S + InterventionDurationCode + 
               TowedIndicator + CustodialArrestIndicator,
             family = binomial, 
             data = data_ready)

summary(model)

exp(coef(model))




# 加载必要库
library(dplyr)
library(broom)       # 用于提取模型结果
library(gt)          # 美化表格（可选）

# 构建逻辑回归模型（你可以自定义变量）
model <- glm(
  VehicleSearchedIndicator ~ 
    SubjectRaceCode_B + SubjectRaceCode_I + SubjectRaceCode_A +
    SubjectSexCode +
    SubjectRaceCode_B:SubjectSexCode +   # 加上交互项
    InterventionReasonCode_V + 
    InterventionTechniqueCode_S +
    InterventionDurationCode +
    TowedIndicator + CustodialArrestIndicator,
  family = binomial,
  data = data_ready
)

# 提取模型结果并计算 Odds Ratio（OR）
results_table <- tidy(model) %>%
  mutate(
    OR = exp(estimate),                      # 计算 odds ratio
    CI_low = exp(estimate - 1.96 * std.error),  # 置信区间下限
    CI_high = exp(estimate + 1.96 * std.error)
  ) %>%
  select(term, estimate, std.error, OR, CI_low, CI_high, p.value)

# 可视化输出：格式清晰的 OR 表格（可选）
results_table %>%
  gt() %>%
  fmt_number(columns = c(estimate, std.error, OR, CI_low, CI_high), decimals = 3) %>%
  fmt_number(columns = p.value, decimals = 4) %>%
  cols_label(
    term = "Variable",
    estimate = "Estimate",
    std.error = "Std. Error",
    OR = "Odds Ratio",
    CI_low = "OR (95% CI) Low",
    CI_high = "OR (95% CI) High",
    p.value = "P-value"
  ) %>%
  tab_header(title = "Logistic Regression Results for Being Searched")
library(ggplot2)
# 先把 race 还原回一个 factor 列（如果你之前只保留了 dummy）
# 重新设置 Race 列，把 Asian 和 Indigenous 合并为 Other
data_ready$Race <- case_when(
  data_ready$SubjectRaceCode_B == 1 ~ "Black",
  data_ready$SubjectRaceCode_I == 1 ~ "Other",
  data_ready$SubjectRaceCode_A == 1 ~ "Other",
  TRUE ~ "White"  # baseline 没 dummy 的默认是白人
)

# 画图：不同种族一条线
ggplot(data_ready, aes(x = InterventionDurationCode, y = VehicleSearchedIndicator, color = Race)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(
    title = "Search Probability by Stop Duration and Race",
    x = "Intervention Duration Code",
    y = "Probability of Being Searched",
    color = "Race"
  ) +
  theme_minimal()

# 加载必要库
library(GGally)
library(dplyr)

# ✅ Step 1: 重新构造种族 Race 列（合并 Asian & Indigenous → Other）
data_ready$Race <- case_when(
  data_ready$SubjectRaceCode_B == 1 ~ "Black",
  data_ready$SubjectRaceCode_I == 1 ~ "Other",
  data_ready$SubjectRaceCode_A == 1 ~ "Other",
  TRUE ~ "White"
)

# ✅ Step 2: 选出你想分析的变量
selected_vars <- c(
  "VehicleSearchedIndicator",     # 是否被搜查（目标变量）
  "CustodialArrestIndicator",     # 是否被逮捕
  "TowedIndicator",               # 是否被拖车
  "ContrabandIndicator",          # 是否发现违禁品
  "InterventionDurationCode",     # 停留时长
  "SubjectSexCode"                # 性别（0/1）
)

# ✅ Step 3: 抽样，提升绘图效率
set.seed(42)  # 固定随机性（可重复）
data_sample <- data_ready %>%
  select(all_of(selected_vars), Race) %>%
  sample_n(5000)

# ✅ Step 4: 生成图像（pairwise plot）
ggpairs(
  data_sample,
  columns = 1:6,  # 不含 Race 列本身
  mapping = aes(color = Race),
  title = "Pairwise Plot of Key Variables (Sampled 5k)"
)


library(corrplot)

cor_data <- data_ready %>%
  select(
    VehicleSearchedIndicator,
    CustodialArrestIndicator,
    TowedIndicator,
    ContrabandIndicator,
    InterventionDurationCode,
    SubjectSexCode
  )

cor_matrix <- cor(cor_data)

corrplot(cor_matrix, method = "circle", type = "lower", tl.col = "black", tl.srt = 45)
library(ggcorrplot)

ggcorrplot(cor_matrix, lab = TRUE, lab_size = 3, type = "lower", colors = c("red", "white", "blue")) +
  ggtitle("Correlation Between Key Variables") +
  theme_minimal()

library(ggplot2)
library(plotly)

# 静态 ggplot（比如之前 race vs duration 搜查概率）
p <- ggplot(data_ready, aes(x = InterventionDurationCode, y = VehicleSearchedIndicator, color = Race)) +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), se = TRUE) +
  labs(title = "Search Probability by Stop Duration and Race") +
  theme_minimal()

# 一行变交互
ggplotly(p)


library(dplyr)
library(ggplot2)
library(scales)

library(dplyr)
library(ggplot2)
library(plotly)

# 1. 读取数据
df <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))

# 2. 计算比例（保留原始种族代码）
disposition_by_race <- df %>%
  group_by(SubjectRaceCode, InterventionDispositionCode) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SubjectRaceCode) %>%
  mutate(proportion = n / sum(n))

# 3. 显式转为 factor 保证排序
disposition_by_race$InterventionDispositionCode <- factor(
  disposition_by_race$InterventionDispositionCode,
  levels = c("W", "V", "I", "M", "U", "N") # 可自定义顺序
)

# 4. 静态 ggplot（facet：每个种族一张图）
p <- ggplot(disposition_by_race, aes(x = InterventionDispositionCode, y = proportion, fill = InterventionDispositionCode)) +
  geom_col() +
  facet_wrap(~ SubjectRaceCode) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = "Disposition Distribution per Race (Interactive)",
    x = "Disposition Code",
    y = "Proportion",
    fill = "Disposition"
  ) +
  theme_minimal()

# 5. 转为交互式 plotly 图
ggplotly(p)


library(dplyr)
library(ggplot2)
library(plotly)
library(scales)
library(here)

# 1. 读取数据
df <- readRDS(here("dataset-ignore", "clean_selected_data.rds"))

# 2. 创建 disposition label（替换缩写为全称）
code_labels <- c(
  "W" = "Written Warning",
  "V" = "Verbal Warning",
  "I" = "Infraction",
  "M" = "Misdemeanor",
  "U" = "Arrest",
  "N" = "None"
)

df <- df %>%
  filter(!is.na(InterventionDispositionCode)) %>%
  mutate(
    DispositionLabel = recode(InterventionDispositionCode, !!!code_labels),
    DispositionLabel = factor(
      DispositionLabel,
      levels = c("Verbal Warning", "Written Warning", "Infraction", "Misdemeanor", "Arrest", "None")
    )
  )

# 3. 计算各种族的 disposition 分布比例
disposition_by_race <- df %>%
  group_by(SubjectRaceCode, DispositionLabel) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(SubjectRaceCode) %>%
  mutate(proportion = n / sum(n))

# 4. 创建 ggplot 图（tooltip 自定义）
p <- ggplot(disposition_by_race, aes(
  x = DispositionLabel,
  y = proportion,
  fill = DispositionLabel,
  text = paste0(
    "Race: ", SubjectRaceCode, "<br>",
    "Disposition: ", DispositionLabel, "<br>",
    "Proportion: ", percent(proportion, accuracy = 0.1)
  )
)) +
  geom_col() +
  facet_wrap(~ SubjectRaceCode) +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Disposition Distribution by Race (Interactive)",
    subtitle = "Disposition Types: Written/Verbal Warning, Infraction, Misdemeanor, Arrest, None",
    x = "Disposition Type",
    y = "Proportion of Stops",
    fill = "Disposition"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 20, hjust = 1, size = 9),
    strip.text = element_text(face = "bold", size = 12)
  )

# 5. 转为交互式图
ggplotly(p, tooltip = "text")


table(df$InterventionDispositionCode, df$CustodialArrestIndicator, useNA = "ifany")





