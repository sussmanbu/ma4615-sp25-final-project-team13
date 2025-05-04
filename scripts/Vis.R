library(tidyverse)
library(here)

# Load cleaned dataset
df <- readRDS(here("dataset-ignore", "clean_selected_data.rds"))

# Check frequency of key fields
df %>% count(InterventionReasonCode, sort = TRUE)
df %>% count(InterventionTechniqueCode, sort = TRUE)
df %>% count(InterventionDispositionCode, sort = TRUE)


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


model <- glm(VehicleSearchedIndicator ~ 
               SubjectRaceCode_B + SubjectRaceCode_I + SubjectRaceCode_A +
               SubjectSexCode + InterventionReasonCode_V + 
               InterventionTechniqueCode_S + InterventionDurationCode + 
               TowedIndicator + CustodialArrestIndicator,
             family = binomial, 
             data = data_ready)

summary(model)

exp(coef(model))

library(ggeffects)

plot(ggpredict(model, terms = c("SubjectRaceCode_B", "SubjectSexCode")))

plot(ggpredict(model, terms = c("SubjectRaceCode_B", "CustodialArrestIndicator")))


plot(predict(model)) 
glm(VehicleSmodel_search <- glm(VehicleSearchedIndicator ~ 
                      SubjectRaceCode_B * SubjectSexCode +
                      InterventionReasonCode_V +
                      InterventionTechniqueCode_S +
                      InterventionDurationCode +
                      TowedIndicator,
                    family = binomial,
                    data = data_ready)

summary(model_search)earchedIndicator ~ SubjectRaceCode_B * SubjectSexCode, family = binomial, data = data_ready)
model_search <- glm(
  VehicleSearchedIndicator ~ 
    SubjectRaceCode_B * SubjectSexCode + 
    InterventionDurationCode + 
    InterventionReasonCode_V + 
    InterventionTechniqueCode_S,
  family = binomial,
  data = data_ready
)
summary(model_search)


model_arrest <- glm(
  CustodialArrestIndicator ~ 
    SubjectRaceCode_B * SubjectSexCode + 
    VehicleSearchedIndicator + 
    InterventionDurationCode,
  family = binomial,
  data = data_ready
)
summary(model_arrest)

model_tow <- glm(
  TowedIndicator ~ 
    SubjectRaceCode_B * SubjectSexCode + 
    InterventionReasonCode_E + 
    InterventionDurationCode,
  family = binomial,
  data = data_ready
)
summary(model_tow)
library(ggeffects)

plot(ggpredict(model_search, terms = c("SubjectRaceCode_B", "SubjectSexCode")))





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






# 安装（如果未安装过）
install.packages("GGally")

# 加载包
library(GGally)

# 选一些你关心的变量（建议全为数值 / 0-1 / 分类转换为数值）
data_ready %>%
  select(
    VehicleSearchedIndicator,
    CustodialArrestIndicator,
    TowedIndicator,
    ContrabandIndicator,
    InterventionDurationCode
  ) %>%
  GGally::ggpairs(
    title = "Variable Correlation Matrix with Logistic Targets"
  )

GGally::ggpairs(
  data_ready,
  columns = c("VehicleSearchedIndicator", "TowedIndicator", "CustodialArrestIndicator", "InterventionDurationCode"),
  mapping = aes(color = Race),
  title = "Pairwise Plot by Race"
)