# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project
library(readr)
library(dplyr)
library(here)
library(tidyverse)
raw_data_path <- here("dataset-ignore", "raw_data.csv")
library(readr)

# 将第 35 列和第 36 列转换为字符类型
raw_data <- read_csv(raw_data_path, col_types = cols(
  IsStatuteCodeValid = col_character(),
  IsStatutatoryCitationValid = col_character()
))

glimpse(raw_data)
head(raw_data)
colSums(is.na(raw_data))

# 计算每列的缺失值数量
null_counts <- colSums(is.na(raw_data))

# 转换为数据框并按降序排列
null_df <- data.frame(
  Column = names(null_counts),
  NullCount = null_counts
) %>%
  arrange(desc(NullCount))

# 绘制log10刻度的缺失值柱状图
ggplot(null_df, aes(y = reorder(Column, NullCount), x = log10(NullCount + 1))) +
  geom_bar(stat = "identity") +
  labs(title = "Log10 Scale of Null Counts per Column (Descending)",
       x = "Log10(Null Count + 1)",
       y = "Columns") +
  theme_minimal()

# -------------------------------------
# 总行数
total_rows <- nrow(raw_data)

# 计算每列的缺失值数量
null_counts <- colSums(is.na(raw_data))

# 筛选缺失值超过 50% 的列
high_nulls <- data.frame(
  Column = names(null_counts),
  NullCount = null_counts,
  NullPercentage = (null_counts / total_rows) * 100
) %>%
  filter(NullPercentage > 50) %>%
  arrange(desc(NullPercentage))

# 查看结果
print(high_nulls)

# -----------
# 选择需要的变量
selected_vars <- c(
  "Department Name", 
  "InterventionDate", 
  "SubjectRaceCode", 
  "SubjectEthnicityCode", 
  "SubjectSexCode", 
  "SubjectAge", 
  "InterventionReasonCode", 
  "InterventionDispositionCode", 
  "VehicleSearchedIndicator", 
  "SearchAuthorizationCode", 
  "ContrabandIndicator", 
  "CustodialArrestIndicator",
)

# 创建 Subdataset
sub_data <- raw_data %>%
  select(all_of(selected_vars))

# 查看数据结构
glimpse(sub_data)
colSums(is.na(sub_data))

cleaned_data <- sub_data %>% na.omit()
dim(cleaned_data)
colSums(is.na(cleaned_data))

library(ggplot2)

# 种族分布柱状图
ggplot(cleaned_data, aes(x = SubjectRaceCode)) +
  geom_bar() +
  labs(title = "Distribution of Subject Race", x = "Race", y = "Count") +
  theme_minimal()

ggplot(cleaned_data, aes(x = SubjectRaceCode, fill = ifelse(VehicleSearchedIndicator %in% c("1", "TRUE"), "Searched", "Not Searched"))) +
  geom_bar(position = "fill") +
  labs(
    title = "Vehicle Search by Race (Proportion)",
    x = "Race",
    y = "Proportion",
    fill = "Vehicle Searched"
  ) +
  theme_minimal()

ggplot(cleaned_data, aes(x = SubjectAge, fill = SubjectRaceCode)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Race", x = "Age", y = "Count") +
  theme_minimal()
glimpse(cleaned_data)
library(dplyr)

# 计算每个种族的逮捕率
library(dplyr)

# 计算每个种族的逮捕率
arrest_rate_data <- cleaned_data %>%
  group_by(SubjectRaceCode) %>%
  summarise(
    TotalStops = n(),
    TotalArrests = sum(CustodialArrestIndicator %in% c("1", "TRUE"), na.rm = TRUE),
    ArrestRate = (TotalArrests / TotalStops) * 100
  )
library(ggplot2)


# 绘制逮捕率柱状图
ggplot(arrest_rate_data, aes(x = SubjectRaceCode, y = ArrestRate, fill = SubjectRaceCode)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", ArrestRate)), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Custodial Arrest Rate by Race",
    x = "Race Code",
    y = "Arrest Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

unique(cleaned_data$CustodialArrestIndicator)
# ------------------

library(ggplot2)

# 绘制分面柱状图
ggplot(cleaned_data, aes(x = SubjectRaceCode, fill = SubjectRaceCode)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ InterventionReasonCode, scales = "free_y") +
  labs(
    title = "Intervention Reasons by Race (Faceted)",
    x = "Race Code",
    y = "Count"
  ) +
  theme_minimal()
# ---------------------
# 计算搜查率
search_rate_data <- cleaned_data %>%
  group_by(SubjectRaceCode) %>%
  summarise(
    TotalStops = n(),
    TotalSearches = sum(VehicleSearchedIndicator %in% c("1", "TRUE"), na.rm = TRUE),
    SearchRate = (TotalSearches / TotalStops) * 100
  )

# 绘制搜查率柱状图
ggplot(search_rate_data, aes(x = SubjectRaceCode, y = SearchRate, fill = SubjectRaceCode)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = sprintf("%.1f%%", SearchRate)), vjust = -0.5, size = 5) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(
    title = "Vehicle Search Rate by Race",
    x = "Race Code",
    y = "Search Rate (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# ---------
ggplot(search_rate_data, aes(x = SubjectRaceCode, y = SearchRate)) +
  geom_point(size = 5, color = "blue") +
  geom_text(aes(label = sprintf("%.1f%%", SearchRate)), vjust = -1, size = 5) +
  labs(
    title = "Dot Plot of Vehicle Search Rate by Race",
    x = "Race Code",
    y = "Search Rate (%)"
  ) +
  theme_minimal()

# ----------
lapply(cleaned_data, unique)

# -------
# 提取需要的变量并创建新的子集
heatmap_data <- raw_data %>%
  select(SubjectRaceCode, StatuteReason, InterventionReasonCode, CustodialArrestIndicator)

# 查看前几行数据
head(heatmap_data)
lapply(heatmap_data, unique)
colSums(is.na(heatmap_data))
heatmap_data <- heatmap_data %>% na.omit()
dim(heatmap_data)
summary(heatmap_data)
library(dplyr)
library(ggplot2)

# 计算种族和拦截原因的频率
heatmap_plot_data <- heatmap_data %>%
  group_by(SubjectRaceCode, StatuteReason) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(SubjectRaceCode) %>%  # 重新按种族分组
  mutate(Proportion = Count / sum(Count) * 100)

# 查看数据
head(heatmap_plot_data)
ggplot(heatmap_plot_data, aes(x = StatuteReason, y = SubjectRaceCode, fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Proportion (%)") +
  labs(
    title = "Heatmap of Statutory Stop Reasons Across Racial Groups",
    x = "Statutory Reason",
    y = "Race"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


head(heatmap_plot_data)

ggplot(heatmap_plot_data, aes(x = SubjectRaceCode, y = StatuteReason, fill = Proportion)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.1f%%", Proportion)), size = 4, color = "black") +
  scale_fill_gradient(low = "#f7fbff", high = "#08306b", name = "Proportion (%)") +
  labs(
    title = "Heatmap of Statutory Stop Reasons Across Racial Groups",
    x = "Race",
    y = "Statutory Reason"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
head(heatmap_data)
ggplot(heatmap_data, aes(x = SubjectRaceCode)) +
  geom_bar() +
  labs(title = "Distribution of Subject Race", x = "Race", y = "Count") +
  theme_minimal()


library(dplyr)

# 计算种族数量和占比
race_proportion <- heatmap_data %>%
  group_by(SubjectRaceCode) %>%
  summarise(Count = n(), .groups = "drop") %>%
  mutate(Proportion = (Count / sum(Count)) * 100)

# 查看结果
print(race_proportion)
###install.packages("here")write_rds(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.rds"))

#  model

library(dplyr)

# 创建新数据集，统一变量
model_data_nb_lr <- cleaned_data %>%
  mutate(CustodialArrestIndicator = case_when(
    CustodialArrestIndicator %in% c("1", "TRUE") ~ "Arrested",
    CustodialArrestIndicator %in% c("0", "FALSE") ~ "Not Arrested",
    TRUE ~ NA_character_
  )) %>%
  na.omit() %>%
  mutate(CustodialArrestIndicator = as.factor(CustodialArrestIndicator))

# 查看数据基本信息
str(model_data_nb_lr)
library(e1071)
# 训练模型
nb_model <- naiveBayes(CustodialArrestIndicator ~ SubjectRaceCode + SubjectSexCode + InterventionReasonCode + SearchAuthorizationCode + ContrabandIndicator, 
                       data = model_data_nb_lr)

# 查看模型摘要
print(nb_model)
# 进行预测
nb_pred <- predict(nb_model, model_data_nb_lr)

# 生成混淆矩阵
nb_confusion_matrix <- table(Predicted = nb_pred, Actual = model_data_nb_lr$CustodialArrestIndicator)
print(nb_confusion_matrix)

# 计算准确率
nb_accuracy <- sum(diag(nb_confusion_matrix)) / sum(nb_confusion_matrix)
print(paste("Naive Bayes Accuracy:", round(nb_accuracy * 100, 2), "%"))




library(biglm)
# 确保所有因子变量正确编码
model_data_nb_lr <- model_data_nb_lr %>%
  mutate(across(where(is.character), as.factor))

# 构建 bigglm 模型
bigglm_model <- bigglm(
  CustodialArrestIndicator ~ SubjectRaceCode + SubjectSexCode + InterventionReasonCode + SearchAuthorizationCode + ContrabandIndicator,
  data = model_data_nb_lr,
  family = binomial()
)

summary(bigglm_model)
library(glmnet)
# 将因子变量转换为虚拟变量
model_data_matrix <- model.matrix(CustodialArrestIndicator ~ SubjectRaceCode + SubjectSexCode + InterventionReasonCode + SearchAuthorizationCode + ContrabandIndicator, 
                                  data = model_data_nb_lr)[, -1]

# 构建模型
logistic_model <- glm(CustodialArrestIndicator ~ ., data = model_data_nb_lr, family = binomial)

# 查看模型摘要
summary(logistic_model)


# 抽取 10% 数据
sample_data <- model_data_nb_lr %>% sample_frac(0.1, replace = FALSE)

# 重新建模
logistic_model <- glm(CustodialArrestIndicator ~ ., data = sample_data, family = binomial())
summary(logistic_model)