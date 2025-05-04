# 加载必要的库
library(tidyverse)
library(broom)
library(ggplot2)
library(knitr)
library(lubridate)
library(here)
library(gt)


clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))

clean_data$InterventionDateTime <- as.POSIXct(
  clean_data$InterventionDateTime,
  format = "%m/%d/%Y %I:%M:%S %p"
)


model_data <- clean_data %>%
  filter(
    !is.na(CustodialArrestIndicator),
    !is.na(SubjectAge),
    SubjectAge >= 15,
    SubjectAge <= 80,
    !is.na(InterventionDateTime)
  ) %>%
  mutate(
    CustodialArrest = as.integer(CustodialArrestIndicator),
    
    SubjectRaceCode = factor(SubjectRaceCode,
                             levels = c("W", "B", "A", "I"),
                             labels = c("White", "Black", "Asian", "American Indian")),
    SubjectRaceCode = relevel(SubjectRaceCode, ref = "White"),
    
    SubjectSexCode = factor(SubjectSexCode,
                            levels = c("F", "M"),
                            labels = c("Female", "Male")),
    
    InterventionDurationCode = factor(InterventionDurationCode,
                                      levels = c("1", "2", "3"),
                                      labels = c("0–15 mins", "16–30 mins", "Over 30 mins")),
    
    ResidentIndicator = factor(ResidentIndicator,
                               levels = c(FALSE, TRUE),
                               labels = c("Non-resident", "Resident")),
    
    Hour = hour(InterventionDateTime)
  )


# Model 1 
logit_model <- glm(
  CustodialArrest ~ SubjectRaceCode * SubjectSexCode +
    SubjectAge + InterventionDurationCode +
    ResidentIndicator + Hour,
  data = model_data,
  family = binomial()
)

logit_table <- tidy(logit_model, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, std.error, p.value)

# 输出表格
kable(logit_table, digits = 3, caption = "Logistic Regression: Odds Ratios, Standard Errors, and p-values")



logit_table <- logit_table %>%
  mutate(
    term_label = str_replace_all(term, "SubjectRaceCode", "Race: "),
    term_label = str_replace_all(term_label, "SubjectSexCode", "Sex: "),
    term_label = str_replace_all(term_label, "InterventionDurationCode", "Duration: "),
    term_label = str_replace_all(term_label, "ResidentIndicator", "Resident: "),
    term_label = str_replace_all(term_label, "Hour", "Hour: "),
    term_label = str_replace_all(term_label, ":", " × ")  # 美化交互项显示
  )

ggplot(logit_table, aes(x = reorder(term_label, estimate), y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Odds Ratios from Logistic Regression Model",
    subtitle = "Log-scaled axis, labeled terms, includes interaction: Race × Sex",
    x = "Predictor",
    y = "Odds Ratio (log scale)"
  ) +
  theme_minimal()



logit_table <- logit_table %>%
  mutate(
    term_label = str_replace_all(term, "SubjectRaceCode", "Race: "),
    term_label = str_replace_all(term_label, "SubjectSexCode", "Sex: "),
    term_label = str_replace_all(term_label, "InterventionDurationCode", "Duration: "),
    term_label = str_replace_all(term_label, "ResidentIndicator", "Resident: "),
    term_label = str_replace_all(term_label, "Hour", "Hour: "),
    term_label = str_replace_all(term_label, ":", " × "),
    
    group = case_when(
      str_detect(term, "SubjectRaceCode") ~ "Race",
      str_detect(term, "SubjectSexCode") ~ "Sex",
      str_detect(term, "InterventionDurationCode") ~ "Duration",
      str_detect(term, "ResidentIndicator") ~ "Residency",
      str_detect(term, "SubjectAge") ~ "Age",
      str_detect(term, "Hour") ~ "Time",
      TRUE ~ "Other"
    )
  )


ggplot(logit_table, aes(x = reorder(term_label, estimate), y = estimate, color = group)) +
  geom_point(size = 3) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Odds Ratios from Logistic Regression Model",
    subtitle = "Log-scaled dot plot, grouped by variable type",
    x = "Predictor",
    y = "Odds Ratio (log scale)",
    color = "Variable Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )




library(gt)
library(dplyr)

# 添加“分组标签”列作为 stub 分块
logit_table_formatted <- logit_table %>%
  mutate(
    Group = case_when(
      str_detect(term, "SubjectRaceCode") ~ "Race",
      str_detect(term, "SubjectSexCode") ~ "Sex",
      str_detect(term, "InterventionDurationCode") ~ "Stop Duration",
      str_detect(term, "ResidentIndicator") ~ "Residency",
      str_detect(term, "SubjectAge") ~ "Age",
      str_detect(term, "Hour") ~ "Time",
      TRUE ~ "Other"
    ),
    # 为 term 设置更易读标签
    term = term %>%
      str_replace_all("SubjectRaceCode", "") %>%
      str_replace_all("SubjectSexCode", "") %>%
      str_replace_all("InterventionDurationCode", "") %>%
      str_replace_all("ResidentIndicator", "") %>%
      str_replace_all("SubjectAge", "Age") %>%
      str_replace_all("Hour", "Hour") %>%
      str_replace_all(":", " × ") %>%
      str_trim()
  ) %>%
  select(Group, term, estimate, std.error, p.value)

# 构建 gt 表格
logit_gt <- logit_table_formatted %>%
  gt(groupname_col = "Group", rowname_col = "term") %>%
  fmt_number(columns = c(estimate, std.error, p.value), decimals = 3) %>%
  tab_header(
    title = md("**Logistic Regression: Odds Ratios by Variable Group**")
  ) %>%
  cols_label(
    estimate = "Odds Ratio",
    std.error = "Std. Error",
    p.value = "p-value"
  ) 

logit_gt


# -----------------------------
# Step 1: 构建新模型（Searched）
# -----------------------------
logit_model_search <- glm(
  VehicleSearchedIndicator ~ SubjectRaceCode * SubjectSexCode +
    SubjectAge + InterventionDurationCode +
    ResidentIndicator + Hour,
  data = model_data,
  family = binomial()
)


logit_table_search <- tidy(logit_model_search, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Group = case_when(
      str_detect(term, "SubjectRaceCode") ~ "Race",
      str_detect(term, "SubjectSexCode") ~ "Sex",
      str_detect(term, "InterventionDurationCode") ~ "Stop Duration",
      str_detect(term, "ResidentIndicator") ~ "Residency",
      str_detect(term, "SubjectAge") ~ "Age",
      str_detect(term, "Hour") ~ "Time",
      TRUE ~ "Other"
    ),
    Variable = term %>%
      str_replace_all("SubjectRaceCode", "") %>%
      str_replace_all("SubjectSexCode", "") %>%
      str_replace_all("InterventionDurationCode", "") %>%
      str_replace_all("ResidentIndicator", "") %>%
      str_replace_all("SubjectAge", "Age") %>%
      str_replace_all("Hour", "Hour") %>%
      str_replace_all(":", " × ") %>%
      str_trim()
  ) %>%
  rename(
    `Odds Ratio` = estimate,
    `Std. Error` = std.error,
    `p-value` = p.value
  ) %>%
  select(Group, Variable, `Odds Ratio`, `Std. Error`, `p-value`)


logit_gt_search <- logit_table_search %>%
  gt(groupname_col = "Group", rowname_col = "Variable") %>%
  fmt_number(
    columns = c("Odds Ratio", "Std. Error", "p-value"),
    decimals = 3
  ) %>%
  tab_header(
    title = "Logistic Regression – Odds Ratios by Variable Group (Searched)"
  ) %>%
  cols_label(
    `Odds Ratio` = "Odds Ratio",
    `Std. Error` = "Std. Error",
    `p-value` = "p-value"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#e6f2ff")
    ),
    locations = cells_row_groups()
  )

logit_gt_search




# 准备绘图数据（你已有 logit_table_search）
logit_table_search <- logit_table_search %>%
  mutate(
    label = paste0(Group, ": ", Variable)
  )

# 绘制 dot plot
ggplot(logit_table_search, aes(x = reorder(label, `Odds Ratio`), y = `Odds Ratio`, color = Group)) +
  geom_point(size = 3) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Odds Ratios from Logistic Regression Model (Searched)",
    subtitle = "Log-scaled dot plot, grouped by variable type",
    x = "Predictor",
    y = "Odds Ratio (log scale)",
    color = "Variable Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )

# redo the model 1 

logit_model_arrest <- glm(
  CustodialArrest ~ SubjectRaceCode * SubjectSexCode +
    SubjectAge + InterventionDurationCode +
    ResidentIndicator + Hour,
  data = model_data,
  family = binomial()
)


logit_table_arrest <- tidy(logit_model_arrest, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    Group = case_when(
      str_detect(term, "SubjectRaceCode") ~ "Race",
      str_detect(term, "SubjectSexCode") ~ "Sex",
      str_detect(term, "InterventionDurationCode") ~ "Stop Duration",
      str_detect(term, "ResidentIndicator") ~ "Residency",
      str_detect(term, "SubjectAge") ~ "Age",
      str_detect(term, "Hour") ~ "Time",
      TRUE ~ "Other"
    ),
    Variable = term %>%
      str_replace_all("SubjectRaceCode", "") %>%
      str_replace_all("SubjectSexCode", "") %>%
      str_replace_all("InterventionDurationCode", "") %>%
      str_replace_all("ResidentIndicator", "") %>%
      str_replace_all("SubjectAge", "Age") %>%
      str_replace_all("Hour", "Hour") %>%
      str_replace_all(":", " × ") %>%
      str_trim()
  ) %>%
  rename(
    `Odds Ratio` = estimate,
    `Std. Error` = std.error,
    `p-value` = p.value
  ) %>%
  select(Group, Variable, `Odds Ratio`, `Std. Error`, `p-value`)



logit_gt_arrest <- logit_table_arrest %>%
  gt(groupname_col = "Group", rowname_col = "Variable") %>%
  fmt_number(
    columns = c("Odds Ratio", "Std. Error", "p-value"),
    decimals = 3
  ) %>%
  tab_header(
    title = "Logistic Regression – Odds Ratios by Variable Group (Custodial Arrest)"
  ) %>%
  cols_label(
    `Odds Ratio` = "Odds Ratio",
    `Std. Error` = "Std. Error",
    `p-value` = "p-value"
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold"),
      cell_fill(color = "#e6f2ff")
    ),
    locations = cells_row_groups()
  )

logit_gt_arrest

# Ensure logit_table_arrest is ready (from previous steps)
# Already includes: Group, Variable, Odds Ratio, etc.

logit_plot_data <- logit_table_arrest %>%
  mutate(
    label = paste0(Group, ": ", Variable)
  )
ggplot(logit_plot_data, aes(x = reorder(label, `Odds Ratio`), y = `Odds Ratio`, color = Group)) +
  geom_point(size = 3) +
  coord_flip() +
  scale_y_log10() +
  labs(
    title = "Odds Ratios from Logistic Regression Model (Custodial Arrest)",
    subtitle = "Log-scale dot plot by variable group",
    x = "Predictor",
    y = "Odds Ratio (log scale)",
    color = "Variable Group"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )



# ---- else best in big picture
# contrabnd
model_data %>%
  filter(!is.na(ContrabandIndicator)) %>%
  group_by(SubjectRaceCode) %>%
  summarise(ContrabandRate = mean(ContrabandIndicator)) %>%
  ggplot(aes(x = SubjectRaceCode, y = ContrabandRate)) +
  geom_col(fill = "darkorange") +
  labs(
    title = "Contraband Found Rate by Race",
    x = "Race",
    y = "Proportion with Contraband Found"
  ) +
  theme_minimal()

ggplot(model_data, aes(x = ContrabandIndicator, y = SubjectAge)) +
  geom_boxplot(outlier.shape = NA, fill = "lightblue", alpha = 0.6) +
  labs(
    title = "Subject Age by Contraband Found",
    x = "Contraband Found",
    y = "Subject Age"
  ) +
  theme_minimal()


#  # 加载所需库
library(tidyverse)
library(corrplot)

# Step 1: 选择相关变量
corr_data <- model_data %>%
  select(
    CustodialArrestIndicator,  # 是否逮捕
    ContrabandIndicator,       # 是否查获违禁品
    TowedIndicator,            # 是否拖车
    VehicleSearchedIndicator,  # 是否搜车
    SubjectAge,                # 年龄
    Hour                       # 拦截发生的时间（小时）
  ) %>%
  # Step 2: 将逻辑变量转为数值（TRUE -> 1, FALSE -> 0）
  mutate(across(everything(), as.numeric))

# Step 3: 计算相关系数矩阵（处理缺失值）
corr_matrix <- cor(corr_data, use = "pairwise.complete.obs")

# Step 4: 绘制相关性热图
corrplot(
  corr_matrix,
  method = "color",        # 使用彩色块
  type = "upper",          # 只显示上三角
  tl.col = "black",        # 标签颜色
  tl.srt = 45,             # 标签旋转角度
  addCoef.col = "black",   # 在方格上添加相关系数
  number.cex = 0.7,        # 相关系数字体大小
  col = colorRampPalette(c("red", "white", "blue"))(200)  # 色带
)