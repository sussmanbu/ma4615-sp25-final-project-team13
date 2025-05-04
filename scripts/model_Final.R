library(tidyverse)
library(broom)
library(ggplot2)
library(knitr)
library(here)

# 读取数据
clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))

# 清洗与转换
model_data <- clean_data %>%
  filter(
    !is.na(CustodialArrestIndicator),
    !is.na(SubjectAge),
    SubjectAge >= 15,
    SubjectAge <= 80
  ) %>%
  mutate(
    CustodialArrest = as.integer(CustodialArrestIndicator),  # 转为 0/1
    SubjectRaceCode = factor(SubjectRaceCode),
    SubjectRaceCode = relevel(SubjectRaceCode, ref = "W"),
    SubjectSexCode = factor(SubjectSexCode),
    InterventionDurationCode = factor(InterventionDurationCode,
                                      levels = c("1", "2", "3"),
                                      labels = c("0–15 mins", "16–30 mins", "Over 30 mins")),
    ResidentIndicator = factor(ResidentIndicator,
                               levels = c(FALSE, TRUE),
                               labels = c("Non-resident", "Resident"))
  )


logit_model <- glm(
  CustodialArrest ~ SubjectRaceCode + SubjectSexCode + SubjectAge +
    InterventionDurationCode + ResidentIndicator,
  data = model_data,
  family = binomial()
)


logit_table <- tidy(logit_model, exponentiate = TRUE) %>%
  filter(term != "(Intercept)") %>%
  select(term, estimate, std.error, p.value)

# 打印输出表格（保留标准列名）
kable(logit_table,  caption = "Logistic Regression: Odds Ratios, Standard Errors, and p-values")

# 创建 label（美化变量名）
logit_table <- logit_table %>%
  mutate(
    term_label = str_replace_all(term, "SubjectRaceCode", "Race: "),
    term_label = str_replace_all(term_label, "SubjectSexCode", "Sex: "),
    term_label = str_replace_all(term_label, "InterventionDurationCode", "Duration: "),
    term_label = str_replace_all(term_label, "ResidentIndicator", "Resident: ")
  )

# 绘图
ggplot(logit_table, aes(x = reorder(term_label, estimate), y = estimate)) +
  geom_point(size = 3, color = "steelblue") +
  coord_flip() +
  scale_y_log10() +  # 👈 添加这一行：对 Odds Ratio 使用 log10 变换
  labs(
    title = "Odds Ratios from Logistic Regression Model",
    subtitle = "Log-scaled axis (no confidence intervals)",
    x = "Predictor",
    y = "Odds Ratio (log scale)"
  ) +
  theme_minimal()