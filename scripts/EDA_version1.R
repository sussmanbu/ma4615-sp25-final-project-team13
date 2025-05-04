library(lubridate)
library(tidyverse)
clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
head(clean_data)
colSums(is.na(clean_data))

head(clean_data$InterventionDateTime, 10)
# 转换 InterventionDateTime 格式
InterventionDateTime_parsed <- mdy_hms(clean_data$InterventionDateTime)

# 创建新的数据框
clean_data_time <- data.frame(
  InterventionDateTime = InterventionDateTime_parsed,
  Year = year(InterventionDateTime_parsed),
  Month = month(InterventionDateTime_parsed, label = TRUE, abbr = TRUE),
  Weekday = wday(InterventionDateTime_parsed, label = TRUE, abbr = TRUE),
  Hour = hour(InterventionDateTime_parsed),
  TimeOfDay = ifelse(hour(InterventionDateTime_parsed) < 6, "Night",
                     ifelse(hour(InterventionDateTime_parsed) < 12, "Morning",
                            ifelse(hour(InterventionDateTime_parsed) < 18, "Afternoon", "Evening")))
)

clean_data_time

#Density Plot – Time of day distribution by race
ggplot(clean_data_time, aes(x = Hour, color = clean_data$SubjectRaceCode)) +
  geom_density(linewidth = 1) +
  labs(
    title = "Time of Day Distribution by Race",
    x = "Hour of Day",
    y = "Density",
    color = "Race"
  ) +
  theme_minimal()

ggplot(clean_data_time, aes(x = Hour, color = clean_data$SubjectRaceCode)) +
  geom_density(linewidth = 1, aes(group = clean_data$SubjectRaceCode), adjust = 1.5) +
  labs(
    title = "Time of Day Distribution by Race (Normalized per Group)",
    x = "Hour of Day",
    y = "Density",
    color = "Race"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold")
  )

clean_data_time$Race <- clean_data$SubjectRaceCode
normalized_data <- clean_data_time %>%
  count(Race, Hour) %>%
  group_by(Race) %>%
  mutate(Proportion = n / sum(n))

ggplot(normalized_data, aes(x = Hour, y = Proportion, color = Race)) +
  geom_line(size = 1.2) +
  labs(
    title = "Normalized Hourly Distribution by Race",
    y = "Proportion within Race"
  )

ggplot(clean_data_time, aes(x = Hour)) +
  geom_bar() +
  facet_wrap(~ Race, scales = "free_y") +
  labs(
    title = "Hourly Stop Count by Race (each facet independently scaled)",
    x = "Hour", y = "Count"
  )



# 创建一个 year-month 列作为时间序列 key
clean_data_time <- clean_data_time %>%
  mutate(
    Race = clean_data$SubjectRaceCode,
    YearMonth = format(InterventionDateTime, "%Y-%m")  # e.g., "2014-01"
  )

# 按 YearMonth 统计拦截数量（可加 group_by(Race) 做对比）
monthly_stops <- clean_data_time %>%
  count(YearMonth)

# 确保顺序正确（按时间排序）
monthly_stops$YearMonth <- as.Date(paste0(monthly_stops$YearMonth, "-01"))
monthly_stops <- monthly_stops[order(monthly_stops$YearMonth), ]

# 折线图
ggplot(monthly_stops, aes(x = YearMonth, y = n)) +
  geom_line(linewidth = 1.2, color = "steelblue") +
  labs(
    title = "Monthly Traffic Stop Trends (All Races)",
    x = "Month",
    y = "Number of Stops"
  ) +
   geom_smooth(method = "loess", se = FALSE)+
  theme_minimal()

# for each race ---------
# 添加种族和 YearMonth 列
clean_data_time <- clean_data_time %>%
  mutate(
    Race = clean_data$SubjectRaceCode,
    YearMonth = format(InterventionDateTime, "%Y-%m")
  )

# 汇总每个种族每个月的拦截数
monthly_race_stops <- clean_data_time %>%
  count(YearMonth, Race)

# 转换 YearMonth 为日期类型用于排序
monthly_race_stops$YearMonth <- as.Date(paste0(monthly_race_stops$YearMonth, "-01"))

# 排序
monthly_race_stops <- monthly_race_stops[order(monthly_race_stops$YearMonth), ]

# 画图：每个种族一个子图（facet）
ggplot(monthly_race_stops, aes(x = YearMonth, y = n)) +
  geom_line(linewidth = 1.1, color = "steelblue") +
  facet_wrap(~ Race, scales = "free_y") +
  labs(
    title = "Monthly Traffic Stops by Race",
    x = "Month (Year)",
    y = "Number of Stops"
  ) +
  theme_minimal()


# Another way  good good
ggplot(monthly_race_stops, aes(x = YearMonth, y = n)) +
  geom_line(linewidth = 0.5, alpha = 0.5, color = "grey60") +  # 原始曲线（淡淡的灰色）
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 1.2) +  # 平滑曲线
  facet_wrap(~ Race, scales = "free_y") +
  labs(
    title = "Smoothed Monthly Traffic Stops by Race",
    x = "Month (Year)",
    y = "Number of Stops"
  ) +
  theme_minimal()



# 选取子数据
binary_data <- clean_data %>%
  select(VehicleSearchedIndicator, CustodialArrestIndicator, 
         TowedIndicator, ContrabandIndicator)

# 转换为 0/1（逻辑变量变成数值型）
binary_numeric <- binary_data * 1

# 计算 Pearson 相关矩阵
cor_matrix <- cor(binary_numeric, use = "complete.obs")
print(cor_matrix)
library(corrplot)

corrplot::corrplot(
  cor_matrix,               # 相关矩阵
  method = "circle",        # 圆圈表示强度（也可以换成 "color", "number" 等）
  type = "lower",           # 只显示下三角
  addCoef.col = "black",    # 添加相关系数文字
  tl.col = "black",         # 标签颜色
  tl.srt = 45,              # 标签旋转角度
  diag = FALSE              # 不显示对角线
)



library(car)
# 构建线性模型（以其中一个变量为响应变量，剩下做解释变量）
model <- glm(CustodialArrestIndicator ~ VehicleSearchedIndicator +
               TowedIndicator + ContrabandIndicator,
             data = clean_data, family = "binomial")

vif(model)


hourly_arrest <- clean_data_time %>%
  group_by(Hour) %>%
  summarise(ArrestRate = mean(CustodialArrestIndicator, na.rm = TRUE), .groups = "drop")

# 画线图
ggplot(hourly_arrest, aes(x = Hour, y = ArrestRate)) +
  geom_line(color = "#E41A1C", size = 1.2) +
  geom_point(color = "#E41A1C", size = 2) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(
    title = "Hourly Custodial Arrest Rate",
    x = "Hour of Day",
    y = "Arrest Rate"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
ggplot(clean_data_time, aes(x = Hour, y = as.numeric(CustodialArrestIndicator))) +
  geom_smooth(method = "loess", se = FALSE, color = "#E41A1C") +
  labs(
    title = "Trend of Custodial Arrests by Hour of Day",
    x = "Hour of Day",
    y = "Probability of Arrest"
  ) +
  theme_minimal()



hourly_arrest <- clean_data_time %>%
  group_by(Hour) %>%
  summarise(ArrestRate = mean(CustodialArrestIndicator, na.rm = TRUE))

ggplot(hourly_arrest, aes(x = Hour, y = ArrestRate)) +
  geom_col(fill = "#377EB8") +
  labs(
    title = "Hourly Custodial Arrest Rate",
    x = "Hour of Day",
    y = "Arrest Rate"
  ) +
  theme_minimal()