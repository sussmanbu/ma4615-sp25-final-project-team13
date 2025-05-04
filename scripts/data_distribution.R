clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
head(clean_data)
colSums(is.na(clean_data))
library(ggplot2)
# Load data
df <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
df$InterventionDateTime <- as.POSIXct(df$InterventionDateTime, format = "%m/%d/%Y %I:%M:%S %p")
df$DayOfWeek <- factor(weekdays(df$InterventionDateTime),
                       levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Mapping for full race names
race_labels <- c(
  "W" = "White",
  "B" = "Black",
  "A" = "Asian",
  "I" = "American Indian"
)
df$RaceFull <- race_labels[df$SubjectRaceCode]

glimpse(df)


ggplot(df, aes(x = SubjectSexCode)) +
  geom_bar(fill = "coral") +
  labs(title = "Distribution of Subject Sex Code", x = "Sex", y = "Count") +
  scale_x_discrete(labels = c(
    "F" = "Female",
    "M" = "Male"
  ))+
  theme_minimal()

ggplot(df, aes(x = InterventionDurationCode)) +
  geom_bar(fill = "skyblue") +
  labs(
    title = "Distribution of Intervention Duration Code",
    x = "Duration Code",
    y = "Count",
    caption = "Note: Length of Stop — 1 = 0–15 mins, 2 = 16–30 mins, 3 = Over 30 mins"
  ) +
  theme_minimal()

library(dplyr)
library(lubridate)

df %>%
  mutate(Month = floor_date(InterventionDateTime, "month")) %>%
  group_by(Month) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Month, y = Count)) +
  geom_line(color = "purple") +
  geom_point(color = "#b39ddb")  +
  labs(title = "Monthly Stop Distribution", x = "Month", y = "Number of Stops") +
  theme_minimal()
# ---dataset 2

library(tidyverse)

# 假设你清洗后的总人口数据是这样结构：
# data frame: acs_clean with columns Town, PopTotal

library(stringr)

race_raw <- read_csv("dataset/ACSST5Y2019_data.csv")
view(race_raw)

# Step 2: 提取 Total 行 + 只保留 Estimate 列（排除 Margin of Error）
total_row <- race_raw |>
  filter(`Label (Grouping)` == "Total:") |>
  select(matches("!!Estimate$"))

# 选择“Total”行（即每个城镇的总人口行），只保留“Estimate”列
acs_total <- race_raw %>%
  filter(`Label (Grouping)` == "Total:") %>%
  select(contains("!!Estimate")) %>%
  pivot_longer(cols = everything(), names_to = "Town", values_to = "PopTotal") %>%
  mutate(
    Town = str_remove(Town, "!!Estimate"),
    Town = str_remove(Town, ", CONNECTICUT"),
    Town = str_remove(Town, ",.*$"),
    Town = str_remove(Town, " TOWN"),
    Town = str_to_upper(str_squish(Town)),
    PopTotal = as.numeric(PopTotal)
  )

acs_total_top <- acs_total %>%
  filter(Town != "CONNECTICUT") %>%  # 去除合计项
  arrange(desc(PopTotal)) %>%
  slice_head(n = 30)

ggplot(acs_total_top, aes(x = reorder(Town, PopTotal), y = PopTotal)) +
  geom_col(fill = "orchid") +
  coord_flip() +
  labs(
    title = "Top 30 Connecticut Towns by Total Population (Excluding State Total)",
    x = "Town",
    y = "Population"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(labels = scales::comma)

# 过滤“Black or African American alone”这一行
acs_black <- race_raw %>%
  filter(str_detect(`Label (Grouping)`, "Black or African American alone")) %>%
  select(contains("!!Estimate")) %>%
  pivot_longer(cols = everything(), names_to = "Town", values_to = "PopBlack") %>%
  mutate(
    Town = str_remove(Town, "!!Estimate"),
    Town = str_remove(Town, ", CONNECTICUT"),
    Town = str_remove(Town, ",.*$"),
    Town = str_remove(Town, " TOWN"),
    Town = str_to_upper(str_squish(Town)),
    PopBlack = as.numeric(PopBlack)
  )

# 画出前 30 个 Black 人口最多的城镇
acs_black_top <- acs_black %>%
  arrange(desc(PopBlack)) %>%
  slice_head(n = 30)

ggplot(acs_black_top, aes(x = reorder(Town, PopBlack), y = PopBlack)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 30 Towns by Black Population",
    x = "Town",
    y = "Black Population"
  ) +
  theme_minimal(base_size = 13) +
  scale_y_continuous(labels = scales::comma)

colnames(race_raw)

library(tidyverse)
library(stringr)

# 读取原始 ACS 数据
race_raw <- read_csv("dataset/ACSST5Y2019_data.csv")

library(tidyverse)
library(stringr)

# 读取原始数据
race_raw <- read_csv("dataset/ACSST5Y2019_data.csv")

# 提取我们需要的种族，并归类为五大类
acs_race <- race_raw %>%
  filter(str_detect(`Label (Grouping)`, "alone|Two or more races|Some other race")) %>%
  mutate(
    Race = case_when(
      str_detect(`Label (Grouping)`, "White alone") ~ "White",
      str_detect(`Label (Grouping)`, "Black or African American alone") ~ "Black",
      str_detect(`Label (Grouping)`, "Asian alone") ~ "Asian",
      str_detect(`Label (Grouping)`, "American Indian and Alaska Native alone") ~ "American Indian",
      TRUE ~ "Other"
    )
  ) %>%
  select(Race, contains("!!Estimate")) %>%  # ✅ 正确抓取 Estimate 列
  pivot_longer(cols = -Race, names_to = "Town", values_to = "Count") %>%
  mutate(
    Town = str_remove(Town, "!!Estimate"),
    Town = str_remove(Town, ", CONNECTICUT"),
    Town = str_remove(Town, ",.*$"),
    Town = str_remove(Town, " TOWN"),
    Town = str_to_upper(str_squish(Town)),
    Count = as.numeric(Count)
  )
# 计算每个镇的总人口
town_pop <- acs_race %>%
  group_by(Town) %>%
  summarise(PopTotal = sum(Count, na.rm = TRUE), .groups = "drop")


top20_towns <- town_pop %>%
  arrange(desc(PopTotal)) %>%
  slice_head(n = 20) %>%
  pull(Town)
acs_race_top <- acs_race %>%
  filter(Town %in% top20_towns) %>%
  inner_join(town_pop, by = "Town") %>%
  group_by(Town) %>%
  mutate(Proportion = Count / PopTotal) %>%
  ungroup()
custom_colors <- c(
  "White" = "#A6CEE3",           # light blue
  "Black" = "#1F78B4",           # dark blue
  "Asian" = "#33A02C",           # green
  "American Indian" = "#FB9A99", # light red
  "Other" = "#B2DF8A"            # soft green
)
ggplot(acs_race_top, aes(x = reorder(Town, -PopTotal), y = Proportion, fill = Race)) +
  geom_col(width = 0.9) +
  coord_flip() +
  labs(
    title = "Racial Composition of Top 20 Connecticut Towns",
    subtitle = "Based on 2015–2019 ACS 5-Year Estimates",
    x = "Town",
    y = "Proportion of Population"
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = custom_colors) +  # ✅ 自定义颜色
  theme_minimal(base_size = 13) +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(face = "bold"),
    legend.title = element_blank()
  )