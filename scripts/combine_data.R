library(tidyverse)
library(here)
options(tigris_use_cache = TRUE)
# Set the path to the raw data file
raw_data_path <- here("dataset-ignore", "raw_data.csv")

# Read the data, specifying certain columns as character type
raw_data <- read_csv(raw_data_path, col_types = cols(
  IsStatuteCodeValid = col_character(),
  IsStatutatoryCitationValid = col_character()
))

glimpse(raw_data$InterventionLocationName)
library(stringr)

# Clean up location names (remove whitespace, force uppercase)
raw_data$InterventionLocationName <- str_to_upper(str_trim(raw_data$InterventionLocationName))

library(tigris)
library(sf)


# 1. Load Connecticut town boundaries using county_subdivisions (complete Town )
ct_towns <- county_subdivisions(state = "CT", cb = TRUE, year = 2020) |>
  st_as_sf()
# Clean InterventionLocationName 
library(stringr)

clean_data <- raw_data |>
  filter(!is.na(InterventionLocationName)) |>
  mutate(
    InterventionLocationName = str_to_upper(str_trim(InterventionLocationName)),
    TownMatched = InterventionLocationName |>
      str_remove_all('[[:punct:]]') |>
      str_squish()
  ) |>
  filter(
    !str_detect(TownMatched, "UNKNOWN|\\d|SCHOOL|STREET|ROAD|/|\\\\|\\*|\\?"),
    !TownMatched %in% c("", "1", "0", "NA")
  )

# 1. 加载 shapefile
ct_towns <- county_subdivisions(state = "CT", cb = TRUE, year = 2020) |>
  st_as_sf()
ct_towns$NAME <- str_to_upper(ct_towns$NAME)

# 2. 然后你才能做 unmatched 检查：
unmatched_before_alias <- clean_data |>
  filter(!(TownMatched %in% ct_towns$NAME)) |>
  count(TownMatched, sort = TRUE)
unmatched_before_alias

# Step 1: Define mapping from local name to official town name
alias_map <- tribble(
  ~from,           ~to,
  "STORRS",        "MANSFIELD",
  "WILLIMANTIC",   "WINDHAM",
  "GROTON CITY",   "GROTON",
  "TERRYVILLE",    "PLYMOUTH",
  "PAWCATUCK",     "STONINGTON",
  "UNIONVILLE",    "FARMINGTON",
  "PLANTSVILLE",   "SOUTHINGTON",
  "SANDY HOOK",    "NEWTOWN",
  "COS COB",       "GREENWICH",
  "RIVERSIDE",     "GREENWICH",
  "MYSTIC",        "STONINGTON",  # could also be Groton, depends
  "OAKVILLE",      "WATERTOWN",
  "MOOSUP",        "PLAINFIELD",
  "OLD MYSTIC",    "STONINGTON",
  "BYRAM",         "GREENWICH",
  "CENTRAL VILLAGE", "PLAINFIELD",
  "OLD GWCH",      "GREENWICH",
  "GLENVILLE",     "GREENWICH",
  "WINSTED",       "WINCHESTER",
  "SOUTHPORT",     "FAIRFIELD"
)

# Merge alias mapping
clean_data <- clean_data |>
  left_join(alias_map, by = c("TownMatched" = "from")) |>
  mutate(
    TownMatched = if_else(!is.na(to), to, TownMatched)
  )
# --- 6. Check unmatched after alias mapping ---
unmatched_after_alias <- clean_data |>
  filter(!(TownMatched %in% ct_towns$NAME)) |>
  count(TownMatched, sort = TRUE)

summary_data <- clean_data |>
  filter(TownMatched %in% ct_towns$NAME) |>   # ✅ 过滤掉假地名
  group_by(TownMatched) |>
  summarise(TotalStops = n(), .groups = "drop")
# --------- 
unmatched_raw <- clean_data |>
  filter(!(TownMatched %in% ct_towns$NAME))
unmatched_raw$InterventionLocationName

clean_data


# --------勘查summary_data
# 找出 summary_data 中仍然不在地图中的（理论上应该为 0）
summary_unmatched_check <- summary_data |>
  filter(!(TownMatched %in% ct_towns$NAME))

# 查看有多少条非法项（应该为 0）
nrow(summary_unmatched_check)

# 如有，显示出来
print(summary_unmatched_check)

# Join town shapefile with summarized stop data
ct_map_data <- ct_towns |>
  left_join(summary_data, by = c("NAME" = "TownMatched"))

# 5. Plot: Total Stop Counts by Town
library(scales)
library(viridis)

ggplot(ct_map_data) +
  geom_sf(aes(fill = TotalStops), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",        # or "magma", "viridis", "inferno"
    direction = 1,
    na.value = "gray90",
    name = "Stop Count"
  ) +
  labs(
    title = "Traffic Stop Count by Town in Connecticut",
    caption = "Data Source: CT Traffic Stop Records"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 10)),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 1)
  )


ggplot(ct_map_data) +
  geom_sf(aes(fill = TotalStops), color = "white", size = 0.1) +
  scale_fill_gradient(
    low = "lightyellow",
    high = "darkred",
    na.value = "gray90",
    name = "Stop Count"
  ) +
  labs(
    title = "Traffic Stop Count by Town in Connecticut",
    caption = "Data Source: CT Traffic Stop Records + TIGRIS"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )




# and then go back to draw that graph
# I also do the Hand-coded mapping for top 40 unmatched places, but it look the same as the graph for top 20, which m
# mean the top 20 cover the most unmatched



# combine data set explore

library(stringr)

race_raw <- read_csv("dataset/ACSST5Y2019_data.csv")
view(race_raw)

# Step 2: 提取 Total 行 + 只保留 Estimate 列（排除 Margin of Error）
total_row <- race_raw |>
  filter(`Label (Grouping)` == "Total:") |>
  select(matches("!!Estimate$"))

# 转置成 Town + PopTotal，并清洗 Town 名称
total_long <- pivot_longer(
  total_row,
  cols = everything(),
  names_to = "Town",
  values_to = "PopTotal"
) |>
  mutate(
    Town = str_to_upper(Town),
    Town = str_remove(Town, "!!ESTIMATE"),
    Town = str_remove(Town, ", CONNECTICUT"),
    Town = str_remove(Town, ",.*$"),         # 去掉 COUNTY 部分
    Town = str_remove(Town, " TOWN"),        # 去掉 " TOWN"
    Town = str_trim(Town),
    PopTotal = as.numeric(PopTotal)
  )
### Step 2: 加载康州地图 ====
ct_towns <- county_subdivisions(state = "CT", cb = TRUE, year = 2020) |>
  st_as_sf()

ct_towns$NAME <- str_to_upper(ct_towns$NAME)

### Step 5: 合并到地图数据 ====
ct_map_pop <- ct_towns |>
  left_join(total_long, by = c("NAME" = "Town"))

library(ggplot2)
library(viridis)  # 🌈 更好看的颜色方案

ggplot(ct_map_pop) +
  geom_sf(aes(fill = PopTotal), color = "gray80", size = 0.2) +
  scale_fill_viridis_c(
    option = "viridis",     # 可改为 "magma", "inferno", "viridis"
    na.value = "gray90",
    name = "Total Population"
  ) +
  labs(
    title = "Connecticut Population Distribution by Town",
    subtitle = "Based on 2015–2019 ACS 5-Year Estimates",
    caption = "Data Source: US Census Bureau"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.position = "right",
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )


# combine the summary 

# -----
------------------------
# Step A: Summarize traffic stop data by town
# -----------------------------
stop_summary <- clean_data |>
  filter(TownMatched %in% ct_towns$NAME) |>  # ✅ 只保留真实 Town
  group_by(TownMatched) |>
  summarise(
    TotalStops = n(),
    BlackStops = sum(SubjectRaceCode == "B", na.rm = TRUE),
    .groups = "drop"
  )
stop_summary
# -----------------------------
# Step B: 已经有 total_long（Town + PopTotal）来自人口数据
total_long <- total_long |>
  mutate(
    Town = str_to_upper(Town),
    Town = str_remove_all(Town, '[[:punct:]]'),
    Town = str_squish(Town)
  )
# -----------------------------
# 你的 total_long 是这样结构：
# Town           PopTotal
# "BRIDGEPORT"   145639
# "HARTFORD"     122105
total_long
# -----------------------------
# 查看人口数据（可选）
print(total_long)

# -----------------------------
# Step C: 合并 stop summary + population 数据
# -----------------------------
combined_summary <- stop_summary |>
  left_join(total_long, by = c("TownMatched" = "Town")) |>
  mutate(
    StopsPer10k = TotalStops / PopTotal * 10000,
    StopRateBlack = BlackStops / TotalStops
  )

# 查看合并结果（可选）
print(combined_summary)

# -----------------------------
# Step D: 合并地图数据（Spatial join）
# -----------------------------
ct_map_combined <- ct_towns |>
  left_join(combined_summary, by = c("NAME" = "TownMatched"))

# -----------------------------
# Step E: Plot - 每万人拦截率地图
# -----------------------------
library(ggplot2)
library(viridis)
library(scales)

ggplot(ct_map_combined) +
  geom_sf(aes(fill = StopsPer10k), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "magma",
    name = "Stops / 10,000",
    labels = comma,
    na.value = "gray90"
  ) +
  labs(
    title = "Standardized Traffic Stop Rate by Town in Connecticut",
    subtitle = "Stops per 10,000 Residents (2013–2019)",
    caption = "Data: CT Traffic Stop Records + ACS B02001"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 1)
  )


top10_stops <- combined_summary |>
  filter(!is.na(StopsPer10k)) |>
  arrange(desc(StopsPer10k)) |>
  slice_head(n = 10)

print(top10_stops)

glimpse(race_raw)

# OverRepIndex
valid_towns <- ct_towns$NAME |> unique()
# -------------------------------
# Step 1: 生成 stop summary（仅真实 Town）
# -------------------------------
stop_summary <- clean_data |>
  filter(TownMatched %in% valid_towns) |>  # ✅ 只保留真实 Town
  group_by(TownMatched) |>
  summarise(
    TotalStops = n(),
    BlackStops = sum(SubjectRaceCode == "B", na.rm = TRUE),
    .groups = "drop"
  )

# -------------------------------
# Step 2: 提取 ACS 中的总人口（Total:）并清洗
# -------------------------------
total_long <- race_raw |>
  filter(`Label (Grouping)` == "Total:") |>
  select(contains("!!Estimate")) |>
  pivot_longer(cols = everything(), names_to = "Town", values_to = "PopTotal") |>
  mutate(
    Town = str_to_upper(Town),
    Town = str_remove(Town, "!!ESTIMATE"),
    Town = str_remove(Town, ", CONNECTICUT"),
    Town = str_remove(Town, ",.*$"),
    Town = str_remove(Town, " TOWN"),
    Town = str_squish(Town),
    PopTotal = as.numeric(PopTotal)
  ) |>
  filter(Town %in% valid_towns)  # ✅ 保留真实 Town
total_long
# -------------------------------
# Step 3: 提取 Black 人口（模糊匹配 "Black"）
# -------------------------------
race_black <- race_raw |>
  filter(str_detect(`Label (Grouping)`, "Black or African American alone")) |>
  select(contains("!!Estimate")) |>
  pivot_longer(cols = everything(), names_to = "Town", values_to = "PopBlack") |>
  mutate(
    Town = str_to_upper(Town),
    Town = str_remove(Town, "!!ESTIMATE"),
    Town = str_remove(Town, ", CONNECTICUT"),
    Town = str_remove(Town, ",.*$"),
    Town = str_remove(Town, " TOWN"),
    Town = str_squish(Town),
    PopBlack = as.numeric(PopBlack)
  ) |>
  filter(Town %in% valid_towns)  # ✅ 保留真实 Town
# -------------------------------
# Step 4: 合并 stop summary + 人口数据，计算指标
# -------------------------------
combined_summary <- stop_summary |>
  left_join(total_long, by = c("TownMatched" = "Town")) |>
  left_join(race_black, by = c("TownMatched" = "Town")) |>
  mutate(
    StopsPer10k = TotalStops / PopTotal * 10000,
    StopRateBlack = BlackStops / TotalStops,
    PopRateBlack = PopBlack / PopTotal,
    OverRepIndex = StopRateBlack / PopRateBlack
  )
combined_summary
# 合并地图
ct_map_combined <- ct_towns |>
  left_join(combined_summary, by = c("NAME" = "TownMatched"))

# 画图
library(ggplot2)
library(viridis)
library(scales)

ggplot(ct_map_combined) +
  geom_sf(aes(fill = OverRepIndex), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log2",
    na.value = "gray90",
    name = "Over-rep Index",
    limits = c(0.25, 8),
    breaks = c(0.5, 1, 2, 4, 8),
    labels = c("0.5×", "1×", "2×", "4×", "8×"),
    oob = squish
  ) +
  labs(
    title = "Overrepresentation of Black Drivers in Traffic Stops",
    subtitle = "Ratio of Stop Rate vs Population Rate (by Town)",
    caption = "Data: CT Traffic Stop Records (2013–2019) + ACS B02001"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 1)
  )


# ---
# ✅ Step 1: capped OverRepIndex（防止极端值视觉主导）
ct_map_combined <- ct_map_combined |>
  mutate(
    OverRepIndexCapped = pmin(OverRepIndex, 4)  # cap at 4×
  )

# ✅ Step 2: 改进后的地图
ggplot(ct_map_combined) +
  geom_sf(aes(fill = OverRepIndexCapped), color = "white", size = 0.2) +
  scale_fill_viridis_c(
    option = "plasma",
    trans = "log2",  # 保持感知上的对数刻度
    na.value = "gray90",
    name = "Over-rep Index",
    limits = c(0.25, 4),    # capped 到 4 倍
    breaks = c(0.5, 1, 2, 4),
    labels = c("0.5×", "1× (parity)", "2×", "4×+"),
    oob = squish  # 将超出部分挤压进颜色范围
  ) +
  labs(
    title = "Disparity Between Traffic Stops and Population Share",
    subtitle = "Ratio of Black Driver Stop Rate to Population Rate (by Town)",
    caption = "Over-representation Index = Stop Rate ÷ Population Rate.\nValues > 1 suggest stops exceed population share.\nGray areas = Missing data. Data: CT Traffic Stop Records (2013–2019) + ACS B02001"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(size = 12, margin = margin(b = 8)),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0)
  )
# Step 5B: 散点图（StopRateBlack vs PopRateBlack）

ggplot(combined_summary, aes(x = PopRateBlack, y = StopRateBlack)) +
  geom_point(alpha = 0.6, color = "#440154") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray40") +
  geom_smooth(method = "lm", se = FALSE, color = "darkorange") +
  labs(
    title = "Stop Rate vs. Population Share (Black Residents)",
    subtitle = "Each point is a Connecticut town",
    x = "Proportion of Black Residents (ACS)",
    y = "Proportion of Stops of Black Drivers",
    caption = "Dotted = Parity | Orange = Linear Trend"
  ) +
  theme_minimal(base_size = 14)

top_overrep <- combined_summary |>
  filter(!is.na(OverRepIndex), OverRepIndex > 2) |>
  arrange(desc(OverRepIndex)) |>
  select(TownMatched, TotalStops, BlackStops, PopTotal, PopBlack, StopRateBlack, PopRateBlack, OverRepIndex)

print(top_overrep)

