# This file is purely as an example.
# Note, you may end up creating more than one cleaned data set and saving that
# to separate files in order to work on different aspects of your project

# Load required libraries
library(tidyverse)
library(here)

# Set the path to the raw data file
raw_data_path <- here("dataset-ignore", "raw_data.csv")

# Read the data, specifying certain columns as character type
raw_data <- read_csv(raw_data_path, col_types = cols(
  IsStatuteCodeValid = col_character(),
  IsStatutatoryCitationValid = col_character()
))

# Explore the data
glimpse(raw_data)
head(raw_data)

# Count missing values in each column
colSums(is.na(raw_data))
# Convert to data frame and sort by missing values in descending order
null_counts <- colSums(is.na(raw_data))
null_df <- data.frame(
  Column = names(null_counts),
  NullCount = null_counts
) |>
  arrange(desc(NullCount))
null_counts
# Plot: Bar chart of null counts per column (log10 scale)
ggplot(null_df, aes(y = reorder(Column, NullCount), x = log10(NullCount + 1))) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Log10 Scale of Null Counts per Column (Descending)",
       x = "Log10(Null Count + 1)",
       y = "Columns") +
  theme_minimal()

# Calculate total number of rows
total_rows <- nrow(raw_data)


# Create data frame with null percentage and filter columns with >50% nulls
null_counts <- colSums(is.na(raw_data))
high_nulls <- data.frame(
  Column = names(null_counts),
  NullCount = null_counts,
  NullPercentage = (null_counts / total_rows) * 100
) |>
  filter(NullPercentage > 0.5) |>
  arrange(desc(NullPercentage))
print(high_nulls)

#Unique
lapply(raw_data$InterventionIdentificationID,unique)
ls(raw_data)
raw_data$StatuteReason





# ------ Subset

selected_vars <- c(
  "InterventionDispositionCode", "CustodialArrestIndicator", "SubjectAge",
  "TowedIndicator", "InterventionDurationCode", "InterventionDateTime",
  "InterventionDate", "Month", "SubjectRaceCode", "SubjectEthnicityCode",
  "SubjectSexCode", "TownRecidentIndicator", "ResidentIndicator",
  "InterventionLocationName", "InterventionReasonCode", "InterventionTechniqueCode",
  "StatuteReason", "SearchAuthorizationCode", "ContrabandIndicator",
  "VehicleSearchedIndicator", "OrganizationIdentificationID", "Department Name"
)

selected_data <- raw_data |>
  select(all_of(selected_vars))
# 查看缺失值个数，并按数量降序排列
na_counts <- sapply(selected_data, function(x) sum(is.na(x)))
na_counts <- sort(na_counts, decreasing = TRUE)

# 打印结果
na_counts
na_summary <- data.frame(
  Variable = names(na_counts),
  MissingCount = as.numeric(na_counts)
)

print(na_summary)


clean_data <- selected_data|>
  na.omit()

# 看unique value

library(dplyr)
library(purrr)

clean_data %>%
  map(~ unique(.x))

# 个别的value修正
table(clean_data$SubjectEthnicityCode)
table(clean_data$TownRecidentIndicator)
table(clean_data$InterventionReasonCode)

clean_data <- clean_data %>%
  mutate(
    # 将 "m" 变为 "M"
    SubjectEthnicityCode = ifelse(SubjectEthnicityCode == "m", "M", SubjectEthnicityCode),
    
    # 将 "R" 替换为 1
    TownRecidentIndicator = ifelse(TownRecidentIndicator == "R", "TRUE", TownRecidentIndicator),
    
    # 将 "no" 替换为 NA
    InterventionReasonCode = ifelse(InterventionReasonCode == "no", NA, InterventionReasonCode)
  )
table(clean_data$SubjectEthnicityCode)
table(clean_data$TownRecidentIndicator)
table(clean_data$InterventionReasonCode)

# 通用转换函数 0 1 false true
to_logical <- function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    case_when(
      x %in% c("1", "TRUE") ~ TRUE,
      x %in% c("0", "FALSE") ~ FALSE,
      TRUE ~ NA
    )
  } else {
    x  # 如果已经是逻辑值就不动
  }
}

# 批量转换指定变量
clean_data <- clean_data %>%
  mutate(
    ContrabandIndicator = to_logical(ContrabandIndicator),
    VehicleSearchedIndicator = to_logical(VehicleSearchedIndicator),
    TownRecidentIndicator = to_logical(TownRecidentIndicator),
    ResidentIndicator = to_logical(ResidentIndicator),
    TowedIndicator = to_logical(TowedIndicator),
    CustodialArrestIndicator = to_logical(CustodialArrestIndicator)
  )

sapply(clean_data[c(
  "ContrabandIndicator", "VehicleSearchedIndicator", 
  "TownRecidentIndicator", "ResidentIndicator", 
  "TowedIndicator", "CustodialArrestIndicator"
)], table)

clean_data <- clean_data|>
  na.omit()

#再看unique值
clean_data %>%
  map(~ unique(.x))


write_rds(clean_data, here::here("dataset-ignore", "clean_selected_data.rds"))


#  EDA  -------------
library(ggplot2)

ggplot(cleaned_data, aes(x = SubjectRaceCode)) +
  geom_bar() +
  labs(title = "Distribution of Subject Race", x = "Race", y = "Count") +
  theme_minimal()

# not used part 
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
# ----------

# Custodial Arrest Rate by Race 
arrest_rate_data <- cleaned_data|>
  group_by(SubjectRaceCode)|>
  summarise(
    TotalStops = n(),
    TotalArrests = sum(CustodialArrestIndicator %in% c("1", "TRUE"), na.rm = TRUE),
    ArrestRate = (TotalArrests / TotalStops) * 100
  )
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


#  Intervention Reasons part 
ggplot(cleaned_data, aes(x = SubjectRaceCode, fill = SubjectRaceCode)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ InterventionReasonCode, scales = "free_y") +
  labs(
    title = "Intervention Reasons by Race (Faceted)",
    x = "Race Code",
    y = "Count"
  ) +
  theme_minimal()




# Search Rate Data
search_rate_data <- cleaned_data|>
  group_by(SubjectRaceCode) |>
  summarise(
    TotalStops = n(),
    TotalSearches = sum(VehicleSearchedIndicator %in% c("1", "TRUE"), na.rm = TRUE),
    SearchRate = (TotalSearches / TotalStops) * 100
  )
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

# dot graph of search Rate Data 
ggplot(search_rate_data, aes(x = SubjectRaceCode, y = SearchRate)) +
  geom_point(size = 5, color = "blue") +
  geom_text(aes(label = sprintf("%.1f%%", SearchRate)), vjust = -1, size = 5) +
  labs(
    title = "Dot Plot of Vehicle Search Rate by Race",
    x = "Race Code",
    y = "Search Rate (%)"
  ) +
  theme_minimal()

# see unique data 
lapply(cleaned_data, unique)


# heatmap
heatmap_data <- raw_data|> 
select(SubjectRaceCode, StatuteReason, InterventionReasonCode, CustodialArrestIndicator)
heatmap_data <- heatmap_data|>na.omit()
heatmap_plot_data <- heatmap_data|>
  group_by(SubjectRaceCode, StatuteReason)|>
  summarise(Count = n(), .groups = "drop") |>
  group_by(SubjectRaceCode) |> 
  mutate(Proportion = Count / sum(Count) * 100)
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
# another count base on the heatmap
ggplot(heatmap_data, aes(x = SubjectRaceCode)) +
  geom_bar() +
  labs(title = "Distribution of Subject Race", x = "Race", y = "Count") +
  theme_minimal()


race_proportion <- heatmap_data |>
  group_by(SubjectRaceCode) |>
  summarise(Count = n(), .groups = "drop") |>
  mutate(Proportion = (Count / sum(Count)) * 100)

print(race_proportion)
  
# another subset model can tr
model_data_nb_lr <- cleaned_data|>
  mutate(CustodialArrestIndicator = case_when(
    CustodialArrestIndicator %in% c("1", "TRUE") ~ "Arrested",
    CustodialArrestIndicator %in% c("0", "FALSE") ~ "Not Arrested",
    TRUE ~ NA_character_
  )) |>
  na.omit() |>
  mutate(CustodialArrestIndicator = as.factor(CustodialArrestIndicator))

str(model_data_nb_lr)
library(e1071)
nb_model <- naiveBayes(CustodialArrestIndicator ~ SubjectRaceCode + SubjectSexCode + InterventionReasonCode + SearchAuthorizationCode + ContrabandIndicator,
                       data = model_data_nb_lr)
# install.packages("here")write_rds(loan_data_clean, file = here::here("dataset", "loan_refusal_clean.rds"))

unique(raw_data$ InterventionLocationName)

