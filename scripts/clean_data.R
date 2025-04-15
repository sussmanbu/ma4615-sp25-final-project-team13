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
  filter(NullPercentage > 0.05) |>
  arrange(desc(NullPercentage))
print(high_nulls)

unique_values <- lapply(raw_data, unique)



# 计算唯一值
unique_values <- lapply(raw_data, unique)

# 转换为整洁的数据框
unique_df <-data.frame(
  datatable(unique_df) )
  
  
#转化 InterventionDateTime 格式
library(lubridate)

raw_data <- raw_data |>
  filter(!is.na(InterventionDateTime))
# 转换日期时间（自动识别 AM/PM）
raw_data$InterventionDateTime <- mdy_hms(raw_data$InterventionDateTime)

head(raw_data$InterventionDateTime, 100)

raw_data <- raw_data |>
  mutate(
    InterventionDate = as_date(InterventionDateTime),      # Only date
    InterventionYear = year(InterventionDateTime),
    InterventionMonth = month(InterventionDateTime, label = TRUE),
    InterventionWeekday = wday(InterventionDateTime, label = TRUE),
    InterventionHour = hour(InterventionDateTime)
  )




unique(raw_data$Day of Week)


# ------ Subset

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
  "CustodialArrestIndicator"
)


sub_data <- raw_data |>
  select(all_of(selected_vars))


glimpse(sub_data)
#view(sub_data)

colSums(is.na(sub_data))

cleaned_data <- sub_data|> na.omit()
dim(cleaned_data)
colSums(is.na(cleaned_data))

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

