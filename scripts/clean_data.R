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
  geom_bar(stat = "identity", fill = "cornflowerblue") +
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
na_counts <- sapply(selected_data, function(x) sum(is.na(x)))
na_counts <- sort(na_counts, decreasing = TRUE)


na_counts
na_summary <- data.frame(
  Variable = names(na_counts),
  MissingCount = as.numeric(na_counts)
)

print(na_summary)


clean_data <- selected_data|>
  na.omit()


library(dplyr)
library(purrr)

clean_data|>
  map(~ unique(.x))


table(clean_data$SubjectEthnicityCode)
table(clean_data$TownRecidentIndicator)
table(clean_data$InterventionReasonCode)

clean_data <- clean_data %>%
  mutate(
    SubjectEthnicityCode = ifelse(SubjectEthnicityCode == "m", "M", SubjectEthnicityCode),
    
    TownRecidentIndicator = ifelse(TownRecidentIndicator == "R", "TRUE", TownRecidentIndicator),
    
    InterventionReasonCode = ifelse(InterventionReasonCode == "no", NA, InterventionReasonCode)
  )
table(clean_data$SubjectEthnicityCode)
table(clean_data$TownRecidentIndicator)
table(clean_data$InterventionReasonCode)


to_logical <- function(x) {
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    case_when(
      x %in% c("1", "TRUE") ~ TRUE,
      x %in% c("0", "FALSE") ~ FALSE,
      TRUE ~ NA
    )
  } else {
    x  
  }
}


clean_data <- clean_data|>
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


clean_data|>
  map(~ unique(.x))


write_rds(clean_data, here::here("dataset-ignore", "clean_selected_data.rds"))
