# scripts/create_shiny_dataset.R

library(tidyverse)
library(here)
library(fs)

# 1) Read the full *traffic-stop* cleaned data
full <- read_rds(here("dataset-ignore", "clean_selected_data.rds"))

# 2) Drop any missing or implausible ages
#    We exclude ages below 15 (unlikely to drive) and above 80 (potential outliers or data errors)
full <- full %>%
  filter(!is.na(SubjectAge), SubjectAge >= 15, SubjectAge <= 80)

# 3) Subset only the columns our Shiny app needs
shiny_data <- full %>%
  select(
    SubjectRaceCode,
    SubjectSexCode,
    SubjectAge,
    InterventionReasonCode,
    `Department Name`,
    InterventionDateTime,
    CustodialArrestIndicator,
    TowedIndicator,
    VehicleSearchedIndicator
  )

# 4) Write out the smaller dataset as RDS for Shiny
fs::dir_create(here("dataset-ignore"))
write_rds(shiny_data, here("dataset-ignore", "shiny_stops.rds"))


# 5) Create a lightweight sample version for deployment
set.seed(2025)
light_shiny_data <- shiny_data %>% sample_n(700500)

write_rds(light_shiny_data, here("dataset-ignore", "light_shiny_stops.rds"))