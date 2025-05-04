library(dplyr)
library(ggcorrplot)
library(here)

# Step 1: Load and select relevant variables
clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))

cor_vars <- clean_data %>%
  select(
    VehicleSearchedIndicator,
    CustodialArrestIndicator,
    TowedIndicator,
    ContrabandIndicator,
    InterventionDurationCode,
    SubjectAge,
    ResidentIndicator
  ) %>%
  mutate(across(everything(), as.numeric))  # Convert logical/factor to numeric

# Step 2: Compute correlation matrix
cor_matrix <- cor(cor_vars, use = "complete.obs")

# Step 3: Plot with ggcorrplot
ggcorrplot(
  cor_matrix,
  hc.order = TRUE,
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c("red", "white", "blue"),
  title = "Correlation Heatmap of Enforcement-Related Variables",
  ggtheme = theme_minimal()
)