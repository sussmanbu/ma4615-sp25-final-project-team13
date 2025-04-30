library(dplyr)
library(corrplot)


cor_vars <- clean_data %>%
  select(
    VehicleSearchedIndicator,
    CustodialArrestIndicator,
    TowedIndicator,
    ContrabandIndicator,
    InterventionDurationCode,
    SubjectAge,
    ResidentIndicator,
    TownRecidentIndicator
  )

cor_vars <- mutate_all(cor_vars, as.numeric)


cor_matrix <- cor(cor_vars, use = "complete.obs")


corrplot(cor_matrix, method = "color", addCoef.col = "black", tl.col = "black")

library(ggcorrplot)

ggcorrplot(cor_matrix, hc.order = TRUE, type = "lower", lab = TRUE)

clean_data$InterventionDispositionCode

