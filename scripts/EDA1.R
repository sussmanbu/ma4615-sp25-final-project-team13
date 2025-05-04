#  EDA  -------------接着clean——scrpt
clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
head(clean_data)
colSums(is.na(clean_data))
library(ggplot2)

ggplot(clean_data, aes(x = SubjectRaceCode)) +
  geom_bar() +
  labs(title = "Distribution of Subject Race", x = "Race", y = "Count") +
  theme_minimal()

ggplot(clean_data, aes(x = SubjectRaceCode)) +
  geom_bar(fill = "darkseagreen") +
  scale_x_discrete(labels = c(
    "W" = "White",
    "B" = "Black",
    "A" = "Asian",
    "I" = "American Indian"
  ))+
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

ggplot(clean_data, aes(x = SubjectAge, fill = SubjectRaceCode)) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Age Distribution by Race", x = "Age", y = "Count") +
  theme_minimal()
glimpse(cleaned_data)
# ----------

# Custodial Arrest Rate by Race 
arrest_rate_data <- clean_data|>
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


raw_data$StatuteReason

