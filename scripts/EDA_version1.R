library(lubridate)
library(tidyverse)
clean_data <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
head(clean_data)
colSums(is.na(clean_data))

head(clean_data$InterventionDateTime, 10)

InterventionDateTime_parsed <- mdy_hms(clean_data$InterventionDateTime)


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




clean_data_time <- clean_data_time %>%
  mutate(
    Race = clean_data$SubjectRaceCode,
    YearMonth = format(InterventionDateTime, "%Y-%m")  # e.g., "2014-01"
  )


monthly_stops <- clean_data_time %>%
  count(YearMonth)


monthly_stops$YearMonth <- as.Date(paste0(monthly_stops$YearMonth, "-01"))
monthly_stops <- monthly_stops[order(monthly_stops$YearMonth), ]


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

clean_data_time <- clean_data_time %>%
  mutate(
    Race = clean_data$SubjectRaceCode,
    YearMonth = format(InterventionDateTime, "%Y-%m")
  )


monthly_race_stops <- clean_data_time %>%
  count(YearMonth, Race)


monthly_race_stops$YearMonth <- as.Date(paste0(monthly_race_stops$YearMonth, "-01"))


monthly_race_stops <- monthly_race_stops[order(monthly_race_stops$YearMonth), ]


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
  geom_line(linewidth = 0.5, alpha = 0.5, color = "grey60") +  
  geom_smooth(method = "loess", se = FALSE, color = "steelblue", linewidth = 1.2) +  
  facet_wrap(~ Race, scales = "free_y") +
  labs(
    title = "Smoothed Monthly Traffic Stops by Race",
    x = "Month (Year)",
    y = "Number of Stops"
  ) +
  theme_minimal()




binary_data <- clean_data %>%
  select(VehicleSearchedIndicator, CustodialArrestIndicator, 
         TowedIndicator, ContrabandIndicator)

binary_numeric <- binary_data * 1


cor_matrix <- cor(binary_numeric, use = "complete.obs")
print(cor_matrix)
library(corrplot)

corrplot::corrplot(
  cor_matrix,               
  method = "circle",        
  type = "lower",          
  addCoef.col = "black",    
  tl.col = "black",         
  tl.srt = 45,              
  diag = FALSE             
)



library(car)

model <- glm(CustodialArrestIndicator ~ VehicleSearchedIndicator +
               TowedIndicator + ContrabandIndicator,
             data = clean_data, family = "binomial")

vif(model)


hourly_arrest <- clean_data_time %>%
  group_by(Hour) %>%
  summarise(ArrestRate = mean(CustodialArrestIndicator, na.rm = TRUE), .groups = "drop")


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