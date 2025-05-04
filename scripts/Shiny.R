# Load required libraries
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(here)

# Load and process data
df <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
df$InterventionDateTime <- as.POSIXct(df$InterventionDateTime, format = "%m/%d/%Y %I:%M:%S %p")
df$DayOfWeek <- factor(weekdays(df$InterventionDateTime), 
                       levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
df <- df %>% filter(!is.na(DayOfWeek))

# Map race codes to full names
race_labels <- c("W" = "White", "B" = "Black", "A" = "Asian", "I" = "American Indian")
df$RaceFull <- recode(df$SubjectRaceCode, !!!race_labels)

# UI
ui <- fluidPage(
  titlePanel("Event Percentages by Race and Day of the Week"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selectedRaces", "Select Races (W=White, B=Black, A=Asian, I=American Indian):", 
                         choices = race_labels, 
                         selected = race_labels)
    ),
    mainPanel(
      plotlyOutput("racePlot"),
      helpText("This chart shows, for each race, the percentage of stops that resulted in arrest, towing, or search on each day of the week.",
               "For example, for Black drivers on Monday, the arrest percentage is: number of Black drivers arrested on Monday / number of Black drivers stopped on Monday.")
    )
  )
)

# Server
server <- function(input, output) {
  output$racePlot <- renderPlotly({
    filtered <- df %>% filter(RaceFull %in% input$selectedRaces)
    
    summary_df <- filtered %>%
      group_by(RaceFull, DayOfWeek) %>%
      summarise(
        Total = n(),
        Arrests = sum(CustodialArrestIndicator == TRUE, na.rm = TRUE),
        Towed = sum(TowedIndicator == TRUE, na.rm = TRUE),
        Searched = sum(VehicleSearchedIndicator == TRUE, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        ArrestPct = Arrests / Total * 100,
        TowedPct = Towed / Total * 100,
        SearchPct = Searched / Total * 100
      ) %>%
      pivot_longer(cols = c(ArrestPct, TowedPct, SearchPct), 
                   names_to = "Event", values_to = "Percent")
    
    p <- ggplot(summary_df, aes(x = DayOfWeek, y = Percent, color = RaceFull,
                                linetype = Event, group = interaction(RaceFull, Event),
                                text = paste0("Race: ", RaceFull,
                                              "<br>Day: ", DayOfWeek,
                                              "<br>Event: ", Event,
                                              "<br>Percent: ", round(Percent, 1), "%"))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = "Percentage of Stops Resulting in Each Event by Race and Day",
        x = "Day of the Week",
        y = "Percentage of Stops (%)",
        color = "Race",
        linetype = "Event"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
}

# Run app
shinyApp(ui = ui, server = server)