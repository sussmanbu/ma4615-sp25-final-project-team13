# Load required libraries
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(here)

# Load data
df <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
df$InterventionDateTime <- as.POSIXct(df$InterventionDateTime, format = "%m/%d/%Y %I:%M:%S %p")
df$DayOfWeek <- factor(weekdays(df$InterventionDateTime),
                       levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Mapping for full race names
race_labels <- c(
  "W" = "White",
  "B" = "Black",
  "A" = "Asian",
  "I" = "American Indian"
)
df$RaceFull <- race_labels[df$SubjectRaceCode]

# UI
ui <- fluidPage(
  titlePanel("Race-Based Event Analysis by Day of the Week"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selectedRaces", "Select Races:", 
                         choices = race_labels, 
                         selected = race_labels),
      checkboxGroupInput("selectedEvents", "Select Event Types:",
                         choices = c("Arrest" = "ArrestPct", "Towed" = "TowedPct", "Searched" = "SearchPct"),
                         selected = c("ArrestPct", "TowedPct", "SearchPct"))
    ),
    mainPanel(
      plotlyOutput("racePlot")
    )
  )
)

# Server
server <- function(input, output) {
  output$racePlot <- renderPlotly({
    filtered <- df %>% 
      filter(RaceFull %in% input$selectedRaces & !is.na(DayOfWeek))
    
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
                   names_to = "Event", values_to = "Percent") %>%
      filter(Event %in% input$selectedEvents)
    
    p <- ggplot(summary_df, aes(x = DayOfWeek, y = Percent, color = RaceFull,
                                linetype = Event, group = interaction(RaceFull, Event),
                                text = paste0("Race: ", RaceFull,
                                              "<br>Day: ", DayOfWeek,
                                              "<br>Event: ", Event,
                                              "<br>Percent: ", round(Percent, 2), "%"))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = c(
        "White" = "#1f77b4",
        "Black" = "#17becf",
        "Asian" = "#2ca02c",
        "American Indian" = "#9467bd"
      )) +
      scale_linetype_manual(values = c(
        "ArrestPct" = "solid",
        "TowedPct" = "dashed",
        "SearchPct" = "dotdash"
      )) +
      labs(
        title = "Percentage of Stops Resulting in an Event (By Race and Day of Week)",
        subtitle = "Each line represents percent of stops for a given race and day that resulted in an Arrest, Search, or Towing",
        x = "Day of the Week",
        y = "Percentage of Stops for Race",
        color = "Race",
        linetype = "Event Type"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 18),
        plot.subtitle = element_text(size = 13),
        legend.title = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "text")
  })
}

# Run app
shinyApp(ui = ui, server = server)
