library(shiny)
library(ggplot2)
library(dplyr)
library(scales)

# --- UI ---
ui <- fluidPage(
  titlePanel("Disposition Type Distribution by Race"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_race", "Choose a Race:",
                  choices = c("W", "B", "A", "I"),
                  selected = "B")
    ),
    mainPanel(
      plotOutput("dispositionPlot")
    )
  )
)

# --- Server ---
server <- function(input, output, session) {
  # Load your data (replace with your actual data)
  df <- readRDS(here::here("dataset-ignore", "clean_selected_data.rds"))
  
  # Map disposition code to label (you already have this)
  code_labels <- c(
    "V" = "Verbal Warning",
    "W" = "Written Warning",
    "I" = "Infraction",
    "M" = "Misdemeanor",
    "U" = "Arrest",
    "N" = "None"
  )
  
  df <- df %>%
    filter(!is.na(InterventionDispositionCode)) %>%
    mutate(
      DispositionLabel = recode(InterventionDispositionCode, !!!code_labels),
      DispositionLabel = factor(DispositionLabel,
                                levels = c("Verbal Warning", "Written Warning", "Infraction", "Misdemeanor", "Arrest", "None"))
    )
  
  output$dispositionPlot <- renderPlot({
    df %>%
      filter(SubjectRaceCode == input$selected_race) %>%
      count(DispositionLabel) %>%
      mutate(prop = n / sum(n)) %>%
      ggplot(aes(x = DispositionLabel, y = prop, fill = DispositionLabel)) +
      geom_col() +
      geom_text(aes(label = percent(prop, accuracy = 0.1)), vjust = -0.2, size = 4) +
      labs(
        title = paste("Disposition Distribution for Race:", input$selected_race),
        x = "Disposition Type",
        y = "Proportion"
      ) +
      scale_y_continuous(labels = percent_format()) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 20, hjust = 1),
        legend.position = "none"
      )
  })
}

# --- Run App ---
shinyApp(ui, server)


