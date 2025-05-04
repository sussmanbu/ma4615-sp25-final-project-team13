# app.R
rm(list = ls()) 
library(shiny)
library(tidyverse)
library(plotly)
library(lubridate)
library(here)

#–– 1) Load the Shiny dataset and munge once at startup
df <- read_rds(here("dataset_for_shiny","shiny_stops.rds")) %>%
  
  mutate(
    InterventionDateTime = mdy_hms(InterventionDateTime),
    DayOfWeek = factor(
      weekdays(InterventionDateTime),
      levels = c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
    ),
    # Map race codes
    RaceFull = recode(SubjectRaceCode,
                      W="White", B="Black", A="Asian", I="American Indian"),
    SexFull  = recode(SubjectSexCode,
                      M="Male", F="Female", .default="Other")
  )

# Possible choices
race_choices   <- unique(df$RaceFull)
reason_choices <- unique(df$InterventionReasonCode)
dept_choices   <- sort(unique(df$`Department Name`))
sex_choices    <- c("All", unique(df$SexFull))
age_range      <- range(df$SubjectAge, na.rm = TRUE)

sliderInput("ageX", "Age Range:",
            min = age_range[1], max = age_range[2],
            value = age_range, sep = "")

metric_choices <- c("Arrest Rate"="ArrestPct",
                    "Search Rate"="SearchPct",
                    "Tow Rate"   ="TowedPct")

#–– 2) UI
ui <- navbarPage("Traffic‐Stop Explorer",
                 tabPanel("By Day of Week",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("race1","Race",   choices=race_choices, selected=race_choices),
                              radioButtons("metric1","Metric",    choices=metric_choices, selected="ArrestPct"),
                              selectInput("sex1","Sex",           choices=sex_choices, selected="All"),
                              sliderInput("age1","Age Range",     min=age_range[1], max=age_range[2],
                                          value=age_range, sep="")
                            ),
                            mainPanel(plotlyOutput("plotDay"))
                          )
                 ),
                 
                 tabPanel("By Stop Reason",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("race2","Race",   choices=race_choices, selected=race_choices),
                              radioButtons("metric2","Metric",    choices=metric_choices, selected="SearchPct"),
                              selectInput("sex2","Sex",           choices=sex_choices, selected="All"),
                              sliderInput("age2","Age Range",     min=age_range[1], max=age_range[2],
                                          value=age_range, sep="")
                            ),
                            mainPanel(plotlyOutput("plotReason"))
                          )
                 ),
                 
                 tabPanel("By Department",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("race3","Race",   choices=race_choices, selected=race_choices),
                              radioButtons("metric3","Metric",    choices=metric_choices, selected="ArrestPct"),
                              selectInput("sex3","Sex",           choices=sex_choices, selected="All"),
                              sliderInput("age3","Age Range",     min=age_range[1], max=age_range[2],
                                          value=age_range, sep=""),
                              selectInput("dept3","Department",   choices=dept_choices, selected=dept_choices[1])
                            ),
                            mainPanel(plotlyOutput("plotDept"))
                          )
                 ),
                 
                 tabPanel("Demographics",
                          sidebarLayout(
                            sidebarPanel(
                              checkboxGroupInput("race4","Race",   choices=race_choices, selected=race_choices),
                              selectInput("sex4","Sex",           choices=sex_choices, selected="All"),
                              sliderInput("age4","Age Range",     min=age_range[1], max=age_range[2],
                                          value=age_range, sep="")
                            ),
                            mainPanel(plotlyOutput("plotDemo"))
                          )
                 )
)

#–– 3) Server
server <- function(input, output, session) {
  
  # A helper to filter by common inputs
  filter_base <- function(df, sel_race, sel_sex, sel_age) {
    df %>%
      filter(RaceFull %in% sel_race,
             SubjectAge >= sel_age[1],
             SubjectAge <= sel_age[2],
             if(sel_sex!="All") SexFull==sel_sex else TRUE)
  }
  
  # Tab 1: Day of Week
  output$plotDay <- renderPlotly({
    df1 <- filter_base(df, input$race1, input$sex1, input$age1) %>%
      group_by(RaceFull, DayOfWeek) %>%
      filter(!is.na(DayOfWeek)) %>% # Filter out NA values for clarity
      summarize(
        Total    = n(),
        Arrests  = sum(CustodialArrestIndicator, na.rm=TRUE),
        Towed    = sum(TowedIndicator,           na.rm=TRUE),
        Searched = sum(VehicleSearchedIndicator, na.rm=TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        ArrestPct  = Arrests  / Total * 100,
        TowedPct   = Towed    / Total * 100,
        SearchPct  = Searched / Total * 100
      ) %>%
      pivot_longer(c(ArrestPct,TowedPct,SearchPct),
                   names_to="Event",values_to="Percent") %>%
      filter(Event==input$metric1)
    # <<< ADDED: Dynamic y-axis label and chart title based on selected metric
    metric_label <- switch(input$metric1,
                           ArrestPct = "Arrest Rate (%)",
                           TowedPct  = "Tow Rate (%)",
                           SearchPct = "Search Rate (%)")
    title_text <- paste0(metric_label, " by Day of Week and Race")
    
    p <- ggplot(df1, aes(x = DayOfWeek, y = Percent, color = RaceFull,
                         group = RaceFull,
                         text = paste0(RaceFull, " – ", DayOfWeek, ": ", round(Percent, 1), "%"))) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      labs(
        title = title_text,                    # <<< ADDED: Dynamic title
        subtitle = "Rates show the percentage of stops that resulted in the selected event, by race and day",
        x = "Day of Week",
        y = metric_label,                      # <<< MODIFIED: Use full label
        color = "Race"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # <<< ADDED
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  
  # Tab 2: Stop Reason
  output$plotReason <- renderPlotly({
    df2 <- filter_base(df, input$race2, input$sex2, input$age2) %>%
      group_by(RaceFull, InterventionReasonCode) %>%
      summarize(
        Total    = n(),
        Arrests  = sum(CustodialArrestIndicator, na.rm = TRUE),
        Towed    = sum(TowedIndicator,           na.rm = TRUE),
        Searched = sum(VehicleSearchedIndicator, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        ArrestPct  = Arrests  / Total * 100,
        TowedPct   = Towed    / Total * 100,
        SearchPct  = Searched / Total * 100
      ) %>%
      pivot_longer(c(ArrestPct, TowedPct, SearchPct),
                   names_to = "Event", values_to = "Percent") %>%
      filter(Event == input$metric2)
    
    # <<< ADDED: Full label and dynamic title
    metric_label <- switch(input$metric2,
                           ArrestPct = "Arrest Rate (%)",
                           TowedPct  = "Tow Rate (%)",
                           SearchPct = "Search Rate (%)")
    title_text <- paste0(metric_label, " by Stop Reason and Race")
    
    # <<< MODIFIED: Tooltip includes Race + Reason + Rate
    p <- ggplot(df2, aes(x = reorder(InterventionReasonCode, Percent),
                         y = Percent,
                         fill = RaceFull,
                         text = paste0(RaceFull, " – ", InterventionReasonCode, ": ", round(Percent, 1), "%"))) +
      geom_col(position = "dodge") +
      scale_x_discrete(labels = c("E" = "Equipment", "I" = "Investigative", "V" = "Violation"))+
      labs(
        title = title_text,  # <<< ADDED
        subtitle = "Each rate shows the percentage of stops with the selected outcome by reason and race",  # <<< ADDED
        x = "Stop Reason",
        y = metric_label,
        fill = "Race"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),  # <<< ADDED
        plot.subtitle = element_text(size = 11, hjust = 0.5),              # <<< ADDED
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
    
    ggplotly(p, tooltip = "text")
  })
  output$plotDept <- renderPlotly({
    df3 <- filter_base(df, input$race3, input$sex3, input$age3) %>%
      filter(`Department Name` == input$dept3) %>%
      group_by(RaceFull) %>%
      summarize(
        Total    = n(),
        Arrests  = sum(CustodialArrestIndicator, na.rm = TRUE),
        Towed    = sum(TowedIndicator,           na.rm = TRUE),
        Searched = sum(VehicleSearchedIndicator, na.rm = TRUE),
        .groups  = "drop"
      ) %>%
      mutate(
        ArrestPct  = Arrests  / Total * 100,
        TowedPct   = Towed    / Total * 100,
        SearchPct  = Searched / Total * 100
      ) %>%
      pivot_longer(c(ArrestPct, TowedPct, SearchPct),
                   names_to = "Event", values_to = "Percent") %>%
      filter(Event == input$metric3)
    
    metric_label <- switch(input$metric3,
                           ArrestPct = "Arrest Rate (%)",
                           TowedPct  = "Tow Rate (%)",
                           SearchPct = "Search Rate (%)")
    title_text <- paste0(metric_label, " by Race in Department: ", input$dept3)
    
    p <- ggplot(df3, aes(x = RaceFull, y = Percent, fill = RaceFull,
                         text = paste0(RaceFull, ": ", round(Percent, 1), "%"))) +
      geom_col() +
      labs(
        title = title_text,  # <<< ADDED
        subtitle = "Rate shows how often an event occurred out of all stops for each race in the selected department",  # <<< ADDED
        x = "Race",
        y = metric_label,
        fill = "Race"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
        plot.subtitle = element_text(size = 11, hjust = 0.5),
        axis.title = element_text(size = 12),
        legend.title = element_text(size = 12)
      )
    
    ggplotly(p, tooltip = "text")
  })
# Tab 4: Demographics (Age distribution)
output$plotDemo <- renderPlotly({
  df4 <- filter_base(df, input$race4, input$sex4, input$age4)

  p <- ggplot(df4, aes(x = SubjectAge, fill = RaceFull)) +
    geom_histogram(bins = 30, color = "white") +  
    geom_smooth(aes(y = ..count..), stat = "bin", bins = 30,
                method = "loess", se = FALSE, color = "gray30", linewidth = 0.8) + 
    facet_wrap(~RaceFull, scales = "free_y") +
    labs(
      title = "Age Distribution of Stopped Individuals by Race",
      subtitle = "Each panel shows the stop count distribution and smoothed trend line per race",
      x = "Age",
      y = "Stop Count",
      fill = "Race"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5),
      axis.title = element_text(size = 12),
      axis.title.x = element_text(size = 9),
      axis.title.y = element_text(size = 9)
    )

  ggplotly(p, tooltip = c("x", "y"))
})
}  # end server

#–– 4) Launch
shinyApp(ui, server)





