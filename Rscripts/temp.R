#Temperature for only corn counties from GEE 
#Last six months and Top ten counties

library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Load Top 10 helper
load_top10 <- function(year, type_label) {
  file <- paste0("Top10_Temp", type_label, "_", year, ".csv")
  read_csv(file, show_col_types = FALSE) %>%
    mutate(date = as.Date(date), year = year)
}

# Load all top 10 datasets into a list
top10_data <- list(
  "Average" = bind_rows(lapply(2021:2024, function(y) load_top10(y, "Avg"))),
  "High"    = bind_rows(lapply(2021:2024, function(y) load_top10(y, "High"))),
  "Low"     = bind_rows(lapply(2021:2024, function(y) load_top10(y, "Low")))
)

# UI
ui <- fluidPage(
  titlePanel("🌡️ MODIS Temperature Trends – Top 10 vs Recent 6 Months"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("source", "Select Data Source:",
                   choices = c("Top 10 Counties", "Recent 6 Months"),
                   selected = "Top 10 Counties"),
      
      selectInput("temp_type", "Temperature Type:",
                  choices = c("Average", "High", "Low"), selected = "Average"),
      
      selectInput("year", "Select Year:", choices = 2021:2025, selected = 2024),
      
      uiOutput("county_selector")
    ),
    
    mainPanel(
      plotlyOutput("temp_plot", height = "600px")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Load recent 6-month filtered file for selected year
  recent_data <- reactive({
    file <- paste0("Filtered_MODIS_Temp_", input$year, ".csv")
    read_csv(file, show_col_types = FALSE) %>%
      mutate(date = as.Date(date), year = year(date))
  })
  
  # Get the right dataset based on input$source
  selected_data <- reactive({
    if (input$source == "Top 10 Counties") {
      top10_data[[input$temp_type]] %>% filter(year == input$year)
    } else {
      recent_data()
    }
  })
  
  # Dynamically update counties
  output$county_selector <- renderUI({
    counties <- selected_data() %>%
      pull(county) %>%
      unique() %>%
      sort()
    
    selectInput("selected_counties", "Select Counties:",
                choices = counties,
                selected = head(counties, 3),
                multiple = TRUE)
  })
  
  # Plot output
  output$temp_plot <- renderPlotly({
    req(input$selected_counties)
    
    df <- selected_data() %>%
      filter(county %in% input$selected_counties)
    
    temp_var <- switch(input$temp_type,
                       "Average" = "T_avg",
                       "High" = "T_day",
                       "Low" = "T_night")
    
    # Debug-safe: check column exists
    if (!temp_var %in% names(df)) return(NULL)
    
    p <- ggplot(df, aes(x = date, y = .data[[temp_var]], color = county)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = rainbow(length(unique(df$county)))) +
      labs(
        title = paste(input$temp_type, "Temperature –", input$year),
        x = "Date", y = "Temperature (°C)", color = "County"
      ) +
      theme_minimal(base_size = 14)
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# Run app
shinyApp(ui, server)
