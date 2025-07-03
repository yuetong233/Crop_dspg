#Temperature using Remote sensing data 
library(shiny)
library(readr)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)

# Load and process temperature data
temp_data <- read_csv("temp.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(T_avg), !is.na(T_day), !is.na(T_night)) %>%
  mutate(year = year(date)) %>%
  pivot_longer(cols = c("T_day", "T_avg", "T_night"), 
               names_to = "type", values_to = "temperature")

# Label formatting
temp_data$type <- factor(temp_data$type,
                         levels = c("T_night", "T_avg", "T_day"),
                         labels = c("Nighttime Low", "Average Temp", "Daytime High"))

# UI
ui <- fluidPage(
  titlePanel("Daily Temperature Tracker: Virginia Corn Regions (2021–Present)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("About This Data"),
      p("This interactive area chart shows stacked daily temperature values over Virginia cornfields from 2021 through the present."),
      p("Temperature values were extracted from Google Earth Engine using MODIS datasets and aggregated for corn regions."),
      tags$ul(
        tags$li("Nighttime Low: Minimum temperature during the night (°C)"),
        tags$li("Average Temp: Mean daily temperature (°C)"),
        tags$li("Daytime High: Maximum temperature during the day (°C)")
      ),
      sliderInput("year_range", "Select Year Range:",
                  min = 2021, max = 2025, value = c(2021, 2025), sep = "")
    ),
    
    mainPanel(
      plotlyOutput("temp_plot", height = "550px")
    )
  )
)

# Server
server <- function(input, output) {
  filtered_data <- reactive({
    temp_data %>%
      filter(year >= input$year_range[1],
             year <= input$year_range[2])
  })
  
  output$temp_plot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = date, y = temperature, fill = type)) +
      geom_area(alpha = 0.8) +
      scale_fill_manual(values = c("Nighttime Low" = "#64b5f6",
                                   "Average Temp" = "#81c784",
                                   "Daytime High" = "#e57373")) +
      labs(title = "Stacked Daily Temperature by Category",
           x = "Date", y = "Temperature (°C)", fill = "Temperature Type") +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
