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

#By County 
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Load and preprocess data
temp_2021 <- read_csv("MODIS_Temp_Weekly_VA_2021.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date), GEOID = as.character(GEOID), T_avg = (T_day + T_night) / 2, year = 2021)

temp_2022 <- read_csv("MODIS_Temp_Weekly_VA_2022.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date), GEOID = as.character(GEOID), T_avg = (T_day + T_night) / 2, year = 2022)

temp_2023 <- read_csv("MODIS_Temp_Weekly_VA_2023.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date), GEOID = as.character(GEOID), T_avg = (T_day + T_night) / 2, year = 2023)

temp_2024 <- read_csv("MODIS_Temp_Weekly_VA_2024.csv", show_col_types = FALSE) %>%
  mutate(date = as.Date(date), GEOID = as.character(GEOID), T_avg = (T_day + T_night) / 2, year = 2024)

# Combine all years
temp_all <- bind_rows(temp_2021, temp_2022, temp_2023, temp_2024)

# UI
ui <- fluidPage(
  titlePanel("Rainbow Line Plot: Weekly Temperature by County"),
  
  p("The plot below shows weekly temperature trends (High, Low, or Average) across selected Virginia counties over time. 
    You can select any number of counties and temperature types. This view helps compare temporal patterns and seasonal variation."),
  
  tabsetPanel(
    tabPanel("2021",
             sidebarLayout(
               sidebarPanel(
                 selectInput("counties_2021", "Select Counties:",
                             choices = sort(unique(temp_all$county[temp_all$year == 2021])),
                             selected = c("Loudoun", "Fairfax"),
                             multiple = TRUE),
                 selectInput("temp_type_2021", "Temperature Type:",
                             choices = c("High (Day)" = "T_day", "Low (Night)" = "T_night", "Average" = "T_avg"),
                             selected = "T_avg")
               ),
               mainPanel(plotlyOutput("plot_2021", height = "650px"))
             )
    ),
    tabPanel("2022",
             sidebarLayout(
               sidebarPanel(
                 selectInput("counties_2022", "Select Counties:",
                             choices = sort(unique(temp_all$county[temp_all$year == 2022])),
                             selected = c("Loudoun", "Fairfax"),
                             multiple = TRUE),
                 selectInput("temp_type_2022", "Temperature Type:",
                             choices = c("High (Day)" = "T_day", "Low (Night)" = "T_night", "Average" = "T_avg"),
                             selected = "T_avg")
               ),
               mainPanel(plotlyOutput("plot_2022", height = "650px"))
             )
    ),
    tabPanel("2023",
             sidebarLayout(
               sidebarPanel(
                 selectInput("counties_2023", "Select Counties:",
                             choices = sort(unique(temp_all$county[temp_all$year == 2023])),
                             selected = c("Loudoun", "Fairfax"),
                             multiple = TRUE),
                 selectInput("temp_type_2023", "Temperature Type:",
                             choices = c("High (Day)" = "T_day", "Low (Night)" = "T_night", "Average" = "T_avg"),
                             selected = "T_avg")
               ),
               mainPanel(plotlyOutput("plot_2023", height = "650px"))
             )
    ),
    tabPanel("2024",
             sidebarLayout(
               sidebarPanel(
                 selectInput("counties_2024", "Select Counties:",
                             choices = sort(unique(temp_all$county[temp_all$year == 2024])),
                             selected = c("Loudoun", "Fairfax"),
                             multiple = TRUE),
                 selectInput("temp_type_2024", "Temperature Type:",
                             choices = c("High (Day)" = "T_day", "Low (Night)" = "T_night", "Average" = "T_avg"),
                             selected = "T_avg")
               ),
               mainPanel(plotlyOutput("plot_2024", height = "650px"))
             )
    )
  )
)

# Server
server <- function(input, output, session) {
  make_plot <- function(year_input, counties, temp_type) {
    filtered <- temp_all %>%
      filter(year == year_input, county %in% counties)
    
    p <- ggplot(filtered, aes(x = date, y = .data[[temp_type]], color = county)) +
      geom_line(size = 1.2) +
      scale_color_manual(values = rainbow(length(unique(filtered$county)))) +
      labs(
        title = paste(year_input, "-", names(which(c(T_day = "High (Day)", T_night = "Low (Night)", T_avg = "Average") == temp_type)),
                      "Temperature by County"),
        x = "Week",
        y = "Temperature (°C)",
        color = "County"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  }
  
  output$plot_2021 <- renderPlotly({ req(input$counties_2021); make_plot(2021, input$counties_2021, input$temp_type_2021) })
  output$plot_2022 <- renderPlotly({ req(input$counties_2022); make_plot(2022, input$counties_2022, input$temp_type_2022) })
  output$plot_2023 <- renderPlotly({ req(input$counties_2023); make_plot(2023, input$counties_2023, input$temp_type_2023) })
  output$plot_2024 <- renderPlotly({ req(input$counties_2024); make_plot(2024, input$counties_2024, input$temp_type_2024) })
}

# Run app
shinyApp(ui, server)

#Last 6 months 
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Define UI
ui <- fluidPage(
  titlePanel("Recent MODIS Temperature Trends (Last 6 Months)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_year", "Select Year:",
                  choices = 2021:2024, selected = 2024),
      
      uiOutput("county_selector"),
      
      selectInput("temp_type", "Temperature Type:",
                  choices = c("High (Day)" = "T_day",
                              "Low (Night)" = "T_night",
                              "Average" = "T_avg"),
                  selected = "T_avg")
    ),
    
    mainPanel(
      plotlyOutput("temp_plot", height = "600px")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Reactive: Load only the selected year's CSV
  temp_data <- reactive({
    file_name <- paste0("Filtered_MODIS_Temp_", input$selected_year, ".csv")
    read_csv(file_name, show_col_types = FALSE)
  })
  
  # Dynamically update county list
  output$county_selector <- renderUI({
    counties <- temp_data() %>%
      pull(county) %>%
      unique() %>%
      sort()
    
    selectInput("selected_counties", "Select Counties:",
                choices = counties,
                selected = head(counties, 1),
                multiple = TRUE)
  })
  
  # Render plot
  output$temp_plot <- renderPlotly({
    req(input$selected_counties, input$temp_type)
    
    df <- temp_data() %>%
      filter(county %in% input$selected_counties)
    
    p <- ggplot(df, aes(x = date, y = .data[[input$temp_type]], color = county)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_manual(values = rainbow(length(unique(df$county)))) +
      labs(
        title = paste("Weekly", names(which(c(T_day = "High", T_night = "Low", T_avg = "Average") == input$temp_type)),
                      "Temperature –", input$selected_year),
        x = "Date",
        y = "Temperature (°C)",
        color = "County"
      ) +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  })
}

# Run app
shinyApp(ui, server)


#TOP 10

