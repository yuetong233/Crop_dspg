#remote sensing data 
library(shiny)
library(readr)
library(plotly)
library(dplyr)

# Load NDVI data
ndvi_data <- read_csv("NDVI_weekly.csv") %>%
  mutate(date = as.Date(date)) %>%
  filter(!is.na(NDVI))

ui <- fluidPage(
  titlePanel("NDVI Weekly Tracker: Virginia Corn Fields (2021–Present)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("About This Data"),
      p("This dashboard visualizes weekly NDVI (Normalized Difference Vegetation Index) values ",
        "over corn fields in Virginia from 2021 through the most recent available Landsat 8 imagery."),
      p("NDVI is a vegetation index calculated using the Landsat 8 Red (Band 4) and Near Infrared (Band 5) bands:"),
      tags$ul(
        tags$li("NDVI = (NIR - Red) / (NIR + Red)"),
        tags$li("NDVI values range from -1 to 1"),
        tags$li("Higher NDVI indicates healthier, greener vegetation")
      ),
      p("Only Landsat 8 pixels located on corn fields (based on USDA Cropland Data Layer) were included."),
      p("Weekly NDVI composites were calculated in Google Earth Engine, cloud-masked, and exported as a time series."),
      
      br(),
      sliderInput("year_range", "Select Year Range:",
                  min = min(ndvi_data$year),
                  max = max(ndvi_data$year),
                  value = c(min(ndvi_data$year), max(ndvi_data$year)),
                  sep = "")
    ),
    
    mainPanel(
      plotlyOutput("ndvi_plot", height = "500px")
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    ndvi_data %>%
      filter(year >= input$year_range[1], year <= input$year_range[2])
  })
  
  output$ndvi_plot <- renderPlotly({
    plot_ly(
      data = filtered_data(),
      x = ~date,
      y = ~NDVI,
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = 'lightgreen', width = 2),
      marker = list(color = 'darkgreen', size = 6)
    ) %>%
      layout(
        title = "Weekly NDVI Over Virginia Corn Fields",
        xaxis = list(title = "Date"),
        yaxis = list(title = "NDVI", range = c(0, 1)),
        hovermode = "x unified"
      )
  })
}

shinyApp(ui = ui, server = server)


#By County
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(plotly)

# Load and clean all years
load_ndvi <- function(file, year) {
  read_csv(file) %>%
    select(GEOID, county, date, mean) %>%
    filter(!is.na(mean), county != "") %>%
    mutate(date = as.Date(date), year = year)
}

ndvi_2021 <- load_ndvi("NDVI_VA_County_Weekly_2021.csv", 2021)
ndvi_2022 <- load_ndvi("NDVI_VA_County_Weekly_2022.csv", 2022)
ndvi_2023 <- load_ndvi("NDVI_VA_County_Weekly_2023.csv", 2023)
ndvi_2024 <- load_ndvi("NDVI_VA_County_Weekly_2024.csv", 2024)

ndvi_all <- bind_rows(ndvi_2021, ndvi_2022, ndvi_2023, ndvi_2024)

# Get full county list
county_list <- sort(unique(ndvi_all$county))

# UI
ui <- fluidPage(
  titlePanel("Weekly NDVI in Virginia Corn Fields (2021–2024)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("counties", "Select Counties to Compare:",
                  choices = county_list,
                  selected = county_list[1],
                  multiple = TRUE),
      helpText("Compare multiple counties. NDVI is shown weekly.")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("2021", plotlyOutput("plot2021")),
        tabPanel("2022", plotlyOutput("plot2022")),
        tabPanel("2023", plotlyOutput("plot2023")),
        tabPanel("2024", plotlyOutput("plot2024"))
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  make_plot <- function(year) {
    plot_data <- ndvi_all %>%
      filter(year == !!year, county %in% input$counties)
    
    if (nrow(plot_data) == 0) return(NULL)
    
    p <- ggplot(plot_data, aes(x = date, y = mean, color = county, group = county)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = paste("NDVI by County -", year),
           x = "Date (Weekly)",
           y = "NDVI",
           color = "County") +
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", "color"))
  }
  
  output$plot2021 <- renderPlotly({ make_plot(2021) })
  output$plot2022 <- renderPlotly({ make_plot(2022) })
  output$plot2023 <- renderPlotly({ make_plot(2023) })
  output$plot2024 <- renderPlotly({ make_plot(2024) })
}

# Run app
shinyApp(ui, server)


