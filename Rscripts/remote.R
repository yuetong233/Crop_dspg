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
