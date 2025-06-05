#Area's Planted by County 
#This data is acres planted with all regions and counties from 2021-2024
library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(tigris)  
options(tigris_use_cache = TRUE)
acres_data <- read_csv("AcresPlanted.csv")
head(acres_data)

acres_data <- acres_data %>%
  mutate(County = tolower(trimws(County)),
         Year = as.factor(Year))

va_counties <- counties(state = "VA", cb = TRUE, class = "sf") %>%
  mutate(NAME = tolower(NAME))  # match case

latest_year <- max(acres_data$Year)

va_map_data <- va_counties %>%
  left_join(acres_data %>% filter(Year == latest_year),
            by = c("NAME" = "County"))
ui <- fluidPage(
  titlePanel("🌽 Virginia Acres Planted by County"),
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year:",
                  choices = sort(unique(acres_data$Year)),
                  selected = latest_year)
    ),
    mainPanel(
      leafletOutput("map", height = 600)
    )
  )
)

server <- function(input, output, session) {
  filtered_data <- reactive({
    va_counties %>%
      left_join(acres_data %>% filter(Year == input$year),
                by = c("NAME" = "County"))
  })
  
  output$map <- renderLeaflet({
    pal <- colorNumeric("YlGn", domain = acres_data$Acres_Planted)
    
    leaflet(data = filtered_data()) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~pal(Acres_Planted),
        color = "white", weight = 1, smoothFactor = 0.5,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(weight = 3, color = "#666", fillOpacity = 0.9),
        label = ~paste0(NAME, ": ", Acres_Planted, " acres"),
        labelOptions = labelOptions(direction = "auto")
      ) %>%
      addLegend("bottomright", pal = pal,
                values = acres_data$Acres_Planted,
                title = "Acres Planted")
  })
}

shinyApp(ui, server)



