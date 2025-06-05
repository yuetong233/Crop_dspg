#Area's Planted by County 
#This data is acres planted with all regions and counties from 2021-2024
library(shiny)
library(leaflet)
library(tigris)
library(sf)
library(dplyr)

options(tigris_use_cache = TRUE)

# Load and clean data
acres_data <- read.csv("AcresPlanted.csv")
acres_data$Value <- as.numeric(acres_data$Value)

acres_data_clean <- acres_data %>%
  mutate(
    County = tolower(County),
    County = gsub(" county", "", County),
    County = trimws(County),
    Year = as.character(Year)
  ) %>%
  group_by(County, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

# Fix city naming to match shapefile
acres_data_clean <- acres_data_clean %>%
  mutate(County = case_when(
    County == "chesapeake city" ~ "chesapeake",
    County == "suffolk city" ~ "suffolk",
    County == "virginia beach city" ~ "virginia beach",
    TRUE ~ County
  ))

# Load VA shapefile
va_counties <- counties(state = "VA", cb = TRUE, year = 2023) %>%
  st_transform(crs = 4326) %>%
  mutate(
    County = tolower(NAME),
    County = gsub(" county", "", County),
    County = trimws(County)
  )

# UI
ui <- fluidPage(
  titlePanel("Acres Planted by County (2021–2024)"),
  
  tabPanel("Acres Planted by County",
           h4("About This Data"),
           p("This section displays total corn acres planted across Virginia counties from 2021 to 2024, based on survey data from the USDA National Agricultural Statistics Service (NASS). The data reflects reported planting activity by county for each year, offering insight into regional trends in corn cultivation."),
           p("Some counties or independent cities may not appear in the visualizations due to missing or unreported values in the NASS dataset. Additionally, large urban areas with minimal agricultural production—such as Fairfax or Arlington—are often excluded due to their limited involvement in crop planting.")
  ),
  
  
  fluidRow(
    column(6, h4("2021"), leafletOutput("map_2021", height = "400px")),
    column(6, h4("2022"), leafletOutput("map_2022", height = "400px"))
  ),
  fluidRow(
    column(6, h4("2023"), leafletOutput("map_2023", height = "400px")),
    column(6, h4("2024"), leafletOutput("map_2024", height = "400px"))
  )
)

# Server
server <- function(input, output, session) {
  years <- c("2021", "2022", "2023", "2024")
  
  for (yr in years) {
    local({
      year <- yr
      map_id <- paste0("map_", year)
      
      year_data <- acres_data_clean %>%
        filter(Year == year)
      
      map_data <- left_join(va_counties, year_data, by = "County") %>%
        st_as_sf()
      
      values <- map_data$Value
      
      pal <- if (length(unique(values[!is.na(values)])) > 1) {
        colorBin("YlGn", domain = values, bins = 5, na.color = "#f0f0f0")
      } else {
        colorBin("YlGn", domain = c(0, 1), bins = 5, na.color = "#f0f0f0")
      }
      
      output[[map_id]] <- renderLeaflet({
        leaflet(map_data) %>%
          addProviderTiles("CartoDB.Positron") %>%
          addPolygons(
            fillColor = ~pal(Value),
            color = "black",
            weight = 1,
            fillOpacity = 0.7,
            label = ~paste0(
              "<strong>", toupper(County), "</strong><br>",
              "Acres Planted: ", ifelse(is.na(Value), "N/A", formatC(Value, format = "f", big.mark = ",", digits = 0))
            ) %>% lapply(htmltools::HTML),
            highlightOptions = highlightOptions(
              weight = 2,
              color = "#666",
              fillOpacity = 0.9,
              bringToFront = TRUE
            )
          ) %>%
          addLegend("bottomright",
                    pal = pal,
                    values = values,
                    title = "Acres Planted",
                    opacity = 1
          ) %>%
          setView(lng = -78.6569, lat = 37.4316, zoom = 6)
      })
    })
  }
}

shinyApp(ui, server)
