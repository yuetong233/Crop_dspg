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


#API stuff
#No report for 2025 until end of 2026
library(shiny)
library(leaflet)
library(dplyr)
library(tigris)
library(sf)
library(rnassqs)

# Authenticate
nassqs_auth("E0DE4B3D-0418-32C4-8541-6C4C8954534A")
options(tigris_use_cache = TRUE)

# Load counties
va_counties <- counties(state = "VA", cb = TRUE, year = 2023) %>%
  st_transform(4326) %>%
  mutate(
    County = tolower(NAME),
    County = gsub(" county", "", County),
    County = trimws(County)
  )

# Get years and data
years <- 2021:2025
get_acres_county <- function(year) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      statisticcat_desc = "AREA PLANTED",
      unit_desc = "ACRES",
      state_name = "VIRGINIA",
      agg_level_desc = "COUNTY",
      source_desc = "SURVEY",
      year = as.character(year)
    ))
    df$Year <- as.character(year)
    df
  }, error = function(e) NULL)
}

raw_data <- bind_rows(lapply(years, get_acres_county)) %>%
  filter(!is.na(county_name)) %>%
  mutate(
    County = tolower(county_name),
    County = gsub(" county", "", County),
    County = trimws(County),
    County = case_when(
      County == "chesapeake city" ~ "chesapeake",
      County == "suffolk city" ~ "suffolk",
      County == "virginia beach city" ~ "virginia beach",
      TRUE ~ County
    ),
    Value = as.numeric(Value)
  ) %>%
  group_by(County, Year) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")

# UI
ui <- fluidPage(
  titlePanel("Corn Acres Planted by County in Virginia"),
  h4("Explore how many acres of corn were planted across Virginia counties by year."),
  p("Click a year below to view a map of planting activity. 2025 data will appear when it's available from USDA NASS."),
  fluidRow(
    lapply(years, function(yr) {
      column(2, actionButton(inputId = paste0("btn_", yr), label = yr))
    })
  ),
  br(),
  leafletOutput("acres_map", height = "600px")
)

# Server
server <- function(input, output, session) {
  selected_year <- reactiveVal("2024")
  
  lapply(years, function(yr) {
    observeEvent(input[[paste0("btn_", yr)]], {
      selected_year(as.character(yr))
    })
  })
  
  output$acres_map <- renderLeaflet({
    year <- selected_year()
    year_data <- raw_data %>% filter(Year == year)
    
    if (nrow(year_data) == 0) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addLabelOnlyMarkers(
          lng = -78.6569, lat = 37.5,
          label = paste("County-level data not available yet for", year),
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
        ) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    } else {
      map_data <- left_join(va_counties, year_data, by = "County") %>% st_as_sf()
      values <- map_data$Value
      
      pal <- if (length(unique(na.omit(values))) > 1) {
        colorBin("YlGn", domain = values, bins = 5, na.color = "#f0f0f0")
      } else {
        colorBin("YlGn", domain = c(0, 1), bins = 5, na.color = "#f0f0f0")
      }
      
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
            weight = 2, color = "#666", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = values, title = "Acres Planted", opacity = 1) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    }
  })
}

shinyApp(ui, server)

#adding area harvested 
