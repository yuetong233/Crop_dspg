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
#MD and NC
library(shiny)
library(leaflet)
library(dplyr)
library(tigris)
library(sf)
library(rnassqs)

# Authenticate NASS API
nassqs_auth("E0DE4B3D-0418-32C4-8541-6C4C8954534A")
options(tigris_use_cache = TRUE)

# States of interest
states <- c("VA", "NC", "MD")
years <- 2021:2025
stats <- c("AREA PLANTED", "AREA HARVESTED")

# Get county shapefiles
all_counties <- bind_rows(lapply(states, function(st) {
  counties(state = st, cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(
      County = tolower(NAME),
      County = gsub(" county", "", County),
      County = trimws(County),
      State = st
    )
}))

# Helper: pull API data
get_county_acres <- function(state, year, stat_cat) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      statisticcat_desc = stat_cat,
      unit_desc = "ACRES",
      state_alpha = state,
      agg_level_desc = "COUNTY",
      source_desc = "SURVEY",
      year = as.character(year)
    ))
    df$Year <- as.character(year)
    df$stat_type <- stat_cat
    df$state_alpha <- state
    df
  }, error = function(e) NULL)
}

# Pull all data
raw_data <- bind_rows(lapply(states, function(st) {
  bind_rows(lapply(years, function(yr) {
    bind_rows(lapply(stats, function(sc) {
      get_county_acres(st, yr, sc)
    }))
  }))
}))

# Clean and reshape
clean_data <- raw_data %>%
  filter(!is.na(county_name)) %>%
  mutate(
    County = tolower(county_name),
    County = gsub(" county", "", County),
    County = trimws(County),
    Value = as.numeric(Value),
    stat_type = ifelse(stat_type == "AREA PLANTED", "Planted", "Harvested")
  ) %>%
  group_by(County, state_alpha, Year, stat_type) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = stat_type, values_from = Value)

# UI
ui <- fluidPage(
  titlePanel("Corn Acres Planted vs Harvested by County (VA, NC, MD)"),
  h4("Click a year to view county-level planting and harvesting data."),
  fluidRow(
    lapply(years, function(yr) {
      column(2, actionButton(paste0("btn_", yr), label = yr))
    })
  ),
  br(),
  leafletOutput("compare_map", height = "600px")
)

# Server
server <- function(input, output, session) {
  selected_year <- reactiveVal("2024")
  
  lapply(years, function(yr) {
    observeEvent(input[[paste0("btn_", yr)]], {
      selected_year(as.character(yr))
    })
  })
  
  output$compare_map <- renderLeaflet({
    year <- selected_year()
    
    year_data <- clean_data %>% filter(Year == year)
    
    if (nrow(year_data) == 0) {
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addLabelOnlyMarkers(
          lng = -78.6569, lat = 37.5,
          label = paste("Data not available for", year),
          labelOptions = labelOptions(noHide = TRUE, textOnly = TRUE)
        ) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    } else {
      map_data <- left_join(all_counties, year_data, by = c("County", "State" = "state_alpha")) %>%
        st_as_sf()
      
      values <- map_data$Planted
      
      pal <- if (length(unique(na.omit(values))) > 1) {
        colorBin("YlGn", domain = values, bins = 5, na.color = "#f0f0f0")
      } else {
        colorBin("YlGn", domain = c(0, 1), bins = 5, na.color = "#f0f0f0")
      }
      
      leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolygons(
          fillColor = ~pal(Planted),
          color = ~case_when(
            State == "VA" ~ "darkgreen",
            State == "NC" ~ "darkgreen",
            State == "MD" ~ "darkgreen",
            TRUE ~ "black"
          ),
          weight = 1.5,
          fillOpacity = 0.7,
          label = ~paste0(
            "<strong>", toupper(County), ", ", State, "</strong><br>",
            "Percent Harvested: ", ifelse(is.na(Harvested) | is.na(Planted) | Planted == 0, "N/A", paste0(round(100 * Harvested / Planted, 1), "%")), "<br>",
            "Planted: ", ifelse(is.na(Planted), "N/A", formatC(round(Planted), format = "d", big.mark = ",")), "<br>",
            "Harvested: ", ifelse(is.na(Harvested), "N/A", formatC(round(Harvested), format = "d", big.mark = ","))
          ) %>% lapply(htmltools::HTML),
          highlightOptions = highlightOptions(
            weight = 3,
            color = "#666",
            fillOpacity = 0.9,
            bringToFront = TRUE
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = values, title = "Acres Planted", opacity = 1) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    }
  })
}

shinyApp(ui, server)
