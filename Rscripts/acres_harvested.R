library(dplyr)
library(readr)
planted <- read_csv("AcresPlanted.csv")
harvested <- read_csv("AcresHarvestedGrains.csv")
clean_data <- function(df, value_col = "Value") {
  df %>%
    mutate(
      !!value_col := as.numeric(.data[[value_col]]),
      County = tolower(County),
      County = gsub(" county", "", County),
      County = trimws(County),
      Year = as.character(Year)
    )
}

planted_clean <- clean_data(planted)
harvested_clean <- clean_data(harvested)
planted_summary <- planted_clean %>%
  group_by(County, Year) %>%
  summarise(Planted = sum(Value, na.rm = TRUE), .groups = "drop")

harvested_summary <- harvested_clean %>%
  group_by(County, Year) %>%
  summarise(Harvested = sum(Value, na.rm = TRUE), .groups = "drop")

merged_data <- full_join(planted_summary, harvested_summary, by = c("County", "Year")) %>%
  mutate(across(c(Planted, Harvested), ~replace_na(., 0)))

merged_data
library(shiny)
library(leaflet)
library(tigris)
library(sf)
library(dplyr)

options(tigris_use_cache = TRUE)
merged_data <- merged_data %>%
  mutate(County = case_when(
    County == "chesapeake city" ~ "chesapeake",
    County == "suffolk city" ~ "suffolk",
    County == "virginia beach city" ~ "virginia beach",
    TRUE ~ County
  ))

va_counties <- counties(state = "VA", cb = TRUE, year = 2023) %>%
  st_transform(crs = 4326) %>%
  mutate(County = tolower(NAME)) %>%
  mutate(County = gsub(" county", "", County), County = trimws(County))

ui <- fluidPage(
  titlePanel("🌽 Acres Planted vs. Harvested by County (2021–2024)"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("year", "Select Year", choices = unique(merged_data$Year), selected = "2021"),
      radioButtons("type", "Data Type", choices = c("Planted", "Harvested"), selected = "Planted")
    ),
    
    mainPanel(
      leafletOutput("county_map", height = "700px")
    )
  )
)

# Server
server <- function(input, output, session) {
  output$county_map <- renderLeaflet({
    # Filter for selected year
    year_data <- merged_data %>%
      filter(Year == input$year) %>%
      select(County, Value = all_of(input$type))
    
    # Merge with shapefile
    map_data <- left_join(va_counties, year_data, by = "County") %>%
      st_as_sf()
    
    pal <- if (length(unique(map_data$Value[!is.na(map_data$Value)])) > 1) {
      colorBin("YlGn", domain = map_data$Value, bins = 5, na.color = "#f0f0f0")
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
          input$type, ": ", ifelse(is.na(Value), "N/A", formatC(Value, format = "f", big.mark = ",", digits = 0))
        ) %>% lapply(htmltools::HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend("bottomright", pal = pal, values = map_data$Value,
                title = paste("Acres", input$type), opacity = 1) %>%
      setView(lng = -78.6569, lat = 37.4316, zoom = 6)
  })
}

shinyApp(ui, server)

