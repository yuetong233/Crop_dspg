#Dashboard
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(rnassqs)
library(zoo)
library(leaflet)
library(sf)
library(tigris)
options(tigris_use_cache = TRUE)

#API Key
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A") 
options(tigris_use_cache = TRUE)


#Processing Data and Cleaning it
#Crop Conditions
get_corn_data <- function(year) {
  nassqs(list(
    commodity_desc = "CORN",
    year = year,
    state_name = "VIRGINIA",
    statisticcat_desc = "CONDITION",
    agg_level_desc = "STATE"
  )) %>%
    filter(unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR", "PCT VERY POOR")) %>%
    mutate(
      week = as.Date(week_ending),
      value = as.numeric(Value),
      condition = gsub("PCT ", "", unit_desc),
      condition = factor(condition, levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")),
      unit_desc = factor(unit_desc, levels = c("PCT VERY POOR", "PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")),
      year = as.character(year)
    )
}

corn_data_list <- lapply(2021:2025, get_corn_data)
names(corn_data_list) <- as.character(2021:2025)

condition_colors <- c(
  "VERY POOR" = "#a50026",
  "POOR" = "#d73027",
  "FAIR" = "#fee08b",
  "GOOD" = "#66bd63",
  "EXCELLENT" = "#1a9850"
)

#County Analysis
states <- c("VA", "NC", "MD")
years <- 2021:2025
stats <- c("AREA PLANTED", "AREA HARVESTED")

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

raw_data <- bind_rows(lapply(states, function(st) {
  bind_rows(lapply(years, function(yr) {
    bind_rows(lapply(stats, function(sc) {
      get_county_acres(st, yr, sc)
    }))
  }))
}))

clean_data <- raw_data %>%
  mutate(
    County = tolower(county_name),
    County = gsub(" county", "", County),
    County = trimws(County),
    stat_type = ifelse(stat_type == "AREA PLANTED", "Planted", "Harvested"),
    Value = as.numeric(Value)
  ) %>%
  filter(!is.na(County)) %>%  # Only filter after defining County
  group_by(County, state_alpha, Year, stat_type) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = stat_type, values_from = Value) %>%
  mutate(
    County = case_when(
      County == "chesapeake city" ~ "chesapeake",
      County == "suffolk city" ~ "suffolk",
      County == "virginia beach city" ~ "virginia beach",
      TRUE ~ County
    )
  )


# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      h1, h2, h3, h4 {
        color: #2e7d32;  /* Dark green */
        font-weight: bold;
      }
    "))
  ),
  tags$head(
    tags$style(HTML("
      .title-box {
        background-color: #e8f5e9;  /* soft pale green */
        padding: 20px;
        border: 2px solid #2e7d32;
        border-radius: 12px;
        margin-bottom: 30px;
        text-align: center;
      }

      .title-box h1 {
        color: #2e7d32;
        margin: 0;
      }
    "))
  ),
  
  div(class = "title-box",
      h1("Planting Progress and Crop Condition Interactive Dashboard")
  ),
  
  navlistPanel(
    widths = c(2, 10),
    
    tabPanel("Objective",
             h3("Dashboard Purpose"),
             p("This dashboard helps Virginia farmers and stakeholders explore crop condition trends using USDA NASS data."),
             h4("Instructions"),
             p("Use the sidebar to explore planting progress, crop quality, remote sensing, and state comparisons.")
    ),
    
    tabPanel("Planting Progress",
             h3("Corn Planting Progress"),
             p("The plot visualizes the growing percentage of corn planted in Virginia over the planting season weeks for the years you select."),
             p("Each line or point represents a different year, showing how planting progresses week by week."),
             p("The visualization helps to compare planting pace across different years, to identify trends and understand how current progress lines up with historical patterns."),
             
             # Controls
             fluidRow(
               column(6,
                      pickerInput("planting_years", "Select Year(s):",
                                  choices = c("2021", "2022", "2023", "2024", as.character(as.numeric(format(Sys.Date(), "%Y")) + 1)),
                                  selected = c("2024"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE))
               ),
               column(6,
                      sliderInput("planting_date_range", "Select Date Range:",
                                  min = as.Date("2021-04-01"),
                                  max = as.Date("2024-07-31"),
                                  value = c(as.Date("2024-04-01"), as.Date("2024-07-31")),
                                  timeFormat = "%Y-%m-%d")
               )
             ),
             
             # Output
             plotlyOutput("planting_plot")
    ),
    
    tabPanel("Crop Conditions",
             h4("About This Data"),
             p("This section presents weekly corn crop condition data in Virginia from 2021 to 2025, retrieved directly from the USDA National Agricultural Statistics Service (NASS) API. The 2025 data reflects current weekly updates and will continue to populate as the growing season progresses."),
             p("The visualization below uses a stacked area chart, where each color band represents a condition category—from 'Very Poor' in red to 'Excellent' in green—providing a clear view of how crop quality shifts week to week throughout the season."),
             h4("Select a year below to view weekly stacked area conditions:"),
             do.call(tabsetPanel, lapply(names(corn_data_list), function(yr) {
               tabPanel(yr, plotlyOutput(paste0("plot_", yr), height = "600px"))
             }))
    ),
    
    tabPanel("Remote Sensing",
             h4("Remote Sensing Placeholder"),
             p("This section will include Remote Sensing data once available.")
    ),
    
    tabPanel("County Analysis",
             h4("About This Data"),
             p("This section displays total corn acres planted and harvested across Virginia, Maryland, and North Carolina, based on real-time data from the USDA National Agricultural Statistics Service (NASS) API. The data automatically updates as new information becomes available, including the 2025 crop conditions once they are released later this year."),
             p("Here, you can explore the amount of corn acres planted, the amount harvested, and the percent harvested, calculated directly from reported figures for each state. This provides insight into regional planting and harvesting activity over time."),
             p("Please note that some counties or independent cities may not appear in the visualizations due to missing or unreported values in the NASS dataset. Additionally, urban areas such as Fairfax or Arlington are often excluded due to their limited involvement in crop production."),
             fluidRow(
               lapply(years, function(yr) {
                 column(2, actionButton(paste0("btn_", yr), label = yr))
               })
             ),
             br(),
             leafletOutput("compare_map", height = "600px")
    ),
    
    tabPanel("Corn Yield Analysis",
             h3("Corn Yield Analysis"),
             p("Explore corn yield trends across Virginia and neighboring states."),
             fluidRow(
               column(4,
                      pickerInput("yield_states", "Select States:",
                                  choices = c("Virginia" = "VA", "North Carolina" = "NC", "Maryland" = "MD"),
                                  selected = c("VA", "NC", "MD"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE))
               ),
               column(4,
                      sliderInput("yield_year_range", "Select Year Range:",
                                  min = 2000,
                                  max = as.numeric(format(Sys.Date(), "%Y")),
                                  value = c(as.numeric(format(Sys.Date(), "%Y")) - 10, as.numeric(format(Sys.Date(), "%Y"))),
                                  step = 1)
               ),
               column(4,
                      numericInput("ma_window", "Moving Average Window:", value = 5, min = 2, max = 10)
               )
             ),
             tabsetPanel(
               tabPanel("Yield Trends", plotlyOutput("yield_plot")),
               tabPanel("Year-over-Year Changes", plotlyOutput("yoy_plot")),
               tabPanel("Summary Statistics", tableOutput("summary_table"))
             )
    )
  ) 
)  


# Server
server <- function(input, output) {
  
  render_plot <- function(data) {
    plot_ly(
      data %>% arrange(week, unit_desc),
      x = ~week,
      y = ~value,
      color = ~condition,
      colors = condition_colors,
      type = "scatter",
      mode = "none",
      stackgroup = "one",
      fill = "tonexty",
      text = ~paste0(
        "<b>Week:</b> ", format(week, "%b %d, %Y"),
        "<br><b>Condition:</b> ", condition,
        "<br><b>Percent:</b> ", value, "%"
      ),
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "Week Ending", tickfont = list(size = 16, family = "Montserrat")),
        yaxis = list(title = "Percent", range = c(0, 100), tickfont = list(size = 16, family = "Montserrat")),
        legend = list(title = list(text = "<b>Condition</b>"), font = list(size = 16)),
        plot_bgcolor = "#fafafa",
        paper_bgcolor = "#fafafa",
        font = list(family = "Montserrat", size = 15)
      )
  }
  
  # Dynamically assign outputs
  for (yr in names(corn_data_list)) {
    local({
      year <- yr
      output[[paste0("plot_", year)]] <- renderPlotly({
        render_plot(corn_data_list[[year]])
      })
    })
  }
  

  
  # Yield data from rnassqs
  yield_data <- reactive({
    req(input$yield_states, input$yield_year_range)
    
    all_data <- lapply(input$yield_states, function(state) {
      nassqs(list(
        commodity_desc = "CORN",
        year = seq(input$yield_year_range[1], input$yield_year_range[2]),
        agg_level_desc = "COUNTY",
        state_alpha = state,
        statisticcat_desc = "YIELD"
      ))
    })
    
    do.call(rbind, Map(function(data, state) {
      data %>% mutate(State = state)
    }, all_data, input$yield_states))
  })
  
  # Yield Trend Plot
  output$yield_plot <- renderPlotly({
    req(yield_data())
    
    state_year_avg <- yield_data() %>%
      group_by(State, year) %>%
      summarize(avg_yield = mean(Value, na.rm = TRUE)) %>%
      group_by(State) %>%
      mutate(moving_avg = zoo::rollmean(avg_yield, k = input$ma_window, fill = NA, align = "right"))
    
    p <- ggplot(state_year_avg, aes(x = year, y = avg_yield, color = State)) +
      geom_line(size = 1.2) +
      geom_line(aes(y = moving_avg, linetype = "Moving Avg"), size = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = "Statewide Corn Yield Trends",
        subtitle = "Annual Average and Moving Average by State",
        x = "Year",
        y = "Average Yield (bushels per acre)"
      ) +
      scale_linetype_manual(values = c("Moving Avg" = "dashed"))
    
    ggplotly(p)
  })
  
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

  # alter the plot 
  output$yoy_plot <- renderPlotly({
    req(yield_data())
    
    yoy_changes <- yield_data() %>%
      group_by(State, county_name) %>%
      arrange(State, county_name, year) %>%
      mutate(yoy_change_pct = (Value - lag(Value)) / lag(Value) * 100)
    
    p <- ggplot(yoy_changes, aes(x = year, y = yoy_change_pct, fill = State)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Year-over-Year Change in Corn Yields",
        subtitle = "Percentage Change by State",
        x = "Year",
        y = "Percentage Change"
      )
    
    ggplotly(p)
  })
  
  # Summary Table
  output$summary_table <- renderTable({
    req(yield_data())
   
    yield_data() %>%
      group_by(State) %>%
      summarize(
        avg_yield = mean(Value, na.rm = TRUE),
        max_yield = max(Value, na.rm = TRUE),
        min_yield = min(Value, na.rm = TRUE),
        yield_range = max_yield - min_yield,
        yield_std = sd(Value, na.rm = TRUE)
      )
  })
  
  # Corn planting progress data
  planting_data <- reactive({
    req(input$planting_years, input$planting_date_range)
    
    all_data <- lapply(input$planting_years, function(year) {
      nassqs(list(
        source_desc = "SURVEY",
        sector_desc = "CROPS",
        group_desc = "FIELD CROPS",
        commodity_desc = "CORN",
        statisticcat_desc = "PROGRESS",
        unit_desc = "PCT PLANTED",
        state_name = "VIRGINIA",
        year = year
      ))
    })
    
    # Combine data and preprocess
    combined_data <- do.call(rbind, all_data)
    
    combined_data %>%
      mutate(week = as.Date(week_ending),
             value = as.numeric(Value)) %>%
      filter(!is.na(week)) %>% # Filter out rows with NA weeks after conversion
      arrange(year, week) %>%
      filter(week >= input$planting_date_range[1], # Filter by selected date range
             week <= input$planting_date_range[2])
  })
  
  # Corn planting progress plot
  output$planting_plot <- renderPlotly({
    req(planting_data())
    
    p <- ggplot(planting_data(), aes(x = week, y = value, color = factor(year), group = factor(year),
                                     text = paste("Year:", year, "<br>Week Ending:", week, "<br>% Planted:", value)))
    
    # Check if there's more than one year selected to draw lines
    if (length(unique(planting_data()$year)) > 1) {
      p <- p + geom_line(size = 1.2)
    }
    
    p <- p + geom_point(size = 2) +
      labs(title = "Corn Planting Progress in Virginia",
           y = "% Planted",
           x = "Week Ending",
           color = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
  # Load and clean acres data
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
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(County = case_when(
      County == "chesapeake city" ~ "chesapeake",
      County == "suffolk city" ~ "suffolk",
      County == "virginia beach city" ~ "virginia beach",
      TRUE ~ County
    ))
  
  va_counties <- counties(state = "VA", cb = TRUE, year = 2023) %>%
    st_transform(crs = 4326) %>%
    mutate(
      County = tolower(NAME),
      County = gsub(" county", "", County),
      County = trimws(County)
    )
  
  # Generate maps for each year
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

# Run the shiny app 
shinyApp(ui = ui, server = server)

