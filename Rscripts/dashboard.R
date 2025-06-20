#Dashboard

#Libraries
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
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE") 

#Processing Data and Cleaning it

#Crop Conditions
get_corn_data <- function(year) {
  data <- nassqs(list(
    commodity_desc = "CORN",
    year = year,
    state_name = "VIRGINIA",
    statisticcat_desc = "CONDITION",
    agg_level_desc = "STATE"
  ))
  if (nrow(data) == 0) {
    return(tibble(
      week = as.Date(character()),
      value = numeric(),
      condition = factor(levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")),
      unit_desc = factor(levels = c("PCT VERY POOR", "PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")),
      year = character()
    ))
  }
  data %>%
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


#Planting Progress
years <- 2021:(as.numeric(format(Sys.Date(), "%Y")))
categories <- c(
  "PCT PLANTED", "PCT EMERGED", "PCT SILKING", "PCT DOUGH",
  "PCT MATURE", "PCT HARVESTED", "PCT DENTED"
)
clean_title <- function(cat) {
  gsub("^PCT ", "", cat)
}

# Actual data
get_data <- function(year, category) {
  tryCatch({
    nassqs(list(
      commodity_desc = "CORN",
      year = year,
      state_name = "VIRGINIA",
      statisticcat_desc = "PROGRESS",
      unit_desc = category
    )) %>%
      mutate(
        week = as.Date(week_ending),
        value = as.numeric(Value)
      ) %>%
      filter(!is.na(week)) %>%
      arrange(week)
  }, error = function(e) NULL)
}

# 5-Year Average
get_avg_data <- function(year, category) {
  tryCatch({
    nassqs(list(
      commodity_desc = "CORN",
      year = year,
      state_name = "VIRGINIA",
      statisticcat_desc = "PROGRESS, 5 YEAR AVG",
      unit_desc = category
    )) %>%
      mutate(
        week = as.Date(week_ending),
        value = as.numeric(Value)
      ) %>%
      filter(!is.na(week)) %>%
      arrange(week)
  }, error = function(e) NULL)
}



#Yield Analysis
yield_data <- reactive({
  req(input$yield_states)
  
  all_data <- lapply(input$yield_states, function(state) {
    nassqs(list(
      commodity_desc = "CORN",
      year = 2015:2023,
      agg_level_desc = "COUNTY",
      state_alpha = state,
      statisticcat_desc = "YIELD"
    ))
  })
  
  do.call(rbind, Map(function(data, state) {
    data %>% mutate(State = state)
  }, all_data, input$yield_states))
})



# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  tags$head(
    tags$style(HTML("
      h1, h2, h3, h4 {
        color: #2e7d32;  /* Dark green */
        font-family: 'Times New Roman', Times, serif !important;
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
             h4("About This Data"),
             p("This section presents weekly corn planting progress in Virginia from 2021 through the current year, sourced directly from the USDA National Agricultural Statistics Service (NASS) API."),
             p("Explore weekly progress for various crop stages. Each plot compares actual progress to the 5-year historical average."),
             
             do.call(tabsetPanel, c(
               list(id = "year_tabs", type = "tabs"),
               lapply(as.character(years), function(yr) {
                 tabPanel(yr,
                          lapply(categories, function(cat) {
                            tagList(
                              div(
                                style = "margin-top: 30px; margin-bottom: 5px;",
                                HTML(paste0("<h4>", clean_title(cat), " Progress</h4>")),
                                p(paste0("This chart compares weekly reported values to the 5-Year Average for corn ", 
                                         tolower(clean_title(cat)), 
                                         " during the ", yr, " season. Significant differences may reflect planting delays, environmental stress, or other agricultural factors."))
                              ),
                              plotlyOutput(outputId = paste0("plot_combined_", yr, "_", gsub("[^A-Za-z]", "_", cat)), height = "350px")
                            )
                          })
                 )
               })
             ))
    ),
             
    
    
    tabPanel("Crop Conditions",
             h4("About This Data"),
             p("This section presents weekly corn crop condition data in Virginia from 2021 to 2025, retrieved directly from the USDA National Agricultural Statistics Service (NASS) API. The 2025 data reflects current weekly updates and will continue to populate as the growing season progresses."),
             p("The visualization below uses a stacked area chart, where each color band represents a condition category—from 'Very Poor' in red to 'Excellent' in green—providing a clear view of how crop quality shifts week to week throughout the season."),
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
    
    tabPanel("Yield Analysis",
             h4("About This Data"),
             p("This dashboard presents an analysis of corn crop yield across Virginia, North Carolina, and Maryland from 2015 to 2023. 
   The data is sourced from the USDA's National Agricultural Statistics Service (NASS) API and includes county-level statistics on corn yield (bushels per acre). 
   Interactive graphics allow users to explore average yields over time, moving averages, and year-over-year changes."),
             
             fluidRow(
               column(4,
                      pickerInput("yield_states", "Select States:",
                                  choices = c("Virginia" = "VA", "North Carolina" = "NC", "Maryland" = "MD"),
                                  selected = c("VA", "NC", "MD"), multiple = TRUE,
                                  options = list(`actions-box` = TRUE))
               ),
               column(4,
                      sliderInput("yoy_year_slider", "Select Year:",
                                  min = 2015, max = 2023, value = 2023, step = 1, sep = "")
               ),
               column(4,
                      numericInput("ma_window", "Moving Average Window:", value = 5, min = 2, max = 10)
               )
             ),
             
             h4("Summary Statistics"),
             div(style = "margin-bottom:-30px;", plotlyOutput("summary_card")),
             
             h4("Yield Trends Over Time"),
             plotlyOutput("yield_plot"),
             
             br(),
             h4("Year-over-Year Yield Change"),
             plotlyOutput("yoy_plot")
    ),
    
    tabPanel("Yield Forecast",
             h4("About This Feature"),
             p("This section provides a weekly forecast of corn yield for Virginia based on current crop conditions. The forecast uses a regression model that incorporates the distribution of crop conditions (Excellent, Good, Fair, Poor) to predict yield deviation from trend."),
             p("The model assumes that better crop conditions (higher percentages of Excellent and Good ratings) will result in yields above trend, while poorer conditions will result in yields below trend."),
             br(),
             fluidRow(
               column(6,
                      plotlyOutput("condition_distribution", height = "400px")
               ),
               column(6,
                      plotlyOutput("yield_forecast", height = "400px")
               )
             )
    ),
    
    tabPanel("Historical Yield Simulation",
             h4("About This Feature"),
             p("This section simulates how weekly corn yield forecasts would have evolved over a historical season based on the actual crop condition data available at each week. This helps visualize how the forecast improves or fluctuates as more weekly data becomes available."),
             br(),
             fluidRow(
               column(4,
                      selectInput("sim_year", "Select Historical Year:",
                                  choices = sort(as.numeric(names(corn_data_list)), decreasing = TRUE),
                                  selected = max(as.numeric(names(corn_data_list))))
               )
             ),
             plotlyOutput("historical_yield_simulation")
    )
  )
)



# Server
server <- function(input, output) {
  
  yield_data <- reactive({
    req(input$yield_states)
    
    all_data <- lapply(input$yield_states, function(state) {
      nassqs(list(
        commodity_desc = "CORN",
        year = 2015:2023,
        agg_level_desc = "COUNTY",
        state_alpha = state,
        statisticcat_desc = "YIELD"
      ))
    })
    
    do.call(rbind, Map(function(data, state) {
      data %>% mutate(State = state)
    }, all_data, input$yield_states))
  })
  
  # --- Crop Condition Plots ---
  lapply(names(corn_data_list), function(yr) {
    output[[paste0("plot_", yr)]] <- renderPlotly({
      df <- corn_data_list[[yr]]
      
      # 🔁 Reapply factor level to lock order
      df$condition <- factor(df$condition, levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT"))
      
      plot_ly(
        data = df %>% arrange(week, condition),
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
          title = list(text = paste("Virginia Corn Conditions in", yr)),
          xaxis = list(title = "Week Ending"),
          yaxis = list(title = "Percent", range = c(0, 100)),
          legend = list(title = list(text = "Condition")),
          plot_bgcolor = "#fafafa",
          paper_bgcolor = "#fafafa"
        )
    })
  })
  
  
  
  
  # --- Summary Card ---
  output$summary_card <- renderPlotly({
    req(yield_data())
    df <- yield_data() %>%
      group_by(State) %>%
      summarise(
        `Average Yield` = mean(Value, na.rm = TRUE),
        `Max Yield` = max(Value, na.rm = TRUE),
        `Min Yield` = min(Value, na.rm = TRUE),
        `Yield Std Dev` = sd(Value, na.rm = TRUE)
      )
    
    plot_ly(
      type = 'table',
      header = list(values = c("State", names(df)[-1]), align = 'center',
                    fill = list(color = '#a5d6a7'), font = list(size = 14)),
      cells = list(
        values = rbind(df$State,
                       format(round(df[[2]], 1), nsmall = 1),
                       format(df[[3]], big.mark=","),
                       format(df[[4]], big.mark=","),
                       round(df[[5]], 1)),
        align = 'center', height = 30
      )
    )
  })
  
  # --- Yield Trends Plot ---
  output$yield_plot <- renderPlotly({
    req(yield_data())
    
    df <- yield_data() %>%
      group_by(State, year) %>%
      summarise(avg_yield = mean(Value, na.rm = TRUE), .groups = "drop") %>%
      group_by(State) %>%
      mutate(moving_avg = zoo::rollmean(avg_yield, k = input$ma_window, fill = NA, align = "right"))
    
    plot_ly(data = df, x = ~year, y = ~avg_yield, color = ~State, type = 'scatter', mode = 'lines+markers',
            name = ~paste(State, "Avg")) %>%
      add_lines(y = ~moving_avg, linetype = I("dash"), name = ~paste(State, "Moving Avg")) %>%
      layout(
        title = list(text = "Statewide Corn Yield Trends", font = list(family = "Times New Roman")),
        xaxis = list(title = "Year"),
        yaxis = list(title = "Average Yield (bushels per acre)"),
        plot_bgcolor = '#ffffff',
        paper_bgcolor = '#ffffff'
      )
  })
  
  # --- Year-over-Year Change Plot ---
  output$yoy_plot <- renderPlotly({
    req(yield_data(), input$yoy_year_slider)
    
    yoy <- yield_data() %>%
      group_by(State, county_name) %>%
      arrange(State, county_name, year) %>%
      mutate(yoy_change_pct = (Value - lag(Value)) / lag(Value) * 100) %>%
      filter(!is.na(yoy_change_pct)) %>%
      group_by(State, year) %>%
      summarise(yoy_change_pct = mean(yoy_change_pct, na.rm = TRUE), .groups = "drop") %>%
      filter(year == input$yoy_year_slider) %>%
      mutate(is_max = yoy_change_pct == max(yoy_change_pct),
             label = paste0(round(yoy_change_pct, 1), "%"))
    
    plot_ly(
      data = yoy,
      x = ~yoy_change_pct,
      y = ~State,
      type = 'bar',
      orientation = 'h',
      text = ~label,
      textposition = 'outside',
      marker = list(
        color = ~ifelse(is_max, '#81c784', '#aed581'),
        line = list(width = ~ifelse(is_max, 4, 1), color = 'black')
      )
    ) %>% layout(
      title = "Year-over-Year Corn Yield Change by State",
      xaxis = list(title = "YoY % Change"),
      yaxis = list(title = "State"),
      plot_bgcolor = '#ffffff',
      paper_bgcolor = '#ffffff'
    )
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
        addLegend("bottomright", pal = pal, values = values, title = "Number of Acres", opacity = 1) %>%
        setView(lng = -78.6569, lat = 37.5, zoom = 6)
    }
  })
  
  #Planting Progress 
  for (yr in years) {
    for (cat in categories) {
      safe_id <- paste0("plot_combined_", yr, "_", gsub("[^A-Za-z]", "_", cat))
      
      local({
        year_inner <- yr
        category_inner <- cat
        output_id <- safe_id
        
        output[[output_id]] <- renderPlotly({
          actual <- get_data(year_inner, category_inner)
          avg <- get_avg_data(year_inner, category_inner)
          
          if ((is.null(actual) || nrow(actual) == 0) && (is.null(avg) || nrow(avg) == 0)) {
            return(plotly_empty(type = "scatter", mode = "lines") %>%
                     layout(title = list(text = paste("No data for", clean_title(category_inner), "-", year_inner))))
          }
          
          combined <- bind_rows(
            if (!is.null(actual)) mutate(actual, Type = "Actual") else NULL,
            if (!is.null(avg)) mutate(avg, Type = "5-Year Avg") else NULL
          )
          
          ggplotly(
            ggplot(combined, aes(x = week, y = value, color = Type)) +
              geom_line(size = 1.2) +
              geom_point(size = 2.5) +
              scale_color_manual(values = c("Actual" = "#1b5e20", "5-Year Avg" = "#a5d6a7")) +
              labs(
                title = paste(clean_title(category_inner), "Progress —", year_inner),
                x = "Week Ending", y = "%", color = "Legend"
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 14, face = "bold"),
                axis.text.x = element_text(angle = 45, hjust = 1)
              ),
            tooltip = c("x", "y", "color")
          )
        })
      })
    }
  }

  # Get crop condition data for forecasting
  get_condition_data <- function() {
    tryCatch({
      data <- nassqs(list(
        commodity_desc = "CORN",
        year = format(Sys.Date(), "%Y"),
        state_name = "VIRGINIA",
        statisticcat_desc = "CONDITION",
        agg_level_desc = "STATE"
      ))
      
      data %>%
        filter(unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR")) %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value),
          condition = gsub("PCT ", "", unit_desc)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # Calculate yield deviation based on conditions
  calculate_yield_deviation <- function(condition_data) {
    if (is.null(condition_data)) return(NULL)
    
    # Calculate weighted score based on conditions
    condition_data %>%
      group_by(week) %>%
      summarise(
        weighted_score = sum(
          case_when(
            condition == "EXCELLENT" ~ value * 1.0,
            condition == "GOOD" ~ value * 0.5,
            condition == "FAIR" ~ value * 0.0,
            condition == "POOR" ~ value * -0.5,
            TRUE ~ 0
          )
        ),
        .groups = "drop"
      ) %>%
      mutate(
        # Convert weighted score to deviation percentage
        # Assuming 100% weighted score = 10% above trend
        deviation_pct = weighted_score / 10
      )
  }
  
  # Render condition distribution plot
  output$condition_distribution <- renderPlotly({
    condition_data <- get_condition_data()
    req(condition_data)
    
    plot_ly(
      data = condition_data,
      x = ~week,
      y = ~value,
      color = ~condition,
      type = "scatter",
      mode = "lines+markers",
      colors = c("EXCELLENT" = "#1a9850", "GOOD" = "#66bd63", 
                 "FAIR" = "#fee08b", "POOR" = "#d73027")
    ) %>%
      layout(
        title = "Current Crop Conditions",
        xaxis = list(title = "Week Ending"),
        yaxis = list(title = "Percentage", range = c(0, 100)),
        showlegend = TRUE
      )
  })
  
  # Render yield forecast plot
  output$yield_forecast <- renderPlotly({
    condition_data <- get_condition_data()
    req(condition_data)
    
    # Get historical trend yield (using average of last 5 years)
    historical_yield <- yield_data() %>%
      filter(State == "VA") %>%
      group_by(year) %>%
      summarise(avg_yield = mean(Value, na.rm = TRUE)) %>%
      tail(5) %>%
      summarise(trend_yield = mean(avg_yield, na.rm = TRUE)) %>%
      pull(trend_yield)
    
    # Calculate deviations
    deviations <- calculate_yield_deviation(condition_data)
    req(deviations)
    
    # Calculate forecasted yields
    forecast_data <- deviations %>%
      mutate(
        forecasted_yield = (1 + deviation_pct/100) * historical_yield,
        trend_line = historical_yield
      )
    
    plot_ly() %>%
      add_trace(
        data = forecast_data,
        x = ~week,
        y = ~forecasted_yield,
        type = "scatter",
        mode = "lines+markers",
        name = "Forecasted Yield",
        line = list(color = "#2e7d32", width = 2),
        marker = list(color = "#66bb6a", size = 8)
      ) %>%
      add_trace(
        data = forecast_data,
        x = ~week,
        y = ~trend_line,
        type = "scatter",
        mode = "lines",
        name = "Trend Yield",
        line = list(color = "#666666", width = 2, dash = "dash")
      ) %>%
      layout(
        title = "Yield Forecast vs Trend",
        xaxis = list(title = "Week Ending"),
        yaxis = list(title = "Yield (bushels/acre)"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      )
  })
  
  # Historical Yield Simulation
  output$historical_yield_simulation <- renderPlotly({
    selected_sim_year <- input$sim_year
    req(selected_sim_year)
    
    sim_condition_data <- corn_data_list[[selected_sim_year]]
    req(sim_condition_data)
    
    # Get historical trend yield (using average of last 5 years)
    historical_yield_sim <- yield_data() %>%
      filter(State == "VA") %>%
      group_by(year) %>% 
      summarise(avg_yield = mean(Value, na.rm = TRUE)) %>%
      tail(5) %>%
      summarise(trend_yield = mean(avg_yield, na.rm = TRUE)) %>%
      pull(trend_yield)
    
    # Get unique weeks in chronological order for the selected year
    unique_weeks <- sim_condition_data %>% 
      distinct(week) %>% 
      arrange(week) %>% 
      pull(week)
    
    forecast_evolution_list <- list()
    
    for (current_week_val in unique_weeks) {
      # Filter data up to the current week
      current_week_date <- as.Date(current_week_val) # Explicitly ensure it's Date
      data_up_to_week <- sim_condition_data %>% 
        filter(week <= current_week_date)
      
      # If there are no condition data up to this week (e.g., very early in the season before first report)
      # then skip this week's forecast
      if (nrow(data_up_to_week) == 0) {
        next
      }
      
      deviations <- calculate_yield_deviation(data_up_to_week)
      
      if (!is.null(deviations) && nrow(deviations) > 0) {
        # Use the deviation from the very last week in the `deviations` dataframe
        # which represents the forecast based on data up to `current_week`
        latest_deviation <- deviations %>% 
          filter(week == max(week)) %>% 
          pull(deviation_pct)
        
        # Ensure latest_deviation is not empty, which can cause type errors if it's numeric(0)
        if (length(latest_deviation) == 0 || !is.finite(latest_deviation)) {
          next 
        }
        
        current_forecasted_yield <- (1 + latest_deviation/100) * historical_yield_sim
        
        if (length(current_forecasted_yield) == 0 || !is.finite(current_forecasted_yield)) {
          next 
        }
        
        forecast_evolution_list <- append(forecast_evolution_list, list(tibble(
          week = current_week_date,
          forecasted_yield = as.numeric(current_forecasted_yield)
        )))
      }
    }
    
    forecast_evolution <- bind_rows(forecast_evolution_list)
    
    req(forecast_evolution)
    
    plot_ly() %>%
      add_trace(
        data = forecast_evolution,
        x = ~week,
        y = ~forecasted_yield,
        type = "scatter",
        mode = "lines+markers",
        name = "Simulated Forecast",
        line = list(color = "#1b5e20", width = 2),
        marker = list(color = "#43a047", size = 8)
      ) %>% 
      add_segments(
        x = min(forecast_evolution$week),
        xend = max(forecast_evolution$week),
        y = historical_yield_sim,
        yend = historical_yield_sim,
        line = list(color = '#666666', width = 2, dash = 'dash'),
        name = "Trend Yield",
        inherit = FALSE
      ) %>%
      layout(
        title = paste("Historical Yield Forecast Simulation for", selected_sim_year),
        xaxis = list(title = "Week Ending"),
        yaxis = list(title = "Yield (bushels/acre)"),
        showlegend = TRUE,
        legend = list(orientation = "h", y = -0.2)
      )
  })
}

# Run the shiny app 
shinyApp(ui = ui, server = server)


