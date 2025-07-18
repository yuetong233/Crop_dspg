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
  filter(!is.na(County)) %>%  
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
clean_title <- function(cat) {
  title <- gsub("^PCT ", "", cat)  # Remove "PCT "
  paste0(toupper(substr(title, 1, 1)), tolower(substr(title, 2, nchar(title))))  # Capitalize first letter
}
categories <- c(
  "PCT PLANTED", "PCT EMERGED", "PCT SILKING",
  "PCT DOUGH", "PCT DENTED", "PCT MATURE", "PCT HARVESTED"
)
get_category_description <- function(cat, year) {
  cat_key <- toupper(trimws(cat))  # Normalize for matching
  cat_clean <- clean_title(cat_key)  # Clean title for fallback
  
  descriptions <- list(
    "PCT PLANTED" = paste0("This chart shows the weekly progress of corn ", cat_clean, " in Virginia during ", year,
                           ". It helps identify early or delayed planting relative to the 5-Year Average."),
    "PCT EMERGED" = paste0("This chart illustrates how corn has ", cat_clean, " throughout the ", year,
                           " season. Early emergence suggests favorable weather and soil conditions."),
    "PCT SILKING" = paste0(cat_clean, " marks the beginning of pollination. This chart tracks its weekly progress during ", year, "."),
    "PCT DOUGH" = paste0("This chart shows how much corn reached the ", cat_clean, " stage in ", year,
                         ", a key indicator of grain development."),
    "PCT DENTED" = paste0("The ", cat_clean, " stage reflects kernel hardening. Track its progress through the ", year, " season."),
    "PCT MATURE" = paste0("This chart displays weekly percentages of corn reaching ", cat_clean, " in ", year,
                          ", which affects harvest scheduling."),
    "PCT HARVESTED" = paste0("This chart tracks how much of the corn crop was ", cat_clean, " each week in ", year,
                             ", compared to the historical average.")
  )
  
  if (!is.null(descriptions[[cat_key]])) {
    descriptions[[cat_key]]
  } else {
    paste0("This chart compares weekly values to the 5-Year Average for corn ", cat_clean, " in ", year, ".")
  }
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
  
  tags$head(
    tags$style(HTML("
    /* Make navlistPanel sidebar buttons prettier */
    .nav-pills > li > a {
      background-color: #e0f8eb;   /* light green background */
      color: #2e7d32;              /* VT green text */
      font-family: 'Times New Roman', serif;
      font-weight: bold;
      border-radius: 10px;
      margin-bottom: 10px;
      padding: 12px;
      transition: all 0.2s ease-in-out;
    }

    .nav-pills > li.active > a {
      background-color: #2e7d32 !important; /* active tab */
      color: white !important;
    }

    .nav-pills > li > a:hover {
      background-color: #c8e6c9;
      color: #1b5e20;
    }
  "))
  ),
  
  
  div(class = "title-box",
      h1("Planting Progress and Crop Condition Interactive Dashboard")
  ),
  
  navlistPanel(
    widths = c(2, 10),
    
    tabPanel("Objective",
             div(
               style = "position: relative; min-height: 100vh; padding: 20px; overflow: hidden;",
               
               # 🌽 Background video
               tags$video(
                 autoplay = NA, muted = NA, loop = NA, playsinline = NA,
                 style = "
        position: absolute;
        top: 0; left: 0;
        width: 100%; height: 100%;
        object-fit: cover;
        z-index: -1;
        opacity: 0.92;
      ",
                 tags$source(
                   src = "https://cdn.pixabay.com/video/2022/09/23/132321-753435412_large.mp4",
                   type = "video/mp4"
                 )
               ),
               
               # 🌽 Mission Statement Card
               absolutePanel(
                 draggable = TRUE,
                 top = "60px", left = "2%", width = 360,
                 style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); font-family: 'Lora', serif;",
                 h4("🌽 Mission Statement", style = "color: #2e7d32; font-weight: bold;"),
                 p("This dashboard was developed through the 2025 Data Science for the Public Good (DSPG) Program at Virginia Tech, in collaboration with the Virginia Corn Board."),
                 p("Our mission is to empower Virginia corn producers, Extension agents, and stakeholders with data-driven tools to support informed planting, management, and marketing decisions."),
                 p("By integrating weekly USDA NASS data, we aim to reduce uncertainty and enhance transparency in Virginia's grain marketing landscape.")
               ),
               
               # 📘 Instructions Card
               absolutePanel(
                 draggable = TRUE,
                 top = "60px", left = "35%", width = 360,
                 style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); font-family: 'Lora', serif;",
                 h4("📘 How to Use This Dashboard", style = "color: #2e7d32; font-weight: bold;"),
                 p("📊 Use the sidebar to navigate through key data modules."),
                 p("🖱️ Hover over plots to view weekly percentages, trends, or county-level insights."),
                 p("📅 Data is updated weekly via the USDA NASS API and includes historical trends.")
               ),
               
               # 🧭 Dashboard Tab Overview Card
               absolutePanel(
                 draggable = TRUE,
                 top = "60px", left = "68%", width = 360,
                 style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); font-family: 'Lora', serif;",
                 h4("🧭 Dashboard Tab Summaries", style = "color: #2e7d32; font-weight: bold;"),
                 tags$ul(
                   tags$li(strong("🌱 Planting Progress:"), " Weekly crop development progress with historical comparison."),
                   tags$li(strong("🌾 Crop Conditions:"), " Quality ratings from Excellent to Very Poor via stacked plots."),
                   tags$li(strong("🛰️ Remote Sensing:"), " Placeholder for satellite imagery and NDVI data (coming soon)."),
                   tags$li(strong("🗺️ County Analysis:"), " Interactive maps showing acres planted/harvested by county."),
                   tags$li(strong("📈 Yield Analysis:"), " Annual yield trends and comparisons across states."),
                   tags$li(strong("🔮 Yield Forecast:"), " Predictive models using condition data to forecast yield."),
                   tags$li(strong("⏳ Historical Simulation:"), " Replay of past years to visualize forecasting evolution.")
                 )
               )
             )
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
                                
                                p(get_category_description(cat, yr))
                                
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
             p("This dashboard presents an analysis of corn crop yield across Virginia, North Carolina, and Maryland from 2015 to 2023. \
   The data is sourced from the USDA's National Agricultural Statistics Service (NASS) API and includes county-level statistics on corn yield (bushels per acre). \
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
             
             h4("Yield Trends Over Time"),
             plotlyOutput("yield_plot"),
             
             h4("Summary Statistics"),
             div(style = "margin-bottom:-30px;", plotlyOutput("summary_card"))
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
}

# Run the shiny app 
shinyApp(ui = ui, server = server)


