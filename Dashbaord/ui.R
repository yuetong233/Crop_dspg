#UI
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
source("functions.R")

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
               
               absolutePanel(
                 draggable = TRUE,
                 top = "60px", left = "2%", width = 360,
                 style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
         padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); font-family: 'Lora', serif;",
                 h4("🌽 Mission Statement", style = "color: #2e7d32; font-weight: bold;"),
                 p("This dashboard was developed through the 2025 Data Science for the Public Good (DSPG) Program at Virginia Tech, in partnership with the Virginia Corn Board."),
                 p("Our mission is to enhance market efficiency and profitability for Virginia corn producers by integrating real-time USDA NASS data into an open-access dashboard."),
                 p("By equipping farmers, Extension agents, and agricultural organizations with timely crop progress and condition insights, we support more informed planting, management, and grain marketing decisions.")
               ),
               
               
               absolutePanel(
                 draggable = TRUE,
                 top = "60px", left = "35%", width = 360,
                 style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
         padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); font-family: 'Lora', serif;",
                 h4("📘 How to Use This Dashboard", style = "color: #2e7d32; font-weight: bold;"),
                 p("📊 Use the sidebar to explore key planting, crop condition, remote sensing, and yield insights."),
                 p("🖱️ Hover over interactive plots and maps to view weekly trends, county-level data, and comparisons across states."),
                 p("📅 Dashboard updates align with USDA NASS weekly releases to ensure timely and accurate information for decision-making.")
               ),
               
               absolutePanel(
                 draggable = TRUE,
                 top = "60px", left = "68%", width = 360,
                 style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
         padding: 20px; box-shadow: 0 4px 12px rgba(0,0,0,0.15); font-family: 'Lora', serif;",
                 h4("🧭 Dashboard Tab Summaries", style = "color: #2e7d32; font-weight: bold;"),
                 tags$ul(
                   tags$li(strong("🌱 Planting Progress:"), " Visualize county-level weekly planting activity compared to prior years and neighboring states."),
                   tags$li(strong("🌾 Crop Conditions:"), " Track crop quality (Excellent to Very Poor) using stacked line plots over the growing season."),
                   tags$li(strong("🛰️ Remote Sensing:"), " (Coming soon) Integrate NDVI and satellite data to monitor field-level vegetative health."),
                   tags$li(strong("🗺️ County Analysis:"), " Interactive maps showing acres planted and harvested, by county and year."),
                   tags$li(strong("📈 Yield Analysis:"), " Historical trends and inter-state yield comparisons to support marketing strategies."),
                   tags$li(strong("🔮 Yield Forecast:"), " Predictive tools leveraging crop condition data for early-season yield estimation."),
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
             h4("About This Data"),              
             p("This section presents weekly vegetation health (NDVI) and MODIS Land Surface Temperature (LST) data over Virginia corn fields from 2021 through 2025."),                            
             
             h4("Understanding NDVI"),              
             p("NDVI (Normalized Difference Vegetation Index) is calculated using Landsat 8 imagery with the formula:"),              
             tags$ul(
               tags$li("NDVI = (NIR - Red) / (NIR + Red)"),                
               tags$li("NIR = Near Infrared (Band 5), Red = Band 4"),                
               tags$li("NDVI values range from -1 to 1, where higher values indicate healthier vegetation.")
             ),              
             p("The NDVI data was processed using Google Earth Engine, filtered using USDA Cropland Data Layers to isolate corn-growing areas, and aggregated weekly at the county level."),                            
             
             h4("Understanding MODIS Temperature"),              
             p("The temperature data comes from MODIS satellite-derived Land Surface Temperature (LST) products, which capture how hot the surface of the earth feels from space. We include weekly composites of day, night, and average temperatures for each county."),
             p("This helps assess heat stress and environmental trends that may impact corn growth over time."),
             
             br(),
             h4("NDVI Trends (Full Year)"),
             
             sidebarLayout(
               sidebarPanel(
                 h5("NDVI Controls"),
                 radioButtons("ndvi_source", "NDVI View:",
                              choices = c("Top 10 Counties", "All Counties"),
                              selected = "All Counties"),
                 selectInput("ndvi_year", "NDVI Year:", choices = 2021:2025, selected = 2025),
                 selectInput("ndvi_county_selector", "NDVI Counties:", choices = NULL, multiple = TRUE),
                 
                 br(), hr(),
                 
                 h5("🌡️ Temperature Controls"),
                 radioButtons("temp_source", "Temperature View:",
                              choices = c("Top 10 Counties", "Last 6 Months"),
                              selected = "Top 10 Counties"),
                 selectInput("temp_year", "Temperature Year:", choices = 2021:2025, selected = 2025),
                 selectInput("temp_type", "Temperature Type:", choices = c("Average", "High", "Low"), selected = "Average"),
                 selectInput("temp_county_selector", "Temperature Counties:", choices = NULL, multiple = TRUE)
               ),
               
               mainPanel(
                 tabsetPanel(
                   tabPanel("NDVI Plot", plotlyOutput("ndvi_plot", height = "500px")),
                   tabPanel("Temperature Plot", plotlyOutput("temp_plot", height = "500px"))
                 )
               )
             )
    ),
    
    
    
    
    tabPanel("County Analysis",
             h4("About This Data"),
             p("This section provides insight into county-level corn production trends across Virginia, Maryland, and North Carolina from 2000 to 2025."),
             p("You can explore historical acres planted, acres harvested, and harvest success rates. All data is sourced from the USDA National Agricultural Statistics Service (NASS) API and updates as new values are released."),
             p("Use the dropdown menus to compare trends across counties within each state. Urban counties such as Fairfax or Arlington may be excluded due to limited or missing agricultural reporting."),
             
             br(), hr(),
             
             h4("Acres Planted (2000–2025)", style = "font-family: 'Times New Roman'; color: darkgreen;"),
             fluidRow(
               column(4, selectInput("state_planted", "Select State", choices = c("VA", "NC", "MD"), selected = "VA")),
               column(4, uiOutput("county_planted_ui"))
             ),
             plotlyOutput("plot_planted", height = "400px"),
             
             br(), hr(),
             
             h4("Acres Harvested (2000–2025)", style = "font-family: 'Times New Roman'; color: darkgreen;"),
             fluidRow(
               column(4, selectInput("state_harvested", "Select State", choices = c("VA", "NC", "MD"), selected = "VA")),
               column(4, uiOutput("county_harvested_ui"))
             ),
             plotlyOutput("plot_harvested", height = "400px"),
             
             br(), hr(),
             
             h4("Harvest Success Rate (%)", style = "font-family: 'Times New Roman'; color: darkgreen;"),
             fluidRow(
               column(4, selectInput("state_success", "Select State", choices = c("VA", "NC", "MD"), selected = "VA")),
               column(4, uiOutput("county_success_ui"))
             ),
             plotlyOutput("plot_success", height = "400px"),
             
             br(), hr(),
             
             h4("County-Level Choropleth Map"),
             fluidRow(
               lapply(years, function(yr) {
                 column(2, actionButton(paste0("btn_", yr), label = yr))
               })
             ),
             br(),
             leafletOutput("compare_map", height = "600px"),
             
             br()
    ),
    
    
    
    
    tabPanel("Yield Analysis",
             h4("Historical Corn Yield Analysis"),
             fluidRow(
               column(6,
                      pickerInput("yield_states", "Select States:",
                                  choices = c("Virginia" = "VA", "North Carolina" = "NC", "Maryland" = "MD"),
                                  selected = c("VA", "NC", "MD"), multiple = TRUE,
                                  options = list(`actions-box` = TRUE)
                      )
               ),
               column(3,
                      selectInput("county_map_state", "Select State for County Map:",
                                  choices = c("Virginia" = "VA", "North Carolina" = "NC", "Maryland" = "MD"),
                                  selected = "VA"
                      )
               ),
               column(3,
                      uiOutput("county_map_year_ui")
               )
             ),
             h4("Yield Trends Over Time"),
             plotlyOutput("yield_state_plot"),
             h4("County Yield Choropleth Map"),
             leafletOutput("yield_county_map", height = "600px")
             
    ),
    
    tabPanel("Yield Forecast",
             h4("About This Feature"),
             p("This section provides a weekly forecast of corn yield for Virginia based on current crop conditions. The forecast uses a regression model that incorporates the distribution of crop conditions (Excellent and Good) to predict yield deviation from trend."),
             p("The model assumes that better crop conditions (higher percentages of Excellent and Good ratings) will result in yields above trend, while poorer conditions will result in yields below trend."),
             br(),
             fluidRow(
               column(6,
                      plotlyOutput("condition_distribution", height = "400px")
               ),
               column(6,
                      plotlyOutput("yield_forecast", height = "400px")
               )
             ),
             br(),
             fluidRow(
               column(12,
                      plotlyOutput("yield_comparison", height = "400px")
               )
             )
    ),
    
    tabPanel("Data Sources",
             h3("Data Sources Summary", style = "color: #2e7d32;"),
             p("This table lists all variables used in the dashboard along with their data sources, update frequency, spatial granularity, and connection type.", style = "font-size: 16px;"),
             
             tags$table(
               class = "table table-hover table-bordered",
               style = "background-color: #f9fdf9; border-color: #c8e6c9;",
               tags$thead(
                 style = "background-color: #a5d6a7; color: black;",
                 tags$tr(
                   tags$th("Dashboard Section"),
                   tags$th("Variable"),
                   tags$th("Source"),
                   tags$th("Update Frequency"),
                   tags$th("Granularity"),
                   tags$th("Data Connection")
                 )
               ),
               tags$tbody(
                 tags$tr(tags$td("Crop Conditions"), tags$td("condition, value, week"), tags$td("USDA NASS API"), tags$td("Weekly during season"), tags$td("State"), tags$td("Live API")),
                 
                 tags$tr(tags$td("Yield Analysis"), tags$td("Value (Yield), year, State"), tags$td("USDA NASS API → CSV"), tags$td("Annually"), tags$td("State"), tags$td("Static CSV")),
                 tags$tr(tags$td("Yield Forecast"), tags$td("PCT POOR, FAIR, GOOD, EXCELLENT"), tags$td("USDA NASS API"), tags$td("Weekly during season"), tags$td("State"), tags$td("Live API")),
                 tags$tr(tags$td("Yield Forecast Comparison"), tags$td("Yield (Value, Year), G+E (GOOD + EXCELLENT), mEVI_678"), tags$td("USDA NASS API, Google Earth Engine (Landsat) → CSV"), tags$td("Annually (Yield), Weekly during season (G+E), May–Sept (manual export, EVI)"), tags$td("State"), tags$td("Static CSV")),
                 
                 tags$tr(tags$td("County Analysis"), tags$td("Planted, Harvested, County, Year"), tags$td("USDA NASS API → CSV"), tags$td("Annually"), tags$td("County"), tags$td("Static CSV")),
                 tags$tr(tags$td("Planting Progress"), tags$td("week, value, category"), tags$td("USDA NASS API"), tags$td("Weekly during season"), tags$td("State"), tags$td("Live API")),
                 
                 tags$tr(tags$td("NDVI (All Counties)"), tags$td("NDVI_mean, county_name, date"), tags$td("Google Earth Engine (Landsat) → CSV"), tags$td("Weekly (manual export)"), tags$td("County"), tags$td("Static CSV")),
                 tags$tr(tags$td("NDVI (Top 10 Counties)"), tags$td("NDVI_mean, county_name, date"), tags$td("Google Earth Engine (Landsat) → CSV"), tags$td("May–Sept (manual export)"), tags$td("County"), tags$td("Static CSV")),
                 
                 tags$tr(tags$td("Temperature (All Counties)"), tags$td("T_day, T_night, T_avg, county, date"), tags$td("MODIS (NASA) → CSV"), tags$td("Weekly (manual export)"), tags$td("County"), tags$td("Static CSV")),
                 tags$tr(tags$td("Temperature (Top 10 Counties)"), tags$td("T_day, T_night, T_avg, county, date"), tags$td("MODIS (NASA) → CSV"), tags$td("May–Sept (manual export)"), tags$td("County"), tags$td("Static CSV"))
                 
                 
               )
             )
    ),
    
    tabPanel(
      "About Us",
      fluidPage(
        h2("About the Project"),
        p("This dashboard was developed through the Data Science for the Public Good (DSPG) program at Virginia Tech, in collaboration with the Virginia Corn Board and the Kohl Centre for Agricultural Economic Development. The project leverages public datasets, remote sensing imagery, and predictive analytics to support Virginia’s corn growers and stakeholders."),
        
        br(),
        
        p("Our goal was to create an interactive, data-driven tool that enables users to explore corn crop trends at both state and county levels. This dashboard integrates data from the USDA National Agricultural Statistics Service (NASS), Google Earth Engine (GEE), and MODIS satellite products to track crop condition, planting progress, weather trends, and yield estimates in near real-time."),
        
        br(),
        
        p("The DSPG program empowers undergraduate and graduate students to apply statistical and computational methods to real-world challenges in agriculture, public health, education, and beyond. By combining stakeholder engagement with technical training, the program fosters the development of tools that are both impactful and accessible."),
        
        br(),
        
        p("With support from the Virginia Corn Board, this tool was developed to improve transparency, planning, and decision-making across the Commonwealth’s corn sector. Farmers, extension agents, and policymakers can use this dashboard to monitor crop progress, assess environmental stressors, and anticipate yield outcomes — ultimately promoting more resilient and informed agricultural systems."),
        
        fluidRow(
          column(6,
                 img(src = "data:image/png;base64,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", height = "150px", style = "margin-top: 10px;")
          ),
          column(6,
                 img(src = "https://raw.githubusercontent.com/yuetong233/Crop_dspg/refs/heads/main/KohlCentre_Vertical_FullColor_RGB%201.jpg", height = "90px", style = "margin-top: 20px; float: right;")
          )
        ),
        
        
        br(), hr(),
        
        h3("Team Members"),
        tags$ul(
          tags$li(strong("Undergraduate Interns:"), " Maryam Rehmatulla and Shlok Kulkarni"),
          tags$li(strong("Graduate Fellow:"), " Yuetong Zhang"),
          tags$li(strong("Faculty Advisors:"), " Dr. Michael Cary, Dr. Mario Ortez Amador, and Dr. Le Wang")
        ),
        
      )
    )
    
    
  )
)

