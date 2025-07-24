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

