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

