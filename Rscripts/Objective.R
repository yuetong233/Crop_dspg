#Objective
library(shiny)
library(bslib)
library(htmltools)

ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Lora")),
  
  # Background using a locally saved .jpg in the same folder as this script
  tags$head(
    tags$style(HTML("
    body::before {
      content: '';
      position: fixed;
      top: 0; left: 0;
      width: 100vw; height: 100vh;
      background-image: url('https://v.ftcdn.net/03/22/96/53/700_F_322965329_ofLH1F3uDTCuO4zBzRJFEEx5w0UVE9ds_ST.mp4');
      background-size: cover;
      background-size: cover;
      background-repeat: no-repeat;
      background-position: center center;
      z-index: -1;
      opacity: 0.92;
    }
  "))
  ),
  
  tagList(
    # 🌽 Mission Statement Card
    absolutePanel(
      draggable = TRUE,
      top = "60px", left = "40px", width = 360,
      style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0px 4px 12px rgba(0,0,0,0.1); font-family: 'Lora', serif;",
      h4("🌽 Mission Statement", style = "color: #2e7d32; font-weight: bold;"),
      p("This dashboard was developed through the 2025 Data Science for the Public Good (DSPG) Program at Virginia Tech, in collaboration with the Virginia Corn Board."),
      p("Our mission is to empower Virginia corn producers, Extension agents, and stakeholders with data-driven tools to support informed planting, management, and marketing decisions."),
      p("By integrating weekly data from the USDA National Agricultural Statistics Service (NASS), we aim to reduce uncertainty and enhance transparency in the grain marketing landscape.")
    ),
    
    # 📘 Instructions Card
    absolutePanel(
      draggable = TRUE,
      top = "60px", left = "440px", width = 360,
      style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0px 4px 12px rgba(0,0,0,0.1); font-family: 'Lora', serif;",
      h4("📘 How to Use This Dashboard", style = "color: #2e7d32; font-weight: bold;"),
      p("📊 The dashboard consists of seven core modules, accessible via the sidebar navigation. Each module provides interactive visualizations, including plots, maps, and forecasts."),
      p("🖱️ Hover over plots to view weekly percentages, tooltips, or county-specific insights."),
      p("📅 The data is updated weekly based on USDA NASS releases. Historical data spans back to 2015 in some tabs for trend analysis."),
      p("🧭 The tools are designed for flexibility — whether you're a farmer checking this week's progress or an analyst modeling crop trends.")
    ),
    
    # 🧭 Dashboard Tabs Overview Card
    absolutePanel(
      draggable = TRUE,
      top = "60px", left = "840px", width = 360,
      style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0px 4px 12px rgba(0,0,0,0.1); font-family: 'Lora', serif;",
      h4("🧭 Dashboard Tab Summaries", style = "color: #2e7d32; font-weight: bold;"),
      tags$ul(
        tags$li(strong("Planting Progress:"), " Track weekly crop development stages (2021–present) and compare to 5-year historical averages."),
        tags$li(strong("Crop Conditions:"), " Visualize weekly condition quality ratings (Excellent to Very Poor) using stacked area plots."),
        tags$li(strong("Remote Sensing:"), " Placeholder for future integration of satellite-derived crop indicators."),
        tags$li(strong("County Analysis:"), " Explore planted and harvested acres by county across VA, NC, and MD."),
        tags$li(strong("Yield Analysis:"), " Analyze yearly trends, moving averages, and year-over-year yield changes."),
        tags$li(strong("Yield Forecast:"), " Generate real-time yield predictions using current crop conditions."),
        tags$li(strong("Historical Yield Simulation:"), " Replay prior years to see how yield forecasts evolve week by week.")
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)


#video maybe
#Objective
library(shiny)
library(bslib)
library(htmltools)

ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Lora")),
  
  # Ensure absolute panels are on top of video
  tags$head(
    tags$style(HTML("
      .absolute-panel {
        position: relative;
        z-index: 1;
      }
    "))
  ),
  
  # 🌽 Background video (looped, autoplayed, muted)
  tags$video(
    id = "bgvid", autoplay = NA, muted = NA, loop = NA, playsinline = NA,
    style = "
      position: fixed;
      top: 0; left: 0;
      width: 100vw; height: 100vh;
      object-fit: cover;
      z-index: -1;
      opacity: 0.92;
    ",
    tags$source(
      src = "https://media.istockphoto.com/id/512010789/video/flying-over-a-golden-cornfield-at-sunrise-sunset-video-clip.mp4?s=mp4-640x640-is&k=20&c=bUpk5TvDTxjeyy9lSPpma7NwpMC_JWn7-S9MW_xdzKo=",
      type = "video/mp4"
    )
  ),
  
  tagList(
    # 🌽 Mission Statement Card
    absolutePanel(
      draggable = TRUE,
      top = "60px", left = "40px", width = 360,
      style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0px 4px 12px rgba(0,0,0,0.1); font-family: 'Lora', serif;",
      h4("🌽 Mission Statement", style = "color: #2e7d32; font-weight: bold;"),
      p("This dashboard was developed through the 2025 Data Science for the Public Good (DSPG) Program at Virginia Tech, in collaboration with the Virginia Corn Board."),
      p("Our mission is to empower Virginia corn producers, Extension agents, and stakeholders with data-driven tools to support informed planting, management, and marketing decisions."),
      p("By integrating weekly data from the USDA National Agricultural Statistics Service (NASS), we aim to reduce uncertainty and enhance transparency in the grain marketing landscape.")
    ),
    
    # 📘 Instructions Card
    absolutePanel(
      draggable = TRUE,
      top = "60px", left = "440px", width = 360,
      style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0px 4px 12px rgba(0,0,0,0.1); font-family: 'Lora', serif;",
      h4("📘 How to Use This Dashboard", style = "color: #2e7d32; font-weight: bold;"),
      p("📊 The dashboard consists of seven core modules, accessible via the sidebar navigation. Each module provides interactive visualizations, including plots, maps, and forecasts."),
      p("🖱️ Hover over plots to view weekly percentages, tooltips, or county-specific insights."),
      p("📅 The data is updated weekly based on USDA NASS releases. Historical data spans back to 2015 in some tabs for trend analysis."),
      p("🧭 The tools are designed for flexibility — whether you're a farmer checking this week's progress or an analyst modeling crop trends.")
    ),
    
    # 🧭 Dashboard Tabs Overview Card
    absolutePanel(
      draggable = TRUE,
      top = "60px", left = "840px", width = 360,
      style = "background-color: #e0f8eb; border: 2px solid #2e7d32; border-radius: 10px;
               padding: 20px; box-shadow: 0px 4px 12px rgba(0,0,0,0.1); font-family: 'Lora', serif;",
      h4("🧭 Dashboard Tab Summaries", style = "color: #2e7d32; font-weight: bold;"),
      tags$ul(
        tags$li(strong("Planting Progress:"), " Track weekly crop development stages (2021–present) and compare to 5-year historical averages."),
        tags$li(strong("Crop Conditions:"), " Visualize weekly condition quality ratings (Excellent to Very Poor) using stacked area plots."),
        tags$li(strong("Remote Sensing:"), " Placeholder for future integration of satellite-derived crop indicators."),
        tags$li(strong("County Analysis:"), " Explore planted and harvested acres by county across VA, NC, and MD."),
        tags$li(strong("Yield Analysis:"), " Analyze yearly trends, moving averages, and year-over-year yield changes."),
        tags$li(strong("Yield Forecast:"), " Generate real-time yield predictions using current crop conditions."),
        tags$li(strong("Historical Yield Simulation:"), " Replay prior years to see how yield forecasts evolve week by week.")
      )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)

