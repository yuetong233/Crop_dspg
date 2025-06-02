#Dashboard Outline
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Load and prepare data
data <- read.csv("GoodCorn.csv")

data <- data %>%
  mutate(WeekNum = as.numeric(gsub("[^0-9]", "", Period))) %>%
  arrange(WeekNum) %>%
  mutate(Period = factor(Period, levels = unique(Period))) %>%
  mutate(Year = as.factor(Year))

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      h3, h4 {
        color: #2c3e50;
        font-weight: 600;
      }
      .tab-pane {
        padding: 20px;
      }
      body {
        background-color: #f8f9fa;
      }
    "))
  ),
  
  titlePanel("🌽 Virginia Corn Condition Dashboard"),
  
  tabsetPanel(
    
    # 1. Overview Tab ----
    tabPanel("Overview",
             fluidRow(
               column(12,
                      h3("Dashboard Purpose"),
                      p("This dashboard will help Virginia farmers and stakeholders explore crop condition trends using USDA NASS data."),
                      h4("Instructions"),
                      p("Use the tabs to explore data by year, quality, and remote sensing.")
               )
             )
    ),
    
    # 2. Conditions by Year Tab ----
    tabPanel("Conditions by Year",
             fluidRow(
               column(4,
                      h4("Select Year(s)"),
                      pickerInput("year", NULL,
                                  choices = c("2021", "2022", "2023", "2024"),
                                  multiple = TRUE,
                                  selected = "2024",
                                  options = list(`actions-box` = TRUE))
               ),
               column(8,
                      h4("Line Plot Placeholder"),
                      p("This will display a line plot of condition trends for the selected year(s).")
               )
             )
    ),
    
    # 3. Quality Comparison Tab ----
    tabPanel("Quality Comparison",
             fluidRow(
               column(4,
                      h4("Filter Options"),
                      pickerInput("good_year", "Select Year(s):",
                                  choices = levels(data$Year),
                                  selected = levels(data$Year),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE)),
                      
                      sliderInput("weekRange", "Select Week Range:",
                                  min = min(data$WeekNum, na.rm = TRUE),
                                  max = max(data$WeekNum, na.rm = TRUE),
                                  value = c(min(data$WeekNum, na.rm = TRUE), max(data$WeekNum, na.rm = TRUE)),
                                  step = 1, sep = "")
               ),
               column(8,
                      plotlyOutput("cornPlot")
               )
             )
    ),
    
    # 4. Remote Sensing Tab ----
    tabPanel("Remote Sensing Data",
             fluidRow(
               column(12,
                      h4("Remote Sensing Placeholder"),
                      p("This section will include Remote Sensing data once available.")
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  output$cornPlot <- renderPlotly({
    filtered_data <- data %>%
      filter(Year %in% input$good_year,
             WeekNum >= input$weekRange[1],
             WeekNum <= input$weekRange[2])
    
    p <- ggplot(filtered_data, aes(
      x = Period, y = Value, color = Year, group = Year,
      text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
    )) +
      geom_line(linewidth = 1.2) +
      geom_point() +
      labs(
        title = "Corn Rated 'Good' by Week in Virginia",
        x = "Week",
        y = "Percent Rated Good"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8))
    
    ggplotly(p, tooltip = "text")
  })
}

# Run App
shinyApp(ui = ui, server = server)

