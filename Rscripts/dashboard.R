#Dashboard
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)

# Load data
good_data <- read.csv("GoodCorn.csv")
poor_data <- read.csv("PoorCorn.csv")

# Preprocess both
good_data <- good_data %>%
  mutate(
    WeekNum = as.numeric(gsub("[^0-9]", "", Period)),
    Period = factor(Period, levels = unique(Period[order(as.numeric(gsub("[^0-9]", "", Period)))])),
    Year = as.factor(Year)
  )

poor_data <- poor_data %>%
  mutate(
    WeekNum = as.numeric(gsub("[^0-9]", "", Period)),
    Period = factor(Period, levels = unique(Period[order(as.numeric(gsub("[^0-9]", "", Period)))])),
    Year = as.factor(Year)
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
             pickerInput("year", "Select Year(s):",
                         choices = c("2021", "2022", "2023", "2024"),
                         multiple = TRUE,
                         selected = "2024",
                         options = list(`actions-box` = TRUE)),
             h4("Planting Progress Line Plot (Coming Soon)"),
             p("This section will display planting progress trends for the selected years.")
    ),
    
    tabPanel("Crop Conditions",
             # Reference paragraph
             h4("About This Data"),
             p("This section presents weekly crop condition data for corn in Virginia from 2021 to 2024, sourced from the USDA National Agricultural Statistics Service (NASS). The data is based on survey responses evaluating crop health, categorized into five condition levels: Excellent, Good, Fair, Poor, and Very Poor."),
             p("Weekly observations span Week 19 to Week 38, which approximately correspond to the months of mid-May through mid-September. These ratings offer insight into how Virginia's corn crops performed throughout the growing season each year."),
             
             # Filters
             pickerInput("selected_years", "Select Year(s):",
                         choices = levels(good_data$Year),
                         selected = levels(good_data$Year),
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE)),
             
             sliderInput("weekRange", "Select Week Range:",
                         min = min(good_data$WeekNum, na.rm = TRUE),
                         max = max(good_data$WeekNum, na.rm = TRUE),
                         value = c(min(good_data$WeekNum, na.rm = TRUE), max(good_data$WeekNum, na.rm = TRUE)),
                         step = 1, sep = ""),
             
             # Tabs for Good and Poor plots
             tabsetPanel(
               tabPanel("Good", plotlyOutput("goodPlot")),
               tabPanel("Poor", plotlyOutput("poorPlot"))
             )
    ),
    
    
    tabPanel("Remote Sensing",
             h4("Remote Sensing Placeholder"),
             p("This section will include Remote Sensing data once available.")
    ),
    
    tabPanel("State Comparison",
             radioButtons("state_compare", "Compare Virginia to:",
                          choices = c("North Carolina", "Maryland"),
                          selected = "North Carolina"),
             h4("State Comparison (Data Needed)"),
             p("This section will show yield comparisons between Virginia and selected states when data is available.")
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_good <- reactive({
    good_data %>%
      filter(
        Year %in% input$selected_years,
        WeekNum >= input$weekRange[1],
        WeekNum <= input$weekRange[2]
      )
  })
  
  filtered_poor <- reactive({
    poor_data %>%
      filter(
        Year %in% input$selected_years,
        WeekNum >= input$weekRange[1],
        WeekNum <= input$weekRange[2]
      )
  })
  
  output$goodPlot <- renderPlotly({
    ggplotly(
      ggplot(filtered_good(), aes(
        x = Period, y = Value, color = Year, group = Year,
        text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
      )) +
        geom_line(linewidth = 1.2) +
        geom_point() +
        labs(title = "Corn Rated 'Good' by Week in Virginia",
             x = "Week", y = "Percent Rated Good") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)),
      tooltip = "text"
    )
  })
  
  output$poorPlot <- renderPlotly({
    ggplotly(
      ggplot(filtered_poor(), aes(
        x = Period, y = Value, color = Year, group = Year,
        text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
      )) +
        geom_line(linewidth = 1.2) +
        geom_point() +
        labs(title = "Corn Rated 'Poor' by Week in Virginia",
             x = "Week", y = "Percent Rated Poor") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)),
      tooltip = "text"
    )
  })
}

# Run App
shinyApp(ui = ui, server = server)
