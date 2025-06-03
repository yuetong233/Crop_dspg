#Dashboard Outline
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(zoo)

# Load and prepare data
data <- read.csv("GoodCorn.csv")

data <- data %>%
  mutate(WeekNum = as.numeric(gsub("[^0-9]", "", Period))) %>%
  arrange(WeekNum) %>%
  mutate(Period = factor(Period, levels = unique(Period))) %>%
  mutate(Year = as.factor(Year))

# UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),  # Simple clean theme
  titlePanel("Planting Progress and Crop Condition Interactive Dashboard"),
  
  navlistPanel(
    widths = c(2, 10),  # Sidebar = 2/12, Main panel = 10/12
    
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
             pickerInput("good_year", "Select Year(s):",
                         choices = levels(data$Year),
                         selected = levels(data$Year),
                         multiple = TRUE,
                         options = list(`actions-box` = TRUE)),
             sliderInput("weekRange", "Select Week Range:",
                         min = min(data$WeekNum, na.rm = TRUE),
                         max = max(data$WeekNum, na.rm = TRUE),
                         value = c(min(data$WeekNum, na.rm = TRUE), max(data$WeekNum, na.rm = TRUE)),
                         step = 1, sep = ""),
             plotlyOutput("cornPlot")
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
