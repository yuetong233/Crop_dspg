#Dashboard Outline
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(zoo)
library(rnassqs)
#install.packages('shinyWidgets')
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")
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
    ),
    
    tabPanel("Corn Yield Analysis",
             h3("Corn Yield Analysis"),
             p("Explore corn yield trends across Virginia and neighboring states."),
             
             # Controls
             fluidRow(
               column(4,
                      pickerInput("yield_states", "Select States:",
                                  choices = c("Virginia" = "VA",
                                              "North Carolina" = "NC",
                                              "Maryland" = "MD"),
                                  selected = c("VA", "NC", "MD"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE))
               ),
               column(4,
                      sliderInput("yield_year_range", "Select Year Range:",
                                  min = 2000,
                                  max = as.numeric(format(Sys.Date(), "%Y")),
                                  value = c(as.numeric(format(Sys.Date(), "%Y")) - 10, 
                                            as.numeric(format(Sys.Date(), "%Y"))),
                                  step = 1)
               ),
               column(4,
                      numericInput("ma_window", "Moving Average Window:",
                                   value = 5,
                                   min = 2,
                                   max = 10)
               )
             ),
             
             # Outputs
             tabsetPanel(
               tabPanel("Yield Trends",
                        plotlyOutput("yield_plot")
               ),
               tabPanel("Year-over-Year Changes",
                        plotlyOutput("yoy_plot")
               ),
               tabPanel("Summary Statistics",
                        tableOutput("summary_table")
               )
             )
    )
  )
)

# Server
server <- function(input, output) {
  # Existing corn plot
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
  
  # Corn yield analysis
  # Get data
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
  
  # Yield trends plot
  output$yield_plot <- renderPlotly({
    req(yield_data())
    
    state_year_avg <- yield_data() %>%
      group_by(State, year) %>%
      summarize(avg_yield = mean(Value, na.rm = TRUE)) %>%
      group_by(State) %>%
      mutate(moving_avg = zoo::rollmean(avg_yield, 
                                        k = input$ma_window, 
                                        fill = NA, 
                                        align = "right"))
    
    p <- ggplot(state_year_avg, aes(x = year, y = avg_yield, color = State)) +
      geom_line(size = 1.2) +
      geom_line(aes(y = moving_avg, linetype = "Moving Avg"), size = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black", face = "bold"),
        legend.title = element_blank()
      ) +
      labs(
        title = "Statewide Corn Yield Trends",
        subtitle = "Annual Average and Moving Average by State",
        x = "Year",
        y = "Average Yield (bushels per acre)"
      ) +
      scale_linetype_manual(values = c("Moving Avg" = "dashed"))
    
    ggplotly(p)
  })
  
  # Year-over-year changes plot
  output$yoy_plot <- renderPlotly({
    req(yield_data())
    
    yoy_changes <- yield_data() %>%
      group_by(State, county_name) %>%
      arrange(State, county_name, year) %>%
      mutate(
        yoy_change = Value - lag(Value),
        yoy_change_pct = (Value - lag(Value)) / lag(Value) * 100
      )
    
    p <- ggplot(yoy_changes, aes(x = year, y = yoy_change_pct, fill = State)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        plot.title = element_text(color = "black", face = "bold")
      ) +
      labs(
        title = "Year-over-Year Change in Corn Yields",
        subtitle = "Percentage Change by State",
        x = "Year",
        y = "Percentage Change"
      )
    
    ggplotly(p)
  })
  
  # Summary statistics table
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
}

# Run App
shinyApp(ui = ui, server = server)

# Remove the explicit runApp call
#shiny::runApp("dashboard.R")
