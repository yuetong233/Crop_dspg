#Dashboard
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(rnassqs)
library(zoo)

# Authenticate the NASS API
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A") 

# Load the good and poor corn data
good_data <- read.csv("GoodCorn.csv")
poor_data <- read.csv("PoorCorn.csv")
verypoor_data <- read.csv("VeryPoorCorn.csv")
excellent_data <- read.csv("ExcellentCorn.csv")
fair_data <- read.csv("FairCorn.csv")

#<<<<<<< HEAD
# Preprocess both the data sets
=======
# Preprocess both
excellent_data <- excellent_data %>%
  mutate(
    WeekNum = as.numeric(gsub("[^0-9]", "", Period)),
    Period = factor(Period, levels = unique(Period[order(as.numeric(gsub("[^0-9]", "", Period)))])),
    Year = as.factor(Year)
  )

fair_data <- fair_data %>%
  mutate(
    WeekNum = as.numeric(gsub("[^0-9]", "", Period)),
    Period = factor(Period, levels = unique(Period[order(as.numeric(gsub("[^0-9]", "", Period)))])),
    Year = as.factor(Year)
  )

>>>>>>> 6ad75bb6824f3045dbd3769b30fb6969b9ebb53d
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

verypoor_data <- verypoor_data %>%
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
    
    tabPanel("Corn Planting Progress",
             h3("Corn Planting Progress"),
             p("The plot visualizes the growing percentage of corn planted in Virginia over the planting season weeks for the years you select. "),
             p("Each line or point represents a different year, showing how planting progresses week by week. "), 
             p("The visualization helps to compare planting pace across different years, to identify trends and understand how current progress lines up with historical patterns."),
             # Controls
             fluidRow(
               column(6,
                      pickerInput("planting_years", "Select Year(s):",
                                  choices = c("2021", "2022", "2023", "2024", as.character(as.numeric(format(Sys.Date(), "%Y")) + 1)),
                                  selected = c("2024"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE))
               ),
               column(6,
                      sliderInput("planting_date_range", "Select Date Range:",
                                  min = as.Date("2021-04-01"), # Assuming planting starts around April 1st
                                  max = as.Date("2024-07-31"), # Assuming planting finishes by end of July
                                  value = c(as.Date("2024-04-01"), as.Date("2024-07-31")),
                                  timeFormat = "%Y-%m-%d")
               )
             ),
             
             # Output
             plotlyOutput("planting_plot")
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
               tabPanel("Excellent", plotlyOutput("excellentPlot")),
               tabPanel("Fair", plotlyOutput("fairPlot")),
               tabPanel("Good", plotlyOutput("goodPlot")),
               tabPanel("Poor", plotlyOutput("poorPlot")),
               tabPanel("Very Poor", plotlyOutput("verypoorPlot"))
             )
    ),
    
    
    tabPanel("Remote Sensing",
             h4("Remote Sensing Placeholder"),
             p("This section will include Remote Sensing data once available.")
    ),
    
    tabPanel("Corn Yield Analysis",
             h3("Corn Yield Analysis"),
             p("Explore corn yield trends across Virginia and neighboring states."),
             
             fluidRow(
               column(4,
                      pickerInput("yield_states", "Select States:",
                                  choices = c("Virginia" = "VA", "North Carolina" = "NC", "Maryland" = "MD"),
                                  selected = c("VA", "NC", "MD"),
                                  multiple = TRUE,
                                  options = list(`actions-box` = TRUE))
               ),
               column(4,
                      sliderInput("yield_year_range", "Select Year Range:",
                                  min = 2000,
                                  max = as.numeric(format(Sys.Date(), "%Y")),
                                  value = c(as.numeric(format(Sys.Date(), "%Y")) - 10, as.numeric(format(Sys.Date(), "%Y"))),
                                  step = 1)
               ),
               column(4,
                      numericInput("ma_window", "Moving Average Window:",
                                   value = 5, min = 2, max = 10)
               )
             ),
             
             tabsetPanel(
               tabPanel("Yield Trends", plotlyOutput("yield_plot")),
               tabPanel("Year-over-Year Changes", plotlyOutput("yoy_plot")),
               tabPanel("Summary Statistics", tableOutput("summary_table"))
             )
    )
  )
)

# Server
server <- function(input, output) {
  
  filtered_excellent <- reactive({
    excellent_data %>%
      filter(
        Year %in% input$selected_years,
        WeekNum >= input$weekRange[1],
        WeekNum <= input$weekRange[2]
      )
  })
  
  filtered_fair <- reactive({
    fair_data %>%
      filter(
        Year %in% input$selected_years,
        WeekNum >= input$weekRange[1],
        WeekNum <= input$weekRange[2]
      )
  })
  
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
  
  filtered_verypoor <- reactive({
    verypoor_data %>%
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
  
  output$verypoorPlot <- renderPlotly({
    ggplotly(
      ggplot(filtered_verypoor(), aes(
        x = Period, y = Value, color = Year, group = Year,
        text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
      )) +
        geom_line(linewidth = 1.2) +
        geom_point() +
        labs(
          title = "Corn Rated 'Very Poor' by Week in Virginia",
          x = "Week", y = "Percent Rated Very Poor"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)),
      tooltip = "text"
    )
  })
  
  output$excellentPlot <- renderPlotly({
    ggplotly(
      ggplot(filtered_excellent(), aes(
        x = Period, y = Value, color = Year, group = Year,
        text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
      )) +
        geom_line(linewidth = 1.2) +
        geom_point() +
        labs(
          title = "Corn Rated 'Excellent' by Week in Virginia",
          x = "Week", y = "Percent Rated Excellent"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)),
      tooltip = "text"
    )
  })
  
  output$fairPlot <- renderPlotly({
    ggplotly(
      ggplot(filtered_fair(), aes(
        x = Period, y = Value, color = Year, group = Year,
        text = paste("Year:", Year, "<br>Week:", Period, "<br>Value:", Value)
      )) +
        geom_line(linewidth = 1.2) +
        geom_point() +
        labs(
          title = "Corn Rated 'Fair' by Week in Virginia",
          x = "Week", y = "Percent Rated Fair"
        ) +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1, size = 8)),
      tooltip = "text"
    )
  })

  
  # Yield data from rnassqs
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
  
  # Yield Trend Plot
  output$yield_plot <- renderPlotly({
    req(yield_data())
    
    state_year_avg <- yield_data() %>%
      group_by(State, year) %>%
      summarize(avg_yield = mean(Value, na.rm = TRUE)) %>%
      group_by(State) %>%
      mutate(moving_avg = zoo::rollmean(avg_yield, k = input$ma_window, fill = NA, align = "right"))
    
    p <- ggplot(state_year_avg, aes(x = year, y = avg_yield, color = State)) +
      geom_line(size = 1.2) +
      geom_line(aes(y = moving_avg, linetype = "Moving Avg"), size = 1.2) +
      geom_point(size = 2) +
      theme_minimal() +
      labs(
        title = "Statewide Corn Yield Trends",
        subtitle = "Annual Average and Moving Average by State",
        x = "Year",
        y = "Average Yield (bushels per acre)"
      ) +
      scale_linetype_manual(values = c("Moving Avg" = "dashed"))
    
    ggplotly(p)
  })
  
  # alter the plot 
  output$yoy_plot <- renderPlotly({
    req(yield_data())
    
    yoy_changes <- yield_data() %>%
      group_by(State, county_name) %>%
      arrange(State, county_name, year) %>%
      mutate(yoy_change_pct = (Value - lag(Value)) / lag(Value) * 100)
    
    p <- ggplot(yoy_changes, aes(x = year, y = yoy_change_pct, fill = State)) +
      geom_boxplot(alpha = 0.7) +
      theme_minimal() +
      labs(
        title = "Year-over-Year Change in Corn Yields",
        subtitle = "Percentage Change by State",
        x = "Year",
        y = "Percentage Change"
      )
    
    ggplotly(p)
  })
  
  # Summary Table
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
  
  # Corn planting progress data
  planting_data <- reactive({
    req(input$planting_years, input$planting_date_range)
    
    all_data <- lapply(input$planting_years, function(year) {
      nassqs(list(
        source_desc = "SURVEY",
        sector_desc = "CROPS",
        group_desc = "FIELD CROPS",
        commodity_desc = "CORN",
        statisticcat_desc = "PROGRESS",
        unit_desc = "PCT PLANTED",
        state_name = "VIRGINIA",
        year = year
      ))
    })
    
    # Combine data and preprocess
    combined_data <- do.call(rbind, all_data)
    
    combined_data %>%
      mutate(week = as.Date(week_ending),
             value = as.numeric(Value)) %>%
      filter(!is.na(week)) %>% # Filter out rows with NA weeks after conversion
      arrange(year, week) %>%
      filter(week >= input$planting_date_range[1], # Filter by selected date range
             week <= input$planting_date_range[2])
  })
  
  # Corn planting progress plot
  output$planting_plot <- renderPlotly({
    req(planting_data())
    
    p <- ggplot(planting_data(), aes(x = week, y = value, color = factor(year), group = factor(year),
                                     text = paste("Year:", year, "<br>Week Ending:", week, "<br>% Planted:", value)))
    
    # Check if there's more than one year selected to draw lines
    if (length(unique(planting_data()$year)) > 1) {
      p <- p + geom_line(size = 1.2)
    }
    
    p <- p + geom_point(size = 2) +
      labs(title = "Corn Planting Progress in Virginia",
           y = "% Planted",
           x = "Week Ending",
           color = "Year") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    ggplotly(p, tooltip = "text")
  })
}

# Run the shiny app 
shinyApp(ui = ui, server = server)

