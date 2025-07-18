#Planting Progress

# --- Libraries ---
library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)

# --- NASS API Auth ---
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# --- UI ---
ui <- fluidPage(
  titlePanel("🌽 Corn Planting Progress in Virginia"),
  p("View weekly planting progress for each year side by side."),
  
  do.call(tabsetPanel, c(
    list(id = "year_tabs", type = "tabs"),
    lapply(as.character(2021:(as.numeric(format(Sys.Date(), "%Y")))), function(yr) {
      tabPanel(yr, plotlyOutput(outputId = paste0("plot_", yr), height = "500px"))
    })
  ))
)

# --- Server ---
server <- function(input, output, session) {
  
  get_year_data <- function(year) {
    tryCatch({
      data <- nassqs(list(
        source_desc = "SURVEY",
        sector_desc = "CROPS",
        group_desc = "FIELD CROPS",
        commodity_desc = "CORN",
        statisticcat_desc = "PROGRESS",
        unit_desc = "PCT PLANTED",
        state_name = "VIRGINIA",
        year = year
      ))
      data %>%
        mutate(
          week = as.Date(week_ending),
          value = as.numeric(Value),
          year = as.character(year)
        ) %>%
        filter(!is.na(week)) %>%
        arrange(week)
    }, error = function(e) NULL)
  }
  
  # Render individual year plots
  for (yr in 2021:(as.numeric(format(Sys.Date(), "%Y")))) {
    local({
      year_inner <- yr
      output[[paste0("plot_", year_inner)]] <- renderPlotly({
        data <- get_year_data(year_inner)
        req(data)
        
        p <- ggplot(data, aes(x = week, y = value)) +
          geom_line(color = "#2e7d32", size = 1.4) +
          geom_point(color = "#66bb6a", size = 2.5) +
          labs(
            title = paste("🌽 Corn Planting Progress —", year_inner),
            x = "Week Ending",
            y = "% Planted"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold", size = 16)
          )
        
        ggplotly(p, tooltip = c("x", "y"))
      })
    })
  }
}
# --- Run App ---
shinyApp(ui, server)

#Corn Yield Analysis 
# --- Libraries ---
library(shiny)
library(shinyWidgets)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)
library(zoo)
library(scales)

# --- Auth ---
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# --- UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      * {
        font-family: 'Times New Roman', Times, serif !important;
      }
    "))
  ),
  titlePanel("Corn Yield Analysis"),
  
  h4("About This Data"),
  p("This dashboard presents an analysis of corn crop yield across Virginia, North Carolina, and Maryland from 2015 to 2023. 
     The data is sourced from the USDA's National Agricultural Statistics Service (NASS) API and includes county-level statistics on corn yield (bushels per acre). 
     Interactive graphics allow users to explore average yields over time, moving averages, and year-over-year changes."),
  
  fluidRow(
    column(4,
           pickerInput("yield_states", "Select States:",
                       choices = c("Virginia" = "VA", "North Carolina" = "NC", "Maryland" = "MD"),
                       selected = c("VA", "NC", "MD"), multiple = TRUE, options = list(`actions-box` = TRUE))
    ),
    column(4,
           sliderInput("yoy_year_slider", "Select Year:",
                       min = 2015, max = 2023, value = 2023, step = 1, sep = "")
    ),
    column(4,
           numericInput("ma_window", "Moving Average Window:", value = 5, min = 2, max = 10)
    )
  ),
  
  h4("Summary Statistics"),
  div(style = "margin-bottom:-30px;", plotlyOutput("summary_card")),  # reduces the gap caused by the table
  
  h4("Yield Trends Over Time"),
  plotlyOutput("yield_plot"),
  
  
  br(),
  h4("Year-over-Year Yield Change"),
  plotlyOutput("yoy_plot")
)

# --- Server ---
server <- function(input, output, session) {
  
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
      header = list(values = c("State", names(df)[-1]), align = 'center', fill = list(color = '#a5d6a7'), font = list(size = 14)),
      cells = list(values = rbind(df$State, format(round(df[[2]], 1), nsmall = 1), format(df[[3]], big.mark=","), format(df[[4]], big.mark=","), round(df[[5]], 1)),
                   align = 'center', height = 30)
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
      layout(title = list(text = "Statewide Corn Yield Trends", font = list(family = "Times New Roman")),
             xaxis = list(title = "Year"),
             yaxis = list(title = "Average Yield (bushels per acre)"),
             plot_bgcolor = '#ffffff',
             paper_bgcolor = '#ffffff')
  })
  
  
  # --- Year-over-Year Change Plot ---
  output$yoy_plot <- renderPlotly({
    req(yield_data(), input$yoy_year_slider)
    
    yoy <- yield_data() %>%
      group_by(State, county_name) %>%
      arrange(State, county_name, year) %>%
      mutate(yoy_change_pct = (Value - lag(Value)) / lag(Value) * 100) %>%
      filter(!is.na(yoy_change_pct)) %>%
      group_by(State, year) %>%
      summarise(yoy_change_pct = mean(yoy_change_pct, na.rm = TRUE), .groups = "drop") %>%
      filter(year == input$yoy_year_slider) %>%
      mutate(is_max = yoy_change_pct == max(yoy_change_pct),
             label = paste0(round(yoy_change_pct, 1), "%"))
    
    plot_ly(
      data = yoy,
      x = ~yoy_change_pct,
      y = ~State,
      type = 'bar',
      orientation = 'h',
      text = ~label,
      textposition = 'outside',
      marker = list(
        color = ~ifelse(is_max, '#81c784', '#aed581'),
        line = list(width = ~ifelse(is_max, 4, 1), color = 'black')
      )
    ) %>% layout(
      title = "Year-over-Year Corn Yield Change by State",
      xaxis = list(title = "YoY % Change"),
      yaxis = list(title = "State"),
      plot_bgcolor = '#ffffff',
      paper_bgcolor = '#ffffff'
    )
  })
}

# --- Run App ---
shinyApp(ui, server)

