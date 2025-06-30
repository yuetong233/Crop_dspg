# Yield Deviation from Trend Dashboard (Standalone Shiny App)
# This app visualizes yield deviation from trend analysis for Virginia corn

library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(rnassqs)
library(tidyr)
library(zoo)

# Set your NASS API key
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# Data preparation (run once at startup)
state <- "VIRGINIA"
years <- 2015:2023

# Get annual yield data (state-level)
yield_data <- nassqs(list(
  commodity_desc = "CORN",
  year = years,
  agg_level_desc = "STATE",
  state_name = state,
  statisticcat_desc = "YIELD"
)) %>%
  mutate(year = as.numeric(year), Yield = as.numeric(Value)) %>%
  select(year, Yield)

# Get all crop condition data (weekly, state-level)
condition_data <- bind_rows(lapply(years, function(yr) {
  nassqs(list(
    commodity_desc = "CORN",
    year = yr,
    state_name = state,
    statisticcat_desc = "CONDITION",
    agg_level_desc = "STATE"
  )) %>%
    mutate(year = as.numeric(yr))
}))

# For each year, get the last available week and the condition ratings for that week
end_of_season_conditions <- condition_data %>%
  filter(unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR", "PCT VERY POOR")) %>%
  mutate(condition = gsub("PCT ", "", unit_desc)) %>%
  group_by(year) %>%
  filter(week_ending == max(week_ending)) %>%
  select(year, condition, Value) %>%
  pivot_wider(names_from = condition, values_from = Value)

# Calculate the trend line of yields (linear trend)
yield_trend_model <- lm(Yield ~ year, data = yield_data)
yield_data <- yield_data %>%
  mutate(Trend_Yield = predict(yield_trend_model, newdata = data.frame(year = year)))

# Calculate each year's percent deviation from trend yield
yield_data <- yield_data %>%
  mutate(DeviationPct = 100 * (Yield - Trend_Yield) / Trend_Yield)

# Prepare merged data for regression
end_of_season_conditions <- end_of_season_conditions %>%
  mutate(GE = as.numeric(GOOD) + as.numeric(EXCELLENT))
yield_cond_merged <- left_join(yield_data, end_of_season_conditions, by = "year")

# Fit regression model: percent deviation from trend yield ~ condition categories (leave out 'Very Poor')
regression_df <- yield_cond_merged %>%
  mutate_at(vars(EXCELLENT, GOOD, FAIR, POOR, `VERY POOR`), as.numeric)
reg_model <- lm(DeviationPct ~ GOOD + FAIR + POOR + EXCELLENT, data = regression_df) # 'VERY POOR' left out

# UI
ui <- fluidPage(
  titlePanel("Yield Deviation from Trend Analysis (Virginia Corn)"),
  sidebarLayout(
    sidebarPanel(
      helpText("This dashboard visualizes the relationship between crop condition ratings and yield deviations from trend for Virginia corn (2015-2023)."),
      selectInput("forecast_year", "Select Forecast Year:", choices = years, selected = max(years))
    ),
    mainPanel(
      h4("Yield vs. Percent Good+Excellent (End of Season)"),
      plotlyOutput("yield_vs_ge_plot", height = "350px"),
      br(),
      h4("Weekly Forecasted % Deviation from Trend Yield"),
      plotlyOutput("weekly_deviation_plot", height = "350px")
    )
  )
)

# Server
server <- function(input, output) {
  # Plot 1: Yield vs. Percent Good+Excellent (end of season)
  output$yield_vs_ge_plot <- renderPlotly({
    p1 <- ggplot(yield_cond_merged, aes(x = GE, y = DeviationPct)) +
      geom_point(size = 3, color = "#2e7d32") +
      geom_smooth(method = "lm", se = FALSE, color = "#1a9850") +
      labs(x = "% Good + Excellent (End of Season)",
           y = "% Deviation from Trend Yield")
    ggplotly(p1)
  })
  
  # Plot 2: Weekly Forecasted % Deviation from Trend Yield (for selected year)
  output$weekly_deviation_plot <- renderPlotly({
    weekly_conditions <- condition_data %>%
      filter(year == input$forecast_year, unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR", "PCT VERY POOR")) %>%
      mutate(condition = gsub("PCT ", "", unit_desc)) %>%
      select(week_ending, condition, Value) %>%
      pivot_wider(names_from = condition, values_from = Value) %>%
      arrange(week_ending)
    weekly_conditions <- weekly_conditions %>%
      mutate_at(vars(EXCELLENT, GOOD, FAIR, POOR, `VERY POOR`), as.numeric)
    weekly_conditions$DeviationPctForecast <- predict(reg_model, newdata = weekly_conditions)
    p2 <- ggplot(weekly_conditions, aes(x = as.Date(week_ending), y = DeviationPctForecast)) +
      geom_line(color = "#2e7d32", size = 1.2) +
      geom_point(color = "#1a9850", size = 2) +
      labs(x = "Week Ending",
           y = "% Deviation from Trend Yield",
           title = paste0("Weekly Forecasted % Deviation from Trend Yield (", input$forecast_year, ")"))
    ggplotly(p2)
  })
}

shinyApp(ui = ui, server = server) 