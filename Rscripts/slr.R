#Simple Linear of Acres Planted based on Acres Planted and Acres Harvested
# Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnassqs)

# Authenticate NASS API
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# Parameters
state <- "VA"
years <- 2021:2025
stats <- c("AREA PLANTED", "AREA HARVESTED")

# Helper to pull NASS API data
get_county_acres <- function(year, stat_cat) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      statisticcat_desc = stat_cat,
      unit_desc = "ACRES",
      state_alpha = state,
      agg_level_desc = "COUNTY",
      source_desc = "SURVEY",
      year = as.character(year)
    ))
    df$Year <- as.numeric(year)
    df$stat_type <- stat_cat
    df
  }, error = function(e) NULL)
}

# Pull and clean data
raw_data <- bind_rows(lapply(years, function(yr) {
  bind_rows(lapply(stats, function(sc) {
    get_county_acres(yr, sc)
  }))
}))

clean_data <- raw_data %>%
  filter(!is.na(county_name)) %>%
  mutate(
    County = tolower(gsub(" county", "", county_name)),
    Value = as.numeric(Value),
    stat_type = ifelse(stat_type == "AREA PLANTED", "Planted", "Harvested")
  ) %>%
  group_by(County, Year, stat_type) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stat_type, values_from = Value)

# Create lagged next-year planted column
county_year_data <- clean_data %>%
  arrange(County, Year) %>%
  group_by(County) %>%
  mutate(Next_Year_Planted = lead(Planted)) %>%
  ungroup() %>%
  filter(!is.na(Next_Year_Planted))

# Fit Simple Linear Regression model
model <- lm(Next_Year_Planted ~ Planted + Harvested, data = county_year_data)

# Generate 2025 predictions using 2024 data
pred_matrix <- predict(model,
                       newdata = clean_data %>% filter(Year == 2024 & !is.na(Planted) & !is.na(Harvested)),
                       interval = "prediction",
                       level = 0.95)

# Combine with original data
predict_2025_ci <- clean_data %>%
  filter(Year == 2024 & !is.na(Planted) & !is.na(Harvested)) %>%
  mutate(
    Predicted_2025_Planted = pred_matrix[, "fit"],
    Lower_CI = pred_matrix[, "lwr"],
    Upper_CI = pred_matrix[, "upr"]
  )


# Plot prediction with confidence intervals
ggplot(predict_2025_ci, aes(x = Planted, y = Predicted_2025_Planted)) +
  geom_point(color = "darkgreen", size = 2.5) +
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 200, alpha = 0.3) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "lightgreen") +
  labs(
    title = "County-Level Forecast of 2025 Corn Acres Planted in Virginia",
    subtitle = "Using 2024 Planted & Harvested Acres with SLR Model + 95% Prediction Intervals",
    x = "2024 Acres Planted",
    y = "Predicted 2025 Acres Planted"
  ) +
  theme_minimal()

#shiny
# app.R

library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnassqs)
library(plotly)

# Authenticate API
nassqs_auth("E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

ui <- fluidPage(
  titlePanel("Actual vs Predicted 2025 Corn Acres Planted in Virginia"),
  sidebarLayout(
    sidebarPanel(
      helpText("Grouped bar chart comparing predicted and actual 2025 corn acres planted."),
      p("Model: Predicted 2025 ~ 2024 Planted + 2024 Harvested"),
      p("Actual 2025 is simulated if NASS data not available.")
    ),
    mainPanel(
      plotlyOutput("barComparison", height = "700px")
    )
  )
)

server <- function(input, output) {
  
  output$barComparison <- renderPlotly({
    
    # --- PARAMETERS ---
    state <- "VA"
    years <- 2021:2025
    stats <- c("AREA PLANTED", "AREA HARVESTED")
    
    # --- DATA COLLECTION ---
    get_county_acres <- function(year, stat_cat) {
      tryCatch({
        df <- nassqs(list(
          commodity_desc = "CORN",
          statisticcat_desc = stat_cat,
          unit_desc = "ACRES",
          state_alpha = state,
          agg_level_desc = "COUNTY",
          source_desc = "SURVEY",
          year = as.character(year)
        ))
        df$Year <- as.numeric(year)
        df$stat_type <- stat_cat
        df
      }, error = function(e) NULL)
    }
    
    raw_data <- bind_rows(lapply(years, function(yr) {
      bind_rows(lapply(stats, function(sc) {
        get_county_acres(yr, sc)
      }))
    }))
    
    clean_data <- raw_data %>%
      filter(!is.na(county_name)) %>%
      mutate(
        County = tolower(gsub(" county", "", county_name)),
        Value = as.numeric(Value),
        stat_type = ifelse(stat_type == "AREA PLANTED", "Planted", "Harvested")
      ) %>%
      group_by(County, Year, stat_type) %>%
      summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = stat_type, values_from = Value)
    
    # --- MODELING ---
    county_year_data <- clean_data %>%
      arrange(County, Year) %>%
      group_by(County) %>%
      mutate(Next_Year_Planted = lead(Planted)) %>%
      ungroup() %>%
      filter(!is.na(Next_Year_Planted))
    
    model <- lm(Next_Year_Planted ~ Planted + Harvested, data = county_year_data)
    
    predict_input <- clean_data %>%
      filter(Year == 2024 & !is.na(Planted) & !is.na(Harvested))
    
    pred_matrix <- predict(model, newdata = predict_input, interval = "prediction", level = 0.95)
    
    predict_2025 <- bind_cols(predict_input, as.data.frame(pred_matrix)) %>%
      rename(Predicted_2025_Planted = fit)
    
    # --- ACTUAL 2025 ---
    actual_2025 <- clean_data %>%
      filter(Year == 2025) %>%
      select(County, Actual_2025_Planted = Planted)
    
    if (nrow(actual_2025) == 0) {
      actual_2025 <- clean_data %>%
        filter(Year == 2024 & !is.na(Planted)) %>%
        select(County, Planted) %>%
        mutate(Actual_2025_Planted = Planted * runif(n(), 0.85, 1.15)) %>%
        select(County, Actual_2025_Planted)
    }
    
    # --- COMBINE + PLOT ---
    comparison <- predict_2025 %>%
      left_join(actual_2025, by = "County") %>%
      select(County, Predicted_2025_Planted, Actual_2025_Planted)
    
    bar_data <- comparison %>%
      pivot_longer(cols = c(Predicted_2025_Planted, Actual_2025_Planted),
                   names_to = "Type", values_to = "Acres") %>%
      mutate(
        Type = recode(Type,
                      Predicted_2025_Planted = "Predicted 2025",
                      Actual_2025_Planted = "Actual 2025")
      ) %>%
      filter(!is.na(Acres))
    
    plot_ly(bar_data,
            x = ~tools::toTitleCase(County),
            y = ~Acres,
            color = ~Type,
            colors = c("Predicted 2025" = "forestgreen", "Actual 2025" = "lightgreen"),
            type = "bar",
            text = ~paste(Type, ": ", round(Acres)),
            hoverinfo = "text") %>%
      layout(
        barmode = "group",
        xaxis = list(title = "County", tickangle = -45),
        yaxis = list(title = "Acres Planted"),
        legend = list(title = list(text = "<b>Legend</b>"))
      )
  })
}

shinyApp(ui = ui, server = server)

