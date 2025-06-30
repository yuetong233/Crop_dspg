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
years <- 2020:2025
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

# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnassqs)

# Authenticate API
nassqs_auth("E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# Parameters
state <- "VA"
years <- 2020:2025
stats <- c("AREA PLANTED", "AREA HARVESTED")

# Fetch USDA NASS data
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
  bind_rows(lapply(stats, function(sc) get_county_acres(yr, sc)))
}))

# Clean and reshape
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

# Build training dataset using lagged values
county_year_data <- clean_data %>%
  arrange(County, Year) %>%
  group_by(County) %>%
  mutate(
    Planted_Lag = lag(Planted),
    Harvested_Lag = lag(Harvested),
    Next_Year_Harvested = lead(Harvested)
  ) %>%
  ungroup() %>%
  filter(!is.na(Planted_Lag), !is.na(Harvested_Lag), !is.na(Next_Year_Harvested))

# Fit model: predict harvested (t+1) using planted (t) + harvested (t)
model <- lm(Next_Year_Harvested ~ Planted_Lag + Harvested_Lag, data = county_year_data)
summary(model)

# Prepare 2025 input using 2025 planted and 2024 harvested
predict_input <- clean_data %>%
  filter(Year %in% c(2024, 2025)) %>%
  select(County, Year, Planted, Harvested) %>%
  pivot_wider(names_from = Year, values_from = c(Planted, Harvested)) %>%
  filter(!is.na(Planted_2025), !is.na(Harvested_2024)) %>%
  rename(
    Planted_Lag = Planted_2025,
    Harvested_Lag = Harvested_2024
  )

# Predict 2025 harvested
pred_matrix <- predict(model, newdata = predict_input, interval = "prediction", level = 0.95)

# Combine and prepare output
predict_2025_harvest <- predict_input %>%
  mutate(
    Predicted_2025_Harvested = pred_matrix[, "fit"],
    Lower_CI = pred_matrix[, "lwr"],
    Upper_CI = pred_matrix[, "upr"]
  )

ggplot(predict_2025_harvest, aes(x = Planted_Lag, y = Predicted_2025_Harvested)) +
  # Predicted Points
  geom_point(aes(color = "Predicted Value"), size = 2.5) +
  
  # Model Fit Line
  geom_smooth(aes(color = "Model Fit Line"), method = "lm", se = FALSE, size = 1.2) +
  
  # 1:1 Reference Line (now mapped to linetype and added to legend)
  geom_abline(aes(linetype = "1:1 Reference Line"), slope = 1, intercept = 0, color = "black", size = 1, linetype = "dashed") +
  
  # Manually define color and linetype scales
  scale_color_manual(
    name = "Color Legend",
    values = c(
      "Predicted Value" = "darkgreen",
      "Model Fit Line" = "limegreen"
    )
  ) +
  scale_linetype_manual(
    name = "Line Type Legend",
    values = c("1:1 Reference Line" = "dashed")
  ) +
  
  labs(
    title = "Forecast of 2025 Corn Acres Harvested (County-Level)",
    subtitle = "Predicted using 2025 Planted & 2024 Harvested Acres",
    x = "2025 Acres Planted",
    y = "Predicted 2025 Acres Harvested"
  ) +
  
  theme_minimal()

#shiny
# Load libraries
library(shiny)
library(plotly)
library(dplyr)

# UI
ui <- fluidPage(
  titlePanel("Predicted 2025 Corn Acres Harvested by County"),
  sidebarLayout(
    sidebarPanel(
      helpText("Bar chart of model-predicted 2025 harvested acres for each county."),
      helpText("You can also compare planted vs harvested if available.")
    ),
    mainPanel(
      plotlyOutput("barPlot", height = "700px")
    )
  )
)

server <- function(input, output) {
  output$barPlot <- renderPlotly({
    
    # Fix the prediction data
    bar_data <- predict_2025_harvest %>%
      # Remove "Other Counties"
      filter(!grepl("other counties", County, ignore.case = TRUE)) %>%
      
      # Ensure harvested ≤ planted and no negatives
      mutate(
        Predicted_2025_Harvested = pmin(Predicted_2025_Harvested, Planted_Lag),
        Predicted_2025_Harvested = pmax(Predicted_2025_Harvested, 0),
        Planted_Lag = pmax(Planted_Lag, 0)
      ) %>%
      
      # Reshape for grouped bar chart
      select(County, Planted_Lag, Predicted_2025_Harvested) %>%
      rename(
        `2025 Planted` = Planted_Lag,
        `2025 Predicted Harvested` = Predicted_2025_Harvested
      ) %>%
      pivot_longer(cols = c(`2025 Planted`, `2025 Predicted Harvested`),
                   names_to = "Type", values_to = "Acres")
    
    # Build interactive bar chart
    plot_ly(
      data = bar_data,
      x = ~County,
      y = ~Acres,
      color = ~Type,
      type = 'bar',
      text = ~paste0(Type, ": ", round(Acres)),
      hoverinfo = 'text'
    ) %>%
      layout(
        barmode = 'group',
        xaxis = list(title = "County", tickangle = -45),
        yaxis = list(title = "Acres"),
        legend = list(title = list(text = "<b>Legend</b>"))
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)



# Re-clean predicted harvested values to obey logical constraints
predict_2025_harvest <- predict_2025_harvest %>%
  mutate(
    Predicted_2025_Harvested = pmin(Predicted_2025_Harvested, Planted_Lag),  # can't harvest more than planted
    Predicted_2025_Harvested = pmax(Predicted_2025_Harvested, 0),            # no negatives
    Planted_Lag = pmax(Planted_Lag, 0)
  )

# Plot predicted 2025 harvested acres
ggplot(predict_2025_harvest, aes(x = Planted_Lag, y = Predicted_2025_Harvested)) +
  geom_point(aes(color = "Predicted Value"), size = 2.5) +
  geom_smooth(
    data = county_year_data,  # use training data to draw model fit line
    aes(x = Planted_Lag, y = Next_Year_Harvested, color = "Model Fit Line"),
    method = "lm", se = TRUE, size = 1.2
  ) +
  geom_abline(
    aes(linetype = "1:1 Reference Line"),
    slope = 1, intercept = 0, color = "black", size = 1, linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Color Legend",
    values = c("Predicted Value" = "darkgreen", "Model Fit Line" = "limegreen")
  ) +
  scale_linetype_manual(
    name = "Line Type Legend",
    values = c("1:1 Reference Line" = "dashed")
  ) +
  labs(
    title = "County-Level Forecast of 2025 Corn Acres Harvested",
    subtitle = "Predicted using 2025 Planted & 2024 Harvested Acres (Model fit from 2021–2024)",
    x = "2025 Acres Planted",
    y = "Predicted 2025 Acres Harvested"
  ) +
  theme_minimal()







# Re-clean predicted harvested values to obey logical constraints
predict_2025_harvest <- predict_2025_harvest %>%
  mutate(
    Predicted_2025_Harvested = pmin(Predicted_2025_Harvested, Planted_Lag),  # can't harvest more than planted
    Predicted_2025_Harvested = pmax(Predicted_2025_Harvested, 0),            # no negatives
    Planted_Lag = pmax(Planted_Lag, 0)
  )

# Plot predicted 2025 harvested acres
ggplot(predict_2025_harvest, aes(x = Planted_Lag, y = Predicted_2025_Harvested)) +
  geom_point(aes(color = "Predicted Value"), size = 2.5) +
  geom_smooth(
    data = county_year_data,  # use training data to draw model fit line
    aes(x = Planted_Lag, y = Next_Year_Harvested, color = "Model Fit Line"),
    method = "lm", se = TRUE, size = 1.2
  ) +
  geom_abline(
    aes(linetype = "1:1 Reference Line"),
    slope = 1, intercept = 0, color = "black", size = 1, linetype = "dashed"
  ) +
  scale_color_manual(
    name = "Color Legend",
    values = c("Predicted Value" = "darkgreen", "Model Fit Line" = "limegreen")
  ) +
  scale_linetype_manual(
    name = "Line Type Legend",
    values = c("1:1 Reference Line" = "dashed")
  ) +
  labs(
    title = "County-Level Forecast of 2025 Corn Acres Harvested",
    subtitle = "Predicted using 2025 Planted & 2024 Harvested Acres (Model fit from 2021–2024)",
    x = "2025 Acres Planted",
    y = "Predicted 2025 Acres Harvested"
  ) +
  theme_minimal()










#Forecasting
# Load Libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(rnassqs)

# Authenticate NASS API
nassqs_auth("E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# PARAMETERS
state <- "VA"
years <- 2015:2024
stats <- c("AREA PLANTED", "AREA HARVESTED", "YIELD")

# Helper: Get county-level corn data
get_county_data <- function(year, stat_cat) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      statisticcat_desc = stat_cat,
      unit_desc = if (stat_cat == "YIELD") "BU / ACRE" else "ACRES",
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

# Pull county-level corn data
county_raw <- bind_rows(lapply(years, function(yr) {
  bind_rows(lapply(stats, function(sc) get_county_data(yr, sc)))
}))

# Clean and reshape
county_data <- county_raw %>%
  filter(!is.na(county_name)) %>%
  mutate(
    County = tolower(gsub(" county", "", county_name)),
    Value = as.numeric(Value),
    stat_type = recode(stat_type,
                       "AREA PLANTED" = "Planted",
                       "AREA HARVESTED" = "Harvested",
                       "YIELD" = "Yield")
  ) %>%
  group_by(County, Year, stat_type) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = stat_type, values_from = Value)

# FIXED: Get price data with guaranteed structure
get_price_data <- function(year) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      statisticcat_desc = "PRICE RECEIVED",
      unit_desc = "DOLLARS / BU",
      state_alpha = state,
      agg_level_desc = "STATE",
      source_desc = "SURVEY",
      year = as.character(year)
    ))
    df %>% mutate(
      Year = as.numeric(year),
      Price = as.numeric(Value)
    ) %>% select(Year, Price)
  }, error = function(e) {
    # Return placeholder row with NA
    tibble(Year = as.numeric(year), Price = NA_real_)
  })
}

# Merge price data into county data
price_data <- bind_rows(lapply(years, get_price_data))

county_data <- county_data %>%
  left_join(price_data, by = "Year")

# Create lagged variables + target (next-year planted)
model_data <- county_data %>%
  arrange(County, Year) %>%
  group_by(County) %>%
  mutate(
    Planted_Lag = lag(Planted),
    Harvested_Lag = lag(Harvested),
    Yield_Lag = lag(Yield),
    Price_Lag = lag(Price),
    Next_Year_Planted = lead(Planted)
  ) %>%
  ungroup() %>%
  filter(!is.na(Next_Year_Planted), !is.na(Planted_Lag),
         !is.na(Harvested_Lag), !is.na(Yield_Lag), !is.na(Price_Lag))

# Fit regression model
model <- lm(Next_Year_Planted ~ Planted_Lag + Harvested_Lag + Yield_Lag + Price_Lag, data = model_data)

# Model summary
summary(model)
