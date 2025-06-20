#Simple Linear Regression Model of Yield based on Crop Conditions
# Libraries
library(rnassqs)
library(dplyr)
library(tidyr)
library(ggplot2)

# Authenticate
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# STEP 1: Get yield data (2010–2023)
years <- 2015:2025

yield_data <- nassqs(list(
  commodity_desc = "CORN",
  year = years,
  state_alpha = "VA",
  agg_level_desc = "STATE",
  statisticcat_desc = "YIELD"
)) %>%
  mutate(year = as.numeric(year),
         yield = as.numeric(Value)) %>%
  select(year, yield) %>%
  arrange(year)

# STEP 2: Calculate trend + deviation
trend_model <- lm(yield ~ year, data = yield_data)

yield_data <- yield_data %>%
  mutate(trend = predict(trend_model),
         pct_deviation = 100 * (yield - trend) / trend)

# STEP 3: Get end-of-season crop condition data
get_end_conditions <- function(y) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      year = y,
      state_name = "VIRGINIA",
      statisticcat_desc = "CONDITION",
      agg_level_desc = "STATE"
    )) %>%
      filter(unit_desc %in% c("PCT VERY POOR", "PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")) %>%
      mutate(value = as.numeric(Value)) %>%
      group_by(unit_desc) %>%
      summarise(end_value = mean(value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = unit_desc, values_from = end_value)
    
    df$year <- y
    df
  }, error = function(e) NULL)
}

condition_data <- bind_rows(lapply(years, get_end_conditions)) %>%
  rename_with(~gsub("PCT ", "", .)) %>%
  rename_with(tolower)

# STEP 4: Merge and model
data_joined <- inner_join(yield_data, condition_data, by = "year") %>%
  mutate(ge = good + excellent)

# Update deviation now that we have full data
data_joined <- data_joined %>%
  mutate(
    trend = predict(trend_model, newdata = data_joined),
    pct_deviation = 100 * (yield - trend) / trend
  )

# Fit regression model (% deviation ~ conditions, leave out very poor)
reg_model <- lm(pct_deviation ~ poor + fair + good + excellent, data = data_joined)
print(summary(reg_model))

# STEP 5: Pull weekly 2021 crop condition data
weekly_data <- nassqs(list(
  commodity_desc = "CORN",
  year = 2021,
  state_name = "VIRGINIA",
  statisticcat_desc = "CONDITION",
  agg_level_desc = "STATE"
)) %>%
  filter(unit_desc %in% c("PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")) %>%
  mutate(week = as.Date(week_ending),
         value = as.numeric(Value)) %>%
  pivot_wider(names_from = unit_desc, values_from = value, values_fn = mean) %>%
  rename_with(~gsub("PCT ", "", .)) %>%
  rename_with(tolower) %>%
  mutate(poor = replace_na(poor, 0),
         fair = replace_na(fair, 0),
         good = replace_na(good, 0),
         excellent = replace_na(excellent, 0),
         week_num = as.numeric(format(week, "%U"))) %>%
  arrange(week)

# STEP 6: Forecast weekly yield
weekly_data <- weekly_data %>%
  mutate(
    pred_pct_deviation = predict(reg_model, newdata = .),
    base_trend_yield = predict(trend_model, newdata = data.frame(year = 2021)),
    predicted_yield = base_trend_yield * (1 + pred_pct_deviation / 100)
  )

# STEP 7: Plot weekly forecasts
ggplot(weekly_data, aes(x = week_num, y = predicted_yield)) +
  geom_line(color = "blue", size = 1.2) +
  geom_point(color = "darkblue", size = 2) +
  labs(title = "2021 Weekly Forecasted Corn Yield (Virginia)",
       x = "Week Number", y = "Forecasted Yield (bu/ac)") +
  theme_minimal()



library(rnassqs)
library(dplyr)
library(ggplot2)

# Authenticate
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

# Get average % Excellent for each year (e.g. 2015–2023)
years <- 2015:2023

get_avg_excellent <- function(year) {
  tryCatch({
    df <- nassqs(list(
      commodity_desc = "CORN",
      year = year,
      state_name = "VIRGINIA",
      statisticcat_desc = "CONDITION",
      agg_level_desc = "STATE"
    )) %>%
      filter(unit_desc == "PCT EXCELLENT") %>%
      mutate(value = as.numeric(Value)) %>%
      summarise(avg_excellent = mean(value, na.rm = TRUE)) %>%
      mutate(year = year)
  }, error = function(e) NULL)
}

condition_data <- bind_rows(lapply(years, get_avg_excellent))

# Get yield data
yield_data <- nassqs(list(
  commodity_desc = "CORN",
  year = years,
  state_alpha = "VA",
  agg_level_desc = "STATE",
  statisticcat_desc = "YIELD"
)) %>%
  mutate(year = as.numeric(year),
         yield = as.numeric(Value)) %>%
  select(year, yield)

# Join and model
slr_data <- inner_join(condition_data, yield_data, by = "year")
model <- lm(yield ~ avg_excellent, data = slr_data)

# Plot: dots + best-fit line
ggplot(slr_data, aes(x = avg_excellent, y = yield)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(title = "Yield vs. % Excellent Crop Condition (VA)",
       x = "Average % Excellent (per year)",
       y = "Yield (bu/ac)") +
  theme_minimal()

