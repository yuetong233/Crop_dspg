#Area's Planted by County 
#This data is acres planted with all regions and counties from 2021-2024
library(dplyr)
library(tidyr)
library(rnassqs)
nassqs_auth("E4A8F7DF-7324-371D-A735-4F0FBC2629EE")
state <- "VA"
years <- 2015:2025
stats <- c("AREA PLANTED", "AREA HARVESTED")

# Helper to pull data
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

# Pull all data
raw_data <- bind_rows(lapply(years, function(yr) {
  bind_rows(lapply(stats, function(sc) {
    get_county_acres(yr, sc)
  }))
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

# Create lagged response variable: Next year's planted per county
county_year_data <- clean_data %>%
  arrange(County, Year) %>%
  group_by(County) %>%
  mutate(Next_Year_Planted = lead(Planted)) %>%
  ungroup() %>%
  filter(!is.na(Next_Year_Planted))

# Fit SLR model
model <- lm(Next_Year_Planted ~ Planted + Harvested, data = county_year_data)

# Predict for 2025 using 2024 data only (if available)
predict_2025 <- clean_data %>%
  filter(Year == 2024 & !is.na(Planted) & !is.na(Harvested)) %>%
  mutate(Predicted_2025_Planted = predict(model, newdata = .)) %>%
  select(County, Planted_2024 = Planted, Harvested_2024 = Harvested, Predicted_2025_Planted)

# View result
print(predict_2025)


