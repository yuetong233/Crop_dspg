library(rnassqs)
library(dplyr)
library(ggplot2)
library(lubridate)
library(tidyr)

nassqs_auth(key = "AD331078-DF4B-3A52-91B3-4C47948F8F0A")

state <- "VIRGINIA"
years <- 2014:2024
forecast_year <- 2025


yield_data <- nassqs(list(
  source_desc = "SURVEY",
  sector_desc = "CROPS",
  group_desc = "FIELD CROPS",
  commodity_desc = "CORN",
  statisticcat_desc = "YIELD",
  unit_desc = "BU / ACRE",
  agg_level_desc = "STATE",
  state_alpha = "VA",
  year = years
)) %>%
  mutate(Year = as.integer(year),
         Yield = as.numeric(Value)) %>%
  select(Year, Yield)

condition_data_raw <- nassqs(list(
  source_desc = "SURVEY",
  sector_desc = "CROPS",
  group_desc = "FIELD CROPS",
  commodity_desc = "CORN",
  statisticcat_desc = "CONDITION",
  agg_level_desc = "STATE",
  state_alpha = "VA",
  year = 2014:2025
))

# Filter for just GOOD and EXCELLENT descriptions
condition_data <- condition_data_raw %>%
  filter(grepl("MEASURED IN PCT GOOD|MEASURED IN PCT EXCELLENT", short_desc, ignore.case = TRUE)) %>%
  filter(!is.na(week_ending)) %>%
  mutate(Week_Ending = as.Date(week_ending)) %>%
  group_by(year, Week_Ending) %>%
  summarise(GE = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop") %>%
  group_by(year) %>%
  filter(Week_Ending == max(Week_Ending)) %>%
  ungroup() %>%
  rename(Year = year)

condition_data <- condition_data %>%
  mutate(Year = as.integer(Year))

# Merge yield and G+E data
merged_data <- yield_data %>%
  inner_join(condition_data, by = "Year") %>%
  arrange(Year)


# Fit trend line: Yield ~ (Year - 2013)
merged_data <- merged_data %>%
  mutate(Year_Index = Year - 2013)

trend_model <- lm(Yield ~ Year_Index, data = merged_data)
merged_data$Trend_Yield <- predict(trend_model, newdata = merged_data)

#Compute percent deviation from trend
merged_data <- merged_data %>%
  mutate(Deviation_Pct = ((Yield - Trend_Yield) / Trend_Yield))


regression_model <- lm(Deviation_Pct ~ GE, data = merged_data)
summary(regression_model)

weekly_2025 <- condition_data_raw %>%
  filter(grepl("MEASURED IN PCT GOOD|MEASURED IN PCT EXCELLENT", short_desc, ignore.case = TRUE)) %>%
  filter(!is.na(week_ending) & year == 2025) %>%
  mutate(Week_Ending = as.Date(week_ending)) %>%
  group_by(Week_Ending) %>%
  summarise(GE = sum(as.numeric(Value), na.rm = TRUE), .groups = "drop") %>%
  arrange(Week_Ending)

# 2. Predict % deviation for each week using regression
weekly_2025$Deviation_Pct <- predict(regression_model, newdata = weekly_2025)

# 3. Get 2024 trend yield to use as base
trend_yield_2024 <- predict(trend_model, newdata = data.frame(Year_Index = 2024 - 2013))

#checking value of trend yield 2024
trend_yield_2024

# 4. Compute forecasted yield for each 2025 week
weekly_2025$Forecasted_Yield <- (1 + weekly_2025$Deviation_Pct / 100) * trend_yield_2024


ggplot(weekly_2025, aes(x = Week_Ending, y = Forecasted_Yield)) +
  geom_line(color = "darkgreen", size = 1.5) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Weekly Forecasted Corn Yield (Virginia, 2025)",
       x = "Week Ending",
       y = "Forecasted Yield (bu/acre)") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )
