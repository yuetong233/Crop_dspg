# Annual yield prediction

rm(list = ls(all = TRUE))
library(dplyr)
library(tidyr)
library(readr)
library(lubridate)

# Import data and cleaning-------------------------------------------------
week_condition <- read.csv("VA_weekly_condition.csv", fill = TRUE)
yield <- read.csv("VA_yield.csv", fill = TRUE)
vis <- read.csv("VA_multi.csv", fill = TRUE)
vi25 <- read.csv("2025_VI.csv", fill = TRUE)

wc_clean <- week_condition[, colSums(is.na(week_condition)) == 0]
yield_clean <- yield[, colSums(is.na(yield)) == 0]

filtered_yield <- yield_clean %>%
  filter(Period == "YEAR")

# Create full sequence of years
year_range <- data.frame(Year = 1984:2025)

# Ensure all years are retained
filtered_yield <- year_range %>%
  left_join(filtered_yield, by = "Year")

# Yearly Condition (Last Week) and Yield ----------------------------------

# Filter relevant columns and extract
condition_summary <- wc_clean %>%
  select(Year, Period, `Data.Item`, Value) %>%
  mutate(
    Condition = toupper(sub(".*IN PCT\\s+", "", `Data.Item`)),
    Week = as.numeric(gsub("WEEK #", "", Period)),
    Value = as.numeric(Value)
  )

# Last week's value for each Year and Condition
yearly_summary <- condition_summary %>%
  group_by(Year, Condition) %>%
  filter(Week == max(Week, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%  # handle duplicates for last week
  ungroup() %>%
  select(Year, Condition, Value) %>%
  pivot_wider(names_from = Condition, values_from = Value)

# Good and Excellent
yearly_summary$`G+E` <- yearly_summary$GOOD + yearly_summary$EXCELLENT

# Merge yield to yearly_summary
yearly_summary <- yearly_summary %>%
  left_join(filtered_yield %>% select(Year, Yield = Value), by = "Year")

############################# 
# 1984-2025 Period
############################# 

# Percentage Deviation (1984-2025)--------------------------------------------

years <- 1984:2025
filtered_yield$year_order <- filtered_yield$Year - 1983

model <- lm(Value ~ year_order, data = filtered_yield) 
summary(model) # significant year_order
alpha <- coef(model)[1]  # 77.5
beta <- coef(model)[2]  # 1.73
r2 <- summary(model)$r.squared # 0.45

filtered_yield$trend_predicted <- alpha + beta * filtered_yield$year_order

# Percent deviation from the trend
filtered_yield$deviation <- (filtered_yield$Value-filtered_yield$trend_predicted)/filtered_yield$trend_predicted
filtered_yield$`Deviation %` <- (filtered_yield$Value-filtered_yield$trend_predicted)/filtered_yield$trend_predicted * 100

# Merge NDVI on Yearly Yield (1985-2025)----------------------------------
vis <- vis[, !names(vis) %in% "has_blue_band"]
combined_vi <- rbind(vis, vi25)

vis_wide <- combined_vi %>%
  pivot_wider(
    id_cols = c(satellite, year),  # keep one row per year (and satellite, if needed)
    names_from = month_name,
    values_from = c(peak_NDVI, peak_GOSAVI, peak_GDVI, peak_EVI, peak_CVI),
    names_glue = "{.value}_{month_name}"  # creates column names like peak_NDVI_May
  )%>%
  # Add vegetation indices across growing seasons
  mutate(
    mNDVI_678   = rowMeans(across(c(peak_NDVI_Jun, peak_NDVI_Jul, peak_NDVI_Aug)), na.rm = TRUE),
    mGOSAVI_678 = rowMeans(across(c(peak_GOSAVI_Jun, peak_GOSAVI_Jul, peak_GOSAVI_Aug)), na.rm = TRUE),
    mGDVI_678   = rowMeans(across(c(peak_GDVI_Jun, peak_GDVI_Jul, peak_GDVI_Aug)), na.rm = TRUE),
    mEVI_678    = rowMeans(across(c(peak_EVI_Jun, peak_EVI_Jul, peak_EVI_Aug)), na.rm = TRUE),
    mCVI_678    = rowMeans(across(c(peak_CVI_Jun, peak_CVI_Jul, peak_CVI_Aug)), na.rm = TRUE)
  )

vis_wide <- vis_wide %>%
  filter(
    satellite != "Landsat4_TM",
    satellite != "Landsat9_OLI2",
    !(satellite == "Landsat7_ETM" & year != 2012)
  )

merged_data <- filtered_yield %>%
  left_join(vis_wide, by = c("Year" = "year"))

# Regress VIs and year_order on yield  ---------------------------

model_EVI <- lm(Value ~ year_order + mEVI_678, data = merged_data)
summary(model_EVI)
# With EVI has a significant positive effect on annual yield. 

merged_data$pred_yield_EVI <- predict(model_EVI, newdata = merged_data)

############################# 
# 2014-2025 Period Using "G+E"
############################# 
# Percentage Deviation (2014-2025)----------------------------------------------------

filtered_subset <- merged_data[merged_data$Year >= 2014 & merged_data$Year <= 2025, ]
filtered_subset <- merge(filtered_subset, yearly_summary[, c("Year", "G+E")],
                         by = "Year", all.x = TRUE)

model_GE <- lm(Value ~ `G+E`, data = filtered_subset)

summary(model_GE)

filtered_subset$pred_yield_GE <- predict(model_GE, newdata = filtered_subset)

# Percent deviation from the trend
filtered_subset$deviation <- (filtered_subset$Value-filtered_subset$trend_predicted)/filtered_subset$trend_predicted
filtered_subset$`Deviation %` <- (filtered_subset$Value-filtered_subset$trend_predicted)/filtered_subset$trend_predicted * 100

library(plotly)

plot_ly() %>%
  # Actual Yield (1984–2025)
  add_lines(data = merged_data, x = ~Year, y = ~Value, name = "Actual Yield",
            line = list(color = 'black', width = 2)) %>%
  add_markers(data = merged_data, x = ~Year, y = ~Value, name = "Actual Points",
              marker = list(color = 'black', size = 6)) %>%
  
  # Trend Yield Line
  add_lines(data = merged_data, x = ~Year, y = ~trend_predicted, name = "Trend Yield",
            line = list(color = 'blue', dash = 'dash', width = 2)) %>%
  
  # EVI Forecast (1984–2025)
  add_lines(data = merged_data, x = ~Year, y = ~pred_yield_EVI, name = "Forecast (EVI)",
            line = list(color = 'green', dash = 'dot', width = 2)) %>%
  add_markers(data = merged_data, x = ~Year, y = ~pred_yield_EVI, name = "EVI Points",
              marker = list(color = 'green', size = 6)) %>%
  
  # G+E Forecast (2014–2025 subset only)
  add_lines(data = filtered_subset, x = ~Year, y = ~pred_yield_GE, name = "Forecast (G+E)",
            line = list(color = 'orange', dash = 'dashdot', width = 2)) %>%
  add_markers(data = filtered_subset, x = ~Year, y = ~pred_yield_GE, name = "G+E Points",
              marker = list(color = 'orange', size = 6)) %>%
  
  # Layout
  layout(
    title = list(text = "Virginia Corn Yield Forecast (1984–2025)", x = 0),
    xaxis = list(title = "Year", dtick = 2),
    yaxis = list(title = "Yield (Bushels per Acre)"),
    legend = list(x = 0.01, y = 0.99),
    hovermode = "x unified"
  )

