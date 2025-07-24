#Libraries
library(shiny)
library(shinyWidgets)
library(bslib)
library(readr)
library(dplyr)
library(ggplot2)
library(plotly)
library(rnassqs)
library(zoo)
library(leaflet)
library(sf)
library(tigris)
library(lubridate)
library(stringr)
library(tidyr)
options(tigris_use_cache = TRUE)

#API Key
nassqs_auth(key = "E4A8F7DF-7324-371D-A735-4F0FBC2629EE")

#Processing Data and Cleaning it
#Crop Conditions
get_corn_data <- function(year) {
  data <- nassqs(list(
    commodity_desc = "CORN",
    year = year,
    state_name = "VIRGINIA",
    statisticcat_desc = "CONDITION",
    agg_level_desc = "STATE"
  ))
  if (nrow(data) == 0) {
    return(tibble(
      week = as.Date(character()),
      value = numeric(),
      condition = factor(levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")),
      unit_desc = factor(levels = c("PCT VERY POOR", "PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")),
      year = character()
    ))
  }
  data %>%
    filter(unit_desc %in% c("PCT EXCELLENT", "PCT GOOD", "PCT FAIR", "PCT POOR", "PCT VERY POOR")) %>%
    mutate(
      week = as.Date(week_ending),
      value = as.numeric(Value),
      condition = gsub("PCT ", "", unit_desc),
      condition = factor(condition, levels = c("VERY POOR", "POOR", "FAIR", "GOOD", "EXCELLENT")),
      unit_desc = factor(unit_desc, levels = c("PCT VERY POOR", "PCT POOR", "PCT FAIR", "PCT GOOD", "PCT EXCELLENT")),
      year = as.character(year)
    )
}

corn_data_list <- lapply(2021:2025, get_corn_data)
names(corn_data_list) <- as.character(2021:2025)

condition_colors <- c(
  "VERY POOR" = "#a50026",
  "POOR" = "#d73027",
  "FAIR" = "#fee08b",
  "GOOD" = "#66bd63",
  "EXCELLENT" = "#1a9850"
)

#County Analysis
states <- c("VA", "NC", "MD")
years <- 2021:2025
stats <- c("AREA PLANTED", "AREA HARVESTED")

all_counties <- bind_rows(lapply(states, function(st) {
  counties(state = st, cb = TRUE, year = 2023) %>%
    st_transform(4326) %>%
    mutate(
      County = tolower(NAME),
      County = gsub(" county", "", County),
      County = trimws(County),
      State = st
    )
}))

get_county_acres <- function(state, year, stat_cat) {
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
    df$Year <- as.character(year)
    df$stat_type <- stat_cat
    df$state_alpha <- state
    df
  }, error = function(e) NULL)
}

# Safely load or generate county-level raw data
if (file.exists("county_data_cache.csv")) {
  raw_data <- read_csv("county_data_cache.csv", show_col_types = FALSE)
} else {
  raw_data <- bind_rows(lapply(states, function(st) {
    bind_rows(lapply(years, function(yr) {
      bind_rows(lapply(stats, function(sc) {
        get_county_acres(st, yr, sc)
      }))
    }))
  }))
  
  write_csv(raw_data, "county_data_cache.csv")
}


clean_data <- raw_data %>%
  mutate(
    County = tolower(county_name),
    County = gsub(" county", "", County),
    County = trimws(County),
    stat_type = ifelse(stat_type == "AREA PLANTED", "Planted", "Harvested"),
    Value = as.numeric(Value)
  ) %>%
  filter(!is.na(County)) %>%  
  group_by(County, state_alpha, Year, stat_type) %>%
  summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = stat_type, values_from = Value) %>%
  mutate(
    County = case_when(
      County == "chesapeake city" ~ "chesapeake",
      County == "suffolk city" ~ "suffolk",
      County == "virginia beach city" ~ "virginia beach",
      TRUE ~ County
    )
  )


#Planting Progress
clean_title <- function(cat) {
  title <- gsub("^PCT ", "", cat)  # Remove "PCT "
  paste0(toupper(substr(title, 1, 1)), tolower(substr(title, 2, nchar(title))))  # Capitalize first letter
}
categories <- c(
  "PCT PLANTED", "PCT EMERGED", "PCT SILKING",
  "PCT DOUGH", "PCT DENTED", "PCT MATURE", "PCT HARVESTED"
)
get_category_description <- function(cat, year) {
  cat_key <- toupper(trimws(cat))  # Normalize for matching
  cat_clean <- clean_title(cat_key)  # Clean title for fallback
  
  descriptions <- list(
    "PCT PLANTED" = paste0("This chart shows the weekly progress of corn ", cat_clean, " in Virginia during ", year,
                           ". It helps identify early or delayed planting relative to the 5-Year Average."),
    "PCT EMERGED" = paste0("This chart illustrates how corn has ", cat_clean, " throughout the ", year,
                           " season. Early emergence suggests favorable weather and soil conditions."),
    "PCT SILKING" = paste0(cat_clean, " marks the beginning of pollination. This chart tracks its weekly progress during ", year, "."),
    "PCT DOUGH" = paste0("This chart shows how much corn reached the ", cat_clean, " stage in ", year,
                         ", a key indicator of grain development."),
    "PCT DENTED" = paste0("The ", cat_clean, " stage reflects kernel hardening. Track its progress through the ", year, " season."),
    "PCT MATURE" = paste0("This chart displays weekly percentages of corn reaching ", cat_clean, " in ", year,
                          ", which affects harvest scheduling."),
    "PCT HARVESTED" = paste0("This chart tracks how much of the corn crop was ", cat_clean, " each week in ", year,
                             ", compared to the historical average.")
  )
  
  if (!is.null(descriptions[[cat_key]])) {
    descriptions[[cat_key]]
  } else {
    paste0("This chart compares weekly values to the 5-Year Average for corn ", cat_clean, " in ", year, ".")
  }
}


# Actual data
get_data <- function(year, category) {
  tryCatch({
    nassqs(list(
      commodity_desc = "CORN",
      year = year,
      state_name = "VIRGINIA",
      statisticcat_desc = "PROGRESS",
      unit_desc = category
    )) %>%
      mutate(
        week = as.Date(week_ending),
        value = as.numeric(Value)
      ) %>%
      filter(!is.na(week)) %>%
      arrange(week)
  }, error = function(e) NULL)
}

# 5-Year Average
get_avg_data <- function(year, category) {
  tryCatch({
    nassqs(list(
      commodity_desc = "CORN",
      year = year,
      state_name = "VIRGINIA",
      statisticcat_desc = "PROGRESS, 5 YEAR AVG",
      unit_desc = category
    )) %>%
      mutate(
        week = as.Date(week_ending),
        value = as.numeric(Value)
      ) %>%
      filter(!is.na(week)) %>%
      arrange(week)
  }, error = function(e) NULL)
}

#Remote Sensing Data
read_ndvi <- function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # Try to figure out the correct column name for county
  county_col <- names(df)[names(df) %in% c("NAME", "county_name", "county")][1]
  
  if (is.null(county_col)) {
    stop(paste("No valid county column found in", file))
  }
  
  df %>%
    rename(county = all_of(county_col)) %>%
    mutate(date = as.Date(date))
}


# Load Temperature data as-is (county column is already named 'county')
read_temp <- function(file) {
  read_csv(file, show_col_types = FALSE)
}

# Load all NDVI recent files
ndvi_recent_all <- function() {
  files <- list.files(pattern = "NDVI_SRbands_Weekly_\\d{4}\\.csv", full.names = TRUE)
  
  lapply(files, function(file) {
    df <- read_ndvi(file)
    df$date <- as.Date(df$date)
    df$year <- lubridate::year(df$date)
    df
  }) %>% bind_rows()
}



# Load top 10 NDVI
ndvi_top10_data <- function() {
  read_ndvi("Top10Counties_NDVI.csv") %>%
    mutate(date = as.Date(date), year = lubridate::year(date))
}

get_valid_ndvi_counties <- function(df, year) {
  # Detect NDVI column
  ndvi_col <- if ("NDVI_mean" %in% names(df)) "NDVI_mean" else "mean_NDVI"
  
  # Detect county column
  county_col <- names(df)[names(df) %in% c("county", "NAME", "county_name")][1]
  if (is.null(county_col)) stop("No county column found in NDVI data.")
  
  df %>%
    filter(lubridate::year(date) == year,
           lubridate::month(date) >= 5,
           lubridate::month(date) <= 9,
           !is.na(.data[[ndvi_col]])) %>%
    distinct(.data[[county_col]]) %>%
    pull()
}





# Load top 10 temperature
temp_top10_all <- function() {
  list(
    Average = bind_rows(
      read_temp("Top10_TempAvg_2021.csv") %>% mutate(year = 2021),
      read_temp("Top10_TempAvg_2022.csv") %>% mutate(year = 2022),
      read_temp("Top10_TempAvg_2023.csv") %>% mutate(year = 2023),
      read_temp("Top10_TempAvg_2024.csv") %>% mutate(year = 2024),
      read_temp("Top10_TempAvg_2025.csv") %>% mutate(year = 2025)
      
    ),
    High = bind_rows(
      read_temp("Top10_TempHigh_2021.csv") %>% mutate(year = 2021),
      read_temp("Top10_TempHigh_2022.csv") %>% mutate(year = 2022),
      read_temp("Top10_TempHigh_2023.csv") %>% mutate(year = 2023),
      read_temp("Top10_TempHigh_2024.csv") %>% mutate(year = 2024),
      read_temp("Top10_TempAvg_2025.csv") %>% mutate(year = 2025)
    ),
    Low = bind_rows(
      read_temp("Top10_TempLow_2021.csv") %>% mutate(year = 2021),
      read_temp("Top10_TempLow_2022.csv") %>% mutate(year = 2022),
      read_temp("Top10_TempLow_2023.csv") %>% mutate(year = 2023),
      read_temp("Top10_TempLow_2024.csv") %>% mutate(year = 2024),
      read_temp("Top10_TempAvg_2025.csv") %>% mutate(year = 2025)
    )
  )
}

temp_recent_all <- function() {
  list(
    Average = bind_rows(
      read_temp("Filtered_MODIS_Temp_2021.csv") %>% mutate(year = 2021),
      read_temp("Filtered_MODIS_Temp_2022.csv") %>% mutate(year = 2022),
      read_temp("Filtered_MODIS_Temp_2023.csv") %>% mutate(year = 2023),
      read_temp("Filtered_MODIS_Temp_2024.csv") %>% mutate(year = 2024),
      read_temp("Filtered_MODIS_Temp_2025.csv") %>% mutate(year = 2025)
    ),
    High = bind_rows(
      read_temp("Filtered_MODIS_Temp_2021.csv") %>% mutate(year = 2021),
      read_temp("Filtered_MODIS_Temp_2022.csv") %>% mutate(year = 2022),
      read_temp("Filtered_MODIS_Temp_2023.csv") %>% mutate(year = 2023),
      read_temp("Filtered_MODIS_Temp_2024.csv") %>% mutate(year = 2024),
      read_temp("Filtered_MODIS_Temp_2025.csv") %>% mutate(year = 2025)
    ),
    Low = bind_rows(
      read_temp("Filtered_MODIS_Temp_2021.csv") %>% mutate(year = 2021),
      read_temp("Filtered_MODIS_Temp_2022.csv") %>% mutate(year = 2022),
      read_temp("Filtered_MODIS_Temp_2023.csv") %>% mutate(year = 2023),
      read_temp("Filtered_MODIS_Temp_2024.csv") %>% mutate(year = 2024),
      read_temp("Filtered_MODIS_Temp_2025.csv") %>% mutate(year = 2025)
    )
  )
}






