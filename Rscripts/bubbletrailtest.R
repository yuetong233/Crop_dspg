library(rnassqs)
library(dplyr)
library(readr)

nassqs_auth("E4A8F7DF-7324-371D-A735-4F0FBC2629EE")  # your API key

states <- c("VA", "NC", "MD")

yield_data_all <- bind_rows(lapply(states, function(state) {
  df <- nassqs(list(
    commodity_desc = "CORN",
    year = 2015:2023,
    agg_level_desc = "COUNTY",
    state_alpha = state,
    statisticcat_desc = "YIELD"
  ))
  df$State <- state
  df
}))

# ✅ Save it inside your Dashboard folder
write_csv(yield_data_all, "~/Desktop/DSPG/Crop_dspg_dashboard/Dashbaord/yield_data_cache.csv")

