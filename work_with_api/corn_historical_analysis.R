
# #In summary, the code fetches 10 years of corn yield data for VA, Nc, and MD and summarizes it 
# at the state level. The code also visualizes trends and moving averages, computes summary statistics
# and analyzes year-over-year changes, making it easy to understand the long term and short term variation in
# corn yield across these states. 




library(rnassqs)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(zoo)
#install.packages('zoo')

#api authentication
nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# Function to get data for a specific state and year range
get_state_data <- function(state, start_year, end_year) {
  params <- list(
    commodity_desc = "CORN",
    year = seq(start_year, end_year),
    agg_level_desc = "COUNTY",
    state_alpha = state,
    statisticcat_desc = "YIELD"
  )
  return(nassqs(params))
}

# Get data for Virginia and neighboring states for the last 10 years
current_year <- as.numeric(format(Sys.Date(), "%Y"))
start_year <- current_year - 10

# Get data for each state
va_data <- get_state_data("VA", start_year, current_year)
nc_data <- get_state_data("NC", start_year, current_year)
md_data <- get_state_data("MD", start_year, current_year)

# Combine all state data
all_states_data <- rbind(
  va_data %>% mutate(State = "Virginia"),
  nc_data %>% mutate(State = "North Carolina"),
  md_data %>% mutate(State = "Maryland")
)

# Aggregate to state-year level
state_year_avg <- all_states_data %>%
  group_by(State, year) %>%
  summarize(
    avg_yield = mean(Value, na.rm = TRUE)
  ) %>%
  arrange(State, year) %>%
  group_by(State) %>%
  mutate(
    moving_avg = zoo::rollmean(avg_yield, k = 5, fill = NA, align = "right")
  )

# Plot: Statewide average yield and 5-year moving average
p <- ggplot(state_year_avg, aes(x = year, y = avg_yield, color = State)) +
  geom_line(size = 1.2) +
  geom_line(aes(y = moving_avg, linetype = "5-Year Moving Avg"), size = 1.2, show.legend = TRUE) +
  geom_point(size = 2) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", face = "bold"),
    legend.title = element_blank()
  ) +
  labs(
    title = "Statewide Corn Yield Trends (Last 10 Years)",
    subtitle = "Annual Average and 5-Year Moving Average by State",
    x = "Year",
    y = "Average Yield (bushels per acre)"
  ) +
  scale_linetype_manual(values = c("5-Year Moving Avg" = "dashed"))

ggsave("corn_yield_trends.png", p, width = 12, height = 8, bg = "white", dpi = 300)

# Calculate and display summary statistics
summary_stats <- all_states_data %>%
  group_by(State) %>%
  summarize(
    avg_yield = mean(Value, na.rm = TRUE),
    max_yield = max(Value, na.rm = TRUE),
    min_yield = min(Value, na.rm = TRUE),
    yield_range = max_yield - min_yield,
    yield_std = sd(Value, na.rm = TRUE)
  )


print("Summary Statistics by State:")
print(summary_stats)

# Save summary statistics to CSV
write.csv(summary_stats, "corn_yield_summary_stats.csv", row.names = FALSE)

# Calculate year-over-year changes
yoy_changes <- all_states_data %>%
  group_by(State, county_name) %>%
  arrange(State, county_name, year) %>%
  mutate(
    yoy_change = Value - lag(Value),
    yoy_change_pct = (Value - lag(Value)) / lag(Value) * 100
  )

# Create a plot showing year-over-year changes
ggplot(yoy_changes, aes(x = year, y = yoy_change_pct, fill = State)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black", face = "bold")
  ) +
  labs(
    title = "Year-over-Year Change in Corn Yields",
    subtitle = "Percentage Change by State",
    x = "Year",
    y = "Percentage Change"
  )

# Save the plot
ggsave("corn_yield_yoy_changes.png", width = 12, height = 8, bg = "white", dpi = 300) 
