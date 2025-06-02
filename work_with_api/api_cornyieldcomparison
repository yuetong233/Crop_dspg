library(rnassqs)
library(dplyr)
library(ggplot2)

nassqs_auth(key = "E0DE4B3D-0418-32C4-8541-6C4C8954534A")

# Get corn yields in Virginia for both 2012 and 2020
params_2012 <- list(commodity_desc = "CORN",
                    year = 2012,
                    agg_level_desc = "COUNTY",
                    state_alpha = "VA",
                    statisticcat_desc = "YIELD"
)

params_2020 <- list(commodity_desc = "CORN",
                    year = 2020,
                    agg_level_desc = "COUNTY",
                    state_alpha = "VA",
                    statisticcat_desc = "YIELD"
)

yields_2012 <- nassqs(params_2012)
yields_2020 <- nassqs(params_2020)

# Combine the datasets
yields_2012$Year <- "2012"
yields_2020$Year <- "2020"
combined_yields <- rbind(yields_2012, yields_2020)

# Calculate the change in yield for each county
yield_change <- combined_yields %>%
  group_by(county_name) %>%
  summarize(
    change = Value[Year == "2020"] - Value[Year == "2012"],
    avg_yield = mean(Value)
  ) %>%
  arrange(desc(change))

# Create a dot plot showing the change in yields
ggplot(combined_yields, aes(x = reorder(county_name, Value), y = Value, color = Year)) +
  geom_line(aes(group = county_name), color = "gray50", alpha = 0.5) +
  geom_point(size = 3) +
  geom_text(data = yield_change, 
            aes(x = county_name, y = max(combined_yields$Value) + 5,
                label = sprintf("%+.1f", change)),
            size = 3, color = "black") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white"),
    axis.text.y = element_text(color = "black", size = 11, margin = margin(r = 10)),
    axis.text.x = element_text(color = "black", size = 10),
    axis.title = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 14, face = "bold"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_line(color = "gray95"),
    legend.position = "top",
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20),
    axis.title.y = element_text(margin = margin(r = 15))
  ) +
  scale_color_manual(values = c("2012" = "cornflowerblue", "2020" = "darkorange")) +
  labs(title = "Change in Corn Yields by County in Virginia (2012 vs 2020)",
       subtitle = "Numbers above show the change in bushels per acre",
       x = "County",
       y = "Yield (bushels per acre)") +
  coord_flip() +
  scale_y_continuous(expand = c(0, 0))

# Save the comparison plot with increased height
ggsave("virginia_corn_yields_comparison.png", width = 12, height = 10, bg = "white", dpi = 300)
