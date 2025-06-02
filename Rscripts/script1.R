
#This data set is from the USDA survey and represents 
#crop conditions of corn that were measured in a good year in weekly Virginia from 2021-2024
data = read.csv("GoodCorn.csv")
head(data)

library(ggplot2)
library(dplyr)
#Initial plot to view
ggplot(data, aes(x = Period, y =Value)) + geom_point()

#Updated plot
data$WeekNum <- as.numeric(gsub("[^0-9]", "", data$Period))
data$Period <- factor(data$Period, levels = data$Period[order(data$WeekNum)])
ggplot(data, aes(x = Period, y = Value, color = Year, group = Year)) +
  geom_line(size = 1.2) +
  labs(
    title = "Virginia Corn Rated 'Good' by Week (2021–2024)",
    x = "Week",
    y = "Percent Rated Good"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 8),
    plot.margin = margin(10, 20, 10, 10)
  )

#Splitting the plots into years
ggplot(data, aes(x = Period, y = Value, group = Year)) +
  geom_line(color = "steelblue", size = 1.2) +
  facet_wrap(~Year, ncol = 2) +
  labs(
    title = "Virginia Corn Rated 'Good' by Week (Faceted by Year)",
    x = "Week",
    y = "Percent Rated Good"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 7),
    strip.text = element_text(size = 12, face = "bold"),
    plot.title = element_text(size = 14, face = "bold")
  )






