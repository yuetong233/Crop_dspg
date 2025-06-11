# Load libraries
library(ggplot2)
library(dplyr)
library(readr)

# Load data
planted <- read_csv("AcresPlanted.csv")
harvested <- read_csv("AcresHarvestedGrains.csv")

# Preview the data
head(planted)
head(harvested)

# OPTIONAL: Rename columns if needed to match (you may need to adjust based on actual column names)
# Example: Assume "County" is the column with county names and "Value" holds acres

# Merge the data by County
combined <- planted %>%
  rename(Planted = Value) %>%
  inner_join(harvested %>% rename(Harvested = Value), by = "County")

# Melt to long format for plotting
library(tidyr)
long_combined <- combined %>%
  pivot_longer(cols = c(Planted, Harvested), names_to = "Type", values_to = "Acres")

# Plot
ggplot(long_combined, aes(x = reorder(County, -Acres), y = Acres, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Acres Planted vs Harvested by County", x = "County", y = "Acres") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
