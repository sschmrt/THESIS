# Load required libraries
library(tidyverse)

# Read the CSV file (update filename)
setwd("C:/Users/marta/Desktop/THESIS/Outputs")
df <- read.csv("Validate_V0Bikes.csv", skip = 7)  # Adjust skip if necessary


# Example clean-up: Remove unwanted columns or reshape
df_clean <- df %>%
  filter(!is.na(mean.speed.bike)) %>%   # Adjust to your actual column names
  mutate(setting = rep(1:5, each = 10))  # Tagging 5 parameter levels

head(df_clean)

# Summarize by parameter setting
summary_stats <- df_clean %>%
  group_by(setting) %>%
  summarise(
    mean_bike_speed = mean(mean.speed.bike),
    sd_bike_speed = sd(mean.speed.bike),
    mean_ped_speed = mean(mean.speed.ped),
    total_severe = mean(total.severe)
    # add more variables as needed
  )

print(summary_stats)

# Plot example: Bike speed across settings
ggplot(summary_stats, aes(x = setting, y = mean_bike_speed)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = mean_bike_speed - sd_bike_speed,
                    ymax = mean_bike_speed + sd_bike_speed),
                width = 0.2) +
  labs(title = "OFAT Sensitivity: Mean Bike Speed",
       x = "Parameter Setting",
       y = "Mean Bike Speed") +
  theme_minimal()