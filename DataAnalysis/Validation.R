library(readr)
library(dplyr)
library(tidyr)

# Step 1: Specify the column names as described
col_names <- c(
  "run", "mean_speed_ped", "mean_speed_bike", "stddev_speed_ped", "stddev_speed_bike",
  "Nb_Bikes", "A_bike", "bik_S", "ped_goer", "Tr_bike", "bik_E", "V0_ped", "D", "ped_S",
  "A_ped", "ped_E", "bik_N", "bike_goer", "Ferry", "ped_N", "bike_comer", "bik_W", "Tr_ped",
  "ped_comer", "ped_W", "Nb_peds", "v0_bike", "total_severe", "total_moderate", "total_mild"
)

# Read the CSV file (update filename)
setwd("C:/Users/marta/Desktop/THESIS/Outputs")
dat <- read_csv("V0Bikes.csv", col_names = col_names, show_col_types = FALSE)

# Step 3: Calculate per-run CVs
dat <- dat %>%
  mutate(
    CV_ped = stddev_speed_ped / mean_speed_ped,
    CV_bike = stddev_speed_bike / mean_speed_bike
  )
# Step 4: Group by v0_bike, calculate summary stats
v0 <- dat %>%
  group_by(v0_bike) %>%
  summarise(
    n_runs = n(),
    mean_mean_speed_ped = mean(mean_speed_ped, na.rm = TRUE),
    mean_mean_speed_bike = mean(mean_speed_bike, na.rm = TRUE),
    mean_stddev_speed_ped = mean(stddev_speed_ped, na.rm = TRUE),
    mean_stddev_speed_bike = mean(stddev_speed_bike, na.rm = TRUE),
    mean_CV_ped = mean(CV_ped, na.rm = TRUE),
    mean_CV_bike = mean(CV_bike, na.rm = TRUE),
    total_severe = mean(total_severe, na.rm = TRUE),
    total_moderate = mean(total_moderate, na.rm = TRUE),
    total_mild = mean(total_mild, na.rm = TRUE)
  ) %>%
  ungroup()

# Step 5: Save summary table for Excel/plots
write_csv(v0, "ofat_summary_by_v0bike.csv")

library(ggplot2)
ggplot(dat, aes(x = as.factor(v0_bike), y = CV_ped)) +
  geom_boxplot() +
  labs(x = "v0_bike", y = "CV (Pedestrians)", title = "CV of Pedestrian Speed by v0_bike")
