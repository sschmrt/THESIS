library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rlang)
library(flextable)
library(officer)
library (gganimate)

setwd("C:/Users/marta/Desktop/THESIS/Outputs")
# PATCH (readr)
patch_colnames <- c("run", "tick", "pxcor", "pycor", "severe", "moderate", "mild", "flow", "break-count")
patchdata <- readr::read_csv("HighPatch.csv", col_names = patch_colnames, show_col_types = FALSE)

# GLOBAL (base R)
global_colnames <- c(
  "run", "tick", "mean-speed-ped", "mean-speed-bike", "stddev-speed-ped", "stddev-speed-bike",
  "Nb-Bikes", "A-bike", "bik_S", "ped-goer", "Tr-bike", "bik_E", "V0-ped", "D", "ped_S", "A-ped",
  "ped_E", "bik_N", "bike-goer", "Ferry", "ped_N", "bike-comer", "bik_W", "Tr-ped", "ped-comer",
  "ped_W", "Nb-peds", "v0-bike", "total-severe", "total-moderate", "total-mild"
)
global_data <- read.csv("HighGlobal.csv", col.names = global_colnames, header = FALSE)

patchdata_clean <- patchdata %>%
  filter(
    run >= 0 & run <= 30,
    tick >= 0 & tick <= 3600,
    pxcor >= -30 & pxcor <= 30,
    pycor >= -30 & pycor <= 30
  )

# ==============================
# 1. Line plots of speed
# ==============================

# Bike speed over time
ggplot(mean_per_tick, aes(x = tick, y = mean_bike)) +
  geom_line(color = "blue") +
  geom_ribbon(aes(ymin = mean_bike - sd_bike, ymax = mean_bike + sd_bike), fill = "blue", alpha = 0.2) +
  labs(title = "Mean Bike Speed Over Time", x = "Tick", y = "Mean Speed (m/s)") +
  theme_minimal()

# Pedestrian speed over time
ggplot(mean_per_tick, aes(x = tick, y = mean_ped)) +
  geom_line(color = "red") +
  geom_ribbon(aes(ymin = mean_ped - sd_ped, ymax = mean_ped + sd_ped), fill = "red", alpha = 0.2) +
  labs(title = "Mean Pedestrian Speed Over Time", x = "Tick", y = "Mean Speed (m/s)") +
  theme_minimal()

# ==============================
# 2. Boxplots of speed per tick
# ==============================

global_long <- global_data %>%
  pivot_longer(cols = c(mean.speed.bike, mean.speed.ped),
               names_to = "agent", values_to = "speed") %>%
  mutate(agent = ifelse(agent == "mean.speed.bike", "Bike", "Pedestrian"))

ggplot(global_long, aes(x = as.factor(tick), y = speed, fill = agent)) +
  geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.8)) +
  labs(title = "Distribution of Speeds per Tick", x = "Tick", y = "Speed (m/s)") +
  theme_minimal() +
  scale_fill_manual(values = c("Bike" = "blue", "Pedestrian" = "red"))

# ==============================
# 3. Conflict heatmap animation (time lapse)
# ==============================

conflict_summary <- patchdata_clean %>%
  group_by(pxcor, pycor, tick) %>%
  summarize(mean_severe = mean(severe, na.rm=TRUE),
            mean_moderate = mean(moderate, na.rm=TRUE),
            mean_mild = mean(mild, na.rm=TRUE),
            .groups = "drop")

# Example: Animate mean_severe conflicts
p_severe <- ggplot(conflict_summary, aes(x = pxcor, y = pycor, fill = mean_severe)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Severe Conflicts (mean) Tick: {frame_time}", fill = "Severe") +
  theme_minimal() +
  coord_equal() +
  transition_time(tick)

animate(p_severe, fps = 5, width = 700, height = 600)

# ==============================
# 4. Flow heatmap animation (time lapse)
# ==============================

flow_summary <- patchdata %>%
  group_by(pxcor, pycor, tick) %>%
  summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")

p_flow <- ggplot(flow_summary, aes(x = pxcor, y = pycor, fill = mean_flow)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Flow per Patch Tick: {frame_time}", fill = "Flow") +
  theme_minimal() +
  coord_equal() +
  transition_time(tick)

animate(p_flow, fps = 5, width = 700, height = 600)
wa
# ==============================
# 5. Waiting heatmap (aggregate)
# ==============================
# 1. Sum waiting per patch per simulation
patch_wait_totals <- patchdata %>%
  group_by(run, pxcor, pycor) %>%
  summarize(total_wait = sum(`break-count`, na.rm = TRUE), .groups = "drop")

# 2. Take the mean of the totals across simulations
waiting_summary <- patch_wait_totals %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_total_wait = mean(total_wait, na.rm = TRUE), .groups = "drop")

ggplot(waiting_summary, aes(x = pxcor, y = pycor, fill = mean_total_wait)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative Waiting per Patch (Mean Across Simulations)", fill = "Mean Total Wait") +
  theme_minimal() +
  coord_equal()
