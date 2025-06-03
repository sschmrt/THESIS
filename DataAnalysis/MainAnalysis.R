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
    !is.na(run),
    run >= 0 & run <= 30,
    tick %in% c(300, 600, 900, 1200, 1500, 1800, 
                2100, 2400, 2700, 3000, 3300, 3600),
    pxcor >= -30 & pxcor <= 30,
    pycor >= -30 & pycor <= 30,
    !is.na(severe) & severe > 0,
    !is.na(moderate) & moderate > 0,
    !is.na(mild) & mild > 0,
    !is.na(flow) & flow > 0,
    !is.na(`break-count`) & `break-count` > 0
  )

# ==============================
# 1. Line plots of speed
# ==============================

mean_per_tick <- global_data %>%
  group_by(tick) %>%
  summarize(
    mean_bike = mean(`mean.speed.bike`, na.rm = TRUE),
    sd_bike = sd(`mean.speed.bike`, na.rm = TRUE),
    mean_ped = mean(`mean.speed.ped`, na.rm = TRUE),
    sd_ped = sd(`mean.speed.ped`, na.rm = TRUE)
  )
# Bike speed over time
mean_per_tick_long <- mean_per_tick %>%
  pivot_longer(
    cols = c(mean_bike, mean_ped, sd_bike, sd_ped),
    names_to = c("metric", "agent"),
    names_pattern = "(mean|sd)_(bike|ped)",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = "metric",
    values_from = "value"
  )

# Now plot both with color/fill by agent
ggplot(mean_per_tick_long, aes(x = tick, y = mean, color = agent, fill = agent)) +
  geom_line(size = 1) +
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2, color = NA) +
  scale_color_manual(values = c("bike" = "blue", "ped" = "red")) +
  scale_fill_manual(values = c("bike" = "blue", "ped" = "red")) +
  labs(title = "Mean Speed Over Time", x = "Tick", y = "Mean Speed (m/s)", color = "Agent", fill = "Agent") +
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

patch_conflict_totals <- patchdata_clean %>%
  group_by(run, pxcor, pycor) %>%
  summarize(
    total_severe = sum(severe, na.rm = TRUE),
    total_moderate = sum(moderate, na.rm = TRUE),
    total_mild = sum(mild, na.rm = TRUE),
    .groups = "drop"
  )

# 2. Mean of totals across runs (simulations)
conflict_summary <- patch_conflict_totals %>%
  group_by(pxcor, pycor) %>%
  summarize(
    mean_total_severe = mean(total_severe, na.rm = TRUE),
    mean_total_moderate = mean(total_moderate, na.rm = TRUE),
    mean_total_mild = mean(total_mild, na.rm = TRUE),
    mean_conflict = mean(total_severe + total_moderate + total_mild, na.rm = TRUE),
    .groups = "drop"
  )

# Plot for severe conflicts
ggplot(conflict_summary, aes(x = pxcor, y = pycor, fill = mean_total_severe)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative Severe Conflicts per Patch (Mean Across Simulations)", fill = "Mean Total Severe") +
  theme_minimal() +
  coord_equal()

# Moderate plot
ggplot(conflict_summary, aes(x = pxcor, y = pycor, fill = mean_total_moderate)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", direction = -1,  na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative Moderate Conflicts per Patch (Mean Across Simulations)", fill = "Mean Total Moderate") +
  theme_minimal() +
  coord_equal()

# Mild plot
ggplot(conflict_summary, aes(x = pxcor, y = pycor, fill = mean_total_mild)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", direction = -1,  na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative Mild Conflicts per Patch (Mean Across Simulations)", fill = "Mean Total Mild") +
  theme_minimal() +
  coord_equal()

# 1. Sum all conflicts per patch per run
patch_conflict_totals$all_conflicts <- patch_conflict_totals$total_severe +
  patch_conflict_totals$total_moderate +
  patch_conflict_totals$total_mild

# 2. Then average across runs
all_conflicts_summary <- patch_conflict_totals %>%
  group_by(pxcor, pycor) %>%
  summarize(
    mean_total_all_conflicts = mean(all_conflicts, na.rm = TRUE),
    .groups = "drop"
  )

# Plot all conflicts
ggplot(all_conflicts_summary, aes(x = pxcor, y = pycor, fill = mean_total_all_conflicts)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative All Conflicts per Patch (Mean Across Simulations)", fill = "Mean Total Conflicts") +
  theme_minimal() +
  coord_equal()

# ==============================
# 4. Flow heatmap animation (time lapse)
# ==============================

flow_summary <- patchdata_clean %>%
  group_by(pxcor, pycor, tick) %>%
  summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")

ggplot(flow_summary, aes(x = pxcor, y = pycor, fill = mean_flow)) +
  geom_tile() +
  scale_fill_viridis_c(option = "inferno", direction = -1, na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative Flow per Patch (Mean Across Simulations)", fill = "Cumulative Flow") +
  theme_minimal() +
  coord_equal()

p_flow <- ggplot(flow_summary, aes(x = pxcor, y = pycor, fill = mean_flow)) +
  geom_tile() +
  scale_fill_viridis_c(option = "magma", direction = -1, na.value = "white") + # invert color scale
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Flow per Patch Tick: {frame_time}", fill = "Flow") +
  theme_minimal() +
  coord_equal() +
  transition_time(tick)

animate(p_flow, fps = 5, width = 700, height = 600)

# ==============================
# 5. Waiting heatmap (aggregate)
# ==============================
# 1. Sum waiting per patch per simulation
patch_wait_totals <- patchdata_clean %>%
  group_by(run, pxcor, pycor) %>%
  summarize(total_wait = sum(`break-count`, na.rm = TRUE), .groups = "drop")

# 2. Take the mean of the totals across simulations
waiting_summary <- patch_wait_totals %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_total_wait = mean(total_wait, na.rm = TRUE), .groups = "drop")

ggplot(waiting_summary, aes(x = pxcor, y = pycor, fill = mean_total_wait)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
  scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
  labs(title = "Cumulative Waiting per Patch (Mean Across Simulations)", fill = "Mean Total Wait") +
  theme_minimal() +
  coord_equal()


# ==============================
# 5. Flow and Conflict Aggregation
# ==============================

# Calculate thresholds
conflict_threshold <- quantile(conflict_summary$mean_conflict, 0.9, na.rm = TRUE)
flow_threshold <- quantile(flow_summary$mean_flow, 0.9, na.rm = TRUE)

# Merge summaries
overlay_data <- conflict_summary %>%
  inner_join(flow_summary, by = c("pxcor", "pycor")) %>%
  mutate(
    high_conflict = mean_conflict >= conflict_threshold,
    high_flow = mean_flow >= flow_threshold,
    both = high_conflict & high_flow
  )

ggplot(overlay_data, aes(x = pxcor, y = pycor)) +
  # Flow as base heatmap
  geom_tile(aes(fill = mean_flow)) +
  # Outline for high conflict
  geom_tile(
    data = subset(overlay_data, high_conflict),
    fill = NA, color = "red", size = 0.8
  ) +
  # Overlay a symbol for areas that are both high conflict and high flow
  geom_point(
    data = subset(overlay_data, both),
    aes(x = pxcor, y = pycor),
    fill = NA, color = "red", size = 4
  ) +
  scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
  labs(
    title = "Overlay of High Conflict and High Flow Areas",
    fill = "Mean Flow"
  ) +
  theme_minimal() +
  coord_equal()
