library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rlang)
library(flextable)
library(officer)
library (gganimate)

# ==============================
# 1. Load and Prepare
# ==============================

# Set Directory
setwd("C:/Users/marta/Desktop/THESIS/Outputs")
# Define File Lists
global_files <- c("LowGlobal.csv", "2030Global.csv", "HighGlobal.csv")
patch_files  <- c("LowPatch.csv", "2030Patch.csv", "HighPatch.csv")

# Define column names
patch_colnames <- c("run", "tick", "pxcor", "pycor", "severe", "moderate", "mild", "flow", "break-count")
global_colnames <- c(
  "run", "tick", "mean.speed.ped", "mean.speed.bike", "stddev-speed-ped", "stddev-speed-bike",
  "Nb-Bikes", "A-bike", "bik_S", "ped-goer", "Tr-bike", "bik_E", "V0-ped", "D", "ped_S", "A-ped",
  "ped_E", "bik_N", "bike-goer", "Ferry", "ped_N", "bike-comer", "bik_W", "Tr-ped", "ped-comer",
  "ped_W", "Nb-peds", "v0-bike", "total-severe", "total-moderate", "total-mild"
)

# Helper Function to Identify Scenario
get_scenario <- function(filename) {
  if (grepl("Low", filename, ignore.case = TRUE)) return("Low Density")
  if (grepl("2030", filename, ignore.case = TRUE)) return("2030")
  if (grepl("High", filename, ignore.case = TRUE)) return("High Density")
  return("Unknown")
}

# LowGlobal
LowGlobal <- read.csv("LowGlobal.csv")
colnames(LowGlobal) <- global_colnames
LowGlobal$Scenario <- get_scenario("LowGlobal.csv")
LowGlobal$Type <- "Global"

# LowPatch
LowPatch <- read.csv("LowPatch.csv")
colnames(LowPatch) <- patch_colnames
LowPatch$Scenario <- get_scenario("LowPatch.csv")
LowPatch$Type <- "Patch"

# 2030Global
X2030Global <- read.csv("2030Global.csv")
colnames(X2030Global) <- global_colnames
X2030Global$Scenario <- get_scenario("2030Global.csv")
X2030Global$Type <- "Global"

# 2030Patch
X2030Patch <- read.csv("2030Patch.csv")
colnames(X2030Patch) <- patch_colnames
X2030Patch$Scenario <- get_scenario("2030Patch.csv")
X2030Patch$Type <- "Patch"

# HighGlobal
HighGlobal <- read.csv("HighGlobal.csv")
colnames(HighGlobal) <- global_colnames
HighGlobal$Scenario <- get_scenario("HighGlobal.csv")
HighGlobal$Type <- "Global"

# HighPatch
HighPatch <- read.csv("HighPatch.csv")
colnames(HighPatch) <- patch_colnames
HighPatch$Scenario <- get_scenario("HighPatch.csv")
HighPatch$Type <- "Patch"

LowPatch_clean <- LowPatch %>%
  filter(
    !is.na(run),
    run >= 0 & run <= 30,
    tick %in% c(300, 600, 900, 1200, 1500, 1800, 
                2100, 2400, 2700, 3000, 3300, 3600),
    pxcor >= -30 & pxcor <= 30,
    pycor >= -30 & pycor <= 30,
    !is.na(severe) & severe >= 0,
    !is.na(moderate) & moderate >= 0,
    !is.na(mild) & mild >= 0,
    !is.na(flow) & flow >= 0,
    !is.na(`break-count`) & `break-count` >= 0
  )

X2030Patch_clean <- X2030Patch %>%
  filter(
    !is.na(run),
    run >= 0 & run <= 30,
    tick %in% c(300, 600, 900, 1200, 1500, 1800, 
                2100, 2400, 2700, 3000, 3300, 3600),
    pxcor >= -30 & pxcor <= 30,
    pycor >= -30 & pycor <= 30,
    !is.na(severe) & severe >= 0,
    !is.na(moderate) & moderate >= 0,
    !is.na(mild) & mild >= 0,
    !is.na(flow) & flow >= 0,
    !is.na(`break-count`) & `break-count` >= 0
  )

HighPatch_clean <- HighPatch %>%
  filter(
    !is.na(run),
    run >= 0 & run <= 30,
    tick %in% c(300, 600, 900, 1200, 1500, 1800, 
                2100, 2400, 2700, 3000, 3300, 3600),
    pxcor >= -30 & pxcor <= 30,
    pycor >= -30 & pycor <= 30,
    !is.na(severe) & severe >= 0,
    !is.na(moderate) & moderate >= 0,
    !is.na(mild) & mild >= 0,
    !is.na(flow) & flow >= 0,
    !is.na(`break-count`) & `break-count` >= 0
  )
# ==============================
# 2. Line plots of speed
# ==============================

plot_global_speeds <- function(global_data, scenario_name) {
  mean_per_tick <- global_data %>%
    group_by(tick) %>%
    summarize(
      mean_bike = mean(`mean.speed.bike`, na.rm = TRUE),
      sd_bike = sd(`mean.speed.bike`, na.rm = TRUE),
      mean_ped = mean(`mean.speed.ped`, na.rm = TRUE),
      sd_ped = sd(`mean.speed.ped`, na.rm = TRUE)
    )
  
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
  
  plot_title <- paste("Mean Speed Over Time -", scenario_name, "Scenario")
  
  p <-    ggplot(mean_per_tick_long, aes(x = tick, y = mean, color = agent, fill = agent)) +
    geom_line(size = 1) +
    geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.2, color = NA) +
    scale_color_manual(values = c("bike" = "blue", "ped" = "red")) +
    scale_fill_manual(values = c("bike" = "blue", "ped" = "red")) +
    labs(
      title = plot_title,
      x = "Tick",
      y = "Mean Speed (m/s)",
      color = "Agent",
      fill = "Agent"
    ) +
    theme_minimal()
    print(p)
}

plot_global_speeds(LowGlobal, "Low Density")
plot_global_speeds(X2030Global, "2030")
plot_global_speeds(HighGlobal, "High Density")
# ==============================
# 3. Boxplots of speed per tick
# ==============================

plot_global_speed_boxplot <- function(global_data, scenario_name) {
  global_long <- global_data %>%
    pivot_longer(cols = c(mean.speed.bike, mean.speed.ped),
                 names_to = "agent", values_to = "speed") %>%
    mutate(agent = ifelse(agent == "mean.speed.bike", "Bike", "Pedestrian"))
  
  p <- ggplot(global_long, aes(x = as.factor(tick), y = speed, fill = agent)) +
    geom_boxplot(outlier.size = 0.5, position = position_dodge(width = 0.8)) +
    labs(
      title = paste("Distribution of Speeds per Tick -", scenario_name, "Scenario"),
      x = "Tick", y = "Speed (m/s)"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Bike" = "blue", "Pedestrian" = "red"))
  
  print(p)
}

# Example usage:
plot_global_speed_boxplot(LowGlobal, "Low Density")
plot_global_speed_boxplot(X2030Global, "2030")
plot_global_speed_boxplot(HighGlobal, "High Density")
# ==============================
# 4. Conflict
# ==============================
plot_conflict_heatmap <- function(patch_conflict_totals, scenario_name, level = c("severe", "moderate", "mild", "all")) {
  level <- match.arg(level)
  
  # Compute summary based on the requested level
  if (level == "all") {
    patch_conflict_totals <- patch_conflict_totals %>%
      mutate(all_conflicts = severe + moderate + mild)
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(all_conflicts, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Conflicts"
    plot_title <- paste("Cumulative All Conflicts per Patch (Mean Across Simulations) -", scenario_name)
    palette <- "inferno"
  } else if (level == "severe") {
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(severe, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Severe"
    plot_title <- paste("Cumulative Severe Conflicts per Patch (Mean Across Simulations) -", scenario_name)
    palette <- "inferno"
  } else if (level == "moderate") {
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(moderate, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Moderate"
    plot_title <- paste("Cumulative Moderate Conflicts per Patch (Mean Across Simulations) -", scenario_name)
    palette <- "plasma"
  } else if (level == "mild") {
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(mild, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Mild"
    plot_title <- paste("Cumulative Mild Conflicts per Patch (Mean Across Simulations) -", scenario_name)
    palette <- "plasma"
  }
  
  # Ensure coordinates are numeric (fixes plotting error)
  conflict_summary <- conflict_summary %>%
    mutate(
      pxcor = as.numeric(pxcor),
      pycor = as.numeric(pycor)
    )
  
  # Now plot
  p <- ggplot(conflict_summary, aes(x = pxcor, y = pycor, fill = mean_total)) +
    geom_tile() +
    scale_fill_viridis_c(option = palette, direction = -1, na.value = "white") +
    scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
    labs(title = plot_title, fill = fill_lab) +
    theme_minimal() +
    coord_equal()
  print(p)
}
plot_conflict_heatmap(HighPatch_clean, "High Density", "severe")
plot_conflict_heatmap(LowPatch_clean, "Low Density", "severe")
plot_conflict_heatmap(LowPatch_clean, "Low Density", "moderate")
plot_conflict_heatmap(LowPatch_clean, "Low Density", "mild")
plot_conflict_heatmap(LowPatch_clean, "Low Density", "all")

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
