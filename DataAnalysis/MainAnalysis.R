library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rlang)
library(flextable)
library(officer)
library (gganimate)
library (patchwork)

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

LowPatch <- LowPatch %>%
  mutate(
    pxcor = suppressWarnings(as.numeric(pxcor)),
    pycor = suppressWarnings(as.numeric(pycor))
  ) %>%
  filter(!is.na(pxcor), !is.na(pycor))

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

X2030Patch <- X2030Patch %>%
  mutate(
    pxcor = suppressWarnings(as.numeric(pxcor)),
    pycor = suppressWarnings(as.numeric(pycor))
  ) %>%
  filter(!is.na(pxcor), !is.na(pycor))

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

HighPatch <- HighPatch %>%
  mutate(
    pxcor = suppressWarnings(as.numeric(pxcor)),
    pycor = suppressWarnings(as.numeric(pycor))
  ) %>%
  filter(!is.na(pxcor), !is.na(pycor))

LowPatch_clean <- LowPatch %>%
  mutate(all_conflicts = severe + moderate + mild) %>%
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
  mutate(all_conflicts = severe + moderate + mild) %>%
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
 mutate(all_conflicts = severe + moderate + mild) %>%
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
  
  plot_title <- paste("Mean Speed Over Time -", scenario_name)
  
  p <-    ggplot(mean_per_tick_long, aes(x = tick, y = mean, color = agent, fill = agent)) +
    geom_line(linewidth = 1) +
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

p_low <- plot_global_speeds(LowGlobal, "Low Density")
p_high <- plot_global_speeds(HighGlobal, "High Density")
p_2030 <- plot_global_speeds(X2030Global, "2030")

p_2030

# Low and High Density beside each other
p_low + p_high

# High and 2030 beside each other
p_high + p_2030


# Helper function to find max and min speeds for each agent in a scenario
find_speed_extremes <- function(global_data, scenario_name) {
  summary <- global_data %>%
    group_by(tick) %>%
    summarize(
      mean_bike = mean(`mean.speed.bike`, na.rm = TRUE),
      mean_ped  = mean(`mean.speed.ped`, na.rm = TRUE)
    )
  
  bike_max <- summary %>% filter(mean_bike == max(mean_bike, na.rm = TRUE))
  bike_min <- summary %>% filter(mean_bike == min(mean_bike, na.rm = TRUE))
  ped_max  <- summary %>% filter(mean_ped  == max(mean_ped, na.rm = TRUE))
  ped_min  <- summary %>% filter(mean_ped  == min(mean_ped, na.rm = TRUE))
  
  cat("\nScenario:", scenario_name, "\n")
  cat("Bike - Max speed at tick", bike_max$tick, ":", bike_max$mean_bike, "\n")
  cat("Bike - Min speed at tick", bike_min$tick, ":", bike_min$mean_bike, "\n")
  cat("Pedestrian - Max speed at tick", ped_max$tick, ":", ped_max$mean_ped, "\n")
  cat("Pedestrian - Min speed at tick", ped_min$tick, ":", ped_min$mean_ped, "\n")
  
  # Optionally, return a data frame
  data.frame(
    Scenario = scenario_name,
    Agent = c("Bike", "Bike", "Pedestrian", "Pedestrian"),
    Type  = c("Max", "Min", "Max", "Min"),
    Tick  = c(bike_max$tick, bike_min$tick, ped_max$tick, ped_min$tick),
    Speed = c(bike_max$mean_bike, bike_min$mean_bike, ped_max$mean_ped, ped_min$mean_ped)
  )
}

# Run for each scenario
extremes_low <- find_speed_extremes(LowGlobal, "Low Density")
extremes_high <- find_speed_extremes(HighGlobal, "High Density")
extremes_2030 <- find_speed_extremes(X2030Global, "2030")

# Combine results if you want a single summary table
extremes_all <- rbind(extremes_low, extremes_high, extremes_2030)
print(extremes_all)

# Helper function: Get ticks in lower and upper 10% of variance for each agent
find_variance_extremes <- function(global_data, scenario_name) {
  # Group by tick and calculate variance per tick for both agents
  var_per_tick <- global_data %>%
    group_by(tick) %>%
    summarize(
      var_bike = var(`mean.speed.bike`, na.rm = TRUE),
      var_ped  = var(`mean.speed.ped`, na.rm = TRUE)
    )
  
  # For each agent, get quantiles
  quant_bike <- quantile(var_per_tick$var_bike, probs = c(0.1, 0.9), na.rm = TRUE)
  quant_ped  <- quantile(var_per_tick$var_ped,  probs = c(0.1, 0.9), na.rm = TRUE)
  
  # Ticks in lower and upper 10% for each
  lower10_bike <- var_per_tick %>% filter(var_bike <= quant_bike[1])
  upper10_bike <- var_per_tick %>% filter(var_bike >= quant_bike[2])
  lower10_ped  <- var_per_tick %>% filter(var_ped  <= quant_ped[1])
  upper10_ped  <- var_per_tick %>% filter(var_ped  >= quant_ped[2])
  
  # Print or return a summary
  cat("\nScenario:", scenario_name, "\n")
  cat("Bike - Lower 10% variance ticks:\n")
  print(lower10_bike[, c("tick", "var_bike")])
  cat("Bike - Upper 10% variance ticks:\n")
  print(upper10_bike[, c("tick", "var_bike")])
  cat("Pedestrian - Lower 10% variance ticks:\n")
  print(lower10_ped[, c("tick", "var_ped")])
  cat("Pedestrian - Upper 10% variance ticks:\n")
  print(upper10_ped[, c("tick", "var_ped")])
  
  # Combine for possible table output
  data.frame(
    Scenario = scenario_name,
    Agent = c(
      rep("Bike", nrow(lower10_bike)), rep("Bike", nrow(upper10_bike)),
      rep("Pedestrian", nrow(lower10_ped)), rep("Pedestrian", nrow(upper10_ped))
    ),
    Quantile = c(
      rep("Lower 10%", nrow(lower10_bike)), rep("Upper 10%", nrow(upper10_bike)),
      rep("Lower 10%", nrow(lower10_ped)),  rep("Upper 10%", nrow(upper10_ped))
    ),
    Tick = c(
      lower10_bike$tick, upper10_bike$tick, lower10_ped$tick, upper10_ped$tick
    ),
    Variance = c(
      lower10_bike$var_bike, upper10_bike$var_bike, lower10_ped$var_ped, upper10_ped$var_ped
    )
  )
}

# Run for each scenario
bikeped_low  <- find_variance_extremes(LowGlobal,    "Low Density")
bikeped_high <- find_variance_extremes(HighGlobal,   "High Density")
bikeped_2030 <- find_variance_extremes(X2030Global,  "2030")

# Combine all for table if needed
bikeped_all <- rbind(bikeped_low, bikeped_high, bikeped_2030)
print(bikeped_all)
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
      title = paste("Distribution of Speeds per Tick -", scenario_name),
      x = "Tick", y = "Speed (m/s)"
    ) +
    theme_minimal() +
    scale_fill_manual(values = c("Bike" = "blue", "Pedestrian" = "red"))
  
  print(p)
}

# Patchwork
boxlow <- plot_global_speed_boxplot(LowGlobal, "Low D")
box2030 <- plot_global_speed_boxplot(X2030Global, "2030")
boxhigh <- plot_global_speed_boxplot(HighGlobal, "High D")

boxlow + boxhigh
boxhigh + box2030
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
    plot_title <- paste("Cumulative All Conflicts per Patch -", scenario_name)
    palette <- "inferno"
  } else if (level == "severe") {
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(severe, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Severe"
    plot_title <- paste("Cumulative Severe Conflicts per Patch -", scenario_name)
    palette <- "inferno"
  } else if (level == "moderate") {
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(moderate, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Moderate"
    plot_title <- paste("Cumulative Moderate Conflicts per Patch -", scenario_name)
    palette <- "plasma"
  } else if (level == "mild") {
    conflict_summary <- patch_conflict_totals %>%
      group_by(pxcor, pycor) %>%
      summarize(mean_total = mean(mild, na.rm = TRUE), .groups = "drop")
    fill_lab <- "Mean Total Mild"
    plot_title <- paste("Cumulative Mild Conflicts per Patch -", scenario_name)
    palette <- "plasma"
  }
  
  # Ensure coordinates are numeric (fixes plotting error)
  conflict_summary <- conflict_summary %>%
    mutate(
      pxcor = as.numeric(pxcor),
      pycor = as.numeric(pycor)
    )
  
  # Now plot
    ggplot(conflict_summary, aes(x = pxcor, y = pycor, fill = mean_total)) +
    geom_tile() +
      scale_fill_gradient(
        low = "white", 
        high = "red", 
        limits = c(0, 2500),
        breaks = seq(0, 2500, by = 250),
        na.value = "white",
        oob = scales::squish
      ) +
    scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
    labs(title = plot_title, fill = fill_lab) +
    theme_minimal() +
    coord_equal()
}
plot_conflict_heatmap(HighPatch_clean, "High Density", "all")
plot_conflict_heatmap(LowPatch_clean, "Low Density", "all")
plot_conflict_heatmap(X2030Patch_clean, "2030", "all")

# ==============================
# 4. Flow heatmap 
# ==============================

plot_flow_heatmap <- function(patchdata_clean, scenario_name = "") {
  flow_summary <- patchdata_clean %>%
    group_by(pxcor, pycor, tick) %>%
    summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")
  
  ggplot(flow_summary, aes(x = pxcor, y = pycor, fill = mean_flow)) +
    geom_tile() +
    geom_tile() +
    scale_fill_viridis_c(
      option = "inferno",
      direction = -1,
      na.value = "white",
      limits = c(0, 2500),
      breaks = seq(0, 2500, by = 250),
      oob = scales::squish
    ) +
    scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
    labs(title = paste("Cumulative Flow per Patch", scenario_name), fill = "Cumulative Flow") +
    theme_minimal() +
    coord_equal()
}

plot_flow_heatmap (HighPatch_clean, "High Density")
plot_flow_heatmap (LowPatch_clean, "Low Density")
plot_flow_heatmap (X2030Patch_clean, "2030")


animate_flow_heatmap <- function(patchdata_clean, scenario_name = "") {
  flow_summary <- patchdata_clean %>%
    group_by(pxcor, pycor, tick) %>%
    summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")
  
  p_flow <- ggplot(flow_summary, aes(x = pxcor, y = pycor, fill = mean_flow)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      na.value = "white",
      limits = c(0, 75),
      breaks = seq(0, 75, by = 25),
      oob = scales::squish
    ) +
    scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
    labs(title = paste("Flow per Patch Tick: {frame_time}", scenario_name), fill = "Flow") +
    theme_minimal() +
    coord_equal() +
    transition_time(tick)
  
  animate(p_flow, fps = 5, width = 700, height = 600)
}

animate_flow_heatmap(X2030Patch_clean, "2030")
# ==============================
# 5. Waiting heatmap (aggregate)
# ==============================
plot_waiting_heatmap <- function(patchdata_clean, scenario_name = "") {
  patch_wait_totals <- patchdata_clean %>%
    group_by(run, pxcor, pycor) %>%
    summarize(total_wait = sum(`break-count`, na.rm = TRUE), .groups = "drop")
  
  waiting_summary <- patch_wait_totals %>%
    group_by(pxcor, pycor) %>%
    summarize(mean_total_wait = mean(total_wait, na.rm = TRUE), .groups = "drop")
  
  ggplot(waiting_summary, aes(x = pxcor, y = pycor, fill = mean_total_wait)) +
    geom_tile() +
    scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      na.value = "white",
      limits = c(0, 60),
      breaks = seq(0, 60, by = 20),
      oob = scales::squish
    ) +
    scale_x_continuous(limits = c(-30, 30), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
    labs(title = paste("Cumulative Waiting per Patch (Mean Across Simulations)", scenario_name), fill = "Mean Total Wait") +
    theme_minimal() +
    coord_equal()
}

plot_waiting_heatmap(LowPatch_clean, "Low")
plot_waiting_heatmap(HighPatch_clean, "High")
plot_waiting_heatmap(X2030Patch_clean, "2030")

# ==============================
# 5. Flow and Conflict Aggregation
# ==============================
summarize_patch_waiting <- function(patchdata_clean) {
  patch_wait_totals <- patchdata_clean %>%
    group_by(run, pxcor, pycor) %>%
    summarize(total_wait = sum(`break-count`, na.rm = TRUE), .groups = "drop")
  
  waiting_summary <- patch_wait_totals %>%
    group_by(pxcor, pycor) %>%
    summarize(mean_waiting = mean(total_wait, na.rm = TRUE), .groups = "drop")
  return(waiting_summary)
}

# 2. Overlay Plot Function (now top-level)
plot_flow_conflict_overlay <- function(flow_summary, conflict_summary, waiting_summary, scenario_name = "") {
  conflict_threshold <- quantile(conflict_summary$mean_conflict, 0.9, na.rm = TRUE)
  flow_threshold <- quantile(flow_summary$mean_flow, 0.9, na.rm = TRUE)
  waiting_threshold <- quantile(waiting_summary$mean_waiting, 0.9, na.rm = TRUE)
  
  overlay_data <- conflict_summary %>%
    inner_join(flow_summary, by = c("pxcor", "pycor")) %>%
    inner_join(waiting_summary, by = c("pxcor", "pycor")) %>%
    mutate(
      high_conflict = mean_conflict >= conflict_threshold,
      high_flow = mean_flow >= flow_threshold,
      high_waiting = mean_waiting >= waiting_threshold,
      both = high_conflict & high_flow
    )
  
  ggplot(overlay_data, aes(x = pxcor, y = pycor)) +
    geom_tile(aes(fill = mean_flow)) +
    geom_tile(
      data = subset(overlay_data, high_waiting),
      aes(x = pxcor, y = pycor),
      fill = scales::alpha("seagreen3", 0.35),  # mellow transparent green
      color = NA
    ) +
    geom_tile(
      data = subset(overlay_data, high_conflict),
      fill = NA, color = "red", size = 0.8
    ) +
    geom_tile(
      data = subset(overlay_data, both),
      aes(x = pxcor, y = pycor),
      fill = scales::alpha("red2", 0.85),  # mellow transparent green
      color = NA
    ) +
    scale_fill_viridis_c(option = "plasma", direction = -1, na.value = "white") +
    scale_y_continuous(limits = c(-10, 10), expand = c(0, 0)) +
    labs(
      title = paste("High Conflict, Flow, and Waiting Areas -", scenario_name),
      fill = "Mean Flow"
    ) +
    theme_minimal() +
    coord_equal()
}

# 3. Summarize all inputs for each scenario
flow_high <- HighPatch_clean %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")
conflict_high <- HighPatch_clean %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_conflict = mean(severe + moderate + mild, na.rm=TRUE), .groups = "drop")
waiting_high <- summarize_patch_waiting(HighPatch_clean)

flow_low <- LowPatch_clean %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")
conflict_low <- LowPatch_clean %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_conflict = mean(severe + moderate + mild, na.rm=TRUE), .groups = "drop")
waiting_low <- summarize_patch_waiting(LowPatch_clean)

flow_x2030 <- X2030Patch_clean %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_flow = mean(flow, na.rm=TRUE), .groups = "drop")
conflict_x2030 <- X2030Patch_clean %>%
  group_by(pxcor, pycor) %>%
  summarize(mean_conflict = mean(severe + moderate + mild, na.rm=TRUE), .groups = "drop")
waiting_x2030 <- summarize_patch_waiting(X2030Patch_clean)

# 4. Now plot (these will work!)
plot_flow_conflict_overlay(flow_high, conflict_high, waiting_high, "High Density")
plot_flow_conflict_overlay(flow_low, conflict_low, waiting_low, "Low Density")
plot_flow_conflict_overlay(flow_x2030, conflict_x2030, waiting_x2030, "2030")
# ==============================
# 6. Summary Data
# ==============================

library(dplyr)
library(flextable)

# Patch-level summary function
summarize_patch_metrics <- function(df, scenario_label, metrics = c("all_conflicts", "break-count")) {
  summary_list <- lapply(metrics, function(var) {
    by_patch <- df %>%
      group_by(pxcor, pycor) %>%
      summarize(val = sum(.data[[var]], na.rm = TRUE), .groups = "drop")
    x <- by_patch$val
    data.frame(
      Scenario = scenario_label,
      Variable = var,
      Mean = round(mean(x, na.rm = TRUE), 1),
      Median = round(median(x, na.rm = TRUE), 1),
      SD = round(sd(x, na.rm = TRUE), 1),
      Variance = round(var(x, na.rm = TRUE), 1),
      Min = round(min(x, na.rm = TRUE), 1),
      Max = round(max(x, na.rm = TRUE), 1),
      Range = round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 1),
      Proportion_Nonzero = round(mean(x > 0, na.rm = TRUE), 1) # 1 decimal here
    )
  })
  do.call(rbind, summary_list)
}

# Global speed summary function
summarize_global_speeds <- function(df, scenario_label) {
  vars <- c("mean.speed.ped", "mean.speed.bike")
  lapply(vars, function(var) {
    x <- df[[var]]
    data.frame(
      Scenario = scenario_label,
      Variable = var,
      Mean = round(mean(x, na.rm = TRUE), 1),
      Median = round(median(x, na.rm = TRUE), 1),
      SD = round(sd(x, na.rm = TRUE), 1),
      Variance = round(var(x, na.rm = TRUE), 1),
      Min = round(min(x, na.rm = TRUE), 1),
      Max = round(max(x, na.rm = TRUE), 1),
      Range = round(max(x, na.rm = TRUE) - min(x, na.rm = TRUE), 1),
      Proportion_Nonzero = round(mean(x > 0, na.rm = TRUE), 1) # 1 decimal here
    )
  }) %>% do.call(rbind, .)
}

# Summarize each scenario
summary_patch_high <- summarize_patch_metrics(HighPatch_clean, "High Density")
summary_patch_low  <- summarize_patch_metrics(LowPatch_clean, "Low Density")
summary_patch_2030 <- summarize_patch_metrics(X2030Patch_clean, "2030")

summary_speed_high <- summarize_global_speeds(HighGlobal, "High Density")
summary_speed_low  <- summarize_global_speeds(LowGlobal, "Low Density")
summary_speed_2030 <- summarize_global_speeds(X2030Global, "2030")

# Combine for tables
table_high_low <- rbind(summary_speed_high, summary_speed_low, summary_patch_high, summary_patch_low)
table_high_2030 <- rbind(summary_speed_high, summary_speed_2030, summary_patch_high, summary_patch_2030)

# Define scenario order
scenario_order <- c("High Density", "Low Density", "2030")

# Order tables by scenario and ensure 1 decimal everywhere
table_high_low <- table_high_low %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_order)
  ) %>%
  arrange(Scenario)

table_high_2030 <- table_high_2030 %>%
  mutate(
    Scenario = factor(Scenario, levels = scenario_order)
  ) %>%
  arrange(Scenario)

# Output as APA-style tables
ft_high_low <- flextable(table_high_low)
ft_high_2030 <- flextable(table_high_2030)

print(ft_high_low)
print(ft_high_2030)

# ==============================
# 7. Compare conflict counts
# ==============================

# Calculate total conflicts for Low Density
total_conflicts_low <- LowPatch_clean %>%
  summarise(total_conflicts = sum(severe + moderate + mild, na.rm = TRUE)) %>%
  pull(total_conflicts)

# Calculate total conflicts for High Density
total_conflicts_high <- HighPatch_clean %>%
  summarise(total_conflicts = sum(severe + moderate + mild, na.rm = TRUE)) %>%
  pull(total_conflicts)

# Calculate total conflicts for 2030
total_conflicts_2030 <- X2030Patch_clean %>%
  summarise(total_conflicts = sum(severe + moderate + mild, na.rm = TRUE)) %>%
  pull(total_conflicts)

# Print out the results
cat("Total Conflicts (Low Density):", total_conflicts_low, "\n")
cat("Total Conflicts (High Density):", total_conflicts_high, "\n")
cat("Total Conflicts (2030):", total_conflicts_2030, "\n")

comparison_table <- data.frame(
  Scenario = c("Low Density", "High Density", "2030"),
  Total_Conflicts = c(total_conflicts_low, total_conflicts_high, total_conflicts_2030),
  Total_Agents = c(1400, 4600, 7820)
)

# Calculate conflict rates
comparison_table$Conflicts_per_Agent <- comparison_table$Total_Conflicts / comparison_table$Total_Agents
comparison_table$Conflicts_per_100_Agents <- (comparison_table$Total_Conflicts / comparison_table$Total_Agents) * 100

print(comparison_table)


# For Low Density
total_flow_low <- sum(LowPatch_clean$flow, na.rm = TRUE)
n_patches_low <- n_distinct(LowPatch_clean$pxcor, LowPatch_clean$pycor)
flow_per_patch_low <- total_flow_low / 3600

# For High Density
total_flow_high <- sum(HighPatch_clean$flow, na.rm = TRUE)
n_patches_high <- n_distinct(HighPatch_clean$pxcor, HighPatch_clean$pycor)
flow_per_patch_high <- total_flow_high / 3600

# For 2030 Scenario
total_flow_2030 <- sum(X2030Patch_clean$flow, na.rm = TRUE)
n_patches_2030 <- n_distinct(X2030Patch_clean$pxcor, X2030Patch_clean$pycor)
flow_per_patch_2030 <- total_flow_2030 / 3600

flow_patch_table <- data.frame(
  Scenario = c("Low Density", "High Density", "2030"),
  Total_Flow = c(total_flow_low, total_flow_high, total_flow_2030),
  Flow_per_Patch = c(flow_per_patch_low, flow_per_patch_high, flow_per_patch_2030)
)

print(flow_patch_table)
