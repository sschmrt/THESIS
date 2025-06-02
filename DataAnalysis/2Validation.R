# ---- LOAD LIBRARIES ----
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rlang)
library(flextable)
library(officer)

# ---- SET WORKING DIRECTORY ----
setwd("C:/Users/marta/Desktop/THESIS/Outputs")
col_names <- c(
  "run", "mean_speed_ped", "mean_speed_bike", "stddev_speed_ped", "stddev_speed_bike",
  "Nb_Bikes", "A-bik", "bik_S", "ped_goer", "Tr-bike", "bik_E", "V0-ped", "D", "ped_S",
  "A-ped", "ped_E", "bik_N", "bike_goer", "Ferry", "ped_N", "bike_comer", "bik_W", "Tr-ped",
  "ped_comer", "ped_W", "Nb_peds", "v0-bike", "total_severe", "total_moderate", "total_mild"
)
outputs <- c("mean_speed_ped", "mean_speed_bike", "total_severe", "total_moderate", "total_mild")

validation <- read_csv("C:/Users/marta/Desktop/THESIS/Outputs/FinalSimulation-global-run.csv", col_names = col_names, show_col_types = FALSE)


# ---- SUMMARY STATISTICS ----
results <- validation %>%
  summarise(
    n = n(),
    
    # Means
    mean_speed_bike_mean = mean(mean_speed_bike, na.rm = TRUE),
    mean_speed_ped_mean = mean(mean_speed_ped, na.rm = TRUE),
    total_severe_mean = mean(total_severe, na.rm = TRUE),
    total_moderate_mean = mean(total_moderate, na.rm = TRUE),
    total_mild_mean = mean(total_mild, na.rm = TRUE),
    
    # SDs
    mean_speed_bike_sd = sd(mean_speed_bike, na.rm = TRUE),
    mean_speed_ped_sd = sd(mean_speed_ped, na.rm = TRUE),
    total_severe_sd = sd(total_severe, na.rm = TRUE),
    total_moderate_sd = sd(total_moderate, na.rm = TRUE),
    total_mild_sd = sd(total_mild, na.rm = TRUE),
    
    # CVs
    mean_speed_bike_cv = mean_speed_bike_sd / mean_speed_bike_mean,
    mean_speed_ped_cv = mean_speed_ped_sd / mean_speed_ped_mean,
    total_severe_cv = total_severe_sd / total_severe_mean,
    total_moderate_cv = total_moderate_sd / total_moderate_mean,
    total_mild_cv = total_mild_sd / total_mild_mean,
    
    # Standard errors
    mean_speed_bike_se = mean_speed_bike_sd / sqrt(n()),
    mean_speed_ped_se = mean_speed_ped_sd / sqrt(n()),
    total_severe_se = total_severe_sd / sqrt(n()),
    total_moderate_se = total_moderate_sd / sqrt(n()),
    total_mild_se = total_mild_sd / sqrt(n())
  ) %>%
  # Bind CIs
  mutate(
    mean_speed_bike_ci_lower = mean_speed_bike_mean - qt(0.975, n - 1) * mean_speed_bike_se,
    mean_speed_bike_ci_upper = mean_speed_bike_mean + qt(0.975, n - 1) * mean_speed_bike_se,
    mean_speed_bike_ci_width = mean_speed_bike_ci_upper - mean_speed_bike_ci_lower,
    mean_speed_bike_ci_relwidth = mean_speed_bike_ci_width / mean_speed_bike_mean,
    
    mean_speed_ped_ci_lower = mean_speed_ped_mean - qt(0.975, n - 1) * mean_speed_ped_se,
    mean_speed_ped_ci_upper = mean_speed_ped_mean + qt(0.975, n - 1) * mean_speed_ped_se,
    mean_speed_ped_ci_width = mean_speed_ped_ci_upper - mean_speed_ped_ci_lower,
    mean_speed_ped_ci_relwidth = mean_speed_ped_ci_width / mean_speed_ped_mean,
    
    total_severe_ci_lower = total_severe_mean - qt(0.975, n - 1) * total_severe_se,
    total_severe_ci_upper = total_severe_mean + qt(0.975, n - 1) * total_severe_se,
    total_severe_ci_width = total_severe_ci_upper - total_severe_ci_lower,
    total_severe_ci_relwidth = total_severe_ci_width / total_severe_mean,
    
    total_moderate_ci_lower = total_moderate_mean - qt(0.975, n - 1) * total_moderate_se,
    total_moderate_ci_upper = total_moderate_mean + qt(0.975, n - 1) * total_moderate_se,
    total_moderate_ci_width = total_moderate_ci_upper - total_moderate_ci_lower,
    total_moderate_ci_relwidth = total_moderate_ci_width / total_moderate_mean,
    
    total_mild_ci_lower = total_mild_mean - qt(0.975, n - 1) * total_mild_se,
    total_mild_ci_upper = total_mild_mean + qt(0.975, n - 1) * total_mild_se,
    total_mild_ci_width = total_mild_ci_upper - total_mild_ci_lower,
    total_mild_ci_relwidth = total_mild_ci_width / total_mild_mean
  )

# Prepare long-format APA-like summary table
apa_results <- tibble(
  Output = c("Mean speed (bike)", "Mean speed (ped)", "Total severe conflicts", "Total moderate conflicts", "Total mild conflicts"),
  Mean = c(results$mean_speed_bike_mean, results$mean_speed_ped_mean, results$total_severe_mean, results$total_moderate_mean, results$total_mild_mean),
  SD = c(results$mean_speed_bike_sd, results$mean_speed_ped_sd, results$total_severe_sd, results$total_moderate_sd, results$total_mild_sd),
  CV = c(results$mean_speed_bike_cv, results$mean_speed_ped_cv, results$total_severe_cv, results$total_moderate_cv, results$total_mild_cv),
  CI = c(
    sprintf("[%.2f, %.2f]", results$mean_speed_bike_ci_lower, results$mean_speed_bike_ci_upper),
    sprintf("[%.2f, %.2f]", results$mean_speed_ped_ci_lower, results$mean_speed_ped_ci_upper),
    sprintf("[%.2f, %.2f]", results$total_severe_ci_lower, results$total_severe_ci_upper),
    sprintf("[%.2f, %.2f]", results$total_moderate_ci_lower, results$total_moderate_ci_upper),
    sprintf("[%.2f, %.2f]", results$total_mild_ci_lower, results$total_mild_ci_upper)
  ),
  CI_Width = c(results$mean_speed_bike_ci_width, results$mean_speed_ped_ci_width, results$total_severe_ci_width, results$total_moderate_ci_width, results$total_mild_ci_width),
  Rel_CI_Width = c(results$mean_speed_bike_ci_relwidth, results$mean_speed_ped_ci_relwidth, results$total_severe_ci_relwidth, results$total_moderate_ci_relwidth, results$total_mild_ci_relwidth)
)

# Format as APA style (rounded for reporting)
apa_results <- apa_results %>%
  mutate(
    Mean = round(Mean, 2),
    SD = round(SD, 2),
    CV = sprintf("%.2f", CV),
    CI_Width = round(CI_Width, 2),
    Rel_CI_Width = sprintf("%.1f%%", 100 * Rel_CI_Width)
  )

print(apa_results)

# ---- visualize ----
# Histogram of key outputs
hist(validation$mean_speed_bike, main="Mean Speed of Bikes", xlab="Mean Speed", col="lightblue")
hist(validation$total_severe, main="Total Severe Conflicts", xlab="Count", col="salmon")
hist(validation$total_moderate, main="Total Moderate Conflicts", xlab="Count", col="salmon")
hist(validation$total_mild, main="Total Mild Conflicts", xlab="Count", col="salmon")

# Boxplot for spread
boxplot(validation$mean_speed_bike, main="Boxplot of Mean Speed (Bikes)")
boxplot(validation$mean_speed_ped, main="Boxplot of Mean Speed (Pedestrians)")
boxplot(validation$total_severe, main="Boxplot of Total Severe Conflicts")
boxplot(validation$total_moderate, main="Boxplot of Total Moderate Conflicts")
boxplot(validation$total_mild, main="Boxplot of Total Mild Conflicts")
