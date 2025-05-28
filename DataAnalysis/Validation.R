library(readr)
library(dplyr)
library(tidyr)
library(purrr)

setwd("C:/Users/marta/Desktop/THESIS/Outputs")

# Variables and files
ofat_vars <- c("v0-bike", "V0-ped", "Tr-ped", "Tr-bike", "A-bik", "A-ped", "D")
csv_files <- paste0(ofat_vars, ".csv")

# Specify column names
col_names <- c(
  "run", "mean_speed_ped", "mean_speed_bike", "stddev_speed_ped", "stddev_speed_bike",
  "Nb_Bikes", "A-bik", "bik_S", "ped_goer", "Tr-bike", "bik_E", "V0-ped", "D", "ped_S",
  "A-ped", "ped_E", "bik_N", "bike_goer", "Ferry", "ped_N", "bike_comer", "bik_W", "Tr-ped",
  "ped_comer", "ped_W", "Nb_peds", "v0-bike", "total_severe", "total_moderate", "total_mild"
)

# Read all CSVs into a named list of data frames
data_list <- map(csv_files, ~read_csv(.x, col_names = col_names))

# Name each data frame in the list by its variable
names(data_list) <- ofat_vars

# Set Up Analysis per Variable
analyze_ofat_csv <- function(csv, var) {
  dat <- read_csv(csv, col_names = col_names, show_col_types = FALSE)
  
  dat <- dat %>%
    mutate(
      CV_ped = stddev_speed_ped / mean_speed_ped,
      CV_bike = stddev_speed_bike / mean_speed_bike
    )
  
  summary_tbl <- dat %>%
    group_by(.data[[var]]) %>%
    summarise(
      n_runs = n(),
      mean_CV_ped = mean(CV_ped, na.rm = TRUE),
      mean_CV_bike = mean(CV_bike, na.rm = TRUE),
      cv_severe = sd(total_severe, na.rm = TRUE) / mean(total_severe, na.rm = TRUE),
      cv_moderate = sd(total_moderate, na.rm = TRUE) / mean(total_moderate, na.rm = TRUE),
      cv_mild = sd(total_mild, na.rm = TRUE) / mean(total_mild, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      total_CV = rowMeans(select(., mean_CV_ped, mean_CV_bike, cv_severe, cv_moderate, cv_mild), na.rm = TRUE)
    )
  
  best_row <- summary_tbl %>% filter(total_CV == min(total_CV, na.rm = TRUE))
  
# Write summary for this variable to CSV
  write_csv(summary_tbl, paste0("ofat_summary_by_", var, ".csv"))
  
  list(summary = summary_tbl, best = best_row)
}

# Process all CSVs and variables
results <- map2(csv_files, ofat_vars, analyze_ofat_csv)
names(results) <- ofat_vars

# Print best parameter value for each OFAT variable
for (v in ofat_vars) {
  cat("Best", v, "for lowest total CV is:\n")
  print(results[[v]]$best[[v]])
}
