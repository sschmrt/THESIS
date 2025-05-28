library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rlang)

setwd("C:/Users/marta/Desktop/THESIS/Outputs")

outputs <- c("mean_speed_ped", "mean_speed_bike", "total_severe", "total_moderate", "total_mild")
ofat_vars <- c("v0-bike", "V0-ped", "Tr-ped", "Tr-bike", "A-bik", "A-ped", "D")
csv_files <- paste0(ofat_vars, ".csv")

col_names <- c(
  "run", "mean_speed_ped", "mean_speed_bike", "stddev_speed_ped", "stddev_speed_bike",
  "Nb_Bikes", "A-bik", "bik_S", "ped_goer", "Tr-bike", "bik_E", "V0-ped", "D", "ped_S",
  "A-ped", "ped_E", "bik_N", "bike_goer", "Ferry", "ped_N", "bike_comer", "bik_W", "Tr-ped",
  "ped_comer", "ped_W", "Nb_peds", "v0-bike", "total_severe", "total_moderate", "total_mild"
)

analyze_ofat_csv_CI <- function(csv, var) {
  dat <- read_csv(csv, col_names = col_names, show_col_types = FALSE)
  
  outputs <- c("mean_speed_ped", "mean_speed_bike", "total_severe", "total_moderate", "total_mild")
  summary_tbl <- dat %>%
    group_by(.data[[var]]) %>%
    summarise(across(
      all_of(outputs),
      list(
        mean = ~mean(.x, na.rm=TRUE),
        sd = ~sd(.x, na.rm=TRUE),
        n = ~sum(!is.na(.x))
      ),
      .names = "{.col}_{.fn}"
    )) %>%
    ungroup()
  
  for (output in outputs) {
    mean_col <- paste0(output, "_mean")
    sd_col <- paste0(output, "_sd")
    n_col <- paste0(output, "_n")
    lower_col <- paste0(output, "_mean_lower")
    upper_col <- paste0(output, "_mean_upper")
    summary_tbl[[lower_col]] <- summary_tbl[[mean_col]] - 1.96 * summary_tbl[[sd_col]] / sqrt(summary_tbl[[n_col]])
    summary_tbl[[upper_col]] <- summary_tbl[[mean_col]] + 1.96 * summary_tbl[[sd_col]] / sqrt(summary_tbl[[n_col]])
  }
  
  write_csv(summary_tbl, paste0("ofat_CI_summary_by_", var, ".csv"))
  
  # Visualization
  for (output in outputs) {
    mean_col <- paste0(output, "_mean")
    lower_col <- paste0(output, "_mean_lower")
    upper_col <- paste0(output, "_mean_upper")
    plot_data <- summary_tbl %>%
      select(all_of(var), all_of(mean_col), all_of(lower_col), all_of(upper_col)) %>%
      rename(mean = all_of(mean_col), lower = all_of(lower_col), upper = all_of(upper_col))
    
    p <- ggplot(plot_data, aes(x = .data[[var]], y = mean)) +
      geom_point() +
      geom_errorbar(aes(ymin = lower, ymax = upper), width = 0.2) +
      labs(title = paste("Mean and 95% CI for", output, "by", var),
           x = var, y = output) +
      theme_minimal()
    ggsave(filename = paste0("CI_plot_", output, "_by_", var, ".png"), plot = p, width = 7, height = 7)
  }
  
  summary_tbl
}
# Process all CSVs and variables with new function
results_CI <- map2(csv_files, ofat_vars, analyze_ofat_csv_CI)
names(results_CI) <- ofat_vars

# Read all the previously created summary tables
summary_files <- paste0("ofat_CI_summary_by_", ofat_vars, ".csv")
summary_list <- map(summary_files, read_csv)
names(summary_list) <- ofat_vars

# Combine all parameter summaries into one long table
all_summaries <- map2_df(summary_list, ofat_vars, ~mutate(.x, parameter = .y, .before = 1))

# For each parameter and value, calculate CI width and CV for each output
for (output in outputs) {
  all_summaries <- all_summaries %>%
    mutate(
      !!paste0(output, "_CI_width") := !!sym(paste0(output, "_mean_upper")) - !!sym(paste0(output, "_mean_lower")),
      !!paste0(output, "_CV") := !!sym(paste0(output, "_sd")) / !!sym(paste0(output, "_mean"))
    )
}

# Reshape to long format for easier highlighting and viewing
long_summary <- pivot_longer(
  all_summaries,
  cols = matches("_mean$|_sd$|_CV$|_CI_width$"),
  names_to = c("output", ".value"),
  names_pattern = "^(.*)_(mean|sd|CV|CI_width)$"
)

# For each parameter/output, flag the value with lowest SD, CI width, CV
long_summary <- long_summary %>%
  group_by(parameter, output) %>%
  mutate(
    lowest_sd = sd == min(sd, na.rm=TRUE),
    lowest_CI_width = CI_width == min(CI_width, na.rm=TRUE),
    lowest_CV = CV == min(CV, na.rm=TRUE)
  ) %>%
  ungroup()

# Optional: Save the table to CSV
write_csv(long_summary, "Parameter_Stability_Summary.csv")

# Example: Print out the most stable values for each parameter/output
most_stable <- long_summary %>%
  filter(lowest_CV | lowest_sd | lowest_CI_width)

print(most_stable)
