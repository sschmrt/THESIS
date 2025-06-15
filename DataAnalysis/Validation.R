# ---- LOAD LIBRARIES ----
library(readr)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rlang)
library(flextable)
library(officer)
library(patchwork)

# ---- SET WORKING DIRECTORY ----
setwd("C:/Users/marta/Desktop/THESIS/Outputs")

# ---- DEFINE PARAMETERS ----
outputs <- c("mean_speed_ped", "mean_speed_bike", "total_severe", "total_moderate", "total_mild")
ofat_vars <- c("v0-bike", "V0-ped", "Tr-ped", "Tr-bike", "A-bik", "A-ped", "D")
csv_files <- paste0(ofat_vars, ".csv")

col_names <- c(
  "run", "mean_speed_ped", "mean_speed_bike", "stddev_speed_ped", "stddev_speed_bike",
  "Nb_Bikes", "A-bik", "bik_S", "ped_goer", "Tr-bike", "bik_E", "V0-ped", "D", "ped_S",
  "A-ped", "ped_E", "bik_N", "bike_goer", "Ferry", "ped_N", "bike_comer", "bik_W", "Tr-ped",
  "ped_comer", "ped_W", "Nb_peds", "v0-bike", "total_severe", "total_moderate", "total_mild"
)

realistic_values <- list(
  "v0-bike" = c(3.0, 4.5, 5.0, 6.0),
  "V0-ped" = c(1.0, 1.3, 1.8),
  "Tr-ped" = c(0.3, 0.5, 1.0, 1.5),
  "Tr-bike" = c(0.3, 0.5, 1.0, 1.5),
  "A-bik" = c(3.0, 3.5, 4.0, 4.5, 5.0),
  "A-ped" = c(4.5, 4.875, 5.25, 5.625, 6.0),
  "D" = c(0.5, 1.0, 1.5, 2.0)
)

# ---- FUNCTION TO ANALYZE EACH PARAMETER CSV AND OUTPUT SUMMARY TABLE ----
analyze_ofat_csv_CI <- function(csv, var) {
  dat <- read_csv(csv, col_names = col_names, show_col_types = FALSE)
  
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
  
  # Visualization for each output
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

# ---- RUN THE ANALYSIS FOR EACH PARAMETER CSV ----
results_CI <- map2(csv_files, ofat_vars, analyze_ofat_csv_CI)
names(results_CI) <- ofat_vars

# ---- BUILD APA-STYLE SUMMARY TABLES FOR EACH PARAMETER ----
summary_files <- paste0("ofat_CI_summary_by_", ofat_vars, ".csv")

create_APA_table <- function(par, file, outputs, realistic_values) {
  tbl <- read_csv(file, show_col_types = FALSE)
  par_values <- tbl[[par]]
  
  # Compute CI width for each output
  for (output in outputs) {
    tbl[[paste0(output, "_CI_width")]] <- tbl[[paste0(output, "_mean_upper")]] - tbl[[paste0(output, "_mean_lower")]]
  }
  
  # Normalize CI width by output (divide by max for each output)
  norm_tbl <- tbl %>%
    mutate(across(ends_with("_CI_width"), ~ .x / max(.x, na.rm = TRUE), .names = "norm_{.col}"))
  
  norm_col_names <- paste0("norm_", outputs, "_CI_width")
  
  # Composite metric: mean of normalized CI widths
  norm_tbl$Composite_Metric <- round(rowMeans(norm_tbl[, norm_col_names], na.rm=TRUE), 3)
  
  # Mark realistic values
  norm_tbl$Realistic <- ifelse(par_values %in% realistic_values[[par]], "Yes", "No")
  
  # Highlight lowest composite(s)
  min_comp <- min(norm_tbl$Composite_Metric, na.rm=TRUE)
  norm_tbl$Lowest_Composite <- ifelse(norm_tbl$Composite_Metric == min_comp, "Yes", "No")
  
  # Prepare table for APA/Word
  pretty_tbl <- norm_tbl %>%
    mutate(Parameter_Value = .data[[par]]) %>%
    select(
      Parameter_Value,
      all_of(norm_col_names),
      Composite_Metric,
      Realistic,
      Lowest_Composite
    )
  
  # Round for display
  pretty_tbl <- pretty_tbl %>%
    mutate(across(starts_with("norm_"), ~ round(.x, 2)))
  
  # Rename columns for APA clarity
  colnames(pretty_tbl) <- gsub("norm_", "Norm. CI Width: ", colnames(pretty_tbl))
  colnames(pretty_tbl) <- gsub("_CI_width", "", colnames(pretty_tbl))
  colnames(pretty_tbl) <- gsub("Composite_Metric", "Composite Metric (Avg. Norm. CI Width)", colnames(pretty_tbl))
  colnames(pretty_tbl) <- gsub("Lowest_Composite", "Lowest Composite", colnames(pretty_tbl))
  
  
  # Create flextable and APA styling
  ft <- flextable(pretty_tbl)
  ft <- set_header_labels(ft,
                          Parameter_Value = "Parameter Value",
                          Realistic = "Realistic",
                          Lowest_Composite = "Lowest Composite"
  )
  ft <- autofit(ft)
  ft <- fontsize(ft, size = 10, part = "all")
  ft <- font(ft, part = "header", fontname = "Times New Roman")
  ft <- font(ft, part = "body", fontname = "Times New Roman")
  # Bold the lowest composite metric(s)
  ft <- bold(
    ft, 
    j = "Composite Metric (Avg. Norm. CI Width)", 
    i = which(pretty_tbl$`Lowest Composite` == "Yes"), 
    bold = TRUE
  )
  # Shading for realistic values
  ft <- bg(ft, i = which(pretty_tbl$Realistic == "Yes"), bg = "#e6f2ff")
  
  # Save as Word file for each parameter
  fname <- paste0("APA_Table_", par, ".docx")
  save_as_docx(ft, path = fname)
  print(paste("Saved APA-style Word table for", par, "as", fname))
}

for (i in seq_along(ofat_vars)) {
  create_APA_table(
    par = ofat_vars[i],
    file = summary_files[i],
    outputs = outputs,
    realistic_values = realistic_values
  )
}

# ---- LOAD REQUIRED LIBRARIES ----
library(ggplot2)
library(readr)
library(dplyr)
library(patchwork) # Or use cowplot::plot_grid

# ---- FUNCTION TO CREATE APA STYLE PLOT ----
create_ofat_plot <- function(summary_file, par, ylab, xlab, title) {
  df <- read_csv(summary_file, show_col_types = FALSE)
  
  ggplot(df, aes_string(x = par, y = "total_mild_mean")) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = total_mild_mean_lower, ymax = total_mild_mean_upper), width = 0.2) +
    theme_minimal(base_size = 14, base_family = "Times") +
    labs(
      title = title,
      x = xlab,
      y = ylab
    ) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5),
      axis.title = element_text(face = "bold"),
      axis.text = element_text(face = "plain")
    )
}

# ---- FILE NAMES ----
summary_files <- paste0("ofat_CI_summary_by_", c("D", "V0-ped", "v0-bike"), ".csv")
ofat_vars <- c("D", "V0-ped", "v0-bike")

# ---- CREATE PLOTS ----
plot1 <- create_ofat_plot(summary_files[1], "D", "Mean Mild Conflicts", "Characteristic Distance (D)", "A) D")
plot2 <- create_ofat_plot(summary_files[2], "V0-ped", "Mean Mild Conflicts", "Desired Velocity Pedestrians (V0-ped)", "B) V0-ped")
plot3 <- create_ofat_plot(summary_files[3], "v0-bike", "Mean Mild Conflicts", "Desired Velocity Bikes (v0-bike)", "C) v0-bike")

# ---- COMBINE PLOTS SIDE BY SIDE ----
combined <- plot1 + plot2 + plot3 + plot_layout(ncol = 3, guides = 'collect')
print(combined)

# ---- SAVE TO FILE ----
ggsave("OFAT_Mild_Conflicts_ThreePanel.png", combined, width = 15, height = 6, dpi = 300)
