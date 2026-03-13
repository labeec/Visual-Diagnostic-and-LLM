# =====================================================
# Final Normality Visualization Module
# =====================================================

library(dplyr)
library(ggplot2)
library(gridExtra)
library(patchwork)

# -----------------------------------------------------
# 1️⃣ Rank Disagreement Severity
# -----------------------------------------------------

rank_disagreement_severity <- function(normality_results) {
  
  normality_results %>%
    group_by(Dataset, Variable) %>%
    summarise(
      Disagreement_Count = n_distinct(Decision, na.rm = TRUE),
      Rejection_Count = sum(Decision == "Non_Normal", na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(Severity_Score = Disagreement_Count * Rejection_Count) %>%
    arrange(desc(Severity_Score))
}

# -----------------------------------------------------
# 2️⃣ Diagnostic Panel Generator (Raw vs Imputed)
# -----------------------------------------------------

create_distribution_plot <- function(x,
                                     dataset_name,
                                     variable_name,
                                     label_text) {
  
  df <- data.frame(value = x)
  
  p1 <- ggplot(df, aes(x = value)) +
    geom_histogram(aes(y = after_stat(density)),
                   bins = 30,
                   fill = "steelblue",
                   alpha = 0.6) +
    geom_density(color = "red", linewidth = 1) +
    stat_function(fun = dnorm,
                  args = list(mean = mean(x),
                              sd = sd(x)),
                  linetype = "dashed") +
    theme_minimal() +
    ggtitle(paste(label_text, "- Histogram + KDE")) +
    annotate("text",
             x = Inf, y = Inf,
             label = paste("n =", length(x)),
             hjust = 1.1, vjust = 2,
             size = 3)
  
  p2 <- ggplot(df, aes(sample = value)) +
    stat_qq() +
    stat_qq_line(color = "red") +
    theme_minimal() +
    ggtitle(paste(label_text, "- Q-Q Plot"))
  
  p3 <- ggplot(df, aes(y = value)) +
    geom_boxplot(fill = "orange", alpha = 0.6) +
    theme_minimal() +
    ggtitle(paste(label_text, "- Boxplot"))
  
  return((p1 | p2) / p3)
}

# -----------------------------------------------------
# 3️⃣ Overlay P-Values Extraction
# -----------------------------------------------------

extract_pvalues_text <- function(normality_results,
                                 dataset_name,
                                 variable_name) {
  
  subset_df <- normality_results %>%
    filter(Dataset == dataset_name,
           Variable == variable_name)
  
  paste(
    subset_df$Test,
    "p=",
    signif(subset_df$P_Value, 3),
    collapse = " | "
  )
}

# -----------------------------------------------------
# 4️⃣ Generate PDF Summary per Dataset
# -----------------------------------------------------

generate_dataset_pdf <- function(dataset_name,
                                 raw_data,
                                 imputed_data,
                                 normality_results,
                                 severity_table,
                                 output_dir = "Results/Final_Normality_Report") {
  
  if (!dir.exists(output_dir))
    dir.create(output_dir, recursive = TRUE)
  
  dataset_vars <- severity_table %>%
    filter(Dataset == dataset_name,
           Disagreement_Count > 1) %>%
    pull(Variable)
  
  if (length(dataset_vars) == 0) return(NULL)
  
  pdf(file = paste0(output_dir, "/", dataset_name, "_normality_summary.pdf"),
      width = 14,
      height = 10,
      onefile = TRUE)
  
  for (var in dataset_vars) {
    
    if (!var %in% colnames(raw_data[[dataset_name]])) next
    
    x_raw <- na.omit(raw_data[[dataset_name]][[var]])
    x_imp <- na.omit(imputed_data[[dataset_name]][[var]])
    
    if (length(x_raw) < 10) next
    
    pvalues_text <- extract_pvalues_text(
      normality_results,
      dataset_name,
      var
    )
    
    raw_plot <- create_distribution_plot(
      x_raw,
      dataset_name,
      var,
      "RAW"
    )
    
    imp_plot <- create_distribution_plot(
      x_imp,
      dataset_name,
      var,
      "IMPUTED"
    )
    
    combined <- (raw_plot | imp_plot) +
      plot_annotation(
        title = paste(dataset_name, "-", var),
        subtitle = pvalues_text
      )
    
    print(combined)
  }
  
  dev.off()
}
# -----------------------------------------------------
# 5️⃣ Master Execution Function
# -----------------------------------------------------
run_full_visual_report <- function(normality_raw,
                                   continuous_data_clean,
                                   imputed_data_clean) {
  
  severity_table <- rank_disagreement_severity(normality_raw)
  
  for (dataset_name in names(continuous_data_clean)) {
    
    generate_dataset_pdf(
      dataset_name = dataset_name,
      raw_data = continuous_data_clean,
      imputed_data = imputed_data_clean,
      normality_results = normality_raw,
      severity_table = severity_table
    )
  }
  
  return(severity_table)
}