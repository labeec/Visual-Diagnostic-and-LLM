library(readr)
library(tools)
library(tidyverse)
library(ggplot2)
library(naniar)
library(VIM)
library(dplyr)
library(tidyr)
library(patchwork)


setwd("~/DataspellProjects/normality")

source("data_loader.R")
all_data <- read_all_datasets("datasets")

#Missing value analysis
source("missing_value.R")
missing_results <- analyze_missing_values(all_data)

missing_overview   <- missing_results$missing_overview
datasets_with_na   <- missing_results$datasets_with_na
datasets_missing   <- missing_results$datasets_missing
missing_detail     <- missing_results$missing_detail

print(missing_overview)
print(datasets_with_na)
print(missing_detail)

#Visualization
source("missing_visualize.R")
plot_missing_grid(datasets_missing)

#Continuous Feature Extraction
source("extract_continuous.R")
target_map <- c(
  diabetes = "Diabetes_binary",
  hepatitis = "class",
  orange_tele = "Churn",
  parkinson_disease = "subject#",
  phishing = "label"
)

continuous_data_10 <- extract_continuous_10(
  datasets = all_data,
  target_map = target_map,
  min_unique = 10
)

continuous_data_rule <- extract_continuous_rule(
  datasets = all_data,
  target_map = target_map
)
#Comparison Table
compare_retention <- function(count_data, rule_data) {
  
  comparison <- data.frame(
    Dataset = names(count_data),
    Count_Method = sapply(count_data, ncol),
    Rule_Method = sapply(rule_data, ncol)
  )
  
  comparison$Difference <- comparison$Count_Method - comparison$Rule_Method
  
  return(comparison)
}

retention_summary <- compare_retention(
  continuous_data_10,
  continuous_data_rule
)

print(retention_summary)

write.csv(
  retention_summary,
  file = "Results/retention_summary.csv",
  row.names = FALSE
)

#Remove datasets with zero columns retained
zero_datasets <- names(continuous_data_rule)[
  sapply(continuous_data_rule, ncol) == 0
]
continuous_data_clean <- continuous_data_rule[
  sapply(continuous_data_rule, ncol) > 0
]

datasets_with_missing_continuous <- names(continuous_data_clean)[
  sapply(continuous_data_clean, function(df) any(is.na(df)))
]
print(datasets_with_missing_continuous)


#Imputation
source("imputation_module.R")
imputed_data_clean <- impute_all_datasets(continuous_data_clean)
sapply(imputed_data_clean, function(df) sum(is.na(df)))

setdiff(names(continuous_data_clean), names(imputed_data_clean))
setdiff(names(imputed_data_clean), names(continuous_data_clean))

column_check <- lapply(names(continuous_data_clean), function(name) {
  identical(
    colnames(continuous_data_clean[[name]]),
    colnames(imputed_data_clean[[name]])
  )
})

names(column_check) <- names(continuous_data_clean)
print(column_check)

sapply(continuous_data_clean, function(df) sum(is.na(df)))
sapply(imputed_data_clean, function(df) sum(is.na(df)))

#Normality testing engine
source("testing_methods.R")
# Run Raw
normality_raw <- run_normality_engine(
  continuous_data_clean,
  data_type = "raw"
)

# Run Imputed
normality_imputed <- run_normality_engine(
  imputed_data_clean,
  data_type = "imputed"
)

# Compare
normality_comparison <- compare_normality_results(
  normality_raw,
  normality_imputed
)

# Save Outputs
if (!dir.exists("Results")) dir.create("Results")

write.csv(normality_raw,
          "Results/normality_raw_results.csv",
          row.names = FALSE)

write.csv(normality_imputed,
          "Results/normality_imputed_results.csv",
          row.names = FALSE)

write.csv(normality_comparison,
          "Results/normality_comparison_results.csv",
          row.names = FALSE)


#Advanced Comparison
source("advanced_comparison.R")
comparison_table <- compare_distributions_advanced(
  continuous_data_clean,
  imputed_data_clean,
  normality_raw,
  normality_imputed
)

write.csv(comparison_table,
          "Results/advanced_distribution_comparison.csv",
          row.names = FALSE)

heatmap_plot <- plot_decision_heatmap(
  normality_raw,
  normality_imputed
)


#Imputation
source("imputation_module.R")
imputed_data <- impute_all_datasets(all_data)
sapply(imputed_data, function(df) sum(is.na(df)))

groupwise_results <- groupwise_distribution_analysis(
  continuous_data_clean,
  imputed_data_clean,
  target_map
)

write.csv(groupwise_results,
          "Results/groupwise_analysis.csv",
          row.names = FALSE)

#Separate comparison
source("separate_comparison.R")

#Shape Table for skew and Kurtosis
source("shape_sk_ku.R")
shape_table <- compute_shape_statistics(continuous_data_clean)

write.csv(shape_table,
          "Results/shape_statistics_raw.csv",
          row.names = FALSE)


#Normality Comparison
source("normality_comparison.R")
comparative_results <- run_normality_comparative_analysis(
  normality_raw = normality_raw,
  normality_imputed = normality_imputed,
  shape_data = shape_table   # optional if you have skew/kurtosis table
)


#Robustness Framework
source("robustness.R")

results_path <- "Results"

# Compute NTRI
NTRI_results <- compute_NTRI(results_path)
print(NTRI_results)

# Run ensemble and ROC
roc_results <- run_ROC_analysis(results_path,
                                alpha = 0.05,
                                threshold = 0.5)

print(paste("AUC:", roc_results$auc))
print("Ensemble Weights:")
print(roc_results$weights)

#Radar Plot Comparison
source("radar_plot.R")
radar_data <- prepare_radar_data(
  rejection_rate = comparative_results$rejection_rate,
  kappa_matrix = as.matrix(comparative_results$kappa_matrix),
  runtime_summary = comparative_results$runtime_summary,
  stability = comparative_results$stability,
  shape_sensitivity = comparative_results$shape_sensitivity
)

plot_normality_radar(radar_data)

#Visualization Methods
source("visualize_methods.R")
severity_table <- run_full_visual_report(
  normality_raw = normality_raw,
  continuous_data_clean = continuous_data_clean,
  imputed_data_clean = imputed_data_clean
)

write.csv(severity_table,
          "Results/disagreement_severity_ranking.csv",
          row.names = FALSE)
