# =====================================================
# Data Loader Module
# =====================================================

library(readr)
library(tools)

read_all_datasets <- function(folder_path = "Datasets_cvs",
                              show_summary = TRUE) {
  
  if (!dir.exists(folder_path)) {
    stop("Folder does not exist.")
  }
  
  files <- list.files(path = folder_path,
                      pattern = "\\.csv$",
                      full.names = TRUE)
  
  if (length(files) == 0) {
    stop("No CSV files found in the folder.")
  }
  
  datasets <- lapply(files, function(file) {
    read_csv(file,
             show_col_types = FALSE,
             na = c("", "NA", "?", "null", "Null"))
  })
  
  names(datasets) <- file_path_sans_ext(basename(files))
  
  if (show_summary) {
    cat("Datasets successfully loaded:\n\n")
    
    for (name in names(datasets)) {
      df <- datasets[[name]]
      cat("Dataset:", name, "\n")
      cat("Rows:", nrow(df), "\n")
      cat("Columns:", ncol(df), "\n")
      cat("Total Missing:", sum(is.na(df)), "\n")
      cat("-----------------------------------\n")
    }
  }
  
  return(datasets)
}
