# =====================================================
# Imputation Module
# Author: Lanre
# Description: Handles dataset-level imputation using mice
# =====================================================

library(mice)

impute_dataset <- function(df, m = 1, seed = 123, maxit = 5) {
  
  set.seed(seed)
  
  # Initialize to inspect variable types
  init <- mice(df, maxit = 0, print = FALSE)
  
  method <- init$method
  predictorMatrix <- init$predictorMatrix
  
  # Numeric variables → predictive mean matching
  numeric_vars <- names(df)[sapply(df, is.numeric)]
  method[numeric_vars] <- "pmm"
  
  # Factor variables → let mice decide automatically
  # (logreg for binary, polyreg for unordered, polr for ordered)
  
  imputed <- mice(df,
                  method = method,
                  predictorMatrix = predictorMatrix,
                  m = m,
                  maxit = maxit,
                  print = FALSE)
  
  completed_df <- complete(imputed, 1)
  
  return(completed_df)
}


# -----------------------------------------------------
# Batch Imputation Function
# -----------------------------------------------------

impute_all_datasets <- function(datasets_list,
                                seed = 123,
                                maxit = 5) {
  
  imputed_list <- list()
  
  for (name in names(datasets_list)) {
    
    df <- datasets_list[[name]]
    
    if (any(is.na(df))) {
      cat("Imputing dataset:", name, "\n")
      imputed_list[[name]] <- impute_dataset(df,
                                             seed = seed,
                                             maxit = maxit)
    } else {
      imputed_list[[name]] <- df
    }
  }
  
  return(imputed_list)
}
