# =====================================================
# Missing Value Visualization Module
# =====================================================

library(naniar)
library(ggplot2)
library(patchwork)
library(grid)

plot_missing_overview <- function(df, dataset_name) {
  
  p <- gg_miss_var(df) +
    ggtitle(dataset_name) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
      plot.title = element_text(size = 10, face = "bold"),
      panel.spacing = unit(1.5, "lines")
    )
  
  return(p)
}


plot_missing_grid <- function(datasets_missing,
                              ncol = 3,
                              output_path = "Results/missing_values_grid.png",
                              width = 18,
                              height = 24) {
  
  if (!dir.exists("Results")) {
    dir.create("Results")
  }
  
  plot_list <- lapply(names(datasets_missing), function(name) {
    plot_missing_overview(datasets_missing[[name]], name)
  })
  
  combined_plot <- wrap_plots(plotlist = plot_list, ncol = ncol)
  
  print(combined_plot)
  
  ggsave(
    filename = output_path,
    plot = combined_plot,
    width = width,
    height = height,
    dpi = 300
  )
}
