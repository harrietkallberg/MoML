library(kernlab)
source("exercise_1.R")
source("exercise_2.R")

#-------------------------------FUNCTIONS---------------------------------------

# Helper function to plot and save KPCA results
# save_path: Full path to file
# title: Title for the plot
plot_kpca <- function(pc_matrix, plot_colors, save_path, title) {
  # Define genre levels for the legend
  genre_levels <- levels(plot_colors)
  # Map levels to specific colors to match Exercise 3 (Blue/Red)
  # Assuming Classical = Blue, Rap = Red
  palette <- c("blue", "red")
  
  png(save_path)
  plot(pc_matrix[, 1], pc_matrix[, 2], 
       col = palette[plot_colors], 
       pch = 19, 
       xlab = "First Principal Component", 
       ylab = "Second Principal Component", 
       main = title)
  
  # Add the legend similar to Exercise 3
  legend("topright", 
         legend = genre_levels, 
         col = palette, 
         pch = 19, 
         title = "Music Genre")
  dev.off()
  
  # Print relative path message
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------

main <- function() {
  
  music_genre_path <- file.path(getwd(), "archive", "music_genre.csv")
  data_cr <- load_data(music_genre_path)
  data_split <- split_data(data_cr)
  train_data <- data_split$train_data
  
  # Setup Plot Directory
  plots_dir <- file.path(getwd(), "ex_4_plots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir)
  
  # Standardize numerical features
  compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness", 
                       "liveness", "loudness", "speechiness", "valence")
  data_numeric <- train_data[, compare_columns]
  data_standardized_df <- as.data.frame(scale(data_numeric))
  
  # 3. Define all full paths in one place
  paths <- list(
    linear = file.path(plots_dir, "kpca_linear_plot.png"),
    poly   = file.path(plots_dir, "kpca_poly_plot.png"),
    rbf    = file.path(plots_dir, "kpca_rbf_plot.png")
  )
  
  # --- KPCA Calculations ---
  
  message("Running Linear KPCA...")
  kpca_linear <- kpca(~., data = data_standardized_df, kernel = "vanilladot", kpar = list())
  pc_linear <- predict(kpca_linear, data_standardized_df)
  
  message("Running Polynomial KPCA...")
  kpca_poly <- kpca(~., data = data_standardized_df, kernel = "polydot", kpar = list(degree = 3))
  pc_poly <- predict(kpca_poly, data_standardized_df)
  
  message("Running RBF KPCA...")
  kpca_rbf <- kpca(~., data = data_standardized_df, kernel = "rbfdot", kpar = list(sigma = 0.1))
  pc_rbf <- predict(kpca_rbf, data_standardized_df)
  
  # --- Plotting ---
  message("\n--- Generating KPCA Plots ---")
  
  # Prepare colors (Factorize genre to ensure consistent indexing)
  plot_colors <- factor(train_data$music_genre, levels = c("Classical", "Rap"))
  
  plot_kpca(pc_linear, plot_colors, paths$linear, "Kernel PCA - Linear")
  plot_kpca(pc_poly,   plot_colors, paths$poly,   "Kernel PCA - Polynomial (deg 3)")
  plot_kpca(pc_rbf,    plot_colors, paths$rbf,    "Kernel PCA - RBF (sigma 0.1)")
  
  message("\nExecution Complete.")
}

# Run only if executed directly
if (sys.nframe() == 0) {
  main()
}