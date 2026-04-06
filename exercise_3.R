# Load necessary libraries
library(ggplot2)
library(scatterplot3d) 
library(rgl)

# Ensure these files contain the helper functions: load_data and split_data
source("exercise_1.R") 
source("exercise_2.R")

#-------------------------------FUNCTIONS---------------------------------------

# Function for performing PCA (Internal scaling is handled here)
perform_pca <- function(data) {
  return(prcomp(data, center = TRUE, scale. = TRUE))
}

# Function to plot variance explained
plot_variance_explained <- function(explained_variance, save_path) {
  png(save_path)
  plot(explained_variance, type = "b", xlab = "Principal Components", 
       ylab = "Proportion of Variance Explained", main = "Variance Explained by PCA Components")
  dev.off()
  
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

# Function for cumulative variance plot
plot_cumulative_variance_explained <- function(explained_variance, save_path) {
  cumulative_variance <- cumsum(explained_variance)
  png(save_path)
  plot(cumulative_variance, type = "b", xlab = "Principal Components", 
       ylab = "Cumulative Variance Explained", main = "Cumulative Variance Explained by PCA Components")
  dev.off()
  
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

# Function for 2D PCA scatter plot
plot_pca_2d <- function(pca_data, save_path) {
  p1 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = music_genre)) +
    geom_point() +
    labs(title = "PCA: First vs. Second Principal Component", 
         x = "First Principal Component", y = "Second Principal Component") +
    scale_color_manual(values = c("Classical" = "blue", "Rap" = "red")) +
    theme_minimal()
  
  ggsave(save_path, plot = p1)
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

# Function for 3D PCA plot
plot_pca_3d <- function(pca_data, save_path) {
  pca_data$music_genre <- factor(pca_data$music_genre, levels = c("Classical", "Rap"))
  genre_colors <- c("Classical" = "blue", "Rap" = "red")
  
  png(save_path)
  scatterplot3d(pca_data$PC1, pca_data$PC2, pca_data$PC3,
                color = genre_colors[as.character(pca_data$music_genre)], 
                pch = 16, main = "3D PCA: First 3 Principal Components",
                xlab = "PC1", ylab = "PC2", zlab = "PC3")
  
  legend("topright", legend = levels(pca_data$music_genre), 
         fill = genre_colors[levels(pca_data$music_genre)], pch = 16)
  dev.off()
  
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------

main <- function() {
  # 1. Setup Directories and Data
  music_genre_path <- file.path(getwd(), "archive", "music_genre.csv")
  plots_dir <- file.path(getwd(), "ex_3_plots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir)
  
  # Load data using your custom logic (Classical & Rap only)
  data_cr <- load_data(music_genre_path, filter_cr = TRUE)
  
  # Split data
  data_split <- split_data(data_cr)
  train_data <- data_split$train_data
  
  # 2. Perform PCA
  # We pass raw data; the function handles scaling internally
  compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness", 
                       "liveness", "loudness", "speechiness", "valence")
  data_numeric <- train_data[, compare_columns]
  pca_result <- perform_pca(data_numeric)
  
  # 3. Prepare data for plotting
  explained_variance <- summary(pca_result)$importance[2, ]
  pca_data <- data.frame(pca_result$x)
  pca_data$music_genre <- train_data$music_genre
  
  # 4. Define all full paths in one place
  paths <- list(
    variance   = file.path(plots_dir, "variance_explained_pca.png"),
    cumulative = file.path(plots_dir, "cumulative_variance_explained_pca.png"),
    scatter2d  = file.path(plots_dir, "pca_first_two_components_colored.pdf"),
    scatter3d  = file.path(plots_dir, "pca_first_three_components_3d.png"),
    weights    = file.path(plots_dir, "pca_feature_weights.csv")
  )
  
  # 5. Execute Plotting Functions
  message("\n--- Generating PCA Outputs ---")
  plot_variance_explained(explained_variance, paths$variance)
  plot_cumulative_variance_explained(explained_variance, paths$cumulative)
  plot_pca_2d(pca_data, paths$scatter2d)
  plot_pca_3d(pca_data, paths$scatter3d)
  
  # 6. Export PCA Weights (Rotation) to CSV
  write.csv(pca_result$rotation, paths$weights)
  cat(paste0("Saved: ", basename(plots_dir), "/pca_feature_weights.csv\n"))
  
  message("\nExecution Complete.")
}

# Standard R execution guard
if (sys.nframe() == 0) {
  main()
}