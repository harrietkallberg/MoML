# Load necessary libraries
library(ggplot2)
library(scatterplot3d)  # For 3D plotting
library(rgl)

# Define a function for loading and cleaning the dataset
load_data <- function(file_path) {
  data_raw <- read.csv(file_path)
  data_raw$music_genre <- as.character(data_raw$music_genre)
  data_with_genre <- data_raw[data_raw$music_genre != "", ]
  cr_data <- data_with_genre[data_with_genre$music_genre %in% c("Classical", "Rap"), ]
  return(cr_data)
}

# Define a function for splitting data into training and testing sets
split_data <- function(data, train_fraction = 0.7, seed = 42) {
  set.seed(seed)
  train_size <- floor(train_fraction * nrow(data))
  train_indices <- sample(seq_len(nrow(data)), size = train_size)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  return(list(train_data = train_data, test_data = test_data))
}

# Define a function for performing PCA
perform_pca <- function(data, compare_columns) {
  data_numeric <- data[, compare_columns]
  data_standardized <- scale(data_numeric)
  pca_result <- prcomp(data_standardized, center = TRUE, scale. = TRUE)
  return(pca_result)
}

# Define a function to plot variance explained by PCA
plot_variance_explained <- function(explained_variance, plot_path) {
  plot(explained_variance, type = "b", xlab = "Principal Components", 
       ylab = "Proportion of Variance Explained", main = "Variance Explained by PCA Components")
  png(plot_path)
  plot(explained_variance, type = "b", xlab = "Principal Components", 
       ylab = "Proportion of Variance Explained", main = "Variance Explained by PCA Components")
  dev.off()
  cat("Variance explained plot saved as PNG in 'plots' folder.\n")
}

# Define a function to create and save 2D PCA scatter plot
plot_pca_2d <- function(pca_data, plot_path) {
  p1 <- ggplot(pca_data, aes(x = PC1, y = PC2, color = music_genre)) +
    geom_point() +
    labs(title = "PCA: First vs. Second Principal Component", 
         x = "First Principal Component", y = "Second Principal Component") +
    scale_color_manual(values = c("Classical" = "blue", "Rap" = "red"))
  ggsave(plot_path, plot = p1)
  cat("PCA scatter plot with colored genres saved as PDF in 'plots' folder.\n")
}

# Define a function for creating and saving 3D PCA plot
plot_pca_3d <- function(pca_data, plot_path) {
  # Ensure music_genre is a factor and assign levels correctly
  pca_data$music_genre <- factor(pca_data$music_genre, levels = c("Classical", "Rap"))
  
  genre_colors <- c("Classical" = "blue", "Rap" = "red")
  
  png(plot_path)
  # Create the 3D plot
  scatter3d <- scatterplot3d(pca_data$PC1, pca_data$PC2, pca_data$PC3,
                             color = genre_colors[as.character(pca_data$music_genre)], 
                             pch = 16, main = "3D PCA: First 3 Principal Components",
                             xlab = "First Principal Component", 
                             ylab = "Second Principal Component", 
                             zlab = "Third Principal Component")
  # Ensure the legend is created correctly
  legend("topright", legend = levels(pca_data$music_genre), 
         fill = genre_colors[levels(pca_data$music_genre)], pch = 16)
  dev.off()
  cat("3D PCA plot saved as PNG in 'plots' folder.\n")
}

# Define a function for creating interactive 3D PCA plot and saving as HTML
plot_interactive_pca_3d <- function(pca_data) {
  genre_colors <- c("Classical" = "blue", "Rap" = "red")
  open3d()
  plot3d(pca_data$PC1, pca_data$PC2, pca_data$PC3, 
         col = genre_colors[as.character(pca_data$music_genre)], 
         xlab = "First Principal Component", 
         ylab = "Second Principal Component", 
         zlab = "Third Principal Component", 
         main = "Interactive 3D PCA: First 3 Principal Components")
  Sys.sleep(360)  # Keep the plot open for 360 seconds
  rgl.postscript("3d_pca_plot.html", fmt = "html")
  cat("Interactive 3D plot saved as HTML\n")
}

# Main script execution

# Step 1: Load and clean data
dir <- getwd()
music_genre_path <- file.path(dir, "archive", "music_genre.csv")
cr_data <- load_data(music_genre_path)

# Step 2: Split data into training and testing sets
data_split <- split_data(cr_data)
train_data <- data_split$train_data
test_data <- data_split$test_data

# Print sizes of the datasets
cat("Total set size:", nrow(cr_data), "\n")
cat("Training set size:", nrow(train_data), "\n")
cat("Testing set size:", nrow(test_data), "\n")

# Step 3: Perform PCA
compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness", "liveness", 
                     "loudness", "speechiness", "valence")
pca_result <- perform_pca(cr_data, compare_columns)

# Get the variance explained by each principal component
explained_variance <- summary(pca_result)$importance[2, ]
plots_dir <- file.path(getwd(), "plots")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Step 4: Plot and save variance explained by PCA components
variance_plot_path <- file.path(plots_dir, "variance_explained_pca.png")
plot_variance_explained(explained_variance, variance_plot_path)

# Step 5: Plot and save the first two principal components in a scatter plot
pca_data <- data.frame(pca_result$x)
pca_data$music_genre <- cr_data$music_genre
scatter_plot_path <- file.path(plots_dir, "pca_first_two_components_colored.pdf")
plot_pca_2d(pca_data, scatter_plot_path)

# Step 6: Plot and save the first three principal components in 3D
pca_3d_plot_path <- file.path(plots_dir, "pca_first_three_components_3d.png")
plot_pca_3d(pca_data, pca_3d_plot_path)

# Step 7: Create and display an interactive 3D PCA plot
plot_interactive_pca_3d(pca_data)