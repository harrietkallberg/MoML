# Load necessary libraries
library(ggplot2)  # for visualisation

# Load helper functions from previous exercises
source("exercise_1.R")  # provides load_data()
source("exercise_2.R")  # provides split_data()

#-------------------------------FUNCTIONS---------------------------------------

# Saves a heatmap of classifier performance across all datasets and metrics.
# save_path: full path to output PNG
plot_heatmap <- function(results_df, save_path) {
  results_long <- reshape(results_df,
                          varying   = c("Accuracy", "Macro_Recall", "Macro_Precision", "Macro_F1"),
                          v.names   = "Value",
                          timevar   = "Metric",
                          times     = c("Accuracy", "Macro_Recall", "Macro_Precision", "Macro_F1"),
                          direction = "long"
  )
  results_long$Method <- paste(results_long$Classifier, "+", results_long$Dataset)
  
  p <- ggplot(results_long, aes(x = Metric, y = Method, fill = Value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(Value, 4)), size = 3) +
    scale_fill_gradient(low = "lightyellow", high = "steelblue",
                        limits = c(0.95, 1.00)) +
    labs(title = "Performance Heatmap: All Classifiers and Datasets",
         x = "Metric", y = "Classifier + Dataset", fill = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(save_path, plot = p, width = 10, height = 7)
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

# Saves a bar chart ranking all methods by Accuracy.
# save_path: full path to output PNG
plot_accuracy_ranking <- function(results_df, save_path) {
  results_df$Method <- paste(results_df$Classifier, "+", results_df$Dataset)
  results_df        <- results_df[order(results_df$Accuracy, decreasing = TRUE), ]
  results_df$Method <- factor(results_df$Method, levels = results_df$Method)
  
  p <- ggplot(results_df, aes(x = Method, y = Accuracy, fill = Classifier)) +
    geom_bar(stat = "identity") +
    geom_text(aes(label = round(Accuracy, 4)), vjust = -0.3, size = 3) +
    scale_fill_manual(values = c("kNN" = "steelblue",
                                 "Logistic Regression" = "tomato")) +
    scale_y_continuous(limits = c(0.94, 1.00)) +
    labs(title = "Accuracy Ranking: All Classifiers and Datasets",
         x = "Classifier + Dataset", y = "Accuracy") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(save_path, plot = p, width = 10, height = 6)
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------

main <- function() {
  # 1. Setup directories and paths
  plots_dir  <- file.path(getwd(), "ex_7_plots")
  tables_dir <- file.path(getwd(), "tables")
  if (!dir.exists(plots_dir)) dir.create(plots_dir)
  
  # 2. Load classification results from exercise 6
  results <- read.csv(file.path(tables_dir, "classification_results.csv"))
  
  # 3. Define all output paths in one place
  paths <- list(
    heatmap = file.path(plots_dir, "performance_heatmap.png"),
    ranking = file.path(plots_dir, "accuracy_ranking.png")
  )
  
  # 4. Generate comparison plots
  message("\n--- Generating Comparison Plots ---")
  plot_heatmap(results, paths$heatmap)
  plot_accuracy_ranking(results, paths$ranking)
  
  # 5. Print summary statistics to support report discussion
  message("\n--- Summary Statistics ---")
  cat("Best:  ", results$Classifier[which.max(results$Accuracy)], "+",
      results$Dataset[which.max(results$Accuracy)],
      "-> Accuracy:", round(max(results$Accuracy), 4), "\n")
  cat("Worst: ", results$Classifier[which.min(results$Accuracy)], "+",
      results$Dataset[which.min(results$Accuracy)],
      "-> Accuracy:", round(min(results$Accuracy), 4), "\n")
  cat("kNN  mean accuracy:", round(mean(results$Accuracy[results$Classifier == "kNN"]), 4), "\n")
  cat("LR   mean accuracy:", round(mean(results$Accuracy[results$Classifier == "Logistic Regression"]), 4), "\n")
  cat("Mean accuracy by dataset:\n")
  for (ds in unique(results$Dataset)) {
    cat(" ", ds, ":", round(mean(results$Accuracy[results$Dataset == ds]), 4), "\n")
  }
  
  message("\nExecution Complete.")
}

# Run only if executed directly
if (sys.nframe() == 0) {
  main()
}