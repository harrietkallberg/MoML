# Load necessary libraries
library(praznik)  # information-theoretic feature selection (Brown et al., 2012)
library(ggplot2)  # bar chart visualisations

# Load helper functions from previous exercises
source("exercise_1.R")  # provides load_data()
source("exercise_2.R")  # provides split_data()

#-------------------------------FUNCTIONS---------------------------------------

# Runs all 8 feature selection methods on the training data.
# Methods rank features by mutual information with the class label Y.
# They differ in how redundancy between selected features is penalised.
# Note: praznik v12 uses updated names for some Brown et al. (2012) criteria:
#   MIFS    -> DISR  (same relevance-redundancy trade-off, symmetrical form)
#   maxMIFS -> JMIM  (same worst-case redundancy logic, joint MI form)
#   CIFE    -> CMI   (same conditional MI criterion)
#   DMIM    -> NJMIM (same decomposition logic, normalised form)
run_feature_selection <- function(X_train, Y_train, k) {
  results <- list(
    MIM     = MIM(X_train,   Y_train, k = k),  # relevance only, no redundancy penalty
    MIFS    = DISR(X_train,  Y_train, k = k),  # penalises redundancy with selected set
    mRMR    = MRMR(X_train,  Y_train, k = k),  # normalised relevance-redundancy balance
    maxMIFS = JMIM(X_train,  Y_train, k = k),  # worst-case redundancy via joint MI
    CIFE    = CMI(X_train,   Y_train, k = k),  # conditional MI, rewards complementarity
    CMIM    = CMIM(X_train,  Y_train, k = k),  # worst-case conditional MI per feature
    JMI     = JMI(X_train,   Y_train, k = k),  # average joint MI with selected set
    DMIM    = NJMIM(X_train, Y_train, k = k)   # normalised joint MI, unique contributions
  )
  return(results)
}

# Prints the ranked feature list for each method to the console.
# Features listed left to right from most to least relevant.
print_selected_features <- function(fs_results) {
  cat("\n=== Ranked Features by Method (most relevant first) ===\n")
  for (method_name in names(fs_results)) {
    features <- names(fs_results[[method_name]]$score)
    cat(method_name, ":", paste(features, collapse = " > "), "\n")
  }
}

# Prints numeric scores for all methods to allow elbow-based cutoff selection.
print_scores <- function(fs_results) {
  cat("\n=== Numeric Feature Scores by Method ===\n")
  for (method_name in names(fs_results)) {
    cat("\n---", method_name, "---\n")
    print(round(fs_results[[method_name]]$score, 4))
  }
}

# Saves a bar chart of feature scores for one method.
# Bars are ordered by rank; the elbow point suggests how many features to retain.
plot_feature_scores <- function(result, method_name, save_path) {
  scores   <- result$score
  features <- names(scores)
  
  # factor() preserves rank order in the plot (prevents alphabetical sorting)
  df <- data.frame(
    Feature = factor(features, levels = features),
    Score   = scores
  )
  
  p <- ggplot(df, aes(x = Feature, y = Score)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Feature Selection Scores:", method_name),
         x = "Feature (ranked)", y = "Mutual Information Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(save_path, plot = p)
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

# Saves a CSV table of the top k retained features per method and prints
# a summary to the console.
# k=4 chosen by elbow method: the majority of methods (mRMR, CIFE, CMIM,
# DMIM) show a sharp drop in scores after the 4th feature, indicating
# diminishing relevance returns beyond this point.
save_retained_features <- function(fs_results, k, save_path) {
  retained <- data.frame(
    Method   = names(fs_results),
    Features = sapply(fs_results, function(r) {
      paste(names(r$score)[1:k], collapse = ", ")
    })
  )
  write.csv(retained, save_path, row.names = FALSE)
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
  cat("\n=== Top", k, "Features Retained per Method ===\n")
  print(retained)
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------

main <- function() {
  # 1. Setup directories and paths
  music_genre_path <- file.path(getwd(), "archive", "music_genre.csv")
  plots_dir        <- file.path(getwd(), "ex_5_plots")
  tables_dir       <- file.path(getwd(), "tables")
  if (!dir.exists(plots_dir))  dir.create(plots_dir)
  if (!dir.exists(tables_dir)) dir.create(tables_dir)
  
  # 2. Load and split data (same seed 42, 70/30 as all previous exercises)
  # Feature selection is applied to training data ONLY to avoid data leakage
  data_cr    <- load_data(music_genre_path, filter_cr = TRUE)
  data_split <- split_data(data_cr)
  train_data <- data_split$train_data
  
  # 3. Prepare feature matrix and label vector
  compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness",
                       "liveness", "loudness", "speechiness", "valence")
  X_train <- train_data[, compare_columns]   # numerical features
  Y_train <- factor(train_data$music_genre)  # class labels as factor
  
  # 4. Run all 8 methods with k=8 to get full ranking of all features
  k          <- length(compare_columns)
  fs_results <- run_feature_selection(X_train, Y_train, k)
  
  # 5. Print ranked feature lists and numeric scores to console
  print_selected_features(fs_results)
  print_scores(fs_results)
  
  # 6. Define all output plot paths in one place
  paths <- list(
    MIM     = file.path(plots_dir, "fs_MIM.png"),
    MIFS    = file.path(plots_dir, "fs_MIFS.png"),
    mRMR    = file.path(plots_dir, "fs_mRMR.png"),
    maxMIFS = file.path(plots_dir, "fs_maxMIFS.png"),
    CIFE    = file.path(plots_dir, "fs_CIFE.png"),
    CMIM    = file.path(plots_dir, "fs_CMIM.png"),
    JMI     = file.path(plots_dir, "fs_JMI.png"),
    DMIM    = file.path(plots_dir, "fs_DMIM.png")
  )
  
  # 7. Save one bar chart per method
  message("\n--- Generating Feature Selection Plots ---")
  for (method_name in names(fs_results)) {
    plot_feature_scores(fs_results[[method_name]], method_name, paths[[method_name]])
  }
  
  # 8. Decide and save the retained features
  # k_retain=4 is the consensus cutoff across all 8 methods based on the
  # elbow in the score profiles. The top 4 features consistently selected
  # are: acousticness, danceability, instrumentalness, energy.
  k_retain <- 4
  save_retained_features(fs_results, k_retain,
                         file.path(tables_dir, "fs_retained_features.csv"))
  
  message("\nExecution Complete.")
}

# Run only if executed directly
if (sys.nframe() == 0) {
  main()
}