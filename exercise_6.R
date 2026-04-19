# Load necessary libraries
library(class)    # provides knn() for k-Nearest Neighbour classification
library(kernlab)  # provides kpca() for Kernel PCA representation
library(ggplot2)  # for results visualisation
library(praznik)

# Load helper functions from all previous exercises
source("exercise_1.R")  # provides load_data()
source("exercise_2.R")  # provides split_data()
source("exercise_3.R")  # provides perform_pca()

#-------------------------------FUNCTIONS---------------------------------------

# Computes the 4 required performance metrics from true and predicted labels.
# Macro metrics are the arithmetic mean of per-class values (see project spec).
compute_metrics <- function(true_labels, pred_labels) {
  classes <- levels(factor(true_labels))
  
  # Per-class recall: fraction of true positives over all actual positives
  recall <- sapply(classes, function(c) {
    tp <- sum(true_labels == c & pred_labels == c)
    fn <- sum(true_labels == c & pred_labels != c)
    if ((tp + fn) == 0) return(0)
    tp / (tp + fn)
  })
  
  # Per-class precision: fraction of true positives over all predicted positives
  precision <- sapply(classes, function(c) {
    tp <- sum(true_labels == c & pred_labels == c)
    fp <- sum(true_labels != c & pred_labels == c)
    if ((tp + fp) == 0) return(0)
    tp / (tp + fp)
  })
  
  # Per-class F1: harmonic mean of recall and precision
  f1 <- 2 * (precision * recall) / (precision + recall)
  f1[is.nan(f1)] <- 0
  
  c(
    Accuracy        = mean(true_labels == pred_labels),
    Macro_Recall    = mean(recall),
    Macro_Precision = mean(precision),
    Macro_F1        = mean(f1)
  )
}

# Trains kNN with k=5 and returns class predictions for the test set.
run_knn <- function(train_x, test_x, train_y, k = 5) {
  knn(train = train_x, test = test_x, cl = train_y, k = k)
}

# Trains Logistic Regression and returns class predictions for the test set.
# glm() with binomial family handles binary classification (Classical vs Rap).
# Labels are encoded as 0/1 for glm() and converted back to class names.
run_logistic <- function(train_x, test_x, train_y) {
  train_df       <- as.data.frame(train_x)
  train_df$label <- as.numeric(train_y) - 1  # Classical=0, Rap=1
  
  model        <- glm(label ~ ., data = train_df, family = binomial)
  probs        <- predict(model, newdata = as.data.frame(test_x), type = "response")
  pred_numeric <- ifelse(probs >= 0.5, 1, 0)
  
  # Map numeric predictions back to original class names
  levels(train_y)[pred_numeric + 1]
}

# Saves a grouped bar chart comparing classifier performance across datasets.
# save_path: full path to output PNG
plot_results <- function(results_df, save_path) {
  # Reshape from wide to long format for ggplot faceting
  results_long <- reshape(results_df,
                          varying   = c("Accuracy", "Macro_Recall", "Macro_Precision", "Macro_F1"),
                          v.names   = "Value",
                          timevar   = "Metric",
                          times     = c("Accuracy", "Macro_Recall", "Macro_Precision", "Macro_F1"),
                          direction = "long"
  )
  
  p <- ggplot(results_long, aes(x = Dataset, y = Value, fill = Classifier)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~Metric) +
    scale_fill_manual(values = c("kNN" = "steelblue", "Logistic Regression" = "tomato")) +
    coord_cartesian(ylim = c(0.9, 1)) +
    labs(title = "Classifier Performance Across Dataset Representations",
         x = "Dataset", y = "Score") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(save_path, plot = p, width = 10, height = 7)
  cat(paste0("Saved: ", basename(dirname(save_path)), "/", basename(save_path), "\n"))
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------

main <- function() {
  # 1. Setup directories and paths
  music_genre_path <- file.path(getwd(), "archive", "music_genre.csv")
  plots_dir        <- file.path(getwd(), "ex_6_plots")
  tables_dir       <- file.path(getwd(), "tables")
  if (!dir.exists(plots_dir))  dir.create(plots_dir)
  if (!dir.exists(tables_dir)) dir.create(tables_dir)
  
  # 2. Load and split data (same seed 42, 70/30 as all previous exercises)
  data_cr    <- load_data(music_genre_path, filter_cr = TRUE)
  data_split <- split_data(data_cr)
  train_data <- data_split$train_data
  test_data  <- data_split$test_data
  
  # 3. Define numerical feature columns (consistent with all previous exercises)
  compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness",
                       "liveness", "loudness", "speechiness", "valence")
  
  # 4. Scale training features; apply same scaling to test set to avoid leakage
  train_scaled <- scale(train_data[, compare_columns])
  test_scaled  <- scale(test_data[, compare_columns],
                        center = attr(train_scaled, "scaled:center"),
                        scale  = attr(train_scaled, "scaled:scale"))
  
  # 5. PCA representation (Exercise 3)
  # Project both sets onto PCA space fitted on training data only
  pca_result <- perform_pca(train_data[, compare_columns])
  train_pca  <- predict(pca_result, newdata = train_data[, compare_columns])
  test_pca   <- predict(pca_result, newdata = test_data[, compare_columns])
  
  # 6. Kernel PCA representation (Exercise 4, RBF kernel, sigma=0.1)
  # Fit on scaled training data; project test data using same kernel
  kpca_result <- kpca(~., data = as.data.frame(train_scaled),
                      kernel = "rbfdot", kpar = list(sigma = 0.1))
  train_kpca  <- as.data.frame(predict(kpca_result, as.data.frame(train_scaled)))
  test_kpca   <- as.data.frame(predict(kpca_result, as.data.frame(test_scaled)))
  
  # 7. Feature selection representation (Exercise 5, top 4 features)
  # MIM used as consensus ranking — most stable method across all 8 criteria
  # Run feature selection on training data only
  fs_results  <- MIM(as.data.frame(train_scaled),
                     factor(train_data$music_genre), k = 4)
  fs_features <- names(fs_results$score)
  train_fs    <- train_scaled[, fs_features]
  test_fs     <- test_scaled[,  fs_features]
  
  # 8. Bundle all dataset representations together
  datasets <- list(
    Original = list(train = train_scaled, test = test_scaled),
    PCA      = list(train = train_pca,    test = test_pca),
    KPCA     = list(train = train_kpca,   test = test_kpca),
    FS       = list(train = train_fs,     test = test_fs)
  )
  
  train_labels <- factor(train_data$music_genre)
  true_labels  <- factor(test_data$music_genre)
  
  # 9. Run both classifiers on all 4 dataset representations
  message("\n--- Running Classifiers ---")
  results <- data.frame()
  
  for (ds_name in names(datasets)) {
    train_x <- datasets[[ds_name]]$train
    test_x  <- datasets[[ds_name]]$test
    
    message(paste("Dataset:", ds_name))
    
    # kNN with k=5 neighbours (as specified in the project)
    message("  Running kNN...")
    pred_knn <- run_knn(train_x, test_x, train_labels)
    results  <- rbind(results, data.frame(
      Classifier = "kNN",
      Dataset    = ds_name,
      t(compute_metrics(true_labels, pred_knn))
    ))
    
    # Logistic Regression (chosen for its simplicity and interpretability)
    message("  Running Logistic Regression...")
    pred_lr  <- run_logistic(train_x, test_x, train_labels)
    results  <- rbind(results, data.frame(
      Classifier = "Logistic Regression",
      Dataset    = ds_name,
      t(compute_metrics(true_labels, pred_lr))
    ))
  }
  
  # 10. Define all output paths in one place
  paths <- list(
    table = file.path(tables_dir, "classification_results_t2.csv"),
    plot  = file.path(plots_dir,  "classification_results_t2.png")
  )
  
  # 11. Save results table and plot
  message("\n--- Saving Results ---")
  write.csv(results, paths$table, row.names = FALSE)
  cat("Saved: tables/classification_results.csv\n")
  cat("\n=== Classification Results ===\n")
  print(round(results[, -c(1, 2)], 4))
  plot_results(results, paths$plot)
  
  message("\nExecution Complete.")
}

# Run only if executed directly
if (sys.nframe() == 0) {
  main()
}