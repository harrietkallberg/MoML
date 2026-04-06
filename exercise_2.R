source("exercise_1.R")

#-------------------------------FUNCTIONS---------------------------------------

# Define a function for splitting data into training and testing sets
split_data <- function(data, train_fraction = 0.7, seed = 42) {
  set.seed(seed)
  train_size <- floor(train_fraction * nrow(data))
  train_indices <- sample(seq_len(nrow(data)), size = train_size)
  train_data <- data[train_indices, ]
  test_data <- data[-train_indices, ]
  return(list(train_data = train_data, test_data = test_data))
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------
main <- function() {
  # Step 1: Load and clean data
  dir <- getwd()
  music_genre_path <- file.path(dir, "archive", "music_genre.csv")
  data_cr <- load_data(music_genre_path)
  
  # Step 2: Split data into training and testing sets
  data_split <- split_data(data_cr)
  train_data <- data_split$train_data
  test_data <- data_split$test_data
  
  # Print sizes of the datasets
  cat("Total set size:", nrow(data_cr), "\n")
  cat("Training set size:", nrow(train_data), "\n")
  cat("Testing set size:", nrow(test_data), "\n")
}

if (sys.nframe() == 0) {
  main()
}