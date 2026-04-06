# Load necessary library
library(ggplot2)

#-------------------------------FUNCTIONS---------------------------------------

# Define a function for loading and cleaning the dataset
load_data <- function(file_path, filter_cr = TRUE) {
  data_raw <- read.csv(file_path)
  data_raw$music_genre <- as.character(data_raw$music_genre)
  
  # 1. Remove rows where genre is empty or NA
  data_with_genre <- data_raw[data_raw$music_genre != "" & !is.na(data_raw$music_genre), ]
  
  removed_empty <- nrow(data_raw) - nrow(data_with_genre)
  message(paste("Removed", removed_empty, "rows without a genre label."))
  
  # 2. Conditional filtering
  if (filter_cr) {
    cr_data <- data_with_genre[data_with_genre$music_genre %in% c("Classical", "Rap"), ]
    removed_non_cr <- nrow(data_with_genre) - nrow(cr_data)
    message(paste("Removed", removed_non_cr, "rows that were not Classical or Rap."))
    return(cr_data)
  } else {
    return(data_with_genre)
  }
}

# Function to generate boxplots
# 'save_prefix' is the full path including directory and suffix (e.g., "ex_1_plots/boxplot_")
generate_boxplots <- function(data, compare_columns, save_prefix) {
  fixed_color <- "lightgrey"
  
  for (col in compare_columns) {
    if (!is.numeric(data[[col]])) {
      message(paste("Skipping non-numeric column:", col))
      next
    }
    
    data_clean <- data[!is.na(data[[col]]) & is.finite(data[[col]]), ]
    data_missing <- data[is.na(data[[col]]) | !is.finite(data[[col]]), ]
    
    if (nrow(data_clean) == 0) next
    
    # Construct the final full path for this specific column
    # Example: "ex_1_plots/boxplot_" + "energy" + ".pdf"
    full_path <- paste0(save_prefix, col, ".pdf")
    
    pdf(full_path)
    p <- ggplot(data_clean, aes(x = music_genre, y = .data[[col]])) + 
      geom_boxplot(fill = fixed_color, color = "black") +
      labs(
        title = paste("Boxplot of", col, "by Genre"),
        x = "Music Genre",
        y = col,
        caption = paste("Missing", col, "but has genre:", nrow(data_missing), "\n",
                        "Missing neither:", nrow(data_clean))
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
    dev.off()
    
    # Clean console message using basename logic
    cat(paste0("Saved: ", basename(dirname(full_path)), "/", basename(full_path), "\n"))
  }
}

#--------------------------MAIN SCRIPT EXECUTION--------------------------------

main <- function() {
  # 1. Setup Directories
  music_genre_path <- file.path(getwd(), "archive", "music_genre.csv")
  plots_dir <- file.path(getwd(), "ex_1_plots")
  if (!dir.exists(plots_dir)) dir.create(plots_dir)
  
  # 2. Load Data
  data_all <- load_data(music_genre_path, filter_cr = FALSE)
  data_cr  <- load_data(music_genre_path, filter_cr = TRUE)
  
  # 3. Define columns to compare
  compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness", 
                       "liveness", "loudness", "speechiness", "valence")
  
  # 4. Define "Base Paths" for the outputs
  # This follows the pattern of your last script by defining the destination in main
  path_prefix_all <- file.path(plots_dir, "boxplot_")
  path_prefix_cr  <- file.path(plots_dir, "boxplot_cr_")
  
  # 5. Generate Boxplots
  message("\n--- Generating Boxplots for All Genres ---")
  generate_boxplots(data_all, compare_columns, path_prefix_all)
  
  message("\n--- Generating Boxplots for Classical vs Rap ---")
  generate_boxplots(data_cr, compare_columns, path_prefix_cr)
  
  message("\nExecution Complete.")
}

if (sys.nframe() == 0) {
  main()
}