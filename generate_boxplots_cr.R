# Load necessary library
library(ggplot2)

# Get the current working directory
dir <- getwd()

# Combine the working directory with the relative path to the file
music_genre_path <- file.path(dir, "archive", "music_genre.csv")

# Load the dataset
data_raw <- read.csv(music_genre_path)

# Convert the music_genre column to character if it's a factor
data_raw$music_genre <- as.character(data_raw$music_genre)

# Convert the 'tempo' column to numeric after setting ? as NA
data_raw$tempo[data_raw$tempo == "?"] <- NA  # Replace "?" with NA
data_raw$tempo <- as.numeric(data_raw$tempo)  # Convert 'tempo' to numeric

# Define the columns to compare
compare_columns <- c("instrumentalness", "danceability", "energy", "acousticness", "liveness", 
                     "loudness", "speechiness", "tempo", "valence")

# Mask out rows where the music_genre is an empty string
data_with_genre <- data_raw[data_raw$music_genre != "", ]
data_with_cr_genre <- data_with_genre[data_with_genre$music_genre %in% c("Classical", "Rap"), ]

# Create the 'plots' subfolder if it doesn't exist
plots_dir <- file.path(getwd(), "plots")
if (!dir.exists(plots_dir)) {
  dir.create(plots_dir)
}

# Define a fixed color for all boxplots
fixed_color <- "lightgrey"

# Loop through each column and create a boxplot for each
for (col in compare_columns) {
  # Ensure the column is numeric and clean missing or infinite values
  if (!is.numeric(data_raw[[col]])) {
    cat(paste("Skipping non-numeric column:", col, "\n"))
    next  # Skip this column if it's not numeric
  }
  
  # Create subsets based on the conditions
  data_with_both <- data_with_cr_genre[!is.na(data_with_cr_genre[[col]]) & is.finite(data_with_cr_genre[[col]]), ]
  data_with_only_genre <- data_with_cr_genre[is.na(data_with_cr_genre[[col]]) | !is.finite(data_with_cr_genre[[col]]), ]
  # Check if the subset is empty
  if (nrow(data_with_both) == 0) {
    cat(paste("Skipping column:", col, "because no data is available for plotting.\n"))
    next
  }
  # Define the PDF file path for saving the plot
  pdf_path <- file.path(plots_dir, paste0("boxplot_", col, "_cr.pdf"))
  
  # Open the PDF device to save the plot
  pdf(pdf_path)
  
  # Create the boxplot for the current column by music_genre with custom colors
  p <- ggplot(data_with_both, aes(x = music_genre, y = get(col))) + 
    geom_boxplot(fill = fixed_color, color = "black") +  # Consistent lightgrey color for boxes
    labs(
      title = paste("Boxplot of", col, "by Genre"),
      x = "Music Genre",
      y = col,
      caption = paste(
        "Missing", col, "but has genre:", nrow(data_with_only_genre), "\n",
        "Missing neither:", nrow(data_with_both)
      )  # Add caption with four counts
    ) +
    theme_minimal() +
    theme(
      legend.title = element_blank(),  # Remove legend title
      legend.position = "top",  # Place legend at the top
      axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels for readability
    )
  
  # Print the plot to the PDF
  print(p)
  
  # Close the PDF device
  dev.off()
  
  # Print a confirmation message
  cat(paste("Boxplot for", col, "has been saved as a PDF in the 'plots' folder.\n"))
}