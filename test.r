# Get the current working directory
dir <- getwd()

# Combine the working directory with the relative path to the file
music_genre_path <- file.path(dir, "archive", "music_genre.csv")

# Load the dataset
data <- read.csv(music_genre_path)

# Check column names and ensure music_genre is correct
#colnames(data)

# Convert the music_genre column to character if it's a factor
data$music_genre <- as.character(data$music_genre)

# Create a subset with just Rap and Classical music genres
subset_data <- subset(data, music_genre %in% c("Rap", "Classical"))

# Create a boxplot to show the distribution of instrumentalness across genres
boxplot(instrumentalness ~ music_genre, data = data, 
        main = "Distribution of Instrumentalness across Genres", 
        xlab = "Music Genre", ylab = "Instrumentalness", 
        col = "lightblue")
