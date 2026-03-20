# Get the current working directory
dir <- getwd()

# Combine the working directory with the relative path to the file
music_genre_path <- file.path(dir, "archive", "music_genre.csv")

# Load the dataset
music_genre <- read.csv(music_genre_path)

# View the first few rows of the dataset
head(music_genre)

# Check the structure of the dataset
str(music_genre)

# Open the dataset in the viewer
View(music_genre)

