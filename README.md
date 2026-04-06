# README: Music Genre Analysis Pipeline (Exercises 1-4)

This project consists of a series of R scripts designed to perform Exploratory Data Analysis (EDA) and Dimensionality Reduction on a music dataset. 
The analysis specifically focuses on the contrasting audio features of **Classical** and **Rap** music.

---

## Project Structure

* **`exercise_1.R`**: Data loading and cleaning utilities, and automated boxplot generation for feature comparison.
* **`exercise_2.R`**: Data splitting functions to create training and testing sets.
* **`exercise_3.R`**: Standard Linear Principal Component Analysis (PCA) with 2D and 3D visualization.
* **`exercise_4.R`**: Advanced Kernel PCA (KPCA) utilizing Linear, Polynomial, and RBF kernels.
* **`archive/`**: Directory for the input dataset (`music_genre.csv`).
* **`ex_1_plots/`** to **`ex_4_plots/`**: Output folders for generated PDF/PNG visualizations and data exports.

---

## Core Features

### 1. Robust Data Ingestion
The `load_data` function cleans the raw dataset by:
* Removing rows with missing or empty genre labels.
* Providing an optional filter (`filter_cr = TRUE`) to isolate Classical and Rap genres.
* Reporting exact row counts of removed data to the console for auditability.

### 2. Exploratory Data Analysis (EDA)
Automated generation of boxplots for attributes like *Acousticness, Energy, and Speechiness*.
* **Contextual Captions**: Each plot includes metadata regarding missing values for that specific feature.
* **Dual-Stream Output**: Generates separate plot sets for the full 10-genre dataset and the Classical/Rap subset.

### 3. Dimensionality Reduction

#### Linear PCA (Exercise 3)

Transforms high-dimensional audio features into a set of linearly uncorrelated components.
* **Variance Analysis**: Generates "Scree Plots" and Cumulative Variance plots to justify the number of components selected.
* **Weight Export**: Saves the feature loadings (rotation) to `pca_feature_weights.csv` to identify which audio traits drive the components.

#### Kernel PCA (Exercise 4)

Applies the "kernel trick" to project data into higher-dimensional spaces where non-linear patterns become separable.
* **Linear Kernel**: Baseline comparison.
* **Polynomial Kernel**: Useful for capturing interactions between features.
* **RBF Kernel**: High-performance non-linear mapping that typically provides the clearest cluster separation for this dataset.

---

## Technical Standards

* **Execution Guard**: All scripts use `if (sys.nframe() == 0)` to ensure that logic only executes when the file is run directly, allowing functions to be safely `source()`-ed into other scripts.
* **Visual Consistency**: 
    * **Classical** is consistently mapped to **Blue**.
    * **Rap** is consistently mapped to **Red**.
* **Path Management**: All file paths are centralized in the `main()` functions to adhere to DRY (Don't Repeat Yourself) principles.
* **Relative Logging**: The console provides clean feedback using relative paths (e.g., `Saved: ex_4_plots/kpca_rbf_plot.png`).

---

## Requirements

The following R packages are required:
* `ggplot2` (Visualization)
* `kernlab` (Kernel PCA)
* `scatterplot3d` (3D Static Plots)
* `rgl` (Interactive 3D Plotting)
