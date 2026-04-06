# README: Music Genre Analysis (Exercises 1-4)

This project provides an R pipeline for cleaning, analyzing, and reducing the dimensionality of music data, focusing on the differences between **Classical** and **Rap**.

---

## Project Structure

* **`exercise_1.R`**: Data cleaning and automated boxplot generation.
* **`exercise_2.R`**: Data splitting utilities.
* **`exercise_3.R`**: Linear PCA with 2D and 3D visualization.
* **`exercise_4.R`**: Kernel PCA (Linear, Polynomial, RBF kernels).
* **`ex_1_plots/` to `ex_4_plots/`**: Output directories for all visualizations.

---

## Technical Features

### 1. Data Processing
The `load_data` function standardizes the dataset by removing empty labels and optionally filtering for Classical and Rap genres.

### 2. Dimensionality Reduction

#### Linear PCA

Projects high-dimensional audio features into uncorrelated components. Includes variance analysis (Scree plots) and feature weight exports.

#### Kernel PCA

Uses the "kernel trick" to map data into higher dimensions for non-linear separation. The RBF kernel provides the most distinct clustering for this dataset.

---

## Standards

* **Consistency**: Classical is always **Blue**; Rap is always **Red**.
* **DRY Principles**: All file paths are centralized in `main()` functions to simplify folder management.
* **Execution**: Scripts use `if (sys.nframe() == 0)` guards to allow for safe function sourcing.
* **Clean Output**: Console logs use relative paths (e.g., `Saved: ex_4_plots/filename.png`).

---

## Requirements

The following R libraries must be installed to run the scripts:

* **`ggplot2`**: For 2D scatter plots and boxplots.
* **`kernlab`**: For Kernel PCA computations.
* **`scatterplot3d`**: For static 3D visualizations.
* **`rgl`**: For interactive 3D plotting.
