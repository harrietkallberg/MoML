# Music Genre Classification Pipeline
### MAAut 2026 — Project 1: Dimension Reduction in Classification

Binary classification pipeline distinguishing **Classical** from **Rap** music,
using a dataset of 50,000 observations across 10 genres. Implements the full
pipeline from data cleaning through dimensionality reduction to classifier
evaluation, as specified in the project brief.

---

## Structure

| Script | Description |
|---|---|
| `exercise_1.R` | Data cleaning and exploratory boxplot generation |
| `exercise_2.R` | Train/test splitting (70/30, `set.seed(42)`) |
| `exercise_3.R` | Linear PCA — variance analysis, loadings, 2D/3D plots |
| `exercise_4.R` | Kernel PCA — Linear, Polynomial (deg 3), RBF (σ = 0.1) |
| `exercise_5.R` | MI-based feature selection via `praznik` (8 methods) |
| `exercise_6.R` | kNN (k=5) and Logistic Regression across 4 representations |
| `exercise_7.R` | Performance comparison and heatmap visualization |

Output plots are saved to `ex_1_plots/` through `ex_7_plots/`.

---

## Requirements

```r
install.packages(c("ggplot2", "kernlab", "scatterplot3d", "rgl", "praznik", "class"))
```

---

## Standards

- Classical = **Blue**, Rap = **Red** throughout all plots
- All transformations fitted on training data only — no leakage
- Scripts use `if (sys.nframe() == 0)` guards for safe sourcing
