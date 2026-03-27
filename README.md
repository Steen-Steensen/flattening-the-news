# Flattening the News — Statistical Analysis Scripts

R scripts for the statistical analysis reported in:

> Steensen et al. (2026). *Flattening the News: Platformisation and the
> Erosion of Topical Diversity Across News Outlet Types*. **New Media & Society*.

---

## Dataset

The analysis uses one file from the public DataverseNO deposit:

> Steen Steensen, 2026, *NorwegianNewsTopics (2023): Topic Modeling of Norwegian News*,
> <https://doi.org/10.18710/473JEF>, DataverseNO, V1

| File | Description |
|---|---|
| `1_NorwegianNewsTopics.csv` | Main dataset: one row per article, with publication metadata and topic1–topic28 scores (N = 224,905) |

Run `00_download_data.R` once to download the file automatically into the
project folder. If the download fails, the file can also be downloaded
manually from the link above.

---

## Getting started

1. Clone or download this repository.
2. Open `3_Flattening_the_news.Rproj` in RStudio — this sets the working directory automatically.
3. Run `00_download_data.R` once to download the input data from DataverseNO.
4. Run `01_pca_permanova.R` and/or `02_diversity_by_type.R`.

---

## Prerequisites

Install required packages before running the scripts:

```r
install.packages(c(
  "dplyr", "tidyr", "purrr",
  "ggplot2", "ggrepel",
  "vegan",
  "readr"
))
```

---

## Scripts

| Script | Produces | Paper location |
|---|---|---|
| `00_download_data.R` | Downloads input CSV from DataverseNO | — |
| `01_pca_permanova.R` | `figure1_pca_online.png` — `figure4_pca_tiktok.png`, `table2_permanova_online.csv` — `table4_permanova_instagram.csv` | Figures 1–4, Tables 2–4 |
| `02_diversity_by_type.R` | `diversity_by_type_and_platform.csv`, `diversity_by_newsroom_all.csv` | Diversity values cited in text |

Scripts `01` and `02` are independent and can be run in any order once the
input CSV is present in the working directory.

---

## Script descriptions

### `01_pca_permanova.R`
Builds outlet-level average topic profiles for each platform (Online,
Facebook, Instagram, TikTok) and runs Principal Component Analysis (PCA).
Produces four scatter plots (Figures 1–4) showing how news outlets cluster
in topic space, with outlets coloured by editorial type (local, regional,
national, niche) and shaped by ownership group.

Also runs PERMANOVA tests (`vegan::adonis2`) for Online, Facebook, and
Instagram (Tables 2–4). TikTok is included in the PCA visualisation but
excluded from PERMANOVA due to the low number of outlets (n = 4). Four model
specifications are tested per platform: (1) Type + Owner (all outlets),
(2) Type only, (3) Owner only, and (4) Type + Owner restricted to outlets
owned by Amedia, Polaris, or Schibsted.

### `02_diversity_by_type.R`
Computes Shannon Entropy, Simpson's Index, and Gini Coefficient for each
outlet on each platform, then aggregates by outlet type (mean, SD, min, max).
These are the diversity values cited throughout the paper text (e.g.,
"national outlets: entropy ≈3.0, Simpson ≈0.93–0.94, Gini ≈0.43").
Outputs a type-level summary CSV and a full per-newsroom CSV.

---

## Relationship to other repositories

This repository contains only the scripts for the PCA, PERMANOVA, and
diversity analyses specific to this article. The full LDA topic modelling
pipeline and social media matching scripts are documented separately at
https://github.com/Steen-Steensen/NorwegianNewsTopics-pipeline
