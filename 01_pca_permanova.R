# =============================================================================
# 01_pca_permanova.R
# Principal Component Analysis (PCA) and PERMANOVA tests of outlet-level
# topic distributions across platforms.
#
# Paper: Steensen et al. (2026). "Flattening the News: Platformisation and
#        the Erosion of Topical Diversity Across News Outlet Types."
#        Digital Journalism.
#
# Produces:
#   Figures 1-4  — PCA scatter plots (Online, Facebook, Instagram, TikTok)
#   Tables 2-4   — PERMANOVA results (Online, Facebook, Instagram)
#
# Input:  1_NorwegianNewsTopics.csv (DataverseNO deposit)
#         https://doi.org/10.18710/473JEF
#
# =============================================================================

# --- Required packages -------------------------------------------------------
# install.packages(c("dplyr", "ggplot2", "ggrepel", "vegan", "readr"))

library(dplyr)
library(ggplot2)
library(ggrepel)
library(vegan)
library(readr)

# =============================================================================
# USER SETTINGS
# =============================================================================

DATA_PATH <- "1_NorwegianNewsTopics.csv"

OUTPUT_DIR <- "."   # folder where figures and tables are saved

# =============================================================================
# SECTION 1: Load and prepare data
# =============================================================================

cat("Loading data from:", DATA_PATH, "\n")
alldata <- read_csv(DATA_PATH, show_col_types = FALSE)

# Topic columns are lowercase in the DataverseNO CSV
topic_cols <- paste0("topic", 1:28)
stopifnot(all(topic_cols %in% names(alldata)))

# Coerce platform flag columns to logical
boolify <- function(x) {
  lx  <- tolower(as.character(x))
  out <- ifelse(lx %in% c("true", "1", "yes"), TRUE,
                ifelse(lx %in% c("false", "0", "no"), FALSE, NA))
  out[is.na(out)] <- FALSE
  out
}

alldata <- alldata %>%
  mutate(
    published_on_fb     = boolify(published_on_fb),
    published_on_insta  = boolify(published_on_insta),
    published_on_tiktok = boolify(published_on_tiktok)
  )

# Fill owner for two independent outlets
alldata <- alldata %>%
  mutate(
    owner = case_when(
      newsroom == "tv2nyheter"   ~ "TV 2 Group",
      newsroom == "klassekampen" ~ "Cooperative",
      TRUE                       ~ owner
    )
  )

cat("Total articles (Online):", nrow(alldata), "\n")
cat("Facebook:", sum(alldata$published_on_fb), "\n")
cat("Instagram:", sum(alldata$published_on_insta), "\n")
cat("TikTok:", sum(alldata$published_on_tiktok), "\n\n")

# =============================================================================
# SECTION 2: Build outlet-level average topic profiles per platform
# =============================================================================

make_profiles <- function(df, filter_col = NULL) {
  if (!is.null(filter_col)) {
    df <- df %>% filter(.data[[filter_col]] == TRUE)
  }
  df %>%
    group_by(newsroom, type, owner) %>%
    summarise(across(all_of(topic_cols), ~ mean(.x, na.rm = TRUE)),
              n_articles = n(),
              .groups = "drop")
}

profiles <- list(
  Online    = make_profiles(alldata),
  Facebook  = make_profiles(alldata, "published_on_fb"),
  Instagram = make_profiles(alldata, "published_on_insta"),
  TikTok    = make_profiles(alldata, "published_on_tiktok")
)

cat("Outlet counts per platform:\n")
for (p in names(profiles)) {
  cat(" ", p, ":", nrow(profiles[[p]]), "outlets\n")
}

# =============================================================================
# SECTION 3: Shared plot settings
# =============================================================================

type_colors <- c(
  "Local"    = "#E41A1C",
  "Regional" = "#377EB8",
  "National" = "#4DAF4A",
  "Niche"    = "#984EA3"
)

owner_shapes <- c(
  "Amedia"       = 15,
  "Polaris"      = 16,
  "Schibsted"    = 17,
  "Tun media"    = 18,
  "Mentor media" = 8,
  "TV 2 Group"   = 3,
  "Cooperative"  = 7
)

newsroom_labels <- c(
  "tv2nyheter"       = "TV 2 Nyheter",
  "aftenposten"      = "Aftenposten",
  "vg"               = "VG",
  "morgenbladet"     = "Morgenbladet",
  "klassekampen"     = "Klassekampen",
  "nationen"         = "Nationen",
  "vartland"         = "Vårt Land",
  "adresseavisen"    = "Adresseavisen",
  "smp"              = "Sunnmørsposten",
  "nordlys"          = "Nordlys",
  "fredriksstadblad" = "Fredriksstad Blad",
  "romerikesblad"    = "Romerikes Blad",
  "avisahordaland"   = "Avisa Hordaland",
  "avisanordland"    = "Avisa Nordland",
  "altaposten"       = "Altaposten",
  "harstadtidende"   = "Harstad Tidende",
  "agderposten"      = "Agderposten",
  "jarlsbergavis"    = "Jarlsberg Avis",
  "strilen"          = "Strilen",
  "glamdalen"        = "Glåmdalen",
  "bergenstidende"   = "Bergens Tidende",
  "nordstrandsblad"  = "Nordstrands Blad"
)

# =============================================================================
# SECTION 4: PCA scatter plots (Figures 1-4)
# =============================================================================

make_pca_plot <- function(df, platform_label) {

  topic_matrix <- df %>% select(all_of(topic_cols)) %>% as.matrix()
  pca          <- prcomp(topic_matrix, scale. = TRUE)

  pct <- round(100 * pca$sdev^2 / sum(pca$sdev^2), 1)

  pca_df <- as.data.frame(pca$x[, 1:2]) %>%
    mutate(
      newsroom = recode(df$newsroom, !!!newsroom_labels),
      type     = df$type,
      owner    = df$owner
    )

  # Print variance explained by PC1+PC2
  cat(platform_label, "— PC1:", pct[1], "%, PC2:", pct[2], "%\n")

  ggplot(pca_df, aes(x = PC1, y = PC2, color = type, shape = owner)) +
    geom_point(size = 3.5, stroke = 0.8) +
    geom_text_repel(
      aes(label = newsroom),
      size          = 3,
      segment.color = "grey55",
      segment.size  = 0.3,
      max.overlaps  = 25,
      seed          = 42
    ) +
    scale_color_manual(values = type_colors, name = "Outlet type") +
    scale_shape_manual(values = owner_shapes, name = "Owner") +
    labs(
      title = paste0("News outlet clustering by topic profile (", platform_label,")"),
      subtitle = "Colour = newsroom type, shape = ownership group",
      x     = paste0("PC1  (", pct[1], "% variance explained)"),
      y     = paste0("PC2  (", pct[2], "% variance explained)")
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title      = element_text(face = "bold", size = 13),
      legend.position = "right",
      panel.grid.minor = element_blank()
    )
}

platform_fig_names <- c(
  Online    = "figure1_pca_online.png",
  Facebook  = "figure2_pca_facebook.png",
  Instagram = "figure3_pca_instagram.png",
  TikTok    = "figure4_pca_tiktok.png"
)

for (platform in names(platform_fig_names)) {
  df_i <- profiles[[platform]]
  if (nrow(df_i) < 3) {
    message("Skipping PCA for ", platform, " — only ", nrow(df_i), " outlets")
    next
  }
  p        <- make_pca_plot(df_i, platform)
  out_file <- file.path(OUTPUT_DIR, platform_fig_names[[platform]])
  ggsave(out_file, plot = p, width = 12, height = 8, dpi = 600)
  cat("Saved:", out_file, "\n")
}

# =============================================================================
# SECTION 5: PERMANOVA tests (Tables 2-4)
# Note: TikTok (n = 4 outlets) has too few degrees of freedom for a stable
# PERMANOVA with both type and owner as predictors; not reported in the paper.
# =============================================================================

run_permanova <- function(df, platform_label, out_csv) {

  cat("\n=== PERMANOVA:", platform_label, "(n =", nrow(df), "outlets) ===\n")

  topic_matrix <- df %>% select(all_of(topic_cols)) %>% as.matrix()
  dist_mat     <- dist(topic_matrix, method = "euclidean")

  # Full model: type + owner (all outlets)
  mod_both  <- adonis2(dist_mat ~ type + owner, data = df, permutations = 999)
  # Type only
  mod_type  <- adonis2(dist_mat ~ type,          data = df, permutations = 999)
  # Owner only
  mod_owner <- adonis2(dist_mat ~ owner,         data = df, permutations = 999)
  # Type + owner restricted to Amedia, Polaris, Schibsted
  df_sub    <- df %>% filter(owner %in% c("Amedia", "Polaris", "Schibsted"))
  dist_sub  <- dist(df_sub %>% select(all_of(topic_cols)), method = "euclidean")
  mod_sub   <- adonis2(dist_sub ~ type + owner, data = df_sub, permutations = 999)

  # Combine into a single table
  label_and_filter <- function(mod, label) {
    as.data.frame(mod) %>%
      filter(!is.na(F)) %>%
      mutate(Model = label,
             Platform = platform_label)
  }

  tbl <- bind_rows(
    label_and_filter(mod_both,  "Type + Owner (all outlets)"),
    label_and_filter(mod_type,  "Type only (all outlets)"),
    label_and_filter(mod_owner, "Owner only (all outlets)"),
    label_and_filter(mod_sub,   "Type + Owner (Amedia, Polaris, Schibsted only)")
  ) %>%
    mutate(
      Significance = case_when(
        `Pr(>F)` <= 0.001 ~ "***",
        `Pr(>F)` <= 0.01  ~ "**",
        `Pr(>F)` <= 0.05  ~ "*",
        `Pr(>F)` <= 0.1   ~ ".",
        TRUE              ~ ""
      )
    ) %>%
    select(Platform, Model, Df, SumOfSqs, R2, F, `Pr(>F)`, Significance)

  print(tbl, digits = 4)

  write_csv(tbl, file.path(OUTPUT_DIR, out_csv))
  cat("Saved:", out_csv, "\n")

  invisible(tbl)
}

run_permanova(profiles[["Online"]],    "Online",    "table2_permanova_online.csv")
run_permanova(profiles[["Facebook"]],  "Facebook",  "table3_permanova_facebook.csv")
run_permanova(profiles[["Instagram"]], "Instagram", "table4_permanova_instagram.csv")

cat("\n--- Done. All PCA figures and PERMANOVA tables saved. ---\n")
