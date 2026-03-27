# =============================================================================
# 02_diversity_by_type.R
# Topical diversity measures (Shannon Entropy, Simpson's Index, Gini
# Coefficient) per outlet type per platform, as reported in the paper text.
#
# Paper: Steensen et al. (2026). "Flattening the News: Platformisation and
#        the Erosion of Topical Diversity Across News Outlet Types."
#        Digital Journalism.
#
# Produces:
#   diversity_by_type_and_platform.csv  — mean ± SD per outlet type × platform
#   diversity_by_newsroom_all.csv       — full per-newsroom table
#
# The values in the paper (e.g., "national outlets: entropy ≈3.0,
# Simpson ≈0.93–0.94, Gini ≈0.43") correspond to the mean and range
# across individual newsrooms within each outlet type.
#
# Input:  1_NorwegianNewsTopics.csv (DataverseNO deposit)
#         https://doi.org/10.18710/473JEF
# =============================================================================

# --- Required packages -------------------------------------------------------
# install.packages(c("dplyr", "tidyr", "purrr", "readr", "openxlsx"))

library(dplyr)
library(tidyr)
library(purrr)
library(readr)

# =============================================================================
# USER SETTINGS
# =============================================================================

DATA_PATH <- "1_NorwegianNewsTopics.csv"

OUTPUT_DIR <- "."

# =============================================================================
# SECTION 1: Load and prepare data
# =============================================================================

cat("Loading data from:", DATA_PATH, "\n")
alldata <- read_csv(DATA_PATH, show_col_types = FALSE)

topic_cols <- paste0("topic", 1:28)
stopifnot(all(topic_cols %in% names(alldata)))

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

# =============================================================================
# SECTION 2: Diversity index functions
# =============================================================================

# Shannon entropy: H = -sum(p * log(p)), uses natural log.
# Higher values = more even distribution across topics.
entropy_shannon <- function(p) {
  p <- p[p > 0]
  -sum(p * log(p))
}

# Simpson's index: D = 1 - sum(p^2).
# Closer to 1 = high diversity (no single topic dominates).
simpson_index <- function(p) {
  1 - sum(p^2)
}

# Gini coefficient: measures inequality across topic shares.
# 0 = perfectly even; 1 = complete concentration on one topic.
gini_coef <- function(p) {
  p  <- p[p >= 0]
  s  <- sum(p)
  if (s == 0) return(NA_real_)
  p  <- sort(p / s)
  n  <- length(p)
  (2 * sum(p * seq_len(n)) / (n * sum(p))) - (n + 1) / n
}

# =============================================================================
# SECTION 3: Compute per-newsroom average topic profiles per platform,
#            then calculate diversity indices for each newsroom × platform
# =============================================================================

make_newsroom_profiles <- function(df, filter_col = NULL, platform_label) {
  if (!is.null(filter_col)) {
    df <- df %>% filter(.data[[filter_col]] == TRUE)
  }
  df %>%
    group_by(newsroom, type) %>%
    summarise(
      n_articles = n(),
      across(all_of(topic_cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      # Re-normalise so topic shares sum to 1 within each outlet
      topic_sum   = sum(c_across(all_of(topic_cols)), na.rm = TRUE),
      across(all_of(topic_cols), ~ .x / topic_sum),
      entropy     = entropy_shannon(c_across(all_of(topic_cols))),
      simpson     = simpson_index(c_across(all_of(topic_cols))),
      gini        = gini_coef(c_across(all_of(topic_cols))),
      platform    = platform_label
    ) %>%
    ungroup() %>%
    select(platform, newsroom, type, n_articles, entropy, simpson, gini)
}

platforms <- list(
  list(label = "Online",    filter = NULL),
  list(label = "Facebook",  filter = "published_on_fb"),
  list(label = "Instagram", filter = "published_on_insta"),
  list(label = "TikTok",    filter = "published_on_tiktok")
)

all_newsrooms <- map_dfr(platforms, function(p) {
  make_newsroom_profiles(alldata, p$filter, p$label)
})

# =============================================================================
# SECTION 4: Aggregate by outlet type × platform
#            (these are the values cited in the paper text)
# =============================================================================

diversity_by_type <- all_newsrooms %>%
  group_by(platform, type) %>%
  summarise(
    n_outlets     = n(),
    entropy_mean  = mean(entropy,  na.rm = TRUE),
    entropy_sd    = sd(entropy,    na.rm = TRUE),
    entropy_min   = min(entropy,   na.rm = TRUE),
    entropy_max   = max(entropy,   na.rm = TRUE),
    simpson_mean  = mean(simpson,  na.rm = TRUE),
    simpson_sd    = sd(simpson,    na.rm = TRUE),
    simpson_min   = min(simpson,   na.rm = TRUE),
    simpson_max   = max(simpson,   na.rm = TRUE),
    gini_mean     = mean(gini,     na.rm = TRUE),
    gini_sd       = sd(gini,       na.rm = TRUE),
    gini_min      = min(gini,      na.rm = TRUE),
    gini_max      = max(gini,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(platform, type)

# Print summary — quick check against paper-reported values
cat("\n=== Diversity by outlet type and platform ===\n")
print(diversity_by_type %>%
        select(platform, type, n_outlets,
               entropy_mean, simpson_mean, gini_mean) %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))),
      n = Inf)

# =============================================================================
# SECTION 5: Export
# =============================================================================

out_type     <- file.path(OUTPUT_DIR, "diversity_by_type_and_platform.csv")
out_newsroom <- file.path(OUTPUT_DIR, "diversity_by_newsroom_all.csv")

write_csv(diversity_by_type, out_type)
write_csv(all_newsrooms, out_newsroom)

cat("\nSaved:", out_type, "\n")
cat("Saved:", out_newsroom, "\n")
cat("\n--- Done. ---\n")
