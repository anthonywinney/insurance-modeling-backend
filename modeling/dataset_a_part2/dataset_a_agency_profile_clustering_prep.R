# =============================================================================
# Dataset A Part 2 — Agency Profile Clustering-Ready Dataset Cleaning
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_agency_profile_clustering_prep.R
#
# Inputs (modeling/dataset_a_part2/outputs/):
#   agency_profile_dataset.csv
#   agency_profile_audit.json
#   agency_profile_feature_summary.json
#   agency_profile_correlations.json
#
# Outputs (modeling/dataset_a_part2/outputs/):
#   agency_profile_clustering_ready.csv
#   agency_profile_clustering_scaled.csv
#   agency_profile_clustering_prep_audit.json
#   agency_profile_clustering_feature_summary.json
#   agency_profile_clustering_correlations.json
#
# Leakage rule:
#   No database access. All cleaning operates on the training-period agency
#   profile produced by dataset_a_agency_profile_audit.R. No test data used.
#
# Stopping point:
#   Clustering-ready dataset created. K-means has NOT been run.
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(dplyr); library(jsonlite)
})

io_dir <- file.path("modeling", "dataset_a_part2", "outputs")

SENTINEL_AP <- 99999    # sentinel in mean_active_producers_train

# Final 15 clustering features — ordered by feature group
FINAL_15 <- c(
  "log_n_train_rows",
  "n_train_years_active",
  "mean_log_wp_train",
  "mean_log_prev_wp_train",
  "mean_log_prev_poly_train",
  "sd_log_wp_train",
  "iqr_log_wp_train",
  "sd_log_prev_poly_train",
  "mean_active_producers_train_clean",
  "n_products_train",
  "n_states_train",
  "product_hhi",
  "state_hhi",
  "pl_share_train",
  "slope_mean_log_wp_by_year"
)

EXCLUDED_REASONS <- list(
  median_log_wp_train        = "near-duplicate of mean_log_wp_train (r=0.94 in audit); mean retained",
  median_log_prev_wp_train   = "near-duplicate of mean_log_prev_wp_train (r=0.93); mean retained",
  median_log_prev_poly_train = "near-duplicate of mean_log_prev_poly_train (r=0.92); mean retained",
  agency_appointment_year    = "perfectly collinear with mean_active_producers_train (r=1.00); sentinel 99999 contamination",
  mean_max_age_train         = "perfectly collinear with mean_active_producers_train (r=1.00); redundant",
  mean_min_age_train         = "perfectly collinear with mean_active_producers_train (r=1.00); redundant",
  n_vendors_train            = "constant across all 1,254 agencies — zero clustering signal",
  top_product_share          = "near-duplicate of product_hhi (r=0.99); HHI retained",
  top_state_share            = "near-duplicate of state_hhi (r=0.99); HHI retained",
  top_vendor_share           = "constant — zero clustering signal",
  vendor_hhi                 = "constant — zero clustering signal",
  cl_share_train             = "perfect inverse of pl_share_train (r=-1.00); including both doubles-weights this dimension"
)

cat("=== Agency Profile Clustering-Ready Dataset Cleaning ===\n\n")

# =============================================================================
# Read inputs
# =============================================================================

cat("Reading input files...\n")

raw <- read.csv(
  file.path(io_dir, "agency_profile_dataset.csv"),
  stringsAsFactors = FALSE, na.strings = "NA"
)
cat(sprintf("  agency_profile_dataset.csv           : %d agencies x %d columns\n",
            nrow(raw), ncol(raw)))

audit_meta  <- fromJSON(file.path(io_dir, "agency_profile_audit.json"))
coverage    <- audit_meta$agency_coverage
cat(sprintf("  agency_profile_audit.json            : coverage metadata loaded\n\n"))

# =============================================================================
# Apply cleaning rules
# =============================================================================

cat("Applying cleaning rules...\n")

df <- raw  # work on a copy

# 1. mean_active_producers_train_clean
#    Recode sentinel (>= 99999) → NA, then impute with median of remaining values.
n_sentinel_ap  <- sum(df$mean_active_producers_train >= SENTINEL_AP, na.rm = TRUE)
df$mean_active_producers_train_clean <- df$mean_active_producers_train
df$mean_active_producers_train_clean[
  !is.na(df$mean_active_producers_train_clean) &
  df$mean_active_producers_train_clean >= SENTINEL_AP
] <- NA_real_

ap_impute_median <- median(df$mean_active_producers_train_clean, na.rm = TRUE)
n_ap_na_before   <- sum(is.na(df$mean_active_producers_train_clean))
df$mean_active_producers_train_clean[is.na(df$mean_active_producers_train_clean)] <- ap_impute_median

cat(sprintf("  mean_active_producers_train_clean:\n"))
cat(sprintf("    Sentinel values (>= %d) recoded : %d agencies (%.1f%%)\n",
            SENTINEL_AP, n_sentinel_ap, 100 * n_sentinel_ap / nrow(df)))
cat(sprintf("    Imputed with median              : %d  (median = %.2f)\n",
            n_ap_na_before, ap_impute_median))
cat(sprintf("    Post-clean range                 : %.1f – %.1f\n",
            min(df$mean_active_producers_train_clean),
            max(df$mean_active_producers_train_clean)))

# 2. sd_log_wp_train — NA occurs for single-row agencies; 0 = no observed volatility
n_sd_wp_imp <- sum(is.na(df$sd_log_wp_train))
df$sd_log_wp_train[is.na(df$sd_log_wp_train)] <- 0
cat(sprintf("  sd_log_wp_train          : %d NA → 0\n", n_sd_wp_imp))

# 3. sd_log_prev_poly_train — same rationale
n_sd_poly_imp <- sum(is.na(df$sd_log_prev_poly_train))
df$sd_log_prev_poly_train[is.na(df$sd_log_prev_poly_train)] <- 0
cat(sprintf("  sd_log_prev_poly_train   : %d NA → 0\n", n_sd_poly_imp))

# 4. slope_mean_log_wp_by_year — NA occurs for < 3 distinct training years; 0 = unestimated trend
n_slope_imp <- sum(is.na(df$slope_mean_log_wp_by_year))
df$slope_mean_log_wp_by_year[is.na(df$slope_mean_log_wp_by_year)] <- 0
cat(sprintf("  slope_mean_log_wp_by_year: %d NA → 0\n\n", n_slope_imp))

# 5. Check all other final features for unexpected missing values
other_feats <- setdiff(
  FINAL_15,
  c("mean_active_producers_train_clean", "sd_log_wp_train",
    "sd_log_prev_poly_train", "slope_mean_log_wp_by_year")
)
other_miss <- sapply(other_feats, function(col) sum(is.na(df[[col]])))
if (any(other_miss > 0)) {
  cat("  [WARN] Unexpected missingness in other final features:\n")
  for (col in names(other_miss[other_miss > 0])) {
    m   <- other_miss[col]
    med <- median(df[[col]], na.rm = TRUE)
    df[[col]][is.na(df[[col]])] <- med
    cat(sprintf("    %s: %d NA → median (%.4f)\n", col, m, med))
  }
} else {
  cat("  All other final features: no missing values\n\n")
}

# =============================================================================
# Build clustering-ready dataset (15 features + audit support columns)
# =============================================================================

SUPPORT_COLS <- c("n_train_rows", "has_trend_support", "n_years_for_trend")

clustering_ready <- df |>
  select(AGENCY_ID, all_of(FINAL_15), all_of(SUPPORT_COLS))

n_agencies <- nrow(clustering_ready)

# =============================================================================
# Validation checks
# =============================================================================

cat("Running validation checks...\n")

# 1. Row count
v_rowcount <- list(expected = 1254L, actual = n_agencies, pass = n_agencies == 1254L)
cat(sprintf("  [%s] Agency count : %d\n",
            if (v_rowcount$pass) "PASS" else "FAIL", n_agencies))

# 2. One row per agency
n_distinct_ids <- n_distinct(clustering_ready$AGENCY_ID)
v_unique <- list(n_distinct = n_distinct_ids, pass = n_distinct_ids == n_agencies)
cat(sprintf("  [%s] One row/agency: %d unique IDs\n",
            if (v_unique$pass) "PASS" else "FAIL", n_distinct_ids))

# 3. Feature count
v_feat_count <- list(n = length(FINAL_15), pass = length(FINAL_15) == 15L)
cat(sprintf("  [%s] Feature count : %d\n",
            if (v_feat_count$pass) "PASS" else "FAIL", length(FINAL_15)))

# 4. Missingness after cleaning
miss_post <- sapply(FINAL_15, function(col) sum(is.na(clustering_ready[[col]])))
n_miss_feats <- sum(miss_post > 0)
v_miss_post  <- list(n_features_with_na = n_miss_feats,
                     per_feature = as.list(miss_post),
                     pass = n_miss_feats == 0)
cat(sprintf("  [%s] No missing values: %d / 15 features fully complete\n",
            if (v_miss_post$pass) "PASS" else "FAIL",
            15L - n_miss_feats))
if (n_miss_feats > 0) {
  for (col in names(miss_post[miss_post > 0]))
    cat(sprintf("       [WARN] %s: %d NA remain\n", col, miss_post[col]))
}

# 5. Constant feature check
sd_vals <- sapply(FINAL_15, function(col) sd(clustering_ready[[col]], na.rm = TRUE))
const_feats <- names(sd_vals[!is.na(sd_vals) & sd_vals == 0])
v_const <- list(n = length(const_feats), names = const_feats, pass = length(const_feats) == 0)
cat(sprintf("  [%s] Constant features: %d\n",
            if (v_const$pass) "PASS" else "FAIL", v_const$n))

# 6. Range check for shares / HHI
range_cols  <- c("product_hhi", "state_hhi", "pl_share_train")
range_checks <- lapply(range_cols, function(col) {
  x     <- clustering_ready[[col]]
  n_oob <- sum(x < 0 | x > 1, na.rm = TRUE)
  list(feature = col, min = round(min(x), 6), max = round(max(x), 6),
       n_out_of_bounds = n_oob, pass = n_oob == 0)
})
all_range_ok <- all(sapply(range_checks, `[[`, "pass"))
cat(sprintf("  [%s] Share / HHI range [0,1]\n",
            if (all_range_ok) "PASS" else "FAIL"))

# 7. Correlation check
cat("  Computing correlations among final 15 features...\n")
corr_mat   <- cor(as.matrix(clustering_ready[, FINAL_15]), use = "pairwise.complete.obs")
corr_mat_r <- round(corr_mat, 4)

pairs <- list()
for (i in seq_len(nrow(corr_mat))) {
  for (j in seq_len(ncol(corr_mat))) {
    if (j > i) {
      r <- corr_mat[i, j]
      if (!is.na(r)) {
        pairs[[length(pairs) + 1]] <- list(
          feature1    = rownames(corr_mat)[i],
          feature2    = colnames(corr_mat)[j],
          correlation = round(r, 4)
        )
      }
    }
  }
}
pairs     <- pairs[order(sapply(pairs, function(p) -abs(p$correlation)))]
high_corr <- Filter(function(p) abs(p$correlation) >= 0.90, pairs)
top_corr  <- head(pairs, 20)

cat(sprintf("  [INFO] High-correlation pairs (|r| >= 0.90): %d\n", length(high_corr)))
for (p in high_corr) {
  cat(sprintf("         %s × %s  r=%.4f\n", p$feature1, p$feature2, p$correlation))
}

# =============================================================================
# Write agency_profile_clustering_ready.csv
# =============================================================================

cat("\n")
out_ready <- file.path(io_dir, "agency_profile_clustering_ready.csv")
write.csv(clustering_ready, file = out_ready, row.names = FALSE, na = "NA")
cat(sprintf("  %-48s  %.1f KB\n", "agency_profile_clustering_ready.csv",
            file.size(out_ready) / 1024))

# =============================================================================
# Z-score scaling
# =============================================================================

scale_params <- setNames(lapply(FINAL_15, function(col) {
  x     <- clustering_ready[[col]]
  mu    <- mean(x, na.rm = TRUE)
  sigma <- sd(x,   na.rm = TRUE)
  list(
    feature       = col,
    mean_before   = round(mu,    6),
    sd_before     = round(sigma, 6),
    scaled_ok     = !is.na(sigma) && sigma > 0,
    zero_variance = isTRUE(is.na(sigma) || sigma == 0)
  )
}), FINAL_15)

scaled_df <- clustering_ready |> select(AGENCY_ID)
for (col in FINAL_15) {
  sp  <- scale_params[[col]]
  col_out <- paste0("scaled_", col)
  if (sp$scaled_ok) {
    scaled_df[[col_out]] <- round(
      (clustering_ready[[col]] - sp$mean_before) / sp$sd_before, 6
    )
  } else {
    cat(sprintf("  [WARN] %s has zero variance — not scaled (set to NA)\n", col))
    scaled_df[[col_out]] <- NA_real_
  }
}

out_scaled <- file.path(io_dir, "agency_profile_clustering_scaled.csv")
write.csv(scaled_df, file = out_scaled, row.names = FALSE, na = "NA")
cat(sprintf("  %-48s  %.1f KB\n", "agency_profile_clustering_scaled.csv",
            file.size(out_scaled) / 1024))

# =============================================================================
# Feature summary statistics (post-cleaning, unscaled)
# =============================================================================

feat_stats <- function(x, is_share = FALSE) {
  n_total <- length(x)
  n_null  <- sum(is.na(x))
  valid   <- x[!is.na(x)]
  if (length(valid) == 0) {
    return(list(n = n_total, null_count = n_null, null_pct = 100,
                mean = NA, median = NA, sd = NA, min = NA,
                p01 = NA, p05 = NA, p25 = NA, p75 = NA, p95 = NA, p99 = NA,
                max = NA, n_distinct = 0L))
  }
  qs  <- unname(quantile(valid, c(0.01, 0.05, 0.25, 0.75, 0.95, 0.99)))
  out <- list(
    n          = n_total,
    null_count = n_null,
    null_pct   = round(100 * n_null / n_total, 4),
    mean       = round(mean(valid),   6),
    median     = round(median(valid), 6),
    sd         = round(sd(valid),     6),
    min        = round(min(valid),    6),
    p01 = round(qs[1], 6), p05 = round(qs[2], 6),
    p25 = round(qs[3], 6), p75 = round(qs[4], 6),
    p95 = round(qs[5], 6), p99 = round(qs[6], 6),
    max        = round(max(valid),    6),
    n_distinct = length(unique(valid))
  )
  if (is_share) {
    out$bounds_ok       <- all(valid >= 0 & valid <= 1)
    out$n_out_of_bounds <- sum(valid < 0 | valid > 1)
  }
  out
}

share_feats    <- c("product_hhi", "state_hhi", "pl_share_train")
feat_summaries <- setNames(lapply(FINAL_15, function(col) {
  feat_stats(clustering_ready[[col]], is_share = col %in% share_feats)
}), FINAL_15)

# =============================================================================
# Build future RF evaluation metadata
# =============================================================================

future_rf_meta <- list(
  primary_comparison = list(
    label            = "Seen-agency test population",
    description      = "2013-2014 test rows whose AGENCY_ID was observed in 2006-2012 training",
    rationale        = "Direct test of whether agency clustering improves holdout prediction when historical agency information is available",
    n_test_agencies_seen_in_train = coverage$n_test_agencies_seen_in_train,
    pct_test_agencies_seen        = coverage$pct_test_agencies_seen,
    n_test_rows_seen_agency       = coverage$n_test_rows_seen_agency,
    pct_test_rows_seen_agency     = coverage$pct_test_rows_seen_agency
  ),
  secondary_sensitivity = list(
    label            = "Full test population (deployment-style)",
    description      = "All 2013-2014 test rows; test-only agencies assigned NEW_AGENCY cluster label",
    rationale        = "Reported separately; not used for primary model selection",
    n_test_only_agencies = coverage$n_test_only_agencies,
    pct_test_only        = coverage$pct_test_only
  ),
  clustering_ready_scope = list(
    note     = "agency_profile_clustering_ready.csv contains all 1,254 training-period agencies. Seen-agency filtering occurs during RF evaluation, not here.",
    n_training_agencies = as.integer(n_agencies),
    n_test_agencies     = as.integer(coverage$n_test_agencies)
  )
)

# =============================================================================
# Write JSON outputs
# =============================================================================

cat("\nWriting JSON outputs...\n")

# Derive the post-clean summary values inside the JSON
ap_post <- clustering_ready$mean_active_producers_train_clean
sentinel_result <- list(
  n_sentinel_recoded    = as.integer(n_sentinel_ap),
  pct_sentinel          = round(100 * n_sentinel_ap / n_agencies, 2),
  n_imputed_to_median   = as.integer(n_ap_na_before),
  median_impute_value   = round(ap_impute_median, 4),
  post_clean_min        = round(min(ap_post),    4),
  post_clean_median     = round(median(ap_post), 4),
  post_clean_mean       = round(mean(ap_post),   4),
  post_clean_max        = round(max(ap_post),    4)
)

# --- agency_profile_clustering_prep_audit.json ---
prep_audit <- list(
  purpose = paste0(
    "Produce a clean, imputed, z-score-scaled 15-feature agency profile matrix for k-means clustering. ",
    "Removes constant vendor features, collinear agency-characteristic features, and redundant ",
    "median/top-share features. Applies sentinel recoding and 0/median imputation. Does NOT run k-means."
  ),
  input_files = c(
    "agency_profile_dataset.csv",
    "agency_profile_audit.json",
    "agency_profile_feature_summary.json",
    "agency_profile_correlations.json"
  ),
  output_files = c(
    "agency_profile_clustering_ready.csv",
    "agency_profile_clustering_scaled.csv",
    "agency_profile_clustering_prep_audit.json",
    "agency_profile_clustering_feature_summary.json",
    "agency_profile_clustering_correlations.json"
  ),
  leakage_statement = "No database access. All operations on training-period agency profiles only. No test data used.",
  n_agencies = as.integer(n_agencies),
  final_feature_set = list(
    n_features    = 15L,
    feature_names = FINAL_15,
    groups = list(
      support_credibility  = c("log_n_train_rows", "n_train_years_active"),
      premium_policy_scale = c("mean_log_wp_train", "mean_log_prev_wp_train", "mean_log_prev_poly_train"),
      volatility_stability = c("sd_log_wp_train", "iqr_log_wp_train", "sd_log_prev_poly_train"),
      agency_characteristic = "mean_active_producers_train_clean",
      breadth              = c("n_products_train", "n_states_train"),
      concentration        = c("product_hhi", "state_hhi"),
      product_line_mix     = "pl_share_train",
      trend                = "slope_mean_log_wp_by_year"
    )
  ),
  excluded_features = EXCLUDED_REASONS,
  cleaning_rules_applied = list(
    mean_active_producers_train_clean = list(
      source             = "mean_active_producers_train",
      sentinel_threshold = SENTINEL_AP,
      sentinel_action    = "values >= 99999 recoded to NA before imputation",
      imputation_method  = "median of non-sentinel values",
      result             = sentinel_result
    ),
    sd_log_wp_train = list(
      imputation_method = "NA → 0 (single-row agencies have no observed within-agency volatility)",
      n_imputed = as.integer(n_sd_wp_imp)
    ),
    sd_log_prev_poly_train = list(
      imputation_method = "NA → 0",
      n_imputed = as.integer(n_sd_poly_imp)
    ),
    slope_mean_log_wp_by_year = list(
      imputation_method = "NA → 0 (insufficient history; support indicators log_n_train_rows and n_train_years_active remain in feature set)",
      n_imputed = as.integer(n_slope_imp)
    )
  ),
  missingness_before_cleaning = list(
    sd_log_wp_train           = list(n_na = as.integer(n_sd_wp_imp),   pct_na = round(100 * n_sd_wp_imp   / n_agencies, 2)),
    sd_log_prev_poly_train    = list(n_na = as.integer(n_sd_poly_imp), pct_na = round(100 * n_sd_poly_imp / n_agencies, 2)),
    slope_mean_log_wp_by_year = list(n_na = as.integer(n_slope_imp),   pct_na = round(100 * n_slope_imp   / n_agencies, 2)),
    mean_active_producers_train_sentinel = list(
      n_sentinel = as.integer(n_sentinel_ap),
      pct        = round(100 * n_sentinel_ap / n_agencies, 2)
    )
  ),
  missingness_after_cleaning = v_miss_post,
  constant_feature_check     = v_const,
  range_checks               = range_checks,
  scaling = list(
    method       = "z-score: scaled_x = (x - mean(x)) / sd(x)",
    scaled_names = paste0("scaled_", FINAL_15),
    parameters   = scale_params
  ),
  validation_checks = list(
    row_count          = v_rowcount,
    one_row_per_agency = v_unique,
    feature_count      = v_feat_count,
    missingness_post   = v_miss_post,
    constant_features  = v_const,
    share_range        = list(pass = all_range_ok, per_feature = range_checks)
  ),
  high_correlation_result = list(
    n_pairs_gte_090 = length(high_corr),
    pairs           = high_corr
  ),
  future_rf_evaluation_plan = future_rf_meta,
  recommendation = list(
    safe_for_kmeans = v_miss_post$pass && v_const$pass && all_range_ok,
    input_for_kmeans = "agency_profile_clustering_scaled.csv",
    n_features       = 15L,
    note = paste0(
      if (length(high_corr) > 0) {
        sprintf(
          "Note: %d high-correlation pair(s) remain (|r|>=0.90). These are accepted as structural ",
          length(high_corr)
        )
      } else {
        "No high-correlation pairs (|r|>=0.90) remain after feature exclusion. "
      },
      "The 15-feature cleaned matrix is ready for k-means. Use agency_profile_clustering_scaled.csv."
    )
  )
)

out_audit <- file.path(io_dir, "agency_profile_clustering_prep_audit.json")
write(toJSON(prep_audit, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_audit)
cat(sprintf("  %-48s  %.1f KB\n", "agency_profile_clustering_prep_audit.json",
            file.size(out_audit) / 1024))

# --- agency_profile_clustering_feature_summary.json ---
feat_sum_out <- list(
  description  = "Descriptive statistics for the final 15 clustering features after all cleaning. Unscaled (interpretable) values.",
  n_agencies   = n_agencies,
  scaling_note = "Use agency_profile_clustering_scaled.csv for k-means input. Statistics here are pre-scaling.",
  features     = feat_summaries
)
out_feat_sum <- file.path(io_dir, "agency_profile_clustering_feature_summary.json")
write(toJSON(feat_sum_out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_feat_sum)
cat(sprintf("  %-48s  %.1f KB\n", "agency_profile_clustering_feature_summary.json",
            file.size(out_feat_sum) / 1024))

# --- agency_profile_clustering_correlations.json ---
corr_mat_list <- setNames(
  lapply(seq_len(nrow(corr_mat_r)), function(i) as.list(corr_mat_r[i, ])),
  rownames(corr_mat_r)
)

corr_note <- if (length(high_corr) == 0) {
  paste0(
    "No feature pairs with |r| >= 0.90 remain in the final 15 features after exclusions. ",
    "The feature set has acceptable multicollinearity for k-means clustering."
  )
} else {
  paste0(
    sprintf("%d pair(s) with |r| >= 0.90 remain. ", length(high_corr)),
    "Review before finalizing k value or feature selection."
  )
}

corr_out <- list(
  description        = "Pairwise Pearson correlations among the final 15 cleaned clustering features (unscaled).",
  n_agencies         = n_agencies,
  feature_names      = FINAL_15,
  correlation_matrix = corr_mat_list,
  top_20_by_abs      = top_corr,
  high_corr_gte_090  = high_corr,
  n_high_corr        = length(high_corr),
  commentary         = corr_note
)
out_corr <- file.path(io_dir, "agency_profile_clustering_correlations.json")
write(toJSON(corr_out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_corr)
cat(sprintf("  %-48s  %.1f KB\n", "agency_profile_clustering_correlations.json",
            file.size(out_corr) / 1024))

cat("\n=== Clustering prep complete ===\n")
cat(sprintf("Outputs in: %s\n", io_dir))
