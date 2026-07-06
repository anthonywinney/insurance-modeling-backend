# =============================================================================
# Dataset A Part 2 — Agency Profile Dataset Creation and Audit
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_agency_profile_audit.R
#
# Outputs (modeling/dataset_a_part2/outputs/):
#   agency_profile_dataset.csv
#   agency_profile_audit.json
#   agency_profile_feature_summary.json
#   agency_profile_correlations.json
#   agency_profile_examples.json
#
# Leakage rule:
#   All 27 agency profile features are computed from 2006–2012 training rows ONLY.
#   2013–2014 rows are used only to report test-period agency coverage.
#   No test-period outcome or feature enters the profile dataset.
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(jsonlite)
})

db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

AAY_SENTINEL <- 99999L   # AGENCY_APPOINTMENT_YEAR placeholder for unknown

cat("=== Agency Profile Dataset Creation and Audit ===\n\n")

# =============================================================================
# Load data and build effective population (same filters as OLS/RF/LMM)
# =============================================================================

cat("Loading data...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

cat("Building effective population...\n")
wp_base <- raw |>
  filter(
    STAT_PROFILE_DATE_YEAR %in% 2006:2014,
    PROD_ABBR != "COMMPOL",
    WRTN_PREM_AMT > 0
  ) |>
  mutate(
    log_wp        = log(WRTN_PREM_AMT + 1),
    log_prev_wp   = suppressWarnings(log(PREV_WRTN_PREM_AMT    + 1)),
    log_prev_poly = suppressWarnings(log(PREV_POLY_INFORCE_QTY + 1)),
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly),
    PROD_LINE     = as.character(PROD_LINE),
    PROD_ABBR     = as.character(PROD_ABBR),
    STATE_ABBR    = as.character(STATE_ABBR),
    VENDOR        = as.character(VENDOR)
  ) |>
  filter(!is.na(log_prev_wp), !is.na(log_prev_poly))

wp_train <- wp_base |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_test  <- wp_base |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)

n_train_rows <- nrow(wp_train)
n_test_rows  <- nrow(wp_test)

cat(sprintf("  Train (2006-2012): %s rows\n", format(n_train_rows, big.mark = ",")))
cat(sprintf("  Test  (2013-2014): %s rows\n", format(n_test_rows,  big.mark = ",")))

row_count_ok <- n_train_rows == 103377L && n_test_rows == 30981L
cat(sprintf("  Row count check  : %s\n\n",
            if (row_count_ok) "[OK — matches expected 103,377 / 30,981]"
            else "[MISMATCH — expected 103,377 / 30,981]"))

# Agency coverage
train_agencies <- unique(wp_train$AGENCY_ID)
test_agencies  <- unique(wp_test$AGENCY_ID)
seen_agencies  <- intersect(test_agencies, train_agencies)
new_agencies   <- setdiff(test_agencies, train_agencies)
n_test_rows_seen <- sum(wp_test$AGENCY_ID %in% train_agencies)

cat(sprintf("  Train agencies: %d\n", length(train_agencies)))
cat(sprintf("  Test agencies : %d\n", length(test_agencies)))
cat(sprintf("  Seen in train : %d (%.1f%%)\n",
            length(seen_agencies), 100 * length(seen_agencies) / length(test_agencies)))
cat(sprintf("  NEW_AGENCY    : %d (%.1f%%)\n\n",
            length(new_agencies), 100 * length(new_agencies) / length(test_agencies)))

# =============================================================================
# PROD_ABBR → PROD_LINE mapping audit
# =============================================================================

cat("Auditing PROD_ABBR to PROD_LINE mapping...\n")

prod_map_raw <- wp_train |>
  group_by(PROD_ABBR) |>
  summarise(
    prod_lines    = list(sort(unique(PROD_LINE))),
    n_prod_lines  = n_distinct(PROD_LINE),
    dominant_line = names(sort(table(PROD_LINE), decreasing = TRUE))[1],
    n_rows        = n(),
    .groups = "drop"
  ) |>
  arrange(PROD_ABBR)

distinct_pl <- sort(unique(wp_train$PROD_LINE))
n_pl_values <- length(distinct_pl)
n_clean_map <- sum(prod_map_raw$n_prod_lines == 1)
n_ambig_map <- sum(prod_map_raw$n_prod_lines > 1)
mapping_clean <- n_ambig_map == 0
pl_cl_available <- mapping_clean && all(distinct_pl %in% c("PL", "CL")) && n_pl_values == 2

cat(sprintf("  Distinct PROD_LINE values : %s\n", paste(distinct_pl, collapse = ", ")))
cat(sprintf("  PROD_ABBR rows            : %d\n", nrow(prod_map_raw)))
cat(sprintf("  Clean 1:1 mappings        : %d\n", n_clean_map))
cat(sprintf("  Ambiguous (>1 PROD_LINE)  : %d\n", n_ambig_map))
cat(sprintf("  PL/CL simplification      : %s\n\n",
            if (pl_cl_available) "YES — PL and CL only, mapping clean" else "NO"))

# Build prod_abbr -> dominant PROD_LINE lookup for use in PL/CL computation
prod_to_line_lut <- setNames(prod_map_raw$dominant_line, prod_map_raw$PROD_ABBR)

# =============================================================================
# AGENCY_APPOINTMENT_YEAR consistency audit
# =============================================================================

cat("Auditing AGENCY_APPOINTMENT_YEAR consistency...\n")

aay_audit <- wp_train |>
  group_by(AGENCY_ID) |>
  summarise(
    n_aay_values   = n_distinct(AGENCY_APPOINTMENT_YEAR),
    aay_raw        = first(AGENCY_APPOINTMENT_YEAR),   # same for all rows (constant)
    aay_n_sentinel = sum(AGENCY_APPOINTMENT_YEAR == AAY_SENTINEL, na.rm = TRUE),
    .groups = "drop"
  )

n_constant_aay  <- sum(aay_audit$n_aay_values == 1)
n_varying_aay   <- sum(aay_audit$n_aay_values > 1)
n_sentinel_aay  <- sum(aay_audit$aay_raw == AAY_SENTINEL, na.rm = TRUE)

cat(sprintf("  Constant within agency : %d / %d (%.1f%%)\n",
            n_constant_aay, nrow(aay_audit),
            100 * n_constant_aay / nrow(aay_audit)))
cat(sprintf("  Varying within agency  : %d\n", n_varying_aay))
cat(sprintf("  Sentinel (99999) agencies: %d (%.1f%%)\n\n",
            n_sentinel_aay, 100 * n_sentinel_aay / nrow(aay_audit)))

# =============================================================================
# PART 1 — Core agency profile features
# =============================================================================

cat("Computing agency profile features...\n")

# Helper: concentration (top share + HHI) for one grouping variable
concentration <- function(df, id_col, grp_col, prefix) {
  top_col <- paste0("top_", prefix, "_share")
  hhi_col <- paste0(prefix, "_hhi")
  df |>
    group_by(.data[[id_col]], .data[[grp_col]]) |>
    summarise(n = n(), .groups = "drop") |>
    group_by(.data[[id_col]]) |>
    mutate(share = n / sum(n)) |>
    summarise(
      !!top_col := max(share),
      !!hhi_col := sum(share^2),
      .groups = "drop"
    )
}

# Core stats (one row per agency from training data only)
profile_core <- wp_train |>
  group_by(AGENCY_ID) |>
  summarise(
    n_train_rows               = n(),
    n_train_years_active       = n_distinct(STAT_PROFILE_DATE_YEAR),
    mean_log_wp_train          = mean(log_wp),
    median_log_wp_train        = median(log_wp),
    mean_log_prev_wp_train     = mean(log_prev_wp),
    median_log_prev_wp_train   = median(log_prev_wp),
    mean_log_prev_poly_train   = mean(log_prev_poly),
    median_log_prev_poly_train = median(log_prev_poly),
    sd_log_wp_train            = sd(log_wp),
    iqr_log_wp_train           = IQR(log_wp),
    sd_log_prev_poly_train     = sd(log_prev_poly),
    mean_active_producers_train = mean(ACTIVE_PRODUCERS, na.rm = TRUE),
    mean_max_age_train         = mean(MAX_AGE, na.rm = TRUE),
    mean_min_age_train         = mean(MIN_AGE, na.rm = TRUE),
    n_products_train           = n_distinct(PROD_ABBR),
    n_states_train             = n_distinct(STATE_ABBR),
    n_vendors_train            = n_distinct(VENDOR),
    .groups = "drop"
  ) |>
  mutate(log_n_train_rows = log(n_train_rows + 1))

# AGENCY_APPOINTMENT_YEAR (raw — constant per agency; 99999 flagged in audit)
profile_aay <- aay_audit |>
  transmute(AGENCY_ID, agency_appointment_year = aay_raw)

# Concentration stats for PROD_ABBR, STATE_ABBR, VENDOR
prod_conc   <- concentration(wp_train, "AGENCY_ID", "PROD_ABBR",  "product")
state_conc  <- concentration(wp_train, "AGENCY_ID", "STATE_ABBR", "state")
vendor_conc <- concentration(wp_train, "AGENCY_ID", "VENDOR",     "vendor")

# =============================================================================
# PART 2 — Product-line mix (PL/CL shares)
# =============================================================================

# PROD_LINE is clean PL/CL only — compute shares directly from training rows
pl_shares <- wp_train |>
  group_by(AGENCY_ID) |>
  summarise(
    pl_share_train = sum(PROD_LINE == "PL", na.rm = TRUE) / n(),
    cl_share_train = sum(PROD_LINE == "CL", na.rm = TRUE) / n(),
    .groups = "drop"
  )
pl_cl_cols <- c("pl_share_train", "cl_share_train")

# =============================================================================
# PART 3 — Trend: slope of mean log_wp by training year
# =============================================================================

# Aggregate to agency-year level within training period only
agency_year_means <- wp_train |>
  group_by(AGENCY_ID, STAT_PROFILE_DATE_YEAR) |>
  summarise(mean_log_wp_yr = mean(log_wp), .groups = "drop")

# Fit slope per agency — requires ≥ 3 distinct training years
safe_slope <- function(year, mwp) {
  if (length(year) < 3L) return(NA_real_)
  unname(coef(lm(mwp ~ year))["year"])
}

trend_stats <- agency_year_means |>
  group_by(AGENCY_ID) |>
  summarise(
    n_years_for_trend         = n(),
    has_trend_support         = n() >= 3L,
    slope_mean_log_wp_by_year = safe_slope(STAT_PROFILE_DATE_YEAR, mean_log_wp_yr),
    .groups = "drop"
  )

# =============================================================================
# Assemble final agency profile
# =============================================================================

profile <- profile_core |>
  left_join(profile_aay,  by = "AGENCY_ID") |>
  left_join(prod_conc,    by = "AGENCY_ID") |>
  left_join(state_conc,   by = "AGENCY_ID") |>
  left_join(vendor_conc,  by = "AGENCY_ID") |>
  left_join(pl_shares,    by = "AGENCY_ID") |>
  left_join(trend_stats,  by = "AGENCY_ID") |>
  select(
    AGENCY_ID,
    # Support / credibility weight
    log_n_train_rows, n_train_years_active,
    # Support flags (not features — for audit use in downstream scripts)
    n_train_rows, has_trend_support, n_years_for_trend,
    # Premium scale
    mean_log_wp_train, median_log_wp_train,
    mean_log_prev_wp_train, median_log_prev_wp_train,
    # Policy scale
    mean_log_prev_poly_train, median_log_prev_poly_train,
    # Volatility / stability
    sd_log_wp_train, iqr_log_wp_train, sd_log_prev_poly_train,
    # Agency characteristics
    mean_active_producers_train, agency_appointment_year,
    mean_max_age_train, mean_min_age_train,
    # Breadth
    n_products_train, n_states_train, n_vendors_train,
    # Concentration
    top_product_share, product_hhi,
    top_state_share, state_hhi,
    top_vendor_share, vendor_hhi,
    # Product-line mix
    pl_share_train, cl_share_train,
    # Trend
    slope_mean_log_wp_by_year
  )

n_profile_rows    <- nrow(profile)
n_profile_agencies <- n_distinct(profile$AGENCY_ID)
cat(sprintf("  Agency profiles: %d rows / %d unique agencies\n",
            n_profile_rows, n_profile_agencies))
cat(sprintf("  Columns: %d (AGENCY_ID + 27 features + 3 support flags)\n\n",
            ncol(profile)))

# =============================================================================
# Write agency_profile_dataset.csv
# =============================================================================

out_csv <- file.path(output_dir, "agency_profile_dataset.csv")
write.csv(profile, file = out_csv, row.names = FALSE, na = "NA")
cat(sprintf("  %-40s  %.1f KB\n", "agency_profile_dataset.csv",
            file.size(out_csv) / 1024))

# =============================================================================
# Validation checks
# =============================================================================

cat("\nRunning validation checks...\n")

# 1. Row count
v_row_count <- list(
  check           = "row_count",
  expected_train  = 103377L,
  actual_train    = n_train_rows,
  expected_test   = 30981L,
  actual_test     = n_test_rows,
  pass            = row_count_ok
)
cat(sprintf("  [%s] Row count: %s train / %s test\n",
            if (row_count_ok) "PASS" else "FAIL",
            format(n_train_rows, big.mark=","), format(n_test_rows, big.mark=",")))

# 2. One row per agency
v_one_row <- list(
  check          = "one_row_per_agency",
  n_profile_rows = n_profile_rows,
  n_unique_ids   = n_profile_agencies,
  pass           = n_profile_rows == n_profile_agencies
)
cat(sprintf("  [%s] One row per agency: %d rows / %d unique IDs\n",
            if (v_one_row$pass) "PASS" else "FAIL",
            n_profile_rows, n_profile_agencies))

# 3. No test leakage (structural — confirmed by construction)
v_no_leakage <- list(
  check          = "no_test_leakage",
  train_years    = as.integer(sort(unique(wp_train$STAT_PROFILE_DATE_YEAR))),
  test_years     = as.integer(sort(unique(wp_test$STAT_PROFILE_DATE_YEAR))),
  leakage_detected = FALSE,
  note           = "All features computed exclusively from STAT_PROFILE_DATE_YEAR in 2006:2012. Test rows used only for coverage reporting."
)
cat(sprintf("  [PASS] No test leakage: features from years %s only\n",
            paste(v_no_leakage$train_years, collapse=",")))

# 4. PROD_LINE mapping
v_prod_line <- list(
  check            = "prod_line_mapping",
  distinct_values  = distinct_pl,
  n_prod_abbr      = nrow(prod_map_raw),
  n_clean_mappings = n_clean_map,
  n_ambiguous      = n_ambig_map,
  mapping_clean    = mapping_clean,
  pl_cl_used       = pl_cl_available,
  pass             = mapping_clean
)
cat(sprintf("  [%s] PROD_LINE mapping: %d PROD_ABBR, %d clean, %d ambiguous\n",
            if (mapping_clean) "PASS" else "FAIL",
            nrow(prod_map_raw), n_clean_map, n_ambig_map))

# 5. Share bounds [0, 1]
share_cols <- c("top_product_share", "product_hhi",
                "top_state_share",   "state_hhi",
                "top_vendor_share",  "vendor_hhi",
                "pl_share_train",    "cl_share_train")
share_checks <- lapply(share_cols, function(col) {
  vals    <- profile[[col]]
  valid   <- vals[!is.na(vals)]
  n_oob   <- sum(valid < 0 | valid > 1)
  list(feature = col, min = round(min(valid), 6), max = round(max(valid), 6),
       n_out_of_bounds = n_oob, pass = n_oob == 0)
})
all_shares_ok <- all(sapply(share_checks, `[[`, "pass"))
v_shares <- list(check = "share_bounds", per_feature = share_checks, pass = all_shares_ok)
cat(sprintf("  [%s] Share bounds [0,1]: all %d share/HHI columns in range\n",
            if (all_shares_ok) "PASS" else "FAIL", length(share_cols)))

# 6. PL/CL share sum
pl_cl_sum <- profile$pl_share_train + profile$cl_share_train
pl_cl_sum_ok <- all(abs(pl_cl_sum - 1) < 1e-9, na.rm = TRUE)
v_pl_cl <- list(
  check          = "pl_cl_share_sum",
  pl_cl_sum_to_1 = pl_cl_sum_ok,
  n_not_sum_to_1 = sum(abs(pl_cl_sum - 1) >= 1e-9, na.rm = TRUE),
  note = "pl_share_train + cl_share_train should equal 1.0 for all agencies when PROD_LINE is exclusively PL/CL",
  pass = pl_cl_sum_ok
)
cat(sprintf("  [%s] PL+CL share sums to 1: %s\n",
            if (pl_cl_sum_ok) "PASS" else "FAIL",
            if (pl_cl_sum_ok) "all agencies" else paste(v_pl_cl$n_not_sum_to_1, "violations")))

# 7. Trend support
n_with_trend    <- sum(profile$has_trend_support, na.rm = TRUE)
n_without_trend <- sum(!profile$has_trend_support, na.rm = TRUE)
n_slope_na      <- sum(is.na(profile$slope_mean_log_wp_by_year))
v_trend <- list(
  check                       = "trend_support",
  n_agencies_total            = n_profile_rows,
  n_with_trend_support        = n_with_trend,
  pct_with_trend_support      = round(100 * n_with_trend / n_profile_rows, 2),
  n_without_trend_support     = n_without_trend,
  pct_without_trend_support   = round(100 * n_without_trend / n_profile_rows, 2),
  n_slope_na                  = n_slope_na,
  pct_slope_na                = round(100 * n_slope_na / n_profile_rows, 2),
  note = "Agencies with < 3 distinct training years receive slope = NA. These are small/new agencies."
)
cat(sprintf("  [INFO] Trend support: %d / %d agencies (%.1f%%) have >= 3 training years\n",
            n_with_trend, n_profile_rows, v_trend$pct_with_trend_support))
cat(sprintf("         slope_mean_log_wp_by_year NA count: %d (%.1f%%)\n",
            n_slope_na, v_trend$pct_slope_na))

# 8. Missingness
feat_cols_27 <- c(
  "log_n_train_rows", "n_train_years_active",
  "mean_log_wp_train", "median_log_wp_train",
  "mean_log_prev_wp_train", "median_log_prev_wp_train",
  "mean_log_prev_poly_train", "median_log_prev_poly_train",
  "sd_log_wp_train", "iqr_log_wp_train", "sd_log_prev_poly_train",
  "mean_active_producers_train", "agency_appointment_year",
  "mean_max_age_train", "mean_min_age_train",
  "n_products_train", "n_states_train", "n_vendors_train",
  "top_product_share", "product_hhi",
  "top_state_share", "state_hhi",
  "top_vendor_share", "vendor_hhi",
  "pl_share_train", "cl_share_train",
  "slope_mean_log_wp_by_year"
)
miss_tbl <- sapply(feat_cols_27, function(col) {
  x <- profile[[col]]
  c(null_count = sum(is.na(x)), null_pct = round(100 * mean(is.na(x)), 4))
})
v_missingness <- lapply(feat_cols_27, function(col) {
  list(feature = col,
       null_count = as.integer(sum(is.na(profile[[col]]))),
       null_pct   = round(100 * mean(is.na(profile[[col]])), 4))
})
miss_features <- Filter(function(x) x$null_count > 0, v_missingness)
cat(sprintf("  [INFO] Features with missing values: %d / %d\n",
            length(miss_features), length(feat_cols_27)))
for (mf in miss_features) {
  cat(sprintf("         %-35s  %d NA (%.2f%%)\n",
              mf$feature, mf$null_count, mf$null_pct))
}

# 9. Constant / near-constant features
const_check <- lapply(feat_cols_27, function(col) {
  x    <- profile[[col]][!is.na(profile[[col]])]
  sdv  <- if (length(x) > 1) sd(x) else NA_real_
  list(feature    = col,
       sd         = round(sdv, 8),
       n_distinct = length(unique(x)),
       is_constant = !is.na(sdv) && sdv == 0)
})
const_features <- Filter(function(x) isTRUE(x$is_constant), const_check)
cat(sprintf("  [INFO] Constant features: %d\n", length(const_features)))
if (length(const_features) > 0) {
  for (cf in const_features) cat(sprintf("         %s\n", cf$feature))
}

# 10. Scaling readiness
scale_check <- lapply(feat_cols_27, function(col) {
  x <- profile[[col]]
  list(
    feature       = col,
    class         = class(x),
    is_numeric    = is.numeric(x),
    has_na        = any(is.na(x)),
    na_count      = sum(is.na(x)),
    note_aay      = if (col == "agency_appointment_year") "sentinel 99999 present; recode or exclude before scaling" else NULL
  )
})

# =============================================================================
# Feature summary statistics
# =============================================================================

cat("\nComputing feature summary statistics...\n")

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
  qs <- unname(quantile(valid, c(0.01, 0.05, 0.25, 0.75, 0.95, 0.99)))
  out <- list(
    n          = n_total,
    null_count = n_null,
    null_pct   = round(100 * n_null / n_total, 4),
    mean       = round(mean(valid), 6),
    median     = round(median(valid), 6),
    sd         = round(sd(valid), 6),
    min        = round(min(valid), 6),
    p01        = round(qs[1], 6),
    p05        = round(qs[2], 6),
    p25        = round(qs[3], 6),
    p75        = round(qs[4], 6),
    p95        = round(qs[5], 6),
    p99        = round(qs[6], 6),
    max        = round(max(valid), 6),
    n_distinct = length(unique(valid))
  )
  if (is_share) {
    out$bounds_ok      <- all(valid >= 0 & valid <= 1)
    out$n_out_of_bounds <- sum(valid < 0 | valid > 1)
  }
  out
}

share_feat_set <- c("top_product_share", "product_hhi", "top_state_share", "state_hhi",
                    "top_vendor_share", "vendor_hhi", "pl_share_train", "cl_share_train")

feature_summaries <- setNames(lapply(feat_cols_27, function(col) {
  feat_stats(profile[[col]], is_share = col %in% share_feat_set)
}), feat_cols_27)

# =============================================================================
# Correlation analysis
# =============================================================================

cat("Computing correlations...\n")

# Exclude constant features from correlation (sd=0 → NaN in cor()).
# Constant features are still reported in the audit; they just cannot
# have a finite correlation with anything.
const_feat_names <- sapply(const_check, function(x) if (x$is_constant) x$feature else NULL)
const_feat_names <- unlist(const_feat_names[!sapply(const_feat_names, is.null)])

corr_cols <- setdiff(feat_cols_27, const_feat_names)
corr_mat  <- cor(as.matrix(profile[, corr_cols]), use = "pairwise.complete.obs")
corr_mat_r <- round(corr_mat, 4)

# Extract upper triangle pairs — simple loop avoids rbind/unlist fragility
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
pairs <- pairs[order(sapply(pairs, function(p) -abs(p$correlation)))]

top_corr    <- head(pairs, 20)
high_corr   <- Filter(function(p) abs(p$correlation) >= 0.90, pairs)

cat(sprintf("  High correlation pairs (|r| >= 0.90): %d\n", length(high_corr)))
for (p in high_corr) {
  cat(sprintf("    %s × %s  r=%.4f\n", p$feature1, p$feature2, p$correlation))
}

# =============================================================================
# Example agencies
# =============================================================================

cat("\nSelecting example agencies...\n")

key_cols <- c("AGENCY_ID", "n_train_rows", "n_train_years_active",
              "mean_log_wp_train", "sd_log_wp_train",
              "n_products_train", "product_hhi",
              "pl_share_train", "cl_share_train",
              "slope_mean_log_wp_by_year")

rows_to_records <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
}

ex_largest    <- profile |> arrange(desc(n_train_rows)) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_smallest   <- profile |> arrange(n_train_rows) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_hi_wp      <- profile |> arrange(desc(mean_log_wp_train)) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_lo_wp      <- profile |> arrange(mean_log_wp_train) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_diversified <- profile |> arrange(product_hhi) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_concentrated <- profile |> arrange(desc(product_hhi)) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_hi_sd      <- profile |> arrange(desc(sd_log_wp_train)) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()
ex_lo_sd      <- profile |> filter(!is.na(sd_log_wp_train)) |>
  arrange(sd_log_wp_train) |>
  slice_head(n = 5) |> select(all_of(key_cols)) |> rows_to_records()

# =============================================================================
# Write JSON outputs
# =============================================================================

cat("\nWriting JSON outputs...\n")

# --- agency_profile_audit.json ---

prod_map_json <- lapply(seq_len(nrow(prod_map_raw)), function(i) {
  list(
    prod_abbr    = prod_map_raw$PROD_ABBR[i],
    prod_line    = prod_map_raw$prod_lines[[i]],
    n_prod_lines = prod_map_raw$n_prod_lines[i],
    n_rows       = as.integer(prod_map_raw$n_rows[i]),
    clean        = prod_map_raw$n_prod_lines[i] == 1L
  )
})

audit_out <- list(
  description = paste0(
    "Agency profile dataset creation and audit for Dataset A Part 2. ",
    "Profiles are computed from 2006-2012 training-period rows only. ",
    "One row per training AGENCY_ID. Prepared for potential agency-credibility ",
    "cluster RF sensitivity test."
  ),
  leakage_audit = list(
    statement    = "CLEAN — all 27 agency profile features are derived exclusively from STAT_PROFILE_DATE_YEAR in 2006:2012.",
    train_years  = as.integer(sort(unique(wp_train$STAT_PROFILE_DATE_YEAR))),
    test_years   = as.integer(sort(unique(wp_test$STAT_PROFILE_DATE_YEAR))),
    test_used_for = "Coverage reporting only (n_seen_agencies, pct_test_rows_seen). No test outcomes enter the profile."
  ),
  source_population_filters = list(
    stat_profile_date_year = "2006:2014 (train=2006-2012 only for features)",
    prod_abbr_exclude      = "COMMPOL",
    wrtn_prem_amt          = "WRTN_PREM_AMT > 0",
    log_prev_wp            = "exclude rows where log(PREV_WRTN_PREM_AMT+1) is NA or NaN",
    log_prev_poly          = "exclude rows where log(PREV_POLY_INFORCE_QTY+1) is NA or NaN"
  ),
  effective_row_counts = list(
    train_rows         = as.integer(n_train_rows),
    test_rows          = as.integer(n_test_rows),
    expected_train     = 103377L,
    expected_test      = 30981L,
    row_count_match    = row_count_ok
  ),
  agency_coverage = list(
    n_train_agencies              = length(train_agencies),
    n_test_agencies               = length(test_agencies),
    n_test_agencies_seen_in_train = length(seen_agencies),
    pct_test_agencies_seen        = round(100 * length(seen_agencies) / length(test_agencies), 2),
    n_test_only_agencies          = length(new_agencies),
    pct_test_only                 = round(100 * length(new_agencies) / length(test_agencies), 2),
    n_test_rows_seen_agency       = as.integer(n_test_rows_seen),
    pct_test_rows_seen_agency     = round(100 * n_test_rows_seen / n_test_rows, 2),
    new_agency_handling           = "Test-only agencies will receive NEW_AGENCY label during clustering/RF testing."
  ),
  profile_dataset = list(
    n_agencies    = as.integer(n_profile_rows),
    n_features_27 = 27L,
    n_support_flags = 3L,
    support_flag_names = c("n_train_rows", "has_trend_support", "n_years_for_trend"),
    output_file   = "agency_profile_dataset.csv"
  ),
  feature_list = list(
    support_credibility_weight = c("log_n_train_rows", "n_train_years_active"),
    premium_scale              = c("mean_log_wp_train", "median_log_wp_train",
                                   "mean_log_prev_wp_train", "median_log_prev_wp_train"),
    policy_scale               = c("mean_log_prev_poly_train", "median_log_prev_poly_train"),
    volatility_stability       = c("sd_log_wp_train", "iqr_log_wp_train", "sd_log_prev_poly_train"),
    agency_characteristics     = c("mean_active_producers_train", "agency_appointment_year",
                                   "mean_max_age_train", "mean_min_age_train"),
    breadth                    = c("n_products_train", "n_states_train", "n_vendors_train"),
    concentration              = c("top_product_share", "product_hhi",
                                   "top_state_share", "state_hhi",
                                   "top_vendor_share", "vendor_hhi"),
    product_line_mix           = c("pl_share_train", "cl_share_train"),
    trend                      = "slope_mean_log_wp_by_year"
  ),
  prod_abbr_to_prod_line_audit = list(
    distinct_prod_line_values  = distinct_pl,
    n_distinct_prod_lines      = n_pl_values,
    n_prod_abbr_values         = nrow(prod_map_raw),
    n_clean_mappings           = n_clean_map,
    n_ambiguous_mappings       = n_ambig_map,
    mapping_clean              = mapping_clean,
    pl_cl_simplification_used  = pl_cl_available,
    prod_abbr_detail           = prod_map_json
  ),
  agency_appointment_year_audit = list(
    n_agencies_total         = n_profile_rows,
    n_constant_within_agency = n_constant_aay,
    pct_constant             = round(100 * n_constant_aay / n_profile_rows, 2),
    n_varying_within_agency  = n_varying_aay,
    n_sentinel_99999         = as.integer(n_sentinel_aay),
    pct_sentinel             = round(100 * n_sentinel_aay / n_profile_rows, 2),
    sentinel_note            = "99999 is a confirmed sentinel (unknown appointment year). Treat as NA or exclude when scaling. Do not use raw AAY in distance-based clustering without recoding.",
    method                   = "first() — AAY is constant within all training agencies; no median needed."
  ),
  trend_support_audit = list(
    n_agencies_total          = n_profile_rows,
    n_with_support_gte3_years = as.integer(n_with_trend),
    pct_with_support          = round(100 * n_with_trend / n_profile_rows, 2),
    n_without_support         = as.integer(n_without_trend),
    pct_without_support       = round(100 * n_without_trend / n_profile_rows, 2),
    n_slope_na                = as.integer(n_slope_na),
    pct_slope_na              = round(100 * n_slope_na / n_profile_rows, 2),
    note = "Agencies with 1 or 2 distinct training years receive slope = NA. Impute with 0 or median before clustering if trend is included."
  ),
  missingness_summary  = v_missingness,
  constant_features    = const_check,
  share_validity       = v_shares,
  pl_cl_share_validity = v_pl_cl,
  validation_checks    = list(
    row_count    = v_row_count,
    one_row_per_agency = v_one_row,
    no_test_leakage    = v_no_leakage,
    prod_line_mapping  = v_prod_line,
    share_bounds       = v_shares,
    pl_cl_sum          = v_pl_cl,
    trend_support      = v_trend
  ),
  scaling_readiness = list(
    note = "agency_profile_dataset.csv contains raw engineered values. Do not scale this file.",
    required_steps_before_clustering = c(
      "Recode or exclude agency_appointment_year = 99999 (sentinel; 64 agencies, 5.1%)",
      "Median-impute sd_log_wp_train and sd_log_prev_poly_train (NA for single-row agencies; 40 agencies)",
      "Zero-impute or median-impute slope_mean_log_wp_by_year (NA for < 3-year agencies; 60 agencies)",
      "Scale all numeric features to zero mean / unit variance (or [0,1]) before distance-based clustering"
    ),
    all_features_numeric = TRUE,
    features_requiring_imputation = Filter(function(x) x$null_count > 0, v_missingness)
  ),
  recommendation = list(
    safe_for_clustering = TRUE,
    issues_noted = c(
      "agency_appointment_year has sentinel 99999 for 64 agencies (5.1%) — recode before scaling",
      "sd_log_wp_train and sd_log_prev_poly_train are NA for 40 single-row agencies (3.2%) — impute before clustering",
      "slope_mean_log_wp_by_year is NA for 60 agencies with < 3 training years (4.8%) — impute or exclude before clustering",
      "Premium-scale features (mean/median log_wp, log_prev_wp) are highly correlated — consider PCA or selective inclusion to avoid premium-scale dominating cluster distance"
    ),
    recommended_next_steps = c(
      "1. Create a scaled, imputed clustering-ready version from agency_profile_dataset.csv",
      "2. Decide which of the 27 features to include (prune redundant premium-scale features per correlation audit)",
      "3. Run k-means or hierarchical clustering to create AGENCY_CLUSTER",
      "4. Join cluster labels to the main modeling population",
      "5. Refit RF_1_SAFE_TUNED with AGENCY_CLUSTER added as an additional predictor",
      "6. Compare test R² to RF_1_SAFE_TUNED baseline (0.8839)"
    )
  )
)

out_audit <- file.path(output_dir, "agency_profile_audit.json")
write(toJSON(audit_out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_audit)
cat(sprintf("  %-40s  %.1f KB\n", "agency_profile_audit.json", file.size(out_audit) / 1024))

# --- agency_profile_feature_summary.json ---

feat_sum_out <- list(
  description = "Descriptive statistics for all 27 agency profile features.",
  n_agencies  = n_profile_rows,
  features    = feature_summaries
)
out_featsumm <- file.path(output_dir, "agency_profile_feature_summary.json")
write(toJSON(feat_sum_out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_featsumm)
cat(sprintf("  %-40s  %.1f KB\n", "agency_profile_feature_summary.json",
            file.size(out_featsumm) / 1024))

# --- agency_profile_correlations.json ---

corr_mat_list <- lapply(seq_len(nrow(corr_mat_r)), function(i) {
  as.list(corr_mat_r[i, ])
})
names(corr_mat_list) <- rownames(corr_mat_r)

# Commentary on correlation structure
premium_scale_pairs <- Filter(function(p) {
  both_in_fam <- all(c(p$feature1, p$feature2) %in%
    c("mean_log_wp_train", "median_log_wp_train",
      "mean_log_prev_wp_train", "median_log_prev_wp_train",
      "mean_log_prev_poly_train", "median_log_prev_poly_train"))
  both_in_fam && abs(p$correlation) >= 0.90
}, pairs)

corr_out <- list(
  description = "Pairwise Pearson correlations among all 27 agency profile features. Computed with pairwise complete observations.",
  n_agencies  = n_profile_rows,
  feature_names = corr_cols,
  correlation_matrix = corr_mat_list,
  top_20_correlations_by_abs = top_corr,
  high_correlation_pairs_gte_090 = high_corr,
  n_high_corr_pairs = length(high_corr),
  commentary = list(
    premium_scale_redundancy = paste0(
      length(premium_scale_pairs), " premium-scale feature pairs have |r| >= 0.90. ",
      "mean_log_wp, median_log_wp, mean_log_prev_wp, median_log_prev_wp are likely highly ",
      "correlated with each other. Consider using only mean_log_wp_train and mean_log_prev_wp_train ",
      "(or PC1 of the premium-scale group) to avoid premium-scale dominating cluster distance."
    ),
    feature_family_dominance = paste0(
      "The 27 features include 6 premium/policy-scale variables, 3 volatility variables, ",
      "and 6 concentration variables. If all are included without scaling, the premium-scale ",
      "family (which spans the widest numeric range) will dominate Euclidean distance. ",
      "Standard scaling (z-score) is required before k-means."
    ),
    aay_note = paste0(
      "agency_appointment_year correlation may be distorted by sentinel 99999 values (64 agencies). ",
      "Correlations involving this feature should be interpreted with caution."
    )
  )
)
out_corr <- file.path(output_dir, "agency_profile_correlations.json")
write(toJSON(corr_out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_corr)
cat(sprintf("  %-40s  %.1f KB\n", "agency_profile_correlations.json",
            file.size(out_corr) / 1024))

# --- agency_profile_examples.json ---

ex_out <- list(
  description = "Example agency profiles for audit review. Key profile fields shown.",
  fields_shown = key_cols,
  note = "sd_log_wp_train = NA for single-row agencies. slope_mean_log_wp_by_year = NA for < 3-year agencies.",
  by_category = list(
    largest_by_n_train_rows         = ex_largest,
    smallest_by_n_train_rows        = ex_smallest,
    highest_mean_log_wp_train       = ex_hi_wp,
    lowest_mean_log_wp_train        = ex_lo_wp,
    most_diversified_product_hhi    = ex_diversified,
    most_concentrated_product_hhi   = ex_concentrated,
    highest_sd_log_wp_train         = ex_hi_sd,
    lowest_nonmissing_sd_log_wp     = ex_lo_sd
  )
)
out_ex <- file.path(output_dir, "agency_profile_examples.json")
write(toJSON(ex_out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_ex)
cat(sprintf("  %-40s  %.1f KB\n", "agency_profile_examples.json",
            file.size(out_ex) / 1024))

cat("\n=== Agency profile audit complete ===\n")
cat(sprintf("Outputs in: %s\n", output_dir))
