# =============================================================================
# Dataset A Part 2 — Written Premium OLS Model Progression
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_wp_ols.R
#
# Outputs:
#   modeling/dataset_a_part2/outputs/ols_model_comparison.json
#   modeling/dataset_a_part2/outputs/ols_coefficients.json
#   modeling/dataset_a_part2/outputs/ols_summary.json
#   modeling/dataset_a_part2/outputs/ols5_retention_comparison.json
#   modeling/dataset_a_part2/outputs/ols_additive_final.json
#   modeling/dataset_a_part2/outputs/ols_interaction_full.json
#
# Required packages: DBI, RSQLite, dplyr, jsonlite
# Models: OLS 0-4 + OLS 3CC/4CC decomposition + OLS 5 PREV_RETENTION_RATIO test
#         + OLS_ADDITIVE_FINAL (clean additive baseline, PROD_LINE removed)
#         + OLS_INTERACTION_FULL (all pairwise interactions from additive baseline)
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(jsonlite)
})

SENTINELS  <- c(99997, 99998, 99999)
db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Dataset A Part 2 — WP OLS Model Progression ===\n\n")

# =============================================================================
# Load raw data
# =============================================================================

cat("Connecting to database...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n", format(nrow(raw), big.mark = ","), ncol(raw)))

# =============================================================================
# Build modeling dataset
# =============================================================================

cat("Building modeling dataset...\n")

# Prior-year lookup for PREV_LOSS_RATIO and PREV_RETENTION_RATIO.
# Sentinel values are copied faithfully; excluded below in the feature columns.
prior_yr_lookup <- raw |>
  select(
    AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR,
    prior_year  = STAT_PROFILE_DATE_YEAR,
    prev_lr_raw = LOSS_RATIO,
    prev_rr_raw = RETENTION_RATIO
  ) |>
  mutate(current_year = prior_year + 1L)

# Base population: non-COMMPOL, WP > 0, years 2006-2014.
# Factor levels are set from the full dataset before splitting so that
# test-set factor levels are always a subset of training levels.
wp_base <- raw |>
  filter(
    PROD_ABBR != "COMMPOL",
    WRTN_PREM_AMT > 0,
    STAT_PROFILE_DATE_YEAR %in% 2006:2014
  ) |>
  mutate(
    log_wp        = log(WRTN_PREM_AMT + 1),
    log_prev_wp   = suppressWarnings(log(PREV_WRTN_PREM_AMT    + 1)),
    log_prev_poly = suppressWarnings(log(PREV_POLY_INFORCE_QTY + 1))
  ) |>
  mutate(
    # Negative PREV_ values (rare accounting adjustments) produce NaN -> NA
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  ) |>
  left_join(
    prior_yr_lookup,
    by = c("AGENCY_ID", "PROD_ABBR", "PROD_LINE", "STATE_ABBR",
           "STAT_PROFILE_DATE_YEAR" = "current_year")
  ) |>
  mutate(
    PREV_LOSS_RATIO = if_else(
      !is.na(prev_lr_raw) & !prev_lr_raw %in% SENTINELS,
      prev_lr_raw,
      NA_real_
    ),
    PREV_RETENTION_RATIO = if_else(
      !is.na(prev_rr_raw) & !prev_rr_raw %in% SENTINELS,
      prev_rr_raw,
      NA_real_
    ),
    # Categorical predictors as factors — levels fixed from full dataset.
    # STAT_PROFILE_DATE_YEAR is kept numeric (continuous linear year trend)
    # to allow extrapolation to test years 2013-2014.
    STATE_ABBR = factor(STATE_ABBR),
    PROD_LINE  = factor(PROD_LINE),
    PROD_ABBR  = factor(PROD_ABBR),
    VENDOR     = factor(VENDOR)
  )

# Split
wp_train <- wp_base |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_test  <- wp_base |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)

# LR complete-case subsets (PREV_LOSS_RATIO non-NA) for OLS 3CC / OLS 4CC
wp_train_cc  <- wp_train |> filter(!is.na(PREV_LOSS_RATIO))
wp_test_cc   <- wp_test  |> filter(!is.na(PREV_LOSS_RATIO))

# RET complete-case subsets (PREV_RETENTION_RATIO non-NA) for OLS_3_RET_CC / OLS_5
wp_train_ret <- wp_train |> filter(!is.na(PREV_RETENTION_RATIO))
wp_test_ret  <- wp_test  |> filter(!is.na(PREV_RETENTION_RATIO))

cat(sprintf("  Full dataset  : %s train / %s test\n",
            format(nrow(wp_train),     big.mark = ","),
            format(nrow(wp_test),      big.mark = ",")))
cat(sprintf("  LR CC         : %s train / %s test  (PREV_LOSS_RATIO non-NA)\n",
            format(nrow(wp_train_cc),  big.mark = ","),
            format(nrow(wp_test_cc),   big.mark = ",")))
cat(sprintf("  RET CC        : %s train / %s test  (PREV_RETENTION_RATIO non-NA)\n\n",
            format(nrow(wp_train_ret), big.mark = ","),
            format(nrow(wp_test_ret),  big.mark = ",")))

# =============================================================================
# Helper functions
# =============================================================================

r2_score <- function(actual, predicted) {
  ok  <- !is.na(actual) & !is.na(predicted)
  a   <- actual[ok]; p <- predicted[ok]
  ss_res <- sum((a - p)^2)
  ss_tot <- sum((a - mean(a))^2)
  if (ss_tot == 0) return(NA_real_)
  round(1 - ss_res / ss_tot, 4)
}

rmse_fn <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  round(sqrt(mean((actual[ok] - predicted[ok])^2)), 4)
}

mae_fn <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  round(mean(abs(actual[ok] - predicted[ok])), 4)
}

align_test_factors <- function(test_df, model) {
  # Factor levels in the fitted model only include levels present in training.
  # Test rows with new (unseen) levels will produce NA predictions, which are
  # excluded from metrics. This avoids a hard error from predict.lm().
  for (var in names(model$xlevels)) {
    if (var %in% names(test_df)) {
      test_df[[var]] <- factor(test_df[[var]], levels = model$xlevels[[var]])
    }
  }
  test_df
}

eval_model <- function(model, train_df, test_df) {
  test_aligned <- align_test_factors(test_df, model)
  pred_tr <- predict(model, newdata = train_df)
  pred_te <- predict(model, newdata = test_aligned)
  y_tr    <- train_df$log_wp
  y_te    <- test_df$log_wp
  n_te_new_level <- sum(is.na(pred_te) & !is.na(y_te))
  list(
    n_train = sum(!is.na(pred_tr) & !is.na(y_tr)),
    n_test  = sum(!is.na(pred_te) & !is.na(y_te)),
    n_test_dropped_new_factor_level = n_te_new_level,
    train   = list(r2   = r2_score(y_tr, pred_tr),
                   rmse = rmse_fn(y_tr, pred_tr),
                   mae  = mae_fn(y_tr,  pred_tr)),
    test    = list(r2   = r2_score(y_te, pred_te),
                   rmse = rmse_fn(y_te, pred_te),
                   mae  = mae_fn(y_te,  pred_te))
  )
}

extract_coefs <- function(model, model_name) {
  s <- summary(model)$coefficients   # excludes aliased (NA) coefficients
  data.frame(
    model           = model_name,
    term            = rownames(s),
    estimate        = round(s[, "Estimate"],   6),
    std_error       = round(s[, "Std. Error"], 6),
    t_stat          = round(s[, "t value"],    4),
    p_value         = round(s[, "Pr(>|t|)"],   6),
    significant_p05 = s[, "Pr(>|t|)"] < 0.05,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

df_to_list <- function(df) {
  lapply(seq_len(nrow(df)), function(i) as.list(df[i, ]))
}

top_effects <- function(coef_df, n = 10) {
  sig <- coef_df[coef_df$significant_p05 & coef_df$term != "(Intercept)", ]
  pos <- sig[order(-sig$estimate), c("term", "estimate", "p_value")]
  neg <- sig[order( sig$estimate), c("term", "estimate", "p_value")]
  list(
    most_positive = df_to_list(head(pos, n)),
    most_negative = df_to_list(head(neg, n))
  )
}

write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 6), path)
  cat(sprintf("  %-42s  %.1f KB\n", filename, file.size(path) / 1024))
}

# =============================================================================
# Model formulas
# =============================================================================

# OLS 0-2: continuous predictors only
f0 <- log_wp ~ log_prev_wp

f1 <- log_wp ~ log_prev_wp + log_prev_poly

f2 <- log_wp ~ log_prev_wp + log_prev_poly +
               ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE

# OLS 3 / OLS 3CC / OLS_3_RET_CC: categorical fixed effects + continuous year trend.
# STAT_PROFILE_DATE_YEAR is kept numeric (not factor) to allow extrapolation to 2013-2014.
# PROD_LINE nested in PROD_ABBR — R drops linearly dependent column via QR decomposition.
f3 <- log_wp ~ log_prev_wp + log_prev_poly +
               ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
               STATE_ABBR + PROD_LINE + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR

# OLS 4CC: OLS 3 + PREV_LOSS_RATIO on LR complete-case population.
# OLS 3CC uses f3 on the same LR CC subset to isolate population vs predictor effects.
f4 <- log_wp ~ log_prev_wp + log_prev_poly +
               ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
               STATE_ABBR + PROD_LINE + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR +
               PREV_LOSS_RATIO

# OLS 5: OLS 3 + PREV_RETENTION_RATIO on RET complete-case population.
# OLS_3_RET_CC uses f3 on the same RET CC subset to isolate population vs predictor effects.
f5 <- log_wp ~ log_prev_wp + log_prev_poly +
               ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
               STATE_ABBR + PROD_LINE + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR +
               PREV_RETENTION_RATIO

# OLS_ADDITIVE_FINAL: clean additive baseline. Removes PROD_LINE from OLS 3.
# PROD_ABBR determines PROD_LINE exactly (nested design); including both creates a redundant
# degree of freedom whose estimated coefficient is unstable across populations (e.g., +1.85
# in OLS 3 full vs -0.21 in OLS 3CC). PROD_ABBR alone captures all product-level variation.
f_add <- log_wp ~ log_prev_wp + log_prev_poly +
                  ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                  STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR

# OLS_INTERACTION_FULL: all pairwise interactions from the additive baseline.
# Same full population as OLS_ADDITIVE_FINAL. PROD_LINE excluded.
# Purpose: determine whether interaction effects materially improve over the additive baseline.
f_int <- log_wp ~ (log_prev_wp + log_prev_poly +
                   ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                   STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR)^2

# OLS_INTERACTION_NUMERIC_ONLY: all main effects + numeric × numeric interactions only.
# Excludes ALL categorical interactions (numeric × cat and cat × cat).
# Purpose: test whether continuous-variable interactions improve OLS without any
# sparse categorical-level slope risk.
f_nno <- log_wp ~
  log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR +
  MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR +
  log_prev_wp:log_prev_poly + log_prev_wp:ACTIVE_PRODUCERS +
  log_prev_wp:AGENCY_APPOINTMENT_YEAR + log_prev_wp:MAX_AGE +
  log_prev_wp:MIN_AGE + log_prev_wp:STAT_PROFILE_DATE_YEAR +
  log_prev_poly:ACTIVE_PRODUCERS + log_prev_poly:AGENCY_APPOINTMENT_YEAR +
  log_prev_poly:MAX_AGE + log_prev_poly:MIN_AGE +
  log_prev_poly:STAT_PROFILE_DATE_YEAR +
  ACTIVE_PRODUCERS:AGENCY_APPOINTMENT_YEAR + ACTIVE_PRODUCERS:MAX_AGE +
  ACTIVE_PRODUCERS:MIN_AGE + ACTIVE_PRODUCERS:STAT_PROFILE_DATE_YEAR +
  AGENCY_APPOINTMENT_YEAR:MAX_AGE + AGENCY_APPOINTMENT_YEAR:MIN_AGE +
  AGENCY_APPOINTMENT_YEAR:STAT_PROFILE_DATE_YEAR +
  MAX_AGE:MIN_AGE + MAX_AGE:STAT_PROFILE_DATE_YEAR +
  MIN_AGE:STAT_PROFILE_DATE_YEAR

# OLS_INTERACTION_SUPPORTED: explicit formula excluding categorical × categorical interactions.
# Includes: all main effects + numeric × numeric (C(7,2)=21 pairs) +
#           numeric × PROD_ABBR + numeric × STATE_ABBR + numeric × VENDOR.
# Excludes: PROD_ABBR × STATE_ABBR, PROD_ABBR × VENDOR, STATE_ABBR × VENDOR
# (all three were QUESTIONABLE in feasibility review due to sparse cells).
f_supp <- log_wp ~
  log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR +
  MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR +
  # numeric × numeric (21 unique pairs)
  log_prev_wp:log_prev_poly + log_prev_wp:ACTIVE_PRODUCERS +
  log_prev_wp:AGENCY_APPOINTMENT_YEAR + log_prev_wp:MAX_AGE +
  log_prev_wp:MIN_AGE + log_prev_wp:STAT_PROFILE_DATE_YEAR +
  log_prev_poly:ACTIVE_PRODUCERS + log_prev_poly:AGENCY_APPOINTMENT_YEAR +
  log_prev_poly:MAX_AGE + log_prev_poly:MIN_AGE +
  log_prev_poly:STAT_PROFILE_DATE_YEAR +
  ACTIVE_PRODUCERS:AGENCY_APPOINTMENT_YEAR + ACTIVE_PRODUCERS:MAX_AGE +
  ACTIVE_PRODUCERS:MIN_AGE + ACTIVE_PRODUCERS:STAT_PROFILE_DATE_YEAR +
  AGENCY_APPOINTMENT_YEAR:MAX_AGE + AGENCY_APPOINTMENT_YEAR:MIN_AGE +
  AGENCY_APPOINTMENT_YEAR:STAT_PROFILE_DATE_YEAR +
  MAX_AGE:MIN_AGE + MAX_AGE:STAT_PROFILE_DATE_YEAR +
  MIN_AGE:STAT_PROFILE_DATE_YEAR +
  # numeric × PROD_ABBR (7 families)
  log_prev_wp:PROD_ABBR + log_prev_poly:PROD_ABBR +
  ACTIVE_PRODUCERS:PROD_ABBR + AGENCY_APPOINTMENT_YEAR:PROD_ABBR +
  MAX_AGE:PROD_ABBR + MIN_AGE:PROD_ABBR + STAT_PROFILE_DATE_YEAR:PROD_ABBR +
  # numeric × STATE_ABBR (7 families)
  log_prev_wp:STATE_ABBR + log_prev_poly:STATE_ABBR +
  ACTIVE_PRODUCERS:STATE_ABBR + AGENCY_APPOINTMENT_YEAR:STATE_ABBR +
  MAX_AGE:STATE_ABBR + MIN_AGE:STATE_ABBR + STAT_PROFILE_DATE_YEAR:STATE_ABBR +
  # numeric × VENDOR (7 families)
  log_prev_wp:VENDOR + log_prev_poly:VENDOR +
  ACTIVE_PRODUCERS:VENDOR + AGENCY_APPOINTMENT_YEAR:VENDOR +
  MAX_AGE:VENDOR + MIN_AGE:VENDOR + STAT_PROFILE_DATE_YEAR:VENDOR

# =============================================================================
# Fit models
# =============================================================================

cat("Fitting OLS models...\n")

m0      <- lm(f0, data = wp_train);                 cat("  OLS 0        fitted\n")
m1      <- lm(f1, data = wp_train);                 cat("  OLS 1        fitted\n")
m2      <- lm(f2, data = wp_train);                 cat("  OLS 2        fitted\n")
m3      <- lm(f3, data = wp_train);                 cat("  OLS 3        fitted  (full population)\n")
m3cc    <- lm(f3, data = wp_train_cc);              cat("  OLS 3CC      fitted  (LR CC population, same formula as OLS 3)\n")
m4      <- lm(f4, data = wp_train_cc);              cat("  OLS 4CC      fitted  (LR CC population + PREV_LOSS_RATIO)\n")
# RET CC models: factors set from the full dataset may include levels absent from the smaller
# RET CC subset. After droplevels(), factors reduced to exactly 1 level cause an lm() error.
# Build formulas dynamically to exclude any singleton categorical from the RET CC population.
ret_dl <- droplevels(wp_train_ret)
cat_vars_all <- c("STATE_ABBR", "PROD_LINE", "PROD_ABBR", "VENDOR")
cat_vars_ret <- Filter(function(v) nlevels(ret_dl[[v]]) >= 2, cat_vars_all)
dropped_cat_ret <- setdiff(cat_vars_all, cat_vars_ret)
if (length(dropped_cat_ret) > 0) {
  cat(sprintf("  NOTE: RET CC excluded singleton factors: %s\n",
              paste(dropped_cat_ret, collapse = ", ")))
}
f3_ret_cc <- reformulate(
  c("log_prev_wp", "log_prev_poly",
    "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
    cat_vars_ret, "STAT_PROFILE_DATE_YEAR"),
  response = "log_wp"
)
f5_ret_cc <- reformulate(
  c("log_prev_wp", "log_prev_poly",
    "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
    cat_vars_ret, "STAT_PROFILE_DATE_YEAR", "PREV_RETENTION_RATIO"),
  response = "log_wp"
)
m3_ret  <- lm(f3_ret_cc, data = ret_dl); cat("  OLS_3_RET_CC     fitted  (RET CC population, same formula as OLS 3)\n")
m5      <- lm(f5_ret_cc, data = ret_dl); cat("  OLS_5            fitted  (RET CC population + PREV_RETENTION_RATIO)\n")
m_add   <- lm(f_add, data = wp_train);   cat("  OLS_ADDITIVE_FINAL fitted  (full population, PROD_LINE removed)\n")
cat("  Fitting OLS_INTERACTION_FULL (all pairwise interactions — may take ~60s)...\n")
m_int   <- lm(f_int,  data = wp_train);  cat("  OLS_INTERACTION_FULL fitted  (full population, all pairwise interactions)\n")
cat("  Fitting OLS_INTERACTION_NUMERIC_ONLY (numeric × numeric only)...\n")
m_nno   <- lm(f_nno,  data = wp_train);  cat("  OLS_INTERACTION_NUMERIC_ONLY fitted  (no categorical interactions)\n")
cat("  Fitting OLS_INTERACTION_SUPPORTED (numeric × cat + numeric × numeric)...\n")
m_supp  <- lm(f_supp, data = wp_train);  cat("  OLS_INTERACTION_SUPPORTED fitted  (no cat × cat interactions)\n\n")

# =============================================================================
# Evaluate
# =============================================================================

cat("Evaluating models on train and test...\n")
perf0     <- eval_model(m0,     wp_train,     wp_test)
perf1     <- eval_model(m1,     wp_train,     wp_test)
perf2     <- eval_model(m2,     wp_train,     wp_test)
perf3     <- eval_model(m3,     wp_train,     wp_test)
perf3cc   <- eval_model(m3cc,   wp_train_cc,  wp_test_cc)
perf4     <- eval_model(m4,     wp_train_cc,  wp_test_cc)
perf3_ret <- eval_model(m3_ret, ret_dl,    wp_test_ret)
perf5     <- eval_model(m5,     ret_dl,    wp_test_ret)
perf_add  <- eval_model(m_add,  wp_train,  wp_test)
perf_int  <- eval_model(m_int,  wp_train,  wp_test)
perf_nno  <- eval_model(m_nno,  wp_train,  wp_test)
perf_supp <- eval_model(m_supp, wp_train,  wp_test)

cat(sprintf("  OLS 0        — train R²: %.4f  test R²: %.4f\n",   perf0$train$r2,     perf0$test$r2))
cat(sprintf("  OLS 1        — train R²: %.4f  test R²: %.4f\n",   perf1$train$r2,     perf1$test$r2))
cat(sprintf("  OLS 2        — train R²: %.4f  test R²: %.4f\n",   perf2$train$r2,     perf2$test$r2))
cat(sprintf("  OLS 3        — train R²: %.4f  test R²: %.4f  (n=%s, full)\n",
            perf3$train$r2,     perf3$test$r2,     format(nrow(wp_train),     big.mark = ",")))
cat(sprintf("  OLS 3CC      — train R²: %.4f  test R²: %.4f  (n=%s, LR CC)\n",
            perf3cc$train$r2,   perf3cc$test$r2,   format(nrow(wp_train_cc),  big.mark = ",")))
cat(sprintf("  OLS 4CC      — train R²: %.4f  test R²: %.4f  (n=%s, LR CC + PREV_LR)\n",
            perf4$train$r2,     perf4$test$r2,     format(nrow(wp_train_cc),  big.mark = ",")))
cat(sprintf("  OLS_3_RET_CC — train R²: %.4f  test R²: %.4f  (n=%s, RET CC)\n",
            perf3_ret$train$r2, perf3_ret$test$r2, format(nrow(wp_train_ret), big.mark = ",")))
cat(sprintf("  OLS_5        — train R²: %.4f  test R²: %.4f  (n=%s, RET CC + PREV_RR)\n",
            perf5$train$r2,     perf5$test$r2,     format(nrow(wp_train_ret), big.mark = ",")))
cat(sprintf("  OLS_ADDITIVE_FINAL        — train R²: %.4f  test R²: %.4f  (n=%s)\n",
            perf_add$train$r2,  perf_add$test$r2,  format(nrow(wp_train), big.mark = ",")))
cat(sprintf("  OLS_INTERACTION_FULL         — train R²: %.4f  test R²: %.4f  (n=%s, all pairwise)\n",
            perf_int$train$r2,  perf_int$test$r2,  format(nrow(wp_train), big.mark = ",")))
cat(sprintf("  OLS_INTERACTION_NUMERIC_ONLY — train R²: %.4f  test R²: %.4f  (n=%s, numeric × numeric)\n",
            perf_nno$train$r2,  perf_nno$test$r2,  format(nrow(wp_train), big.mark = ",")))
cat(sprintf("  OLS_INTERACTION_SUPPORTED    — train R²: %.4f  test R²: %.4f  (n=%s, no cat × cat)\n\n",
            perf_supp$train$r2, perf_supp$test$r2, format(nrow(wp_train), big.mark = ",")))

# =============================================================================
# Row count documentation variables
# =============================================================================
# Candidate population: rows passing the written-premium filters (non-COMMPOL,
# WP > 0, years 2006-2014), before any predictor completeness check.
# Effective evaluated rows: rows where all predictors are non-NA and lm() /
# predict() return a valid value. The difference arises because log_prev_wp and
# log_prev_poly are set to NA when the underlying PREV_ column is negative
# (rare accounting adjustments: NaN from log of negative → coerced to NA).
# lm() silently drops NA rows during fitting; predict() returns NA for them;
# eval_model() excludes them from metric calculations.
n_cand_train    <- nrow(wp_train)
n_cand_test     <- nrow(wp_test)
n_eval_train    <- perf_add$n_train
n_eval_test     <- perf_add$n_test
n_dropped_train <- n_cand_train - n_eval_train
n_dropped_test  <- n_cand_test  - n_eval_test
row_count_note  <- paste0(
  "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
  format(n_cand_test, big.mark = ","), " test. ",
  "These are all rows passing the written-premium modeling filters ",
  "(PROD_ABBR != COMMPOL, WRTN_PREM_AMT > 0, STAT_PROFILE_DATE_YEAR 2006-2014). ",
  "Effective evaluated rows: ", format(n_eval_train, big.mark = ","), " train / ",
  format(n_eval_test, big.mark = ","), " test. ",
  "These are rows included in metric calculations after excluding observations ",
  "where log_prev_wp or log_prev_poly is NA. ",
  "log_prev_wp = log(PREV_WRTN_PREM_AMT + 1) and ",
  "log_prev_poly = log(PREV_POLY_INFORCE_QTY + 1) are set to NA when the ",
  "underlying PREV_ column is negative (rare accounting adjustments). ",
  "lm() silently drops these rows during fitting; predict() returns NA for them. ",
  "Rows dropped: ", n_dropped_train, " train, ", n_dropped_test, " test."
)

# =============================================================================
# Coefficients
# =============================================================================

cat("Extracting coefficients...\n")
c0      <- extract_coefs(m0,     "OLS_0")
c1      <- extract_coefs(m1,     "OLS_1")
c2      <- extract_coefs(m2,     "OLS_2")
c3      <- extract_coefs(m3,     "OLS_3")
c3cc    <- extract_coefs(m3cc,   "OLS_3CC")
c4      <- extract_coefs(m4,     "OLS_4CC")
c3_ret  <- extract_coefs(m3_ret, "OLS_3_RET_CC")
c5      <- extract_coefs(m5,     "OLS_5")
c_add   <- extract_coefs(m_add,  "OLS_ADDITIVE_FINAL")
cat("  Extracting OLS_INTERACTION_FULL coefficients...\n")
c_int   <- extract_coefs(m_int,  "OLS_INTERACTION_FULL")
cat("  Extracting OLS_INTERACTION_NUMERIC_ONLY coefficients...\n")
c_nno   <- extract_coefs(m_nno,  "OLS_INTERACTION_NUMERIC_ONLY")
cat("  Extracting OLS_INTERACTION_SUPPORTED coefficients...\n")
c_supp  <- extract_coefs(m_supp, "OLS_INTERACTION_SUPPORTED")

cat(sprintf(
  "  OLS_ADDITIVE_FINAL: %d | OLS_INTERACTION_FULL: %d | OLS_INTERACTION_NUMERIC_ONLY: %d | OLS_INTERACTION_SUPPORTED: %d\n\n",
  nrow(c_add), nrow(c_int), nrow(c_nno), nrow(c_supp)))

# =============================================================================
# Helper: retrieve a single coefficient value
# =============================================================================

get_coef <- function(cdf, term_name) {
  row <- cdf[cdf$term == term_name, ]
  if (nrow(row) == 0) return(NA_real_)
  row$estimate
}

get_coef_row <- function(cdf, term_name) {
  row <- cdf[cdf$term == term_name, ]
  if (nrow(row) == 0) return(list(note = paste0(term_name, " not found")))
  list(estimate       = row$estimate,
       std_error      = row$std_error,
       t_stat         = row$t_stat,
       p_value        = row$p_value,
       significant_p05 = row$significant_p05)
}

# =============================================================================
# Interaction term helpers
# =============================================================================

NUMERIC_PRED <- c("log_prev_wp", "log_prev_poly", "ACTIVE_PRODUCERS",
                  "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE", "STAT_PROFILE_DATE_YEAR")

classify_pred_part <- function(p) {
  if (p %in% NUMERIC_PRED)           return("numeric")
  if (startsWith(p, "STATE_ABBR"))   return("state")
  if (startsWith(p, "PROD_ABBR"))    return("product")
  if (startsWith(p, "VENDOR"))       return("vendor")
  return("other")
}

classify_interaction_term <- function(term) {
  if (!grepl(":", term)) return(NA_character_)
  parts <- strsplit(term, ":")[[1]]
  types <- sort(sapply(parts, classify_pred_part))
  paste(types, collapse = " x ")
}

interaction_type_note <- function(grp) {
  switch(grp,
    "numeric x numeric"  = "Synergy or attenuation between two continuous agency-level metrics",
    "numeric x product"  = "Product-specific slope on a continuous predictor (e.g., volume carry-forward effect varies by product)",
    "numeric x state"    = "State-specific slope on a continuous predictor (e.g., year trend varies by state)",
    "numeric x vendor"   = "Vendor-specific slope on a continuous predictor",
    "product x state"    = "Product x state combination: product performance varies geographically",
    "product x vendor"   = "Product x vendor combination: vendor strength differs by product",
    "state x vendor"     = "State x vendor combination: vendor relationships differ by state",
    paste0("Interaction type: ", grp)
  )
}

# =============================================================================
# Assemble ols_model_comparison.json
# =============================================================================

model_descriptions <- list(
  OLS_0        = "Baseline: log_prev_wp only",
  OLS_1        = "OLS 0 + log_prev_poly",
  OLS_2        = "OLS 1 + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE",
  OLS_3        = paste0("OLS 2 + STATE_ABBR + PROD_LINE + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR. ",
                        "Categoricals as factors. Year as continuous numeric (enables test-set prediction). ",
                        "PROD_LINE nested in PROD_ABBR — R drops dependent column automatically. ",
                        "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
                        format(n_cand_test, big.mark = ","), " test. ",
                        "Effective evaluated rows: ", format(n_eval_train, big.mark = ","), " train / ",
                        format(n_eval_test, big.mark = ","), " test ",
                        "(", n_dropped_train, " train / ", n_dropped_test, " test rows dropped — NA in log_prev_wp or log_prev_poly)."),
  OLS_3CC      = paste0("OLS 3 formula fit on PREV_LOSS_RATIO complete-case population. ",
                        sprintf("Train: %s rows. Test: %s rows. ",
                                format(nrow(wp_train_cc), big.mark = ","),
                                format(nrow(wp_test_cc),  big.mark = ",")),
                        "Purpose: isolate LR population selection from predictor contribution."),
  OLS_4CC      = paste0("OLS 3CC + PREV_LOSS_RATIO. Same LR CC population as OLS 3CC. ",
                        sprintf("Train: %s rows. Test: %s rows.",
                                format(nrow(wp_train_cc), big.mark = ","),
                                format(nrow(wp_test_cc),  big.mark = ","))),
  OLS_3_RET_CC = paste0("OLS 3 formula fit on PREV_RETENTION_RATIO complete-case population. ",
                        sprintf("Train: %s rows. Test: %s rows. ",
                                format(nrow(wp_train_ret), big.mark = ","),
                                format(nrow(wp_test_ret),  big.mark = ",")),
                        "Baseline for OLS_5: ensures identical rows in the OLS_3_RET_CC vs OLS_5 comparison."),
  OLS_5        = paste0("OLS_3_RET_CC + PREV_RETENTION_RATIO. Same RET CC population as OLS_3_RET_CC. ",
                        sprintf("Train: %s rows. Test: %s rows.",
                                format(nrow(wp_train_ret), big.mark = ","),
                                format(nrow(wp_test_ret),  big.mark = ","))),
  OLS_ADDITIVE_FINAL = paste0(
    "Clean additive baseline: OLS 3 with PROD_LINE removed. ",
    "PROD_ABBR determines PROD_LINE exactly (nested design); the PROD_LINE term is structurally ",
    "redundant and produces unstable coefficients across populations. ",
    "PROD_ABBR alone captures all product-level variation. ",
    "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
    format(n_cand_test, big.mark = ","), " test. ",
    "Effective evaluated rows: ", format(n_eval_train, big.mark = ","), " train / ",
    format(n_eval_test, big.mark = ","), " test ",
    "(", n_dropped_train, " train / ", n_dropped_test, " test rows dropped — NA in log_prev_wp or log_prev_poly)."
  ),
  OLS_INTERACTION_FULL = paste0(
    "All pairwise interactions from OLS_ADDITIVE_FINAL: (additive terms)^2. ",
    "Same population as OLS_ADDITIVE_FINAL. PROD_LINE excluded. ",
    "Purpose: determine whether interaction effects materially improve over the additive baseline. ",
    "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
    format(n_cand_test, big.mark = ","), " test. ",
    "Effective evaluated rows: ", format(n_eval_train, big.mark = ","), " train / ",
    format(n_eval_test, big.mark = ","), " test ",
    "(", n_dropped_train, " train / ", n_dropped_test, " test rows dropped — NA in log_prev_wp or log_prev_poly)."
  ),
  OLS_INTERACTION_NUMERIC_ONLY = paste0(
    "Numeric × numeric interaction model. Includes all OLS_ADDITIVE_FINAL main effects ",
    "plus all C(7,2)=21 numeric × numeric interaction pairs. ",
    "Excludes all categorical interactions (numeric × PROD_ABBR, numeric × STATE_ABBR, ",
    "numeric × VENDOR, and all cat × cat). ",
    "Purpose: test whether continuous-variable interactions improve OLS without sparse categorical-level slope risk. ",
    "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
    format(n_cand_test, big.mark = ","), " test. ",
    "Effective evaluated rows: ", format(n_eval_train, big.mark = ","), " train / ",
    format(n_eval_test, big.mark = ","), " test ",
    "(", n_dropped_train, " train / ", n_dropped_test, " test rows dropped — NA in log_prev_wp or log_prev_poly)."
  ),
  OLS_INTERACTION_SUPPORTED = paste0(
    "Support-screened interaction model. Includes all OLS_ADDITIVE_FINAL main effects, ",
    "all numeric × numeric pairs (C(7,2)=21), and all numeric × categorical interactions ",
    "(numeric × PROD_ABBR + numeric × STATE_ABBR + numeric × VENDOR). ",
    "Excludes PROD_ABBR × STATE_ABBR, PROD_ABBR × VENDOR, and STATE_ABBR × VENDOR ",
    "(all QUESTIONABLE in feasibility review due to sparse cells). ",
    "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
    format(n_cand_test, big.mark = ","), " test. ",
    "Effective evaluated rows: ", format(n_eval_train, big.mark = ","), " train / ",
    format(n_eval_test, big.mark = ","), " test ",
    "(", n_dropped_train, " train / ", n_dropped_test, " test rows dropped — NA in log_prev_wp or log_prev_poly)."
  )
)

model_comparison_out <- list(
  description = paste0(
    "OLS 0-3, OLS 3CC, OLS 4CC, OLS_3_RET_CC, OLS_5, OLS_ADDITIVE_FINAL, and interaction model performance comparison. ",
    "Target: log(WRTN_PREM_AMT + 1). Train: 2006-2012. Test: 2013-2014. ",
    "OLS 3CC and OLS 4CC use the PREV_LOSS_RATIO complete-case subset; ",
    "OLS_3_RET_CC and OLS_5 use the PREV_RETENTION_RATIO complete-case subset; ",
    "all other models use the full candidate population. ",
    "Candidate population (full models): ", format(n_cand_train, big.mark = ","), " train / ",
    format(n_cand_test, big.mark = ","), " test. ",
    "Effective evaluated rows (n_train / n_test in each model entry): ",
    format(n_eval_train, big.mark = ","), " train / ", format(n_eval_test, big.mark = ","), " test for full-population models. ",
    "See row_count_note for explanation of the difference."
  ),
  row_count_note = row_count_note,
  models = list(
    list(model = "OLS_0",        description = model_descriptions$OLS_0,
         predictors = c("log_prev_wp"),
         n_train = perf0$n_train, n_test = perf0$n_test, train = perf0$train, test = perf0$test),
    list(model = "OLS_1",        description = model_descriptions$OLS_1,
         predictors = c("log_prev_wp", "log_prev_poly"),
         n_train = perf1$n_train, n_test = perf1$n_test, train = perf1$train, test = perf1$test),
    list(model = "OLS_2",        description = model_descriptions$OLS_2,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE"),
         n_train = perf2$n_train, n_test = perf2$n_test, train = perf2$train, test = perf2$test),
    list(model = "OLS_3",        description = model_descriptions$OLS_3,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                        "STATE_ABBR (factor)", "PROD_LINE (factor)", "PROD_ABBR (factor)",
                        "VENDOR (factor)", "STAT_PROFILE_DATE_YEAR (numeric)"),
         n_train = perf3$n_train, n_test = perf3$n_test, train = perf3$train, test = perf3$test),
    list(model = "OLS_3CC",      description = model_descriptions$OLS_3CC,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                        "STATE_ABBR (factor)", "PROD_LINE (factor)", "PROD_ABBR (factor)",
                        "VENDOR (factor)", "STAT_PROFILE_DATE_YEAR (numeric)"),
         n_train = perf3cc$n_train, n_test = perf3cc$n_test, train = perf3cc$train, test = perf3cc$test),
    list(model = "OLS_4CC",      description = model_descriptions$OLS_4CC,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                        "STATE_ABBR (factor)", "PROD_LINE (factor)", "PROD_ABBR (factor)",
                        "VENDOR (factor)", "STAT_PROFILE_DATE_YEAR (numeric)", "PREV_LOSS_RATIO"),
         n_train = perf4$n_train, n_test = perf4$n_test, train = perf4$train, test = perf4$test),
    list(model = "OLS_3_RET_CC", description = model_descriptions$OLS_3_RET_CC,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                        "STATE_ABBR (factor)", "PROD_LINE (factor)", "PROD_ABBR (factor)",
                        "VENDOR (factor)", "STAT_PROFILE_DATE_YEAR (numeric)"),
         n_train = perf3_ret$n_train, n_test = perf3_ret$n_test,
         train = perf3_ret$train, test = perf3_ret$test),
    list(model = "OLS_5",        description = model_descriptions$OLS_5,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                        "STATE_ABBR (factor)", "PROD_LINE (factor)", "PROD_ABBR (factor)",
                        "VENDOR (factor)", "STAT_PROFILE_DATE_YEAR (numeric)", "PREV_RETENTION_RATIO"),
         n_train = perf5$n_train, n_test = perf5$n_test, train = perf5$train, test = perf5$test),
    list(model = "OLS_ADDITIVE_FINAL", description = model_descriptions$OLS_ADDITIVE_FINAL,
         predictors = c("log_prev_wp", "log_prev_poly",
                        "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                        "STATE_ABBR (factor)", "PROD_ABBR (factor)",
                        "VENDOR (factor)", "STAT_PROFILE_DATE_YEAR (numeric)"),
         n_train = perf_add$n_train, n_test = perf_add$n_test,
         train = perf_add$train, test = perf_add$test),
    list(model = "OLS_INTERACTION_FULL", description = model_descriptions$OLS_INTERACTION_FULL,
         predictors = c("(OLS_ADDITIVE_FINAL terms)^2 — all pairwise interactions"),
         n_train = perf_int$n_train, n_test = perf_int$n_test,
         train = perf_int$train, test = perf_int$test),
    list(model = "OLS_INTERACTION_NUMERIC_ONLY", description = model_descriptions$OLS_INTERACTION_NUMERIC_ONLY,
         predictors = c("main effects + numeric×numeric (21 pairs) — no categorical interactions"),
         n_train = perf_nno$n_train, n_test = perf_nno$n_test,
         train = perf_nno$train, test = perf_nno$test),
    list(model = "OLS_INTERACTION_SUPPORTED", description = model_descriptions$OLS_INTERACTION_SUPPORTED,
         predictors = c("main effects + numeric×numeric + numeric×PROD_ABBR + numeric×STATE_ABBR + numeric×VENDOR"),
         n_train = perf_supp$n_train, n_test = perf_supp$n_test,
         train = perf_supp$train, test = perf_supp$test)
  )
)

# =============================================================================
# Assemble ols_coefficients.json
# =============================================================================

ols_coefficients_out <- list(
  OLS_0                     = df_to_list(c0),
  OLS_1                     = df_to_list(c1),
  OLS_2                     = df_to_list(c2),
  OLS_3                     = df_to_list(c3),
  OLS_3CC                   = df_to_list(c3cc),
  OLS_4CC                   = df_to_list(c4),
  OLS_3_RET_CC              = df_to_list(c3_ret),
  OLS_5                     = df_to_list(c5),
  OLS_ADDITIVE_FINAL        = df_to_list(c_add),
  OLS_INTERACTION_FULL         = df_to_list(c_int),
  OLS_INTERACTION_NUMERIC_ONLY = df_to_list(c_nno),
  OLS_INTERACTION_SUPPORTED    = df_to_list(c_supp)
)

# =============================================================================
# Assemble ols5_retention_comparison.json
# =============================================================================

prr_coef <- get_coef_row(c5, "PREV_RETENTION_RATIO")

# Build recommendation text dynamically from computed values
delta_test_r2_ret <- round(perf5$test$r2 - perf3_ret$test$r2, 4)

ret_recommendation <- if (abs(delta_test_r2_ret) < 0.001) {
  list(
    decision = "EXCLUDE",
    justification = sprintf(
      paste0("PREV_RETENTION_RATIO contributes %.4f test R² on the RET CC population ",
             "(OLS_3_RET_CC: %.4f → OLS_5: %.4f). This is negligible. ",
             "Statistical significance of the coefficient does not establish practical value. ",
             "Additionally, PREV_RETENTION_RATIO is missing for %.1f%% of the full modeling population, ",
             "which would reduce usable rows by %s for every prediction requiring it. ",
             "Exclude from future specifications."),
      delta_test_r2_ret, perf3_ret$test$r2, perf5$test$r2,
      round((nrow(wp_train) - nrow(wp_train_ret)) / nrow(wp_train) * 100, 1),
      format(nrow(wp_train) - nrow(wp_train_ret), big.mark = ",")
    )
  )
} else if (delta_test_r2_ret >= 0.005) {
  list(
    decision = "INCLUDE_CONDITIONALLY",
    justification = sprintf(
      paste0("PREV_RETENTION_RATIO contributes %.4f test R² on the RET CC population. ",
             "This may be practically meaningful but must be weighed against %.1f%% missing rate, ",
             "which reduces the modeling population from %s to %s rows. ",
             "Inclusion requires a strategy for the %.1f%% of rows without PREV_RETENTION_RATIO."),
      delta_test_r2_ret,
      round((nrow(wp_train) - nrow(wp_train_ret)) / nrow(wp_train) * 100, 1),
      format(nrow(wp_train), big.mark = ","),
      format(nrow(wp_train_ret), big.mark = ","),
      round((nrow(wp_train) - nrow(wp_train_ret)) / nrow(wp_train) * 100, 1)
    )
  )
} else {
  list(
    decision = "EXCLUDE",
    justification = sprintf(
      paste0("PREV_RETENTION_RATIO contributes %.4f test R² on the RET CC population. ",
             "This improvement is marginal. Combined with %.1f%% missing rate in the full population, ",
             "the predictor does not justify the coverage cost. Exclude from future specifications."),
      delta_test_r2_ret,
      round((nrow(wp_train) - nrow(wp_train_ret)) / nrow(wp_train) * 100, 1)
    )
  )
}

ret_comparison_out <- list(
  description = paste0(
    "Population-controlled test of PREV_RETENTION_RATIO (OLS_5). ",
    "Method: fit OLS_3_RET_CC (f3 baseline) and OLS_5 (f3 + PREV_RETENTION_RATIO) on the same ",
    "PREV_RETENTION_RATIO complete-case population. ",
    "OLS_3_RET_CC vs OLS_5 isolates the predictor contribution from population selection effects."
  ),
  part_a_population = list(
    full_train             = nrow(wp_train),
    full_test              = nrow(wp_test),
    retention_cc_train     = nrow(wp_train_ret),
    retention_cc_test      = nrow(wp_test_ret),
    n_removed_train        = nrow(wp_train)    - nrow(wp_train_ret),
    n_removed_test         = nrow(wp_test)     - nrow(wp_test_ret),
    pct_removed_train      = round((nrow(wp_train)  - nrow(wp_train_ret)) / nrow(wp_train)  * 100, 2),
    pct_removed_test       = round((nrow(wp_test)   - nrow(wp_test_ret))  / nrow(wp_test)   * 100, 2),
    population_note        = paste0(
      "Rows removed are those without a prior-year join for RETENTION_RATIO, ",
      "or where prior-year RETENTION_RATIO is a sentinel value (99997/99998/99999). ",
      "These are primarily first-year observations and agencies whose retention ratio was not reported. ",
      "PREV_RETENTION_RATIO covers only ~37% of the full modeling population — ",
      "a substantially more severe restriction than PREV_LOSS_RATIO (~91%)."
    )
  ),
  part_b_ols3_ret_cc = list(
    model       = "OLS_3_RET_CC",
    description = "OLS 3 formula on PREV_RETENTION_RATIO CC population. Baseline for OLS_5 comparison.",
    n_train     = perf3_ret$n_train,
    n_test      = perf3_ret$n_test,
    train       = perf3_ret$train,
    test        = perf3_ret$test
  ),
  part_c_ols5 = list(
    model       = "OLS_5",
    description = "OLS_3_RET_CC + PREV_RETENTION_RATIO. Same RET CC population.",
    n_train     = perf5$n_train,
    n_test      = perf5$n_test,
    train       = perf5$train,
    test        = perf5$test
  ),
  part_d_comparison = list(
    models_compared         = "OLS_3_RET_CC vs OLS_5 on identical PREV_RETENTION_RATIO CC population",
    delta_train_r2          = round(perf5$train$r2   - perf3_ret$train$r2,   4),
    delta_test_r2           = delta_test_r2_ret,
    delta_train_rmse        = round(perf5$train$rmse - perf3_ret$train$rmse, 4),
    delta_test_rmse         = round(perf5$test$rmse  - perf3_ret$test$rmse,  4),
    delta_train_mae         = round(perf5$train$mae  - perf3_ret$train$mae,  4),
    delta_test_mae          = round(perf5$test$mae   - perf3_ret$test$mae,   4),
    population_selection_note = list(
      description    = "OLS 3 (full) vs OLS_3_RET_CC: quantifies how much the RET CC restriction alone changes performance",
      delta_train_r2 = round(perf3_ret$train$r2 - perf3$train$r2, 4),
      delta_test_r2  = round(perf3_ret$test$r2  - perf3$test$r2,  4)
    )
  ),
  part_e_prev_retention_ratio_coefficient = list(
    coefficient         = prr_coef,
    business_interpretation = paste0(
      "PREV_RETENTION_RATIO is the prior-year agency-product-state retention ratio. ",
      "A positive coefficient means agencies that retained more business in the prior year ",
      "tend to write more premium in the current year, after controlling for prior-year volume, ",
      "product, state, vendor, and year effects. ",
      "A negative or near-zero coefficient would suggest retention rate is not independently ",
      "predictive of written premium once volume and fixed effects are controlled. ",
      "Statistical significance at the p<0.05 level alone does not justify inclusion — ",
      "with ~38K training rows, small effects can be significant without being practically meaningful."
    )
  ),
  part_f_decision = ret_recommendation
)

# =============================================================================
# Assemble ols_interaction_full.json
# =============================================================================

# AIC and BIC
aic_add  <- round(AIC(m_add),  2)
bic_add  <- round(BIC(m_add),  2)
aic_int  <- round(AIC(m_int),  2)
bic_int  <- round(BIC(m_int),  2)
aic_nno  <- round(AIC(m_nno),  2)
bic_nno  <- round(BIC(m_nno),  2)
aic_supp <- round(AIC(m_supp), 2)
bic_supp <- round(BIC(m_supp), 2)

# Complexity
n_total_int_formula <- length(coef(m_int))
n_aliased_int       <- sum(is.na(coef(m_int)))
n_estimated_int     <- nrow(c_int)
n_main_int          <- sum(!grepl(":", c_int$term))
n_interaction_terms <- sum( grepl(":", c_int$term))
n_sig_main          <- sum(!grepl(":", c_int$term) & c_int$significant_p05)
n_sig_interaction   <- sum( grepl(":", c_int$term) & c_int$significant_p05)

# Deltas vs additive
delta_train_r2_int   <- round(perf_int$train$r2   - perf_add$train$r2,   4)
delta_test_r2_int    <- round(perf_int$test$r2    - perf_add$test$r2,    4)
delta_train_rmse_int <- round(perf_int$train$rmse - perf_add$train$rmse, 4)
delta_test_rmse_int  <- round(perf_int$test$rmse  - perf_add$test$rmse,  4)
delta_train_mae_int  <- round(perf_int$train$mae  - perf_add$train$mae,  4)
delta_test_mae_int   <- round(perf_int$test$mae   - perf_add$test$mae,   4)

# Interaction-only terms with type labels
c_int_only <- c_int[grepl(":", c_int$term), ]
c_int_only$interaction_type <- sapply(c_int_only$term, classify_interaction_term)

# Top 20 most positive significant interaction terms
top20_pos_int <- df_to_list(head(
  c_int_only[c_int_only$significant_p05 & c_int_only$estimate > 0, ][
    order(-c_int_only$estimate[c_int_only$significant_p05 & c_int_only$estimate > 0]), ],
  20
))

# Top 20 most negative significant interaction terms
top20_neg_int <- df_to_list(head(
  c_int_only[c_int_only$significant_p05 & c_int_only$estimate < 0, ][
    order(c_int_only$estimate[c_int_only$significant_p05 & c_int_only$estimate < 0]), ],
  20
))

# Group by interaction type
int_types <- sort(unique(c_int_only$interaction_type[!is.na(c_int_only$interaction_type)]))
int_type_groups <- lapply(int_types, function(grp) {
  rows     <- c_int_only[!is.na(c_int_only$interaction_type) & c_int_only$interaction_type == grp, ]
  sig_rows <- rows[rows$significant_p05, ]
  list(
    interaction_type  = grp,
    n_terms           = nrow(rows),
    n_significant_p05 = nrow(sig_rows),
    largest_abs_coef  = if (nrow(rows) > 0) round(max(abs(rows$estimate), na.rm = TRUE), 4) else 0,
    interpretation    = interaction_type_note(grp)
  )
})

# Overfitting flag
overfit_flag <- (delta_train_r2_int - delta_test_r2_int) > 0.02

# Recommendation
int_recommendation <- if (delta_test_r2_int >= 0.02 && !overfit_flag) {
  list(
    decision = "ADVANCE_FOR_PRUNING",
    justification = sprintf(
      paste0("OLS_INTERACTION_FULL achieves delta test R² = %+.4f vs OLS_ADDITIVE_FINAL (%.4f → %.4f). ",
             "Improvement is material and generalizes to held-out data. ",
             "Recommend pruning: fit a reduced interaction model retaining only significant interaction terms."),
      delta_test_r2_int, perf_add$test$r2, perf_int$test$r2
    )
  )
} else if (delta_test_r2_int >= 0.005 && !overfit_flag) {
  list(
    decision = "BORDERLINE_ADVANCE_FOR_PRUNING",
    justification = sprintf(
      paste0("OLS_INTERACTION_FULL achieves delta test R² = %+.4f vs OLS_ADDITIVE_FINAL (%.4f → %.4f). ",
             "Improvement is modest. BIC delta = %.0f. ",
             "Pruning may or may not recover a useful specification; decision is borderline. ",
             "Recommend pruning only if specific interaction terms have clear business interpretation."),
      delta_test_r2_int, perf_add$test$r2, perf_int$test$r2,
      bic_int - bic_add
    )
  )
} else {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0("OLS_INTERACTION_FULL achieves delta test R² = %+.4f vs OLS_ADDITIVE_FINAL (%.4f → %.4f). ",
             "Out-of-sample performance collapses: the model fails catastrophically on held-out data ",
             "due to a rank-deficient fit (R warning: 'prediction from rank-deficient fit; doubtful cases'). ",
             "AIC and BIC both improved numerically (AIC delta = %.0f, BIC delta = %.0f) but are not ",
             "reliable for a rank-deficient model — the training log-likelihood improvement reflects ",
             "overfitting of 628 interaction terms, not generalizable signal. ",
             "Complexity expanded from %d to %d estimated coefficients with %d aliased. ",
             "Recommend retaining OLS_ADDITIVE_FINAL as the OLS specification. ",
             "Note: log_prev_wp:PROD_ABBR interactions show strong signal in isolation ",
             "(|t| > 40 for GARAGE, WORKCOMP) and may merit a targeted future test."),
      delta_test_r2_int, perf_add$test$r2, perf_int$test$r2,
      aic_int - aic_add, bic_int - bic_add,
      nrow(c_add), nrow(c_int), n_aliased_int
    )
  )
}

interaction_full_out <- list(
  description = paste0(
    "Full pairwise interaction evaluation. OLS_INTERACTION_FULL = (OLS_ADDITIVE_FINAL terms)^2. ",
    "Purpose: determine whether interaction effects materially improve out-of-sample predictive ",
    "performance over the additive baseline. Does not advance to pruning."
  ),
  row_count_note = row_count_note,
  part_a_ols_interaction_full = list(
    model               = "OLS_INTERACTION_FULL",
    formula             = "(log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR)^2",
    n_candidate_train   = n_cand_train,
    n_candidate_test    = n_cand_test,
    n_eval_train        = perf_int$n_train,
    n_eval_test         = perf_int$n_test,
    train               = perf_int$train,
    test                = perf_int$test
  ),
  part_b_comparison_vs_additive = list(
    ols_additive_final   = list(train = perf_add$train, test = perf_add$test),
    ols_interaction_full = list(train = perf_int$train, test = perf_int$test),
    delta_train_r2       = delta_train_r2_int,
    delta_test_r2        = delta_test_r2_int,
    delta_train_rmse     = delta_train_rmse_int,
    delta_test_rmse      = delta_test_rmse_int,
    delta_train_mae      = delta_train_mae_int,
    delta_test_mae       = delta_test_mae_int
  ),
  part_c_complexity = list(
    ols_additive_final = list(
      n_estimated   = nrow(c_add),
      n_significant = sum(c_add$significant_p05, na.rm = TRUE),
      aic           = aic_add,
      bic           = bic_add
    ),
    ols_interaction_full = list(
      n_in_formula       = n_total_int_formula,
      n_aliased          = n_aliased_int,
      n_estimated        = n_estimated_int,
      n_main_effects     = n_main_int,
      n_interaction_terms = n_interaction_terms,
      n_sig_main          = n_sig_main,
      n_sig_interaction   = n_sig_interaction,
      aic                 = aic_int,
      bic                 = bic_int
    ),
    delta_aic = round(aic_int - aic_add, 2),
    delta_bic = round(bic_int - bic_add, 2)
  ),
  part_d_interaction_terms = list(
    top_20_most_positive_significant = top20_pos_int,
    top_20_most_negative_significant = top20_neg_int,
    by_interaction_type              = int_type_groups
  ),
  part_e_overfitting = list(
    delta_train_r2              = delta_train_r2_int,
    delta_test_r2               = delta_test_r2_int,
    delta_train_rmse            = delta_train_rmse_int,
    delta_test_rmse             = delta_test_rmse_int,
    overfit_flag                = overfit_flag,
    overfit_criterion           = "Flagged if (delta_train_r2 - delta_test_r2) > 0.02",
    rank_deficient_warning      = TRUE,
    rank_deficient_note         = paste0(
      "R issued a rank-deficient fit warning during test-set prediction: ",
      "'prediction from rank-deficient fit; attr(*, non-estim) has doubtful cases'. ",
      "The model matrix has 82 aliased (exactly collinear) terms. ",
      "For test rows involving interaction patterns not seen in training, ",
      "the corresponding coefficients are undetermined and predictions are unreliable. ",
      "This causes the catastrophic test R² = -42.16."
    ),
    bic_improved_not_worsened   = (bic_int < bic_add),
    bic_delta                   = round(bic_int - bic_add, 2),
    bic_note                    = paste0(
      "AIC and BIC both improved substantially (lower is better): AIC delta = ",
      round(aic_int - aic_add, 0), ", BIC delta = ", round(bic_int - bic_add, 0), ". ",
      "This reflects the large improvement in training log-likelihood from 628 interaction terms. ",
      "However, AIC/BIC are not reliable for rank-deficient models — they assume the model ",
      "is well-identified, which is violated here. The training improvement captured by AIC/BIC ",
      "is real but corresponds to overfitting, not genuine signal extraction."
    ),
    interpretation          = paste0(
      "Catastrophic test performance (R² = -42.16, RMSE = 14.52 vs additive RMSE = 1.09). ",
      "Train R² improved by +0.0487 while test R² collapsed. ",
      "Cause: rank-deficient model with 628 interaction terms memorizes training patterns; ",
      "many product × vendor and product × state interactions are specific to training data. ",
      "AIC/BIC show improvement but are not valid for rank-deficient fits."
    )
  ),
  part_f_recommendation = int_recommendation
)

# =============================================================================
# Assemble ols_additive_final.json
# =============================================================================

# Aliased (NA) coefficients — present in coef() but dropped by summary()
aliased_terms_add <- names(which(is.na(coef(m_add))))
n_coef_total_add  <- length(coef(m_add))
n_aliased_add     <- length(aliased_terms_add)

delta_test_r2_add  <- round(perf_add$test$r2   - perf3$test$r2,   4)
delta_test_rmse_add <- round(perf_add$test$rmse - perf3$test$rmse, 4)
delta_test_mae_add  <- round(perf_add$test$mae  - perf3$test$mae,  4)

add_recommendation <- if (abs(delta_test_r2_add) <= 0.001) {
  list(
    replace_ols3_as_baseline = TRUE,
    decision = "REPLACE",
    justification = sprintf(
      paste0("OLS_ADDITIVE_FINAL achieves test R² %.4f vs OLS 3 test R² %.4f (delta = %+.4f). ",
             "Performance is statistically equivalent. ",
             "Removing PROD_LINE eliminates a structurally redundant degree of freedom whose ",
             "coefficient was unstable across populations (+1.85 in OLS 3 full vs -0.21 in OLS 3CC). ",
             "PROD_ABBR fully captures product-level variation. OLS_ADDITIVE_FINAL is the preferred ",
             "additive baseline for all subsequent modeling (interaction testing, Random Forest, LMM)."),
      perf_add$test$r2, perf3$test$r2, delta_test_r2_add
    )
  )
} else if (delta_test_r2_add > 0.001) {
  list(
    replace_ols3_as_baseline = TRUE,
    decision = "REPLACE",
    justification = sprintf(
      paste0("OLS_ADDITIVE_FINAL achieves test R² %.4f vs OLS 3 test R² %.4f (delta = %+.4f). ",
             "Performance improved by removing the redundant PROD_LINE term. ",
             "OLS_ADDITIVE_FINAL is the preferred additive baseline."),
      perf_add$test$r2, perf3$test$r2, delta_test_r2_add
    )
  )
} else {
  list(
    replace_ols3_as_baseline = FALSE,
    decision = "RETAIN_OLS3",
    justification = sprintf(
      paste0("OLS_ADDITIVE_FINAL achieves test R² %.4f vs OLS 3 test R² %.4f (delta = %+.4f). ",
             "Unexpectedly, removing PROD_LINE reduces performance. Investigate before replacing OLS 3."),
      perf_add$test$r2, perf3$test$r2, delta_test_r2_add
    )
  )
}

additive_final_out <- list(
  description = paste0(
    "Additive baseline finalization. OLS_ADDITIVE_FINAL = OLS 3 with PROD_LINE removed. ",
    "PROD_ABBR determines PROD_LINE exactly (nested relationship); the PROD_LINE term is ",
    "structurally redundant. Removing it produces a cleaner, more stable specification."
  ),
  part_a_ols_additive_final = list(
    model       = "OLS_ADDITIVE_FINAL",
    formula     = "log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR",
    predictors_excluded_vs_ols3 = "PROD_LINE",
    n_train     = perf_add$n_train,
    n_test      = perf_add$n_test,
    train       = perf_add$train,
    test        = perf_add$test
  ),
  part_b_comparison_vs_ols3 = list(
    ols3 = list(
      train = perf3$train,
      test  = perf3$test
    ),
    ols_additive_final = list(
      train = perf_add$train,
      test  = perf_add$test
    ),
    delta_train_r2    = round(perf_add$train$r2   - perf3$train$r2,   4),
    delta_test_r2     = delta_test_r2_add,
    delta_train_rmse  = round(perf_add$train$rmse - perf3$train$rmse, 4),
    delta_test_rmse   = delta_test_rmse_add,
    delta_train_mae   = round(perf_add$train$mae  - perf3$train$mae,  4),
    delta_test_mae    = delta_test_mae_add
  ),
  part_c_coefficient_review = list(
    n_coefficients_in_formula    = n_coef_total_add,
    n_aliased                    = n_aliased_add,
    aliased_terms                = if (n_aliased_add == 0) "none" else aliased_terms_add,
    n_estimated                  = nrow(c_add),
    n_significant_p05            = sum(c_add$significant_p05, na.rm = TRUE),
    n_nonsignificant             = sum(!c_add$significant_p05, na.rm = TRUE),
    ols3_coefficient_count       = nrow(c3),
    delta_vs_ols3                = nrow(c_add) - nrow(c3),
    prod_line_instability_note   = paste0(
      "In OLS 3, PROD_LINEPL = +1.849 on the full population but PROD_LINEPL = -0.209 on the ",
      "LR CC population — a sign flip of magnitude 2.06. This instability arises because PROD_LINE ",
      "is a linear combination of PROD_ABBR indicators; the partial effect of PROD_LINE cannot be ",
      "cleanly separated from PROD_ABBR when both are in the same model. Removing PROD_LINE ",
      "stabilizes the PROD_ABBR coefficients."
    ),
    largest_significant_effects = top_effects(c_add)
  ),
  part_d_baseline_decision = add_recommendation
)

# =============================================================================
# Assemble ols_summary.json
# =============================================================================

plr_result <- get_coef_row(c4, "PREV_LOSS_RATIO")
prr_result <- get_coef_row(c5, "PREV_RETENTION_RATIO")

lp_prog <- list(
  OLS_0                     = get_coef(c0,     "log_prev_wp"),
  OLS_1                     = get_coef(c1,     "log_prev_wp"),
  OLS_2                     = get_coef(c2,     "log_prev_wp"),
  OLS_3                     = get_coef(c3,     "log_prev_wp"),
  OLS_3CC                   = get_coef(c3cc,   "log_prev_wp"),
  OLS_4CC                   = get_coef(c4,     "log_prev_wp"),
  OLS_3_RET_CC              = get_coef(c3_ret, "log_prev_wp"),
  OLS_5                     = get_coef(c5,     "log_prev_wp"),
  OLS_ADDITIVE_FINAL        = get_coef(c_add,  "log_prev_wp"),
  OLS_INTERACTION_FULL         = get_coef(c_int,  "log_prev_wp"),
  OLS_INTERACTION_NUMERIC_ONLY = get_coef(c_nno,  "log_prev_wp"),
  OLS_INTERACTION_SUPPORTED    = get_coef(c_supp, "log_prev_wp")
)

r2_prog <- data.frame(
  model      = c("OLS_0", "OLS_1", "OLS_2", "OLS_3", "OLS_3CC", "OLS_4CC",
                 "OLS_3_RET_CC", "OLS_5", "OLS_ADDITIVE_FINAL",
                 "OLS_INTERACTION_FULL", "OLS_INTERACTION_NUMERIC_ONLY", "OLS_INTERACTION_SUPPORTED"),
  population = c("full", "full", "full", "full", "LR_CC", "LR_CC",
                 "RET_CC", "RET_CC", "full", "full", "full", "full"),
  n_train    = c(perf0$n_train, perf1$n_train, perf2$n_train, perf3$n_train,
                 perf3cc$n_train, perf4$n_train, perf3_ret$n_train, perf5$n_train,
                 perf_add$n_train, perf_int$n_train, perf_nno$n_train, perf_supp$n_train),
  n_test     = c(perf0$n_test,  perf1$n_test,  perf2$n_test,  perf3$n_test,
                 perf3cc$n_test, perf4$n_test,  perf3_ret$n_test, perf5$n_test,
                 perf_add$n_test, perf_int$n_test, perf_nno$n_test, perf_supp$n_test),
  train_r2   = c(perf0$train$r2,   perf1$train$r2,   perf2$train$r2,
                 perf3$train$r2,   perf3cc$train$r2,  perf4$train$r2,
                 perf3_ret$train$r2, perf5$train$r2,  perf_add$train$r2,
                 perf_int$train$r2, perf_nno$train$r2, perf_supp$train$r2),
  test_r2    = c(perf0$test$r2,    perf1$test$r2,    perf2$test$r2,
                 perf3$test$r2,    perf3cc$test$r2,   perf4$test$r2,
                 perf3_ret$test$r2, perf5$test$r2,    perf_add$test$r2,
                 perf_int$test$r2,  perf_nno$test$r2,  perf_supp$test$r2),
  train_rmse = c(perf0$train$rmse, perf1$train$rmse, perf2$train$rmse,
                 perf3$train$rmse, perf3cc$train$rmse, perf4$train$rmse,
                 perf3_ret$train$rmse, perf5$train$rmse, perf_add$train$rmse,
                 perf_int$train$rmse,  perf_nno$train$rmse, perf_supp$train$rmse),
  test_rmse  = c(perf0$test$rmse,  perf1$test$rmse,  perf2$test$rmse,
                 perf3$test$rmse,  perf3cc$test$rmse,  perf4$test$rmse,
                 perf3_ret$test$rmse, perf5$test$rmse,  perf_add$test$rmse,
                 perf_int$test$rmse,  perf_nno$test$rmse, perf_supp$test$rmse),
  train_mae  = c(perf0$train$mae,  perf1$train$mae,  perf2$train$mae,
                 perf3$train$mae,  perf3cc$train$mae,  perf4$train$mae,
                 perf3_ret$train$mae, perf5$train$mae,  perf_add$train$mae,
                 perf_int$train$mae,  perf_nno$train$mae, perf_supp$train$mae),
  test_mae   = c(perf0$test$mae,   perf1$test$mae,   perf2$test$mae,
                 perf3$test$mae,   perf3cc$test$mae,   perf4$test$mae,
                 perf3_ret$test$mae, perf5$test$mae,   perf_add$test$mae,
                 perf_int$test$mae,  perf_nno$test$mae,  perf_supp$test$mae)
)

decomposition_out <- list(
  description = paste0(
    "Decomposes each CC model's performance gain into population selection vs predictor contribution. ",
    "For PREV_LOSS_RATIO: OLS 3 → OLS 3CC (selection) vs OLS 3CC → OLS 4CC (predictor). ",
    "For PREV_RETENTION_RATIO: OLS 3 → OLS_3_RET_CC (selection) vs OLS_3_RET_CC → OLS_5 (predictor)."
  ),
  prev_loss_ratio = list(
    population_selection = list(
      comparison         = "OLS 3 (full) vs OLS 3CC (LR CC, same formula)",
      n_full_train       = nrow(wp_train),
      n_cc_train         = nrow(wp_train_cc),
      pct_dropped_train  = round((nrow(wp_train) - nrow(wp_train_cc)) / nrow(wp_train) * 100, 2),
      delta_test_r2      = round(perf3cc$test$r2 - perf3$test$r2, 4)
    ),
    predictor_contribution = list(
      comparison         = "OLS 3CC vs OLS 4CC: same LR CC population",
      delta_train_r2     = round(perf4$train$r2   - perf3cc$train$r2,   4),
      delta_test_r2      = round(perf4$test$r2    - perf3cc$test$r2,    4),
      delta_test_rmse    = round(perf4$test$rmse  - perf3cc$test$rmse,  4),
      coefficient        = plr_result
    )
  ),
  prev_retention_ratio = list(
    population_selection = list(
      comparison         = "OLS 3 (full) vs OLS_3_RET_CC (RET CC, same formula)",
      n_full_train       = nrow(wp_train),
      n_cc_train         = nrow(wp_train_ret),
      pct_dropped_train  = round((nrow(wp_train) - nrow(wp_train_ret)) / nrow(wp_train) * 100, 2),
      delta_test_r2      = round(perf3_ret$test$r2 - perf3$test$r2, 4)
    ),
    predictor_contribution = list(
      comparison         = "OLS_3_RET_CC vs OLS_5: same RET CC population",
      delta_train_r2     = round(perf5$train$r2   - perf3_ret$train$r2,   4),
      delta_test_r2      = round(perf5$test$r2    - perf3_ret$test$r2,    4),
      delta_test_rmse    = round(perf5$test$rmse  - perf3_ret$test$rmse,  4),
      coefficient        = prr_result
    )
  )
)

ols_summary_out <- list(
  description = paste0(
    "OLS 0-3, OLS 3CC, OLS 4CC, OLS_3_RET_CC, OLS_5, OLS_ADDITIVE_FINAL, and interaction model progression ",
    "for log(WRTN_PREM_AMT + 1). ",
    "Exploratory phase: OLS_ADDITIVE_FINAL is the recommended additive baseline; ",
    "all three interaction variants (FULL, SUPPORTED, NUMERIC_ONLY) failed to improve out-of-sample performance. ",
    "OLS specification is locked as OLS_ADDITIVE_FINAL (test R² = 0.7552)."
  ),
  row_count_note = row_count_note,
  current_baseline = list(
    model = "OLS_ADDITIVE_FINAL",
    rationale = "OLS 3 with PROD_LINE removed. Equivalent predictive performance. Stable coefficients. No redundant terms."
  ),
  model_descriptions                       = model_descriptions,
  performance_table                        = df_to_list(r2_prog),
  population_predictor_decomposition       = decomposition_out,
  log_prev_wp_coefficient_progression      = lp_prog,
  prev_loss_ratio_OLS_4CC                  = plr_result,
  prev_retention_ratio_OLS_5               = prr_result,
  largest_significant_effects_OLS_3              = top_effects(c3),
  largest_significant_effects_OLS_3CC            = top_effects(c3cc),
  largest_significant_effects_OLS_4CC            = top_effects(c4),
  largest_significant_effects_OLS_3_RET_CC       = top_effects(c3_ret),
  largest_significant_effects_OLS_5              = top_effects(c5),
  largest_significant_effects_OLS_ADDITIVE_FINAL        = top_effects(c_add),
  largest_significant_effects_OLS_INTERACTION_FULL         = top_effects(c_int),
  largest_significant_effects_OLS_INTERACTION_NUMERIC_ONLY = top_effects(c_nno),
  largest_significant_effects_OLS_INTERACTION_SUPPORTED    = top_effects(c_supp),
  notes = paste0(
    "STAT_PROFILE_DATE_YEAR is included as a continuous numeric variable (not a factor) in OLS 3+. ",
    "Treating it as a factor would prevent prediction for test years 2013-2014 (unseen levels). ",
    "OLS_ADDITIVE_FINAL removes PROD_LINE from OLS 3: PROD_ABBR determines PROD_LINE exactly, ",
    "making the PROD_LINE term structurally redundant and unstable across populations. ",
    "OLS 3CC and OLS 4CC use the PREV_LOSS_RATIO CC subset (~91.2% of full data). ",
    "OLS_3_RET_CC and OLS_5 use the PREV_RETENTION_RATIO CC subset (~37% of full data). ",
    "For each lagged predictor test, the CC baseline is the correct comparison. ",
    "Direct comparison to OLS 3 (full pop) confounds population selection with predictor contribution. ",
    "Row counts: see row_count_note field. The candidate population (",
    format(n_cand_train, big.mark = ","), " train / ", format(n_cand_test, big.mark = ","), " test) ",
    "differs from the effective evaluated rows (",
    format(n_eval_train, big.mark = ","), " train / ", format(n_eval_test, big.mark = ","), " test) ",
    "because ", n_dropped_train, " train and ", n_dropped_test, " test rows have NA in ",
    "log_prev_wp or log_prev_poly and are excluded from model fitting and metric calculations."
  )
)

# =============================================================================
# Assemble ols_interaction_numeric_only.json
# =============================================================================

aliased_nno   <- names(which(is.na(coef(m_nno))))
n_aliased_nno <- length(aliased_nno)
n_total_nno   <- length(coef(m_nno))

# Pre-compute for reference in comparison tables (full definitions in supported_out section)
n_aliased_supp_pre <- sum(is.na(coef(m_supp)))

delta_tr_r2_nno   <- round(perf_nno$train$r2   - perf_add$train$r2,   4)
delta_te_r2_nno   <- round(perf_nno$test$r2    - perf_add$test$r2,    4)
delta_tr_rmse_nno <- round(perf_nno$train$rmse - perf_add$train$rmse, 4)
delta_te_rmse_nno <- round(perf_nno$test$rmse  - perf_add$test$rmse,  4)
delta_tr_mae_nno  <- round(perf_nno$train$mae  - perf_add$train$mae,  4)
delta_te_mae_nno  <- round(perf_nno$test$mae   - perf_add$test$mae,   4)

c_nno_int  <- c_nno[grepl(":", c_nno$term), ]
c_nno_main <- c_nno[!grepl(":", c_nno$term), ]
n_sig_int_nno <- sum(c_nno_int$significant_p05, na.rm = TRUE)

nno_rank_deficient <- (n_aliased_nno > 0)
overfit_nno        <- (delta_tr_r2_nno - delta_te_r2_nno) > 0.02

# Per-term detail for all 21 numeric × numeric interactions
nno_int_detail <- lapply(seq_len(nrow(c_nno_int)), function(i) {
  r <- c_nno_int[i, ]
  a <- strsplit(r$term, ":")[[1]]
  list(
    term            = r$term,
    var1            = a[1],
    var2            = a[2],
    estimate        = r$estimate,
    std_error       = r$std_error,
    t_stat          = r$t_stat,
    p_value         = r$p_value,
    significant_p05 = r$significant_p05
  )
})

# Strongest positive/negative/most significant
nno_sig <- c_nno_int[!is.na(c_nno_int$significant_p05) & c_nno_int$significant_p05, ]
nno_strongest_pos  <- if (nrow(nno_sig) > 0) nno_sig$term[which.max(nno_sig$estimate)] else "none"
nno_strongest_neg  <- if (nrow(nno_sig) > 0) nno_sig$term[which.min(nno_sig$estimate)] else "none"
nno_most_sig       <- if (nrow(c_nno_int) > 0) c_nno_int$term[which.min(c_nno_int$p_value)] else "none"

nno_recommendation <- if (nno_rank_deficient) {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_NUMERIC_ONLY is rank-deficient (%d aliased terms). ",
        "Retain OLS_ADDITIVE_FINAL."
      ),
      n_aliased_nno
    )
  )
} else if (delta_te_r2_nno < 0) {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_NUMERIC_ONLY reduces test R² by %.4f (additive: %.4f → numeric-only: %.4f). ",
        "Test RMSE worsens by %.4f. Numeric interactions do not improve out-of-sample performance. ",
        "Retain OLS_ADDITIVE_FINAL. Close the OLS interaction branch and proceed to Random Forest."
      ),
      abs(delta_te_r2_nno), perf_add$test$r2, perf_nno$test$r2, delta_te_rmse_nno
    )
  )
} else if (delta_te_r2_nno >= 0.005 && !overfit_nno) {
  list(
    decision = "ADVANCE_OLS_INTERACTION_NUMERIC_ONLY",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_NUMERIC_ONLY achieves delta test R² = %+.4f (additive: %.4f → numeric-only: %.4f). ",
        "No rank deficiency. Train/test gap within acceptable range (gap delta = %.4f). ",
        "AIC delta = %.0f, BIC delta = %.0f. ",
        "%d of 21 numeric interactions significant at p<0.05. ",
        "Recommend adopting as improved OLS specification."
      ),
      delta_te_r2_nno, perf_add$test$r2, perf_nno$test$r2,
      delta_tr_r2_nno - delta_te_r2_nno,
      aic_nno - aic_add, bic_nno - bic_add,
      n_sig_int_nno
    )
  )
} else if (delta_te_r2_nno >= 0.001) {
  list(
    decision = "CONSIDER_NUMERIC_INTERACTION_SUBSET",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_NUMERIC_ONLY achieves delta test R² = %+.4f (additive: %.4f → numeric-only: %.4f). ",
        "Improvement is marginal. AIC delta = %.0f, BIC delta = %.0f. ",
        "Train/test gap delta = %.4f. %d of 21 terms significant. ",
        "Consider a smaller subset targeting only the most impactful numeric interactions."
      ),
      delta_te_r2_nno, perf_add$test$r2, perf_nno$test$r2,
      aic_nno - aic_add, bic_nno - bic_add,
      delta_tr_r2_nno - delta_te_r2_nno,
      n_sig_int_nno
    )
  )
} else {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_NUMERIC_ONLY achieves delta test R² = %+.4f (additive: %.4f → numeric-only: %.4f). ",
        "Out-of-sample improvement is negligible. ",
        "The 21 numeric interaction terms add complexity without meaningful generalization gain. ",
        "Retain OLS_ADDITIVE_FINAL. Close the OLS interaction branch and proceed to Random Forest."
      ),
      delta_te_r2_nno, perf_add$test$r2, perf_nno$test$r2
    )
  )
}

numeric_only_out <- list(
  description = paste0(
    "Numeric × numeric interaction evaluation. OLS_INTERACTION_NUMERIC_ONLY includes all ",
    "OLS_ADDITIVE_FINAL main effects plus all C(7,2)=21 pairwise numeric × numeric interactions. ",
    "All categorical interactions are excluded (numeric × PROD_ABBR, numeric × STATE_ABBR, ",
    "numeric × VENDOR, and all cat × cat). ",
    "This is the third and final OLS interaction test. ",
    "OLS_INTERACTION_FULL (all pairwise, 628 interaction terms) failed due to 82 aliased terms from ",
    "sparse categorical cells. OLS_INTERACTION_SUPPORTED (numeric × cat + numeric × numeric, 279 interaction terms) ",
    "failed due to 22 aliased terms from sparse individual PROD_ABBR and VENDOR levels. ",
    "This model removes all categorical interaction risk."
  ),
  row_count_note = row_count_note,
  part_a_model_specification = list(
    model    = "OLS_INTERACTION_NUMERIC_ONLY",
    baseline = "OLS_ADDITIVE_FINAL",
    formula  = paste0(
      "log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + ",
      "MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR + ",
      "log_prev_wp:log_prev_poly + log_prev_wp:ACTIVE_PRODUCERS + ",
      "log_prev_wp:AGENCY_APPOINTMENT_YEAR + log_prev_wp:MAX_AGE + ",
      "log_prev_wp:MIN_AGE + log_prev_wp:STAT_PROFILE_DATE_YEAR + ",
      "log_prev_poly:ACTIVE_PRODUCERS + log_prev_poly:AGENCY_APPOINTMENT_YEAR + ",
      "log_prev_poly:MAX_AGE + log_prev_poly:MIN_AGE + log_prev_poly:STAT_PROFILE_DATE_YEAR + ",
      "ACTIVE_PRODUCERS:AGENCY_APPOINTMENT_YEAR + ACTIVE_PRODUCERS:MAX_AGE + ",
      "ACTIVE_PRODUCERS:MIN_AGE + ACTIVE_PRODUCERS:STAT_PROFILE_DATE_YEAR + ",
      "AGENCY_APPOINTMENT_YEAR:MAX_AGE + AGENCY_APPOINTMENT_YEAR:MIN_AGE + ",
      "AGENCY_APPOINTMENT_YEAR:STAT_PROFILE_DATE_YEAR + MAX_AGE:MIN_AGE + ",
      "MAX_AGE:STAT_PROFILE_DATE_YEAR + MIN_AGE:STAT_PROFILE_DATE_YEAR"
    ),
    excluded_interaction_families = list(
      "numeric × PROD_ABBR (removed — sparse individual PROD_ABBR levels caused 22 aliased terms in OLS_INTERACTION_SUPPORTED)",
      "numeric × STATE_ABBR (removed — part of supported failure)",
      "numeric × VENDOR (removed — VENDORF caused 3 aliased terms in OLS_INTERACTION_SUPPORTED)",
      "PROD_ABBR × STATE_ABBR (QUESTIONABLE in feasibility review)",
      "PROD_ABBR × VENDOR (QUESTIONABLE in feasibility review)",
      "STATE_ABBR × VENDOR (QUESTIONABLE in feasibility review)"
    ),
    n_candidate_train = n_cand_train,
    n_candidate_test  = n_cand_test,
    n_eval_train      = perf_nno$n_train,
    n_eval_test       = perf_nno$n_test
  ),
  part_b_comparison = list(
    models = list(
      ols_additive_final = list(
        train = perf_add$train, test = perf_add$test,
        n_coefficients = nrow(c_add), n_aliased = 0L
      ),
      ols_interaction_full = list(
        train = perf_int$train, test = perf_int$test,
        n_coefficients = nrow(c_int), n_aliased = n_aliased_int,
        note = "Rank-deficient — 82 aliased terms — test R² invalid"
      ),
      ols_interaction_supported = list(
        train = perf_supp$train, test = perf_supp$test,
        n_coefficients = nrow(c_supp), n_aliased = n_aliased_supp_pre,
        note = "Rank-deficient — 22 aliased terms — test R² invalid"
      ),
      ols_interaction_numeric_only = list(
        train = perf_nno$train, test = perf_nno$test,
        n_coefficients = nrow(c_nno), n_aliased = n_aliased_nno
      )
    ),
    delta_vs_additive = list(
      delta_train_r2   = delta_tr_r2_nno,
      delta_test_r2    = delta_te_r2_nno,
      delta_train_rmse = delta_tr_rmse_nno,
      delta_test_rmse  = delta_te_rmse_nno,
      delta_train_mae  = delta_tr_mae_nno,
      delta_test_mae   = delta_te_mae_nno
    )
  ),
  part_c_complexity = list(
    n_formula_terms           = n_total_nno,
    n_estimated_coefficients  = nrow(c_nno),
    n_aliased                 = n_aliased_nno,
    aliased_terms             = if (n_aliased_nno == 0) "none" else aliased_nno,
    n_main_effect_coefs       = nrow(c_nno_main),
    n_numeric_x_numeric_coefs = nrow(c_nno_int),
    n_sig_interactions_p05    = n_sig_int_nno,
    aic = aic_nno,
    bic = bic_nno,
    aic_delta_vs_additive = round(aic_nno - aic_add, 2),
    bic_delta_vs_additive = round(bic_nno - bic_add, 2),
    aic_additive_final        = aic_add,
    bic_additive_final        = bic_add,
    aic_interaction_full      = aic_int,
    bic_interaction_full      = bic_int,
    aic_interaction_supported = aic_supp,
    bic_interaction_supported = bic_supp,
    aic_note = if (n_aliased_nno > 0) {
      "AIC/BIC unreliable — rank-deficient model"
    } else {
      "AIC/BIC valid — full-rank model"
    }
  ),
  part_d_numeric_interaction_effects = list(
    all_21_terms             = nno_int_detail,
    strongest_positive_term  = nno_strongest_pos,
    strongest_negative_term  = nno_strongest_neg,
    most_significant_term    = nno_most_sig,
    n_significant_p05        = n_sig_int_nno,
    practical_significance_note = if (n_sig_int_nno == 0) {
      "No numeric interactions are statistically significant."
    } else if (max(abs(c_nno_int$estimate), na.rm = TRUE) < 0.001) {
      "Statistically significant interactions exist but all have very small coefficients — likely not practically meaningful."
    } else {
      sprintf(
        "%d of 21 numeric interactions are significant at p<0.05. Largest absolute coefficient: %.6f (term: %s).",
        n_sig_int_nno,
        max(abs(c_nno_int$estimate), na.rm = TRUE),
        c_nno_int$term[which.max(abs(c_nno_int$estimate))]
      )
    }
  ),
  part_e_generalization = list(
    delta_train_r2          = delta_tr_r2_nno,
    delta_test_r2           = delta_te_r2_nno,
    train_test_r2_gap_delta = round(delta_tr_r2_nno - delta_te_r2_nno, 4),
    test_rmse_improves      = (delta_te_rmse_nno < 0),
    test_mae_improves       = (delta_te_mae_nno  < 0),
    rank_deficient          = nno_rank_deficient,
    n_aliased_terms         = n_aliased_nno,
    overfit_flag            = overfit_nno,
    overfit_criterion       = "Flagged if (delta_train_r2 - delta_test_r2) > 0.02",
    prediction_warning      = if (nno_rank_deficient) {
      "Rank-deficiency warning during prediction"
    } else {
      "No rank-deficiency warning — full-rank fit"
    },
    interpretation = if (nno_rank_deficient) {
      "Model is rank-deficient. Generalization results unreliable."
    } else if (delta_te_r2_nno < 0) {
      sprintf(
        "Test R² decreases by %.4f. Numeric interactions do not generalize. Keep OLS_ADDITIVE_FINAL.",
        abs(delta_te_r2_nno)
      )
    } else if (overfit_nno) {
      sprintf(
        "Train improvement (%.4f) substantially exceeds test improvement (%.4f). Overfitting detected.",
        delta_tr_r2_nno, delta_te_r2_nno
      )
    } else {
      sprintf(
        "Train R² improves by %.4f and test R² improves by %.4f. Train/test gap delta = %.4f.",
        delta_tr_r2_nno, delta_te_r2_nno, delta_tr_r2_nno - delta_te_r2_nno
      )
    }
  ),
  part_f_recommendation = nno_recommendation
)

# =============================================================================
# Assemble ols_interaction_supported.json
# =============================================================================

# Aliased terms for OLS_INTERACTION_SUPPORTED
aliased_supp    <- names(which(is.na(coef(m_supp))))
n_aliased_supp  <- length(aliased_supp)
n_total_supp    <- length(coef(m_supp))

# Deltas vs OLS_ADDITIVE_FINAL
delta_tr_r2_supp   <- round(perf_supp$train$r2   - perf_add$train$r2,   4)
delta_te_r2_supp   <- round(perf_supp$test$r2    - perf_add$test$r2,    4)
delta_tr_rmse_supp <- round(perf_supp$train$rmse - perf_add$train$rmse, 4)
delta_te_rmse_supp <- round(perf_supp$test$rmse  - perf_add$test$rmse,  4)
delta_tr_mae_supp  <- round(perf_supp$train$mae  - perf_add$train$mae,  4)
delta_te_mae_supp  <- round(perf_supp$test$mae   - perf_add$test$mae,   4)

# Term family breakdown
c_supp_int   <- c_supp[grepl(":", c_supp$term), ]
c_supp_main  <- c_supp[!grepl(":", c_supp$term), ]

c_supp_int$interaction_type <- sapply(c_supp_int$term, classify_interaction_term)

n_numnum  <- sum(c_supp_int$interaction_type == "numeric x numeric")
n_numprod <- sum(c_supp_int$interaction_type == "numeric x product")
n_numst   <- sum(c_supp_int$interaction_type == "numeric x state")
n_numvend <- sum(c_supp_int$interaction_type == "numeric x vendor")
n_sig_int_supp <- sum(c_supp_int$significant_p05, na.rm = TRUE)

# Top 20 interactions by estimate
sig_int_supp <- c_supp_int[c_supp_int$significant_p05 & !is.na(c_supp_int$significant_p05), ]
top20_pos_supp <- df_to_list(head(sig_int_supp[order(-sig_int_supp$estimate), ], 20))
top20_neg_supp <- df_to_list(head(sig_int_supp[order( sig_int_supp$estimate), ], 20))

# Per-family summary helper
fam_summary <- function(fam_type, type_note) {
  rows <- c_supp_int[c_supp_int$interaction_type == fam_type, ]
  if (nrow(rows) == 0) return(list(n_terms = 0L))
  list(
    n_terms          = nrow(rows),
    n_significant    = sum(rows$significant_p05, na.rm = TRUE),
    largest_abs_coef = round(max(abs(rows$estimate), na.rm = TRUE), 6),
    interpretation   = type_note
  )
}

# Rank-deficiency check for generalization review
supp_rank_deficient <- (n_aliased_supp > 0)
overfit_supp <- (delta_tr_r2_supp - delta_te_r2_supp) > 0.02

# Recommendation logic
supp_recommendation <- if (supp_rank_deficient) {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_SUPPORTED has %d aliased term(s) — rank deficiency persists despite ",
        "excluding cat × cat interactions. AIC/BIC are unreliable. ",
        "Retain OLS_ADDITIVE_FINAL as the OLS specification."
      ),
      n_aliased_supp
    )
  )
} else if (delta_te_r2_supp < 0) {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_SUPPORTED reduces test R² by %.4f (additive: %.4f → supported: %.4f). ",
        "Test RMSE worsens by %.4f. Interactions do not improve out-of-sample performance. ",
        "Retain OLS_ADDITIVE_FINAL."
      ),
      abs(delta_te_r2_supp), perf_add$test$r2, perf_supp$test$r2, delta_te_rmse_supp
    )
  )
} else if (delta_te_r2_supp >= 0.02 && !overfit_supp) {
  list(
    decision = "ADVANCE_FOR_PRUNING",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_SUPPORTED achieves delta test R² = %+.4f (additive: %.4f → supported: %.4f). ",
        "Out-of-sample improvement is material and the train/test gap is within acceptable range. ",
        "Recommend pruning: fit a reduced interaction model retaining only ",
        "the most impactful and interpretable interaction families."
      ),
      delta_te_r2_supp, perf_add$test$r2, perf_supp$test$r2
    )
  )
} else if (delta_te_r2_supp >= 0.005) {
  list(
    decision = "ADVANCE_FOR_PRUNING",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_SUPPORTED achieves delta test R² = %+.4f (additive: %.4f → supported: %.4f). ",
        "Out-of-sample improvement is modest but consistent. AIC delta = %.0f, BIC delta = %.0f. ",
        "Train/test R² gap delta = %.4f. ",
        "Recommend pruning: a reduced model targeting the strongest interaction families ",
        "may recover most of this gain with fewer terms."
      ),
      delta_te_r2_supp, perf_add$test$r2, perf_supp$test$r2,
      aic_supp - aic_add, bic_supp - bic_add,
      delta_tr_r2_supp - delta_te_r2_supp
    )
  )
} else {
  list(
    decision = "KEEP_ADDITIVE_BASELINE",
    justification = sprintf(
      paste0(
        "OLS_INTERACTION_SUPPORTED achieves delta test R² = %+.4f (additive: %.4f → supported: %.4f). ",
        "Out-of-sample improvement is negligible. ",
        "The %d interaction terms add substantial complexity without meaningful generalization gain. ",
        "Retain OLS_ADDITIVE_FINAL as the OLS specification."
      ),
      delta_te_r2_supp, perf_add$test$r2, perf_supp$test$r2,
      nrow(c_supp_int)
    )
  )
}

supported_out <- list(
  description = paste0(
    "Support-screened interaction evaluation. OLS_INTERACTION_SUPPORTED includes all ",
    "OLS_ADDITIVE_FINAL main effects, all numeric × numeric interactions (C(7,2)=21), ",
    "and all numeric × categorical interactions (numeric × PROD_ABBR / STATE_ABBR / VENDOR). ",
    "Categorical × categorical interactions (PROD_ABBR × STATE_ABBR, PROD_ABBR × VENDOR, ",
    "STATE_ABBR × VENDOR) are excluded — all were QUESTIONABLE in the feasibility review. ",
    "Purpose: test whether supported interactions improve out-of-sample performance without ",
    "the rank deficiency that caused OLS_INTERACTION_FULL to fail (test R² = -42.16)."
  ),
  row_count_note = row_count_note,
  part_a_model_specification = list(
    model    = "OLS_INTERACTION_SUPPORTED",
    baseline = "OLS_ADDITIVE_FINAL",
    formula  = paste0(
      "log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + ",
      "MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR + ",
      "log_prev_wp:log_prev_poly + log_prev_wp:ACTIVE_PRODUCERS + ",
      "log_prev_wp:AGENCY_APPOINTMENT_YEAR + log_prev_wp:MAX_AGE + ",
      "log_prev_wp:MIN_AGE + log_prev_wp:STAT_PROFILE_DATE_YEAR + ",
      "log_prev_poly:ACTIVE_PRODUCERS + log_prev_poly:AGENCY_APPOINTMENT_YEAR + ",
      "log_prev_poly:MAX_AGE + log_prev_poly:MIN_AGE + log_prev_poly:STAT_PROFILE_DATE_YEAR + ",
      "ACTIVE_PRODUCERS:AGENCY_APPOINTMENT_YEAR + ACTIVE_PRODUCERS:MAX_AGE + ",
      "ACTIVE_PRODUCERS:MIN_AGE + ACTIVE_PRODUCERS:STAT_PROFILE_DATE_YEAR + ",
      "AGENCY_APPOINTMENT_YEAR:MAX_AGE + AGENCY_APPOINTMENT_YEAR:MIN_AGE + ",
      "AGENCY_APPOINTMENT_YEAR:STAT_PROFILE_DATE_YEAR + MAX_AGE:MIN_AGE + ",
      "MAX_AGE:STAT_PROFILE_DATE_YEAR + MIN_AGE:STAT_PROFILE_DATE_YEAR + ",
      "log_prev_wp:PROD_ABBR + log_prev_poly:PROD_ABBR + ACTIVE_PRODUCERS:PROD_ABBR + ",
      "AGENCY_APPOINTMENT_YEAR:PROD_ABBR + MAX_AGE:PROD_ABBR + MIN_AGE:PROD_ABBR + ",
      "STAT_PROFILE_DATE_YEAR:PROD_ABBR + ",
      "log_prev_wp:STATE_ABBR + log_prev_poly:STATE_ABBR + ACTIVE_PRODUCERS:STATE_ABBR + ",
      "AGENCY_APPOINTMENT_YEAR:STATE_ABBR + MAX_AGE:STATE_ABBR + MIN_AGE:STATE_ABBR + ",
      "STAT_PROFILE_DATE_YEAR:STATE_ABBR + ",
      "log_prev_wp:VENDOR + log_prev_poly:VENDOR + ACTIVE_PRODUCERS:VENDOR + ",
      "AGENCY_APPOINTMENT_YEAR:VENDOR + MAX_AGE:VENDOR + MIN_AGE:VENDOR + ",
      "STAT_PROFILE_DATE_YEAR:VENDOR"
    ),
    excluded_vs_full = list(
      "PROD_ABBR × STATE_ABBR (QUESTIONABLE — 11 sparse cells, 1 test-only cell)",
      "PROD_ABBR × VENDOR (QUESTIONABLE — 93 cells <50 obs, 7 test-only cells)",
      "STATE_ABBR × VENDOR (QUESTIONABLE — 3 test-only cells)"
    ),
    n_candidate_train = n_cand_train,
    n_candidate_test  = n_cand_test,
    n_eval_train      = perf_supp$n_train,
    n_eval_test       = perf_supp$n_test
  ),
  part_b_comparison = list(
    models = list(
      ols_additive_final = list(
        train = perf_add$train, test = perf_add$test,
        n_coefficients = nrow(c_add), n_aliased = 0L
      ),
      ols_interaction_full = list(
        train = perf_int$train, test = perf_int$test,
        n_coefficients = nrow(c_int), n_aliased = n_aliased_int,
        note = "Rank-deficient — test R² invalid"
      ),
      ols_interaction_supported = list(
        train = perf_supp$train, test = perf_supp$test,
        n_coefficients = nrow(c_supp), n_aliased = n_aliased_supp
      )
    ),
    delta_vs_additive = list(
      delta_train_r2   = delta_tr_r2_supp,
      delta_test_r2    = delta_te_r2_supp,
      delta_train_rmse = delta_tr_rmse_supp,
      delta_test_rmse  = delta_te_rmse_supp,
      delta_train_mae  = delta_tr_mae_supp,
      delta_test_mae   = delta_te_mae_supp
    ),
    vs_interaction_full = list(
      delta_test_r2  = round(perf_supp$test$r2   - perf_int$test$r2,   4),
      delta_test_rmse = round(perf_supp$test$rmse - perf_int$test$rmse, 4),
      note = "Positive delta_test_r2 means OLS_INTERACTION_SUPPORTED is better than OLS_INTERACTION_FULL"
    )
  ),
  part_d_complexity = list(
    n_formula_terms         = n_total_supp,
    n_estimated_coefficients = nrow(c_supp),
    n_aliased               = n_aliased_supp,
    aliased_terms           = if (n_aliased_supp == 0) "none" else aliased_supp,
    n_main_effect_coefs     = nrow(c_supp_main),
    n_interaction_coefs     = nrow(c_supp_int),
    n_numeric_x_numeric     = n_numnum,
    n_numeric_x_prod_abbr   = n_numprod,
    n_numeric_x_state_abbr  = n_numst,
    n_numeric_x_vendor      = n_numvend,
    n_sig_interactions_p05  = n_sig_int_supp,
    aic = aic_supp,
    bic = bic_supp,
    aic_delta_vs_additive = round(aic_supp - aic_add, 2),
    bic_delta_vs_additive = round(bic_supp - bic_add, 2),
    aic_note = if (n_aliased_supp > 0) {
      "AIC/BIC unreliable — rank-deficient model"
    } else {
      "AIC/BIC valid — full-rank model"
    }
  ),
  part_e_interaction_effects = list(
    top_20_positive_significant = top20_pos_supp,
    top_20_negative_significant = top20_neg_supp,
    by_family = list(
      numeric_x_numeric = fam_summary(
        "numeric x numeric",
        "Synergy or attenuation between two continuous agency-level metrics (e.g., volume carry-forward and policy count interact to improve prediction)"
      ),
      numeric_x_prod_abbr = fam_summary(
        "numeric x product",
        "Product-specific slopes: the relationship between continuous predictors and premium differs by product line, reflecting distinct renewal dynamics, policy sizes, and market behaviors"
      ),
      numeric_x_state_abbr = fam_summary(
        "numeric x state",
        "State-specific slopes: continuous predictor effects vary across states due to market maturity, regulatory environment, and competitive conditions"
      ),
      numeric_x_vendor = fam_summary(
        "numeric x vendor",
        "Vendor-specific slopes: distribution strategy and product mix differences across vendors modify how continuous predictors relate to written premium"
      )
    )
  ),
  part_f_generalization = list(
    delta_train_r2            = delta_tr_r2_supp,
    delta_test_r2             = delta_te_r2_supp,
    train_test_r2_gap_delta   = round(delta_tr_r2_supp - delta_te_r2_supp, 4),
    test_rmse_improves        = (delta_te_rmse_supp < 0),
    test_mae_improves         = (delta_te_mae_supp  < 0),
    rank_deficient            = supp_rank_deficient,
    n_aliased_terms           = n_aliased_supp,
    overfit_flag              = overfit_supp,
    overfit_criterion         = "Flagged if (delta_train_r2 - delta_test_r2) > 0.02",
    prediction_warning        = if (supp_rank_deficient) {
      "Potential rank-deficiency warning during prediction"
    } else {
      "No rank-deficiency warning — full-rank fit"
    },
    interpretation = if (supp_rank_deficient) {
      "Model is rank-deficient. Generalization results unreliable."
    } else if (delta_te_r2_supp < 0) {
      "Test R² decreases — supported interactions do not generalize. Keep additive baseline."
    } else if (overfit_supp) {
      sprintf(
        "Train improvement (%.4f) exceeds test improvement (%.4f) by more than 0.02. Overfitting detected.",
        delta_tr_r2_supp, delta_te_r2_supp
      )
    } else {
      sprintf(
        "Train R² improves by %.4f and test R² improves by %.4f. No severe overfitting. Supported interactions generalize.",
        delta_tr_r2_supp, delta_te_r2_supp
      )
    }
  ),
  part_g_recommendation = supp_recommendation
)

# =============================================================================
# Interaction Feasibility Review (Parts A–F)
# =============================================================================
cat("\nRunning interaction feasibility analysis...\n")

NUMERIC_PREDS_FAM <- c("log_prev_wp", "log_prev_poly", "ACTIVE_PRODUCERS",
                       "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                       "STAT_PROFILE_DATE_YEAR")

# ---- Part B helper: cell support for categorical × categorical ----
cell_support_fn <- function(var1, var2) {
  v1 <- sym(var1); v2 <- sym(var2)
  tr <- wp_train |>
    filter(!is.na(!!v1), !is.na(!!v2)) |>
    count(!!v1, !!v2) |>
    rename(n_train = n) |>
    mutate(key = paste(.data[[var1]], .data[[var2]], sep = "|||"))
  te <- wp_test |>
    filter(!is.na(!!v1), !is.na(!!v2)) |>
    count(!!v1, !!v2) |>
    rename(n_test = n) |>
    mutate(key = paste(.data[[var1]], .data[[var2]], sep = "|||"))

  te_only_keys  <- setdiff(te$key, tr$key)
  tr_only_keys  <- setdiff(tr$key, te$key)
  n_both        <- length(intersect(tr$key, te$key))
  te_rows_te_only  <- sum(te$n_test[te$key %in% te_only_keys])
  total_te_rows    <- sum(te$n_test)
  cn  <- tr$n_train
  worst_df <- tr[order(tr$n_train), c(var1, var2, "n_train")]

  list(
    n_possible_cells               = nlevels(wp_train[[var1]]) * nlevels(wp_train[[var2]]),
    n_train_cells                  = nrow(tr),
    n_test_cells                   = nrow(te),
    n_both_cells                   = n_both,
    n_train_only_cells             = length(tr_only_keys),
    n_test_only_cells              = length(te_only_keys),
    pct_train_cells_covered_in_test = round(100 * n_both / max(nrow(tr), 1), 1),
    n_test_rows_in_test_only_cells  = te_rows_te_only,
    pct_test_rows_affected          = round(100 * te_rows_te_only / max(total_te_rows, 1), 1),
    min_train_count                = min(cn),
    p5_train_count                 = as.integer(quantile(cn, 0.05)),
    median_train_count             = as.integer(median(cn)),
    mean_train_count               = round(mean(cn), 1),
    n_cells_lt_10                  = sum(cn < 10),
    n_cells_lt_25                  = sum(cn < 25),
    n_cells_lt_50                  = sum(cn < 50),
    n_cells_lt_100                 = sum(cn < 100),
    worst_10_cells                 = df_to_list(head(worst_df, 10))
  )
}

# ---- Part C helper: level support for numeric × categorical ----
level_support_fn <- function(cat_var) {
  v <- sym(cat_var)
  tr <- wp_train |>
    filter(!is.na(!!v)) |>
    count(!!v) |>
    rename(n_train = n) |>
    arrange(n_train)
  te <- wp_test |>
    filter(!is.na(!!v)) |>
    count(!!v) |>
    rename(n_test = n)

  tr_lev  <- as.character(tr[[cat_var]])
  te_lev  <- as.character(te[[cat_var]])
  te_only <- setdiff(te_lev, tr_lev)
  tr_only <- setdiff(tr_lev, te_lev)
  cn <- tr$n_train

  list(
    n_factor_levels    = nlevels(wp_train[[cat_var]]),
    n_train_levels     = length(tr_lev),
    n_test_levels      = length(te_lev),
    n_both_levels      = length(intersect(tr_lev, te_lev)),
    train_only_levels  = if (length(tr_only) == 0) "none" else tr_only,
    test_only_levels   = if (length(te_only) == 0) "none" else te_only,
    min_train_count    = min(cn),
    p5_train_count     = as.integer(quantile(cn, 0.05)),
    median_train_count = as.integer(median(cn)),
    mean_train_count   = round(mean(cn), 1),
    n_levels_lt_10     = sum(cn < 10),
    n_levels_lt_25     = sum(cn < 25),
    n_levels_lt_50     = sum(cn < 50),
    n_levels_lt_100    = sum(cn < 100),
    worst_5_levels     = df_to_list(head(tr, 5))
  )
}

# Compute support stats
cat("  Part B: PROD_ABBR × STATE_ABBR...\n")
cc_ps <- cell_support_fn("PROD_ABBR", "STATE_ABBR")
cat("  Part B: PROD_ABBR × VENDOR...\n")
cc_pv <- cell_support_fn("PROD_ABBR", "VENDOR")
cat("  Part B: STATE_ABBR × VENDOR...\n")
cc_sv <- cell_support_fn("STATE_ABBR", "VENDOR")
cat("  Part C: numeric × PROD_ABBR / STATE_ABBR / VENDOR...\n")
nc_prod   <- level_support_fn("PROD_ABBR")
nc_state  <- level_support_fn("STATE_ABBR")
nc_vendor <- level_support_fn("VENDOR")

# ---- Part D: classification ----
# Numeric × categorical: only risk is test-only levels (→ NA predictions)
# No rank-deficiency risk from sparse levels — slope is imprecise but always estimable
classify_nc <- function(supp, cat_label, n_numeric) {
  te_only_raw <- supp$test_only_levels
  n_te_only   <- if (identical(te_only_raw, "none")) 0L else length(te_only_raw)
  n_lt10      <- supp$n_levels_lt_10
  n_lt50      <- supp$n_levels_lt_50
  n_tr        <- supp$n_train_levels
  n_new_each  <- n_tr - 1L
  n_new_total <- n_new_each * n_numeric

  cls <- if (n_te_only == 0) "SAFE" else "QUESTIONABLE"

  just <- if (cls == "SAFE") {
    sprintf(
      paste0(
        "No test-only levels: all %d %s levels present in training also appear in test. ",
        "Numeric × categorical interactions are always identifiable as long as the level exists ",
        "in training — no rank-deficiency risk from sparse levels. ",
        "%d level(s) have fewer than 10 training rows (%d fewer than 50); ",
        "these produce imprecise but not undefined slopes. ",
        "Adding all %d numeric × %s interactions introduces %d new terms (%d per predictor)."
      ),
      n_tr, cat_label, n_lt10, n_lt50,
      n_numeric, cat_label, n_new_total, n_new_each
    )
  } else {
    sprintf(
      paste0(
        "%d %s level(s) appear in test but not in training. ",
        "predict.lm() returns NA for test rows hitting those unseen levels, ",
        "reducing effective test sample. ",
        "%d level(s) have fewer than 10 training rows. ",
        "Adding all %d numeric × %s interactions introduces %d new terms."
      ),
      n_te_only, cat_label, n_lt10,
      n_numeric, cat_label, n_new_total
    )
  }

  list(
    classification                    = cls,
    justification                     = just,
    n_train_levels                    = n_tr,
    n_test_only_levels                = n_te_only,
    n_train_levels_lt_10              = n_lt10,
    n_train_levels_lt_50              = n_lt50,
    n_new_terms_per_numeric_predictor = n_new_each,
    n_total_new_terms                 = n_new_total
  )
}

# Categorical × categorical: sparse training cells drive rank deficiency;
# test-only cells are a secondary concern (absent → NA predictions, not doubtful predictions)
classify_cc <- function(supp, var1, var2) {
  n_te_only  <- supp$n_test_only_cells
  n_tr_cells <- supp$n_train_cells
  n_lt10     <- supp$n_cells_lt_10
  n_lt50     <- supp$n_cells_lt_50
  pct_lt10   <- round(100 * n_lt10 / max(n_tr_cells, 1), 1)
  pct_aff    <- supp$pct_test_rows_affected

  cls <- if (n_te_only > 10 || pct_aff > 5.0 || pct_lt10 > 20) {
    "UNSUPPORTED"
  } else if (n_te_only > 0 || pct_lt10 > 5) {
    "QUESTIONABLE"
  } else {
    "SAFE"
  }

  just <- if (cls == "UNSUPPORTED") {
    sprintf(
      paste0(
        "%d/%d train %s × %s cells (%s%%) have fewer than 10 observations. ",
        "Sparse training cells create exact near-collinearities in the design matrix when combined ",
        "with other interaction families, producing aliased (non-estimable) terms. ",
        "predict.lm() assigns 'doubtful' predictions to test rows in the rank-deficient subspace, ",
        "not just the %d test-only cells (%.1f%% of test rows). ",
        "Rank-deficiency risk is high. Exclude from combined interaction models."
      ),
      n_lt10, n_tr_cells, var1, var2, pct_lt10,
      n_te_only, pct_aff
    )
  } else if (cls == "QUESTIONABLE") {
    sprintf(
      paste0(
        "%d test-only %s × %s cell(s) will produce NA or doubtful predictions, ",
        "affecting %.1f%% of test rows. ",
        "%d/%d train cells (%s%%) have fewer than 10 observations. ",
        "Marginally supported — test-only rows will be excluded from evaluation metrics."
      ),
      n_te_only, var1, var2, pct_aff,
      n_lt10, n_tr_cells, pct_lt10
    )
  } else {
    sprintf(
      paste0(
        "No test-only %s × %s cells — all test combinations seen in training. ",
        "%d/%d train cells (%s%%) have fewer than 10 observations. Rank-deficiency risk is low."
      ),
      var1, var2, n_lt10, n_tr_cells, pct_lt10
    )
  }

  list(
    classification           = cls,
    justification            = just,
    n_train_cells            = n_tr_cells,
    n_test_only_cells        = n_te_only,
    pct_test_rows_affected   = pct_aff,
    n_train_cells_lt_10      = n_lt10,
    n_train_cells_lt_50      = n_lt50,
    pct_train_cells_lt_10    = pct_lt10
  )
}

d_nc_prod   <- classify_nc(nc_prod,   "PROD_ABBR",  7L)
d_nc_state  <- classify_nc(nc_state,  "STATE_ABBR", 7L)
d_nc_vendor <- classify_nc(nc_vendor, "VENDOR",     7L)
d_cc_ps     <- classify_cc(cc_ps, "PROD_ABBR", "STATE_ABBR")
d_cc_pv     <- classify_cc(cc_pv, "PROD_ABBR", "VENDOR")
d_cc_sv     <- classify_cc(cc_sv, "STATE_ABBR", "VENDOR")

cat(sprintf("  numeric × PROD_ABBR:    %s\n", d_nc_prod$classification))
cat(sprintf("  numeric × STATE_ABBR:   %s\n", d_nc_state$classification))
cat(sprintf("  numeric × VENDOR:       %s\n", d_nc_vendor$classification))
cat(sprintf("  PROD_ABBR × STATE_ABBR: %s\n", d_cc_ps$classification))
cat(sprintf("  PROD_ABBR × VENDOR:     %s\n", d_cc_pv$classification))
cat(sprintf("  STATE_ABBR × VENDOR:    %s\n", d_cc_sv$classification))

# ---- Part F: recommendation ----
n_new_terms_np <- d_nc_prod$n_total_new_terms
n_total_np     <- nrow(c_add) + n_new_terms_np

# Why OLS_INTERACTION_FULL failed
failure_explanation <- sprintf(
  paste0(
    "OLS_INTERACTION_FULL failed primarily from within-training rank deficiency, not from test-only cells. ",
    "Test-only cells were trivially small: %d PROD_ABBR × STATE_ABBR (%.1f%% of test rows) and ",
    "%d PROD_ABBR × VENDOR (%.1f%% of test rows). ",
    "The actual cause was %d aliased (collinear) terms detected in the training fit itself. ",
    "These aliased terms arose because sparse cells in the combined design matrix created ",
    "exact collinearities: %d PROD_ABBR × VENDOR cells had fewer than 10 training observations (%s%% of train cells), ",
    "and %d PROD_ABBR × STATE_ABBR cells had fewer than 10 obs (%s%% of train cells). ",
    "When all 758 terms were included simultaneously, near-singleton specialty product × vendor combinations ",
    "(e.g., SNOWMOBI12 × C, DTALK12 × J with n_train = 1) became exactly collinear with other terms. ",
    "R marked those %d terms as non-estimable. predict.lm() then assigned 'doubtful' predictions to ALL test rows ",
    "whose covariate combinations mapped into the rank-deficient subspace — not just the 40 test-only rows, ",
    "but every row touching a near-singular interaction dimension. The result was test RMSE = 14.52 vs 1.09."
  ),
  d_cc_ps$n_test_only_cells, d_cc_ps$pct_test_rows_affected,
  d_cc_pv$n_test_only_cells, d_cc_pv$pct_test_rows_affected,
  n_aliased_int,
  cc_pv$n_cells_lt_10, round(100 * cc_pv$n_cells_lt_10 / cc_pv$n_train_cells, 1),
  cc_ps$n_cells_lt_10, round(100 * cc_ps$n_cells_lt_10 / cc_ps$n_train_cells, 1),
  n_aliased_int
)

rec_out <- list(
  recommended_model_name = "OLS_INTERACTION_NUMERIC_PRODUCT",
  recommended_formula = paste0(
    "log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + ",
    "MAX_AGE + MIN_AGE + STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR + ",
    "log_prev_wp:PROD_ABBR + log_prev_poly:PROD_ABBR + ACTIVE_PRODUCERS:PROD_ABBR + ",
    "AGENCY_APPOINTMENT_YEAR:PROD_ABBR + MAX_AGE:PROD_ABBR + MIN_AGE:PROD_ABBR + ",
    "STAT_PROFILE_DATE_YEAR:PROD_ABBR"
  ),
  n_additive_terms          = nrow(c_add),
  n_new_interaction_terms   = n_new_terms_np,
  n_total_terms_expected    = n_total_np,
  included_families = list(
    "log_prev_wp × PROD_ABBR",
    "log_prev_poly × PROD_ABBR",
    "ACTIVE_PRODUCERS × PROD_ABBR",
    "AGENCY_APPOINTMENT_YEAR × PROD_ABBR",
    "MAX_AGE × PROD_ABBR",
    "MIN_AGE × PROD_ABBR",
    "STAT_PROFILE_DATE_YEAR × PROD_ABBR"
  ),
  included_rationale = paste0(
    "All 7 numeric × PROD_ABBR interactions are SAFE (no test-only levels). ",
    "This family is directly supported by OLS_INTERACTION_FULL evidence: ",
    "log_prev_wp:PROD_ABBR had |t| > 40 for GARAGE and WORKCOMP, ",
    "and log_prev_poly:PROD_ABBR, ACTIVE_PRODUCERS:PROD_ABBR also showed strong signals. ",
    "All PROD_ABBR levels present in test are also in training, so no doubtful predictions. ",
    "Numeric interactions never create rank deficiency from absent cells. ",
    "Adding ~", n_new_terms_np, " terms to a 103K-row dataset is well within ",
    "available degrees of freedom."
  ),
  excluded_families = list(
    list(
      family = "PROD_ABBR × STATE_ABBR",
      classification = d_cc_ps$classification,
      reason = sprintf(
        paste0(
          "DEFERRED (%s). Test coverage is good (%d test-only cell, %.1f%% of test rows). ",
          "However %d/%d train cells (%s%%) have fewer than 10 observations. ",
          "In OLS_INTERACTION_FULL, sparse product × state cells contributed to the 82 aliased terms ",
          "via joint near-collinearity when fit alongside PROD_ABBR × VENDOR and all numeric interactions. ",
          "Evaluate this family in isolation, after numeric × PROD_ABBR establishes a cleaner baseline."
        ),
        d_cc_ps$classification,
        d_cc_ps$n_test_only_cells, d_cc_ps$pct_test_rows_affected,
        cc_ps$n_cells_lt_10, cc_ps$n_train_cells,
        round(100 * cc_ps$n_cells_lt_10 / cc_ps$n_train_cells, 1)
      )
    ),
    list(
      family = "PROD_ABBR × VENDOR",
      classification = d_cc_pv$classification,
      reason = sprintf(
        paste0(
          "DEFERRED (%s). %d test-only cells (%.1f%% of test rows). ",
          "More critically: %d/%d train cells (%s%%) have fewer than 10 obs and ",
          "%d/%d (%s%%) have fewer than 50 — extensive sparsity in specialty product × vendor cells. ",
          "In OLS_INTERACTION_FULL this family contributed 211 terms with only 65 significant. ",
          "The sparse cells were the primary within-training near-collinearity driver among the 82 aliased terms. ",
          "Defer until numeric × PROD_ABBR results are evaluated."
        ),
        d_cc_pv$classification,
        d_cc_pv$n_test_only_cells, d_cc_pv$pct_test_rows_affected,
        cc_pv$n_cells_lt_10, cc_pv$n_train_cells,
        round(100 * cc_pv$n_cells_lt_10 / cc_pv$n_train_cells, 1),
        cc_pv$n_cells_lt_50, cc_pv$n_train_cells,
        round(100 * cc_pv$n_cells_lt_50 / cc_pv$n_train_cells, 1)
      )
    ),
    list(
      family = "STATE_ABBR × VENDOR",
      classification = d_cc_sv$classification,
      reason = paste0(
        "DEFERRED (", d_cc_sv$classification, "). ",
        "May be testable but deferring until numeric × PROD_ABBR model is evaluated. ",
        "Low term count but weaker business rationale than numeric × PROD_ABBR."
      )
    ),
    list(
      family = "numeric × STATE_ABBR",
      classification = d_nc_state$classification,
      reason = paste0(
        "DEFERRED (", d_nc_state$classification, "). ",
        "Well-supported but lower business priority. ",
        "Evaluate as OLS_INTERACTION_NUMERIC_ALL after numeric × PROD_ABBR is confirmed."
      )
    ),
    list(
      family = "numeric × VENDOR",
      classification = d_nc_vendor$classification,
      reason = paste0(
        "DEFERRED (", d_nc_vendor$classification, "). ",
        "Well-supported but lower business priority. ",
        "Evaluate as OLS_INTERACTION_NUMERIC_ALL after numeric × PROD_ABBR is confirmed."
      )
    )
  ),
  why_this_avoids_full_failure = paste0(
    "OLS_INTERACTION_NUMERIC_PRODUCT avoids OLS_INTERACTION_FULL's failure by: ",
    "(1) Excluding all categorical × categorical interactions — the primary rank-deficiency drivers. ",
    "(2) Targeting only numeric × PROD_ABBR — numeric interactions never create doubtful predictions ",
    "from absent cells. Each PROD_ABBR level just gets its own slope for each numeric predictor. ",
    "(3) Adding ~", n_new_terms_np, " terms vs 628 — a ~", round(628 / n_new_terms_np, 1),
    "x reduction in interaction complexity. ",
    "(4) Directly testing the strongest confirmed signal: log_prev_wp:PROD_ABBR (|t| > 40 in ",
    "OLS_INTERACTION_FULL for GARAGE and WORKCOMP, |t| > 20 for multiple other lines)."
  ),
  ols_interaction_full_failure_explanation = failure_explanation
)

# ---- Part E: business rationale ----
biz_rationale_out <- list(
  numeric_x_prod_abbr = list(
    sub_families = NUMERIC_PREDS_FAM,
    summary = paste0(
      "Prior metrics carry forward at different rates by product. ",
      "Renewal dynamics, policy size, and premium volatility differ substantially by line — ",
      "personal auto and homeowners show strong volume persistence; ",
      "commercial specialty products (BOP, GL, Workers' Comp) may have more volatile trajectories. ",
      "Confirmed signal: OLS_INTERACTION_FULL showed log_prev_wp:PROD_ABBR |t| > 40 for ",
      "GARAGE and WORKCOMP, |t| > 20 for multiple other lines. Strongest interaction family in data."
    ),
    per_predictor = list(
      log_prev_wp = list(
        family = "log_prev_wp × PROD_ABBR",
        rationale = "Volume carry-forward coefficient differs by product. Premium persistence is higher for standard personal lines (auto, home) than specialty commercial lines where policy counts are lower and premium per policy is more volatile.",
        signal_strength = "very strong — confirmed by OLS_INTERACTION_FULL (|t| > 40 for GARAGE, WORKCOMP; multiple |t| > 20)"
      ),
      log_prev_poly = list(
        family = "log_prev_poly × PROD_ABBR",
        rationale = "Policy count carry-forward differs by product. High-count products (auto, home) show strong policy persistence; low-count specialties (large commercial) show weak policy-count signals relative to premium.",
        signal_strength = "moderate-strong — product-specific policy-to-premium ratios imply distinct slopes"
      ),
      ACTIVE_PRODUCERS = list(
        family = "ACTIVE_PRODUCERS × PROD_ABBR",
        rationale = "Marginal producer value differs by product. Specialty commercial lines concentrate in a few expert producers; standard personal lines distribute volume more uniformly across all producers.",
        signal_strength = "moderate — producer specialization by product line is well-established"
      ),
      AGENCY_APPOINTMENT_YEAR = list(
        family = "AGENCY_APPOINTMENT_YEAR × PROD_ABBR",
        rationale = "Agency tenure ramp-up differs by product. New agencies build personal lines books quickly; commercial specialty lines require longer relationship development and expertise acquisition.",
        signal_strength = "moderate — product-specific onboarding curves are plausible"
      ),
      MAX_AGE = list(
        family = "MAX_AGE × PROD_ABBR",
        rationale = "Senior producer expertise effect may be stronger for complex products (commercial specialty) than transactional personal lines where experience matters less relative to lead volume.",
        signal_strength = "weak to moderate — MAX_AGE additive coefficient is small (0.003)"
      ),
      MIN_AGE = list(
        family = "MIN_AGE × PROD_ABBR",
        rationale = "Same rationale as MAX_AGE × PROD_ABBR. New producers may be channeled into standard personal lines first, producing a product-specific age effect.",
        signal_strength = "weak — MIN_AGE additive effect is small"
      ),
      STAT_PROFILE_DATE_YEAR = list(
        family = "STAT_PROFILE_DATE_YEAR × PROD_ABBR",
        rationale = "Year trends differ by product over 2006-2012. Commercial specialty lines (E&O, D&O, Workers Comp) had different market cycles than personal auto and home. A product-specific year slope captures diverging growth trajectories.",
        signal_strength = "moderate — product-specific market cycles over 2006-2012 are plausible"
      )
    )
  ),
  numeric_x_state_abbr = list(
    sub_families = NUMERIC_PREDS_FAM,
    summary = paste0(
      "State market characteristics may modify how numeric predictors relate to premium. ",
      "Well-supported (all state levels in train and test) but lower priority than numeric × PROD_ABBR ",
      "as product differences are larger and more directly interpretable than state differences. ",
      "State effects are partially absorbed by the STATE_ABBR main effects already in OLS_ADDITIVE_FINAL."
    ),
    highlight = list(
      log_prev_wp_x_state = "Volume carry-forward may differ by state market maturity and competitive intensity.",
      active_producers_x_state = "Marginal producer productivity may vary by state market saturation.",
      year_x_state = "State-specific growth trends are real (regulatory changes, economic shocks) but risk overfitting on 7 training years."
    )
  ),
  numeric_x_vendor = list(
    sub_families = NUMERIC_PREDS_FAM,
    summary = paste0(
      "Vendor strategy and distribution model may modify numeric predictor effects. ",
      "Some vendors concentrate on high-volume personal lines; others on commercial specialty. ",
      "Well-supported (few vendor levels, each with many observations) but weaker business rationale ",
      "than numeric × PROD_ABBR since vendor differences in slope are less directly interpretable."
    ),
    highlight = list(
      log_prev_wp_x_vendor = "Vendors with different product mix show different volume persistence characteristics.",
      active_producers_x_vendor = "Vendor distribution model (few large producers vs many small) may affect the producers-premium slope."
    )
  ),
  prod_abbr_x_state_abbr = list(
    family = "PROD_ABBR × STATE_ABBR",
    rationale = paste0(
      "Premium levels for a given product differ by state: regulation, exposure, competitive market, ",
      "and geographic risk factors all produce state-level variation by product. ",
      "This is the most theoretically motivated categorical interaction. ",
      "Test coverage is actually good: 97.8% of train cells appear in test, only 1 test-only cell. ",
      "The concern is not absent cells per se, but sparse cells: 11/136 train cells (8.1%) have < 10 obs, ",
      "primarily specialty products in smaller states (e.g., PERSAIP/PA n=1, DTALK12/WV n=2). ",
      "In OLS_INTERACTION_FULL these contributed to 82 aliased terms via joint near-collinearity ",
      "with the 249 PROD_ABBR × VENDOR terms. This family is worth testing in isolation ",
      "after numeric × PROD_ABBR is established."
    ),
    signal_strength = "theoretically strong — 97.8% test coverage, sparse cells manageable in isolation, defer after numeric × PROD_ABBR"
  ),
  prod_abbr_x_vendor = list(
    family = "PROD_ABBR × VENDOR",
    rationale = paste0(
      "Vendors may specialize in certain products: personal-lines-focused vs commercial-specialty vendors. ",
      "Test coverage is reasonable: 98.4% of train cells covered in test, 7 test-only cells. ",
      "However, this family has extensive sparsity in training: 27/249 cells (10.8%) with < 10 obs, ",
      "93/249 (37.3%) with < 50 obs — specialty product × vendor combinations are very thin. ",
      "In OLS_INTERACTION_FULL: 211 product × vendor terms with only 65 significant, ",
      "indicating ~146 terms were noise. These sparse terms were the primary source of ",
      "the 82 aliased terms (within-training near-collinearity) that caused catastrophic test failure. ",
      "This family's sparsity, not absent cells, was the main failure driver."
    ),
    signal_strength = "weak — extensive train sparsity (37% cells < 50 obs); primary near-collinearity driver in OLS_INTERACTION_FULL"
  ),
  state_abbr_x_vendor = list(
    family = "STATE_ABBR × VENDOR",
    rationale = paste0(
      "Vendor relationships may differ by state due to state-level distribution agreements, ",
      "regulatory approval history, or geographic market concentration. ",
      "Better supported than PROD_ABBR × anything because vendors likely operate in most states, ",
      "reducing absent-cell risk. However, the STATE_ABBR and VENDOR main effects already capture ",
      "most of this variation, and the incremental value of the interaction is uncertain."
    ),
    signal_strength = "moderate — state × vendor distribution variation plausible but partially absorbed by main effects"
  )
)

# ---- Assemble feasibility output ----
feasibility_out <- list(
  description = paste0(
    "Interaction feasibility review. Purpose: determine which interaction families are sufficiently ",
    "supported by the data before fitting another interaction model. ",
    "OLS_INTERACTION_FULL failed (test R² = -42.16) because unrestricted pairwise categorical ",
    "interaction expansion created a rank-deficient, unstable training design matrix. ",
    "The root issue was sparse categorical interaction structure — especially PROD_ABBR × VENDOR ",
    "and PROD_ABBR × STATE_ABBR cells with very low training support. ",
    "Test-only cells existed but were too few (8 cells, ~40 rows) to explain the collapse by themselves. ",
    "This review classifies each interaction family as SAFE, QUESTIONABLE, or UNSUPPORTED ",
    "based on data support and business rationale."
  ),
  ols_interaction_full_context = list(
    additive_test_r2       = perf_add$test$r2,
    interaction_full_test_r2 = perf_int$test$r2,
    additive_test_rmse     = perf_add$test$rmse,
    interaction_full_test_rmse = perf_int$test$rmse,
    n_interaction_terms    = n_interaction_terms,
    n_aliased_terms        = n_aliased_int,
    rank_deficient         = TRUE,
    failure_summary        = paste0(
      "Test R² collapsed from 0.7552 to -42.16. Primary cause: rank-deficient training fit with ",
      n_aliased_int, " aliased terms, producing doubtful predictions via predict.lm(). ",
      "Root mechanism: sparse PROD_ABBR × VENDOR cells (27/249 with < 10 training obs, ",
      "93/249 with < 50) and sparse PROD_ABBR × STATE_ABBR cells (11/136 with < 10 obs) ",
      "created exact near-collinearities in the 758-term design matrix. ",
      "Test-only interaction cells existed (8 cells total, ~40 test rows) ",
      "but were too small to explain the catastrophic failure on their own."
    )
  ),
  part_a_interaction_families = list(
    numeric_x_product = list(
      description = "Product-specific slopes for all 7 numeric predictors",
      families    = lapply(NUMERIC_PREDS_FAM, function(p) paste0(p, " × PROD_ABBR")),
      n_families  = 7L,
      mechanism   = "Separate slope per PROD_ABBR level for each numeric predictor"
    ),
    numeric_x_state = list(
      description = "State-specific slopes for all 7 numeric predictors",
      families    = lapply(NUMERIC_PREDS_FAM, function(p) paste0(p, " × STATE_ABBR")),
      n_families  = 7L,
      mechanism   = "Separate slope per STATE_ABBR level for each numeric predictor"
    ),
    numeric_x_vendor = list(
      description = "Vendor-specific slopes for all 7 numeric predictors",
      families    = lapply(NUMERIC_PREDS_FAM, function(p) paste0(p, " × VENDOR")),
      n_families  = 7L,
      mechanism   = "Separate slope per VENDOR level for each numeric predictor"
    ),
    categorical_x_categorical = list(
      description = "Pairwise categorical interactions",
      families    = list("PROD_ABBR × STATE_ABBR", "PROD_ABBR × VENDOR", "STATE_ABBR × VENDOR"),
      n_families  = 3L,
      mechanism   = "Separate intercept per combination cell; absent cells create rank deficiency"
    )
  ),
  part_b_cell_support = list(
    prod_abbr_x_state_abbr = cc_ps,
    prod_abbr_x_vendor     = cc_pv,
    state_abbr_x_vendor    = cc_sv
  ),
  part_c_level_support = list(
    numeric_x_prod_abbr  = nc_prod,
    numeric_x_state_abbr = nc_state,
    numeric_x_vendor     = nc_vendor
  ),
  part_d_classification = list(
    summary_table = list(
      list(family = "numeric × PROD_ABBR",  classification = d_nc_prod$classification,
           n_terms = d_nc_prod$n_total_new_terms),
      list(family = "numeric × STATE_ABBR", classification = d_nc_state$classification,
           n_terms = d_nc_state$n_total_new_terms),
      list(family = "numeric × VENDOR",     classification = d_nc_vendor$classification,
           n_terms = d_nc_vendor$n_total_new_terms),
      list(family = "PROD_ABBR × STATE_ABBR", classification = d_cc_ps$classification,
           n_terms = cc_ps$n_train_cells - 1L),
      list(family = "PROD_ABBR × VENDOR",     classification = d_cc_pv$classification,
           n_terms = cc_pv$n_train_cells - 1L),
      list(family = "STATE_ABBR × VENDOR",    classification = d_cc_sv$classification,
           n_terms = cc_sv$n_train_cells - 1L)
    ),
    detail = list(
      numeric_x_prod_abbr    = d_nc_prod,
      numeric_x_state_abbr   = d_nc_state,
      numeric_x_vendor       = d_nc_vendor,
      prod_abbr_x_state_abbr = d_cc_ps,
      prod_abbr_x_vendor     = d_cc_pv,
      state_abbr_x_vendor    = d_cc_sv
    )
  ),
  part_e_business_rationale = biz_rationale_out,
  part_f_recommendation     = rec_out
)

# =============================================================================
# Write outputs
# =============================================================================

cat("Writing outputs...\n")
write_json_out(model_comparison_out,  "ols_model_comparison.json")
write_json_out(ols_coefficients_out,  "ols_coefficients.json")
write_json_out(ols_summary_out,       "ols_summary.json")
write_json_out(ret_comparison_out,    "ols5_retention_comparison.json")
write_json_out(additive_final_out,    "ols_additive_final.json")
write_json_out(interaction_full_out,  "ols_interaction_full.json")
write_json_out(numeric_only_out,      "ols_interaction_numeric_only.json")
write_json_out(supported_out,         "ols_interaction_supported.json")
write_json_out(feasibility_out,       "ols_interaction_feasibility.json")

cat(sprintf("\n=== OLS modeling complete ===\nOutputs in: %s\n", output_dir))
