# =============================================================================
# Dataset A Part 2 — RF Final All-Data Refit and 2015 Predictions
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_wp_rf_final.R
#
# Purpose: Refit RF_1_SAFE_TUNED specification on all 2006-2014 eligible rows.
#          Generate 2015 one-year-ahead predictions and deployment artifacts.
#          Model selection is COMPLETE. This script does NOT run new experiments.
#
# Final selected model: RF_1_SAFE_TUNED
#   Locked holdout: Test R2=0.8839  RMSE=0.7532  MAE=0.4428  (2013-2014)
#
# Outputs: modeling/dataset_a_part2/outputs/
#   rf_final_all_data_summary.json
#   rf_final_all_data_metadata.json
#   rf_final_all_data_importance.json
#   rf_final_all_data_predictions_sample.json
#   rf_final_2015_predictions.csv
#   rf_final_2015_predictions.json
#   rf_final_prediction_input_template.json
#   rf_final_all_data.rds
#
# Do NOT modify: dataset_a_wp_rf.R, dataset_a_wp_ols.R, dataset_a_wp_lmm.R,
#   dataset_a_wp_rf_agency_clusters.R, dataset_a_modeling.R,
#   dataset_a_agency_profile_audit.R, dataset_a_agency_profile_clustering_prep.R,
#   server.js
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(ranger); library(jsonlite)
})

SEED       <- 42L
db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

RF_PREDICTORS             <- c("log_prev_wp", "log_prev_poly",
                                "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR",
                                "MAX_AGE", "MIN_AGE", "STAT_PROFILE_DATE_YEAR",
                                "STATE_ABBR", "PROD_ABBR", "VENDOR")
RF_NUMERIC_PREDICTORS     <- c("log_prev_wp", "log_prev_poly",
                                "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR",
                                "MAX_AGE", "MIN_AGE", "STAT_PROFILE_DATE_YEAR")
RF_CATEGORICAL_PREDICTORS <- c("STATE_ABBR", "PROD_ABBR", "VENDOR")
P <- length(RF_PREDICTORS)  # 10

# Locked RF_1_SAFE_TUNED hyperparameters (from rf_summary.json)
FINAL_NUM_TREES       <- 500L
FINAL_MTRY            <- 3L
FINAL_MIN_NODE_SIZE   <- 5L
FINAL_SAMPLE_FRACTION <- 0.6
FINAL_REPLACE         <- FALSE

LOCKED_HOLDOUT <- list(
  model        = "RF_1_SAFE_TUNED",
  train_years  = "2006-2012",
  test_years   = "2013-2014",
  n_train_rows = 103377L,
  n_test_rows  = 30981L,
  test_r2      = 0.8839,
  test_rmse    = 0.7532,
  test_mae     = 0.4428,
  source       = "modeling/dataset_a_part2/outputs/rf_summary.json",
  note         = paste0(
    "Official holdout metrics. RF_FINAL_ALL_DATA is a refit of this specification. ",
    "OOB metrics from the all-data refit are diagnostics only and do not replace these."
  )
)

cat("=== Dataset A Part 2 — RF Final All-Data Refit ===\n\n")
cat("Selected specification : RF_1_SAFE_TUNED\n")
cat(sprintf("Locked holdout         : Test R2=%.4f  RMSE=%.4f  MAE=%.4f  (2013-2014)\n\n",
            LOCKED_HOLDOUT$test_r2, LOCKED_HOLDOUT$test_rmse, LOCKED_HOLDOUT$test_mae))

# =============================================================================
# Helpers
# =============================================================================

r2_score <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  a <- actual[ok]; p <- predicted[ok]
  ss_res <- sum((a - p)^2); ss_tot <- sum((a - mean(a))^2)
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

write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
  cat(sprintf("  %-55s %.1f KB\n", filename, file.size(path) / 1024))
}

group_of <- function(v) {
  switch(v,
    log_prev_wp             = "prior_performance_lag",
    log_prev_poly           = "prior_performance_lag",
    ACTIVE_PRODUCERS        = "agency_characteristics",
    AGENCY_APPOINTMENT_YEAR = "agency_characteristics",
    MAX_AGE                 = "agency_characteristics",
    MIN_AGE                 = "agency_characteristics",
    STATE_ABBR              = "geography",
    PROD_ABBR               = "product",
    VENDOR                  = "vendor",
    STAT_PROFILE_DATE_YEAR  = "time_trend",
    "other"
  )
}

# =============================================================================
# Load raw data
# =============================================================================

cat("Connecting to database...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  Raw rows: %s  x  %d columns\n\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

# =============================================================================
# Build 2006-2014 all-data training population
# =============================================================================

cat("Building 2006-2014 all-data training population...\n")

wp_alldata <- raw |>
  filter(
    STAT_PROFILE_DATE_YEAR %in% 2006:2014,
    PROD_ABBR != "COMMPOL",
    WRTN_PREM_AMT > 0
  ) |>
  mutate(
    log_wp        = log(WRTN_PREM_AMT + 1),
    log_prev_wp   = suppressWarnings(log(PREV_WRTN_PREM_AMT    + 1)),
    log_prev_poly = suppressWarnings(log(PREV_POLY_INFORCE_QTY + 1))
  ) |>
  mutate(
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  ) |>
  filter(!is.na(log_wp), !is.na(log_prev_wp), !is.na(log_prev_poly))

alldata_state_levels  <- sort(unique(wp_alldata$STATE_ABBR))
alldata_prod_levels   <- sort(unique(wp_alldata$PROD_ABBR))
alldata_vendor_levels <- sort(unique(wp_alldata$VENDOR))

wp_alldata <- wp_alldata |>
  mutate(
    STATE_ABBR = factor(STATE_ABBR, levels = alldata_state_levels),
    PROD_ABBR  = factor(PROD_ABBR,  levels = alldata_prod_levels),
    VENDOR     = factor(VENDOR,     levels = alldata_vendor_levels)
  )

n_alldata  <- nrow(wp_alldata)
n_expected <- 134358L
n_prior_tr <- sum(wp_alldata$STAT_PROFILE_DATE_YEAR %in% 2006:2012)
n_prior_te <- sum(wp_alldata$STAT_PROFILE_DATE_YEAR %in% 2013:2014)

cat(sprintf("  Expected all-data rows : %s\n", format(n_expected, big.mark = ",")))
cat(sprintf("  Actual all-data rows   : %s\n", format(n_alldata,  big.mark = ",")))
cat(sprintf("  Row count check        : %s\n",
            if (n_alldata == n_expected) "PASS" else
              sprintf("DIFF (%+d) — 2006-2012: %s, 2013-2014: %s",
                      n_alldata - n_expected,
                      format(n_prior_tr, big.mark = ","),
                      format(n_prior_te, big.mark = ","))))
cat(sprintf("  Training years         : %s\n",
            paste(sort(unique(wp_alldata$STAT_PROFILE_DATE_YEAR)), collapse = ", ")))
cat(sprintf("  STATE_ABBR levels      : %d\n", length(alldata_state_levels)))
cat(sprintf("  PROD_ABBR levels       : %d  (%s)\n",
            length(alldata_prod_levels), paste(alldata_prod_levels, collapse = ", ")))
cat(sprintf("  VENDOR levels          : %d\n\n", length(alldata_vendor_levels)))

rf_alldata <- wp_alldata |> select(all_of(c(RF_PREDICTORS, "log_wp")))

# =============================================================================
# Fit RF_FINAL_ALL_DATA
# =============================================================================

cat("Fitting RF_FINAL_ALL_DATA on 2006-2014 all eligible data...\n")
cat(sprintf("  num.trees=%d  mtry=%d  min.node.size=%d  sample.fraction=%.1f  replace=%s\n",
            FINAL_NUM_TREES, FINAL_MTRY, FINAL_MIN_NODE_SIZE,
            FINAL_SAMPLE_FRACTION, tolower(as.character(FINAL_REPLACE))))

t0 <- proc.time()
rf_final <- ranger(
  log_wp ~ .,
  data                      = rf_alldata,
  num.trees                 = FINAL_NUM_TREES,
  mtry                      = FINAL_MTRY,
  min.node.size             = FINAL_MIN_NODE_SIZE,
  replace                   = FINAL_REPLACE,
  sample.fraction           = FINAL_SAMPLE_FRACTION,
  importance                = "permutation",
  respect.unordered.factors = "order",
  seed                      = SEED
)
rf_final_time <- round((proc.time() - t0)["elapsed"], 1)

oob_r2   <- round(rf_final$r.squared, 4)
oob_rmse <- round(sqrt(rf_final$prediction.error), 4)

cat(sprintf("  RF_FINAL_ALL_DATA done in %.1fs\n", rf_final_time))
cat(sprintf("  OOB R2: %.4f  OOB RMSE: %.4f  (diagnostic only — NOT holdout)\n\n",
            oob_r2, oob_rmse))

train_preds <- predict(rf_final, data = rf_alldata)$predictions
train_r2    <- r2_score(rf_alldata$log_wp, train_preds)
train_rmse  <- rmse_fn(rf_alldata$log_wp, train_preds)
train_mae   <- mae_fn(rf_alldata$log_wp,  train_preds)
cat(sprintf("  Train R2: %.4f  RMSE: %.4f  MAE: %.4f  (within-sample diagnostic)\n\n",
            train_r2, train_rmse, train_mae))

# =============================================================================
# Feature importance
# =============================================================================

imp_raw_final   <- sort(rf_final$variable.importance, decreasing = TRUE)
imp_total_final <- sum(abs(imp_raw_final))

imp_list_final <- lapply(seq_along(imp_raw_final), function(i) {
  v <- names(imp_raw_final)[i]
  list(variable     = v,
       importance   = round(imp_raw_final[[i]], 6),
       rank         = i,
       pct_of_total = round(imp_raw_final[[i]] / imp_total_final * 100, 2),
       group        = group_of(v))
})

# Locked RF_1_SAFE_TUNED importance (from rf_feature_importance.json)
imp_list_locked <- list(
  list(variable="log_prev_wp",             importance=5.952889, rank=1,  pct_of_total=60.77, group="prior_performance_lag"),
  list(variable="log_prev_poly",           importance=1.653958, rank=2,  pct_of_total=16.88, group="prior_performance_lag"),
  list(variable="PROD_ABBR",               importance=1.032208, rank=3,  pct_of_total=10.54, group="product"),
  list(variable="AGENCY_APPOINTMENT_YEAR", importance=0.263883, rank=4,  pct_of_total=2.69,  group="agency_characteristics"),
  list(variable="ACTIVE_PRODUCERS",        importance=0.227611, rank=5,  pct_of_total=2.32,  group="agency_characteristics"),
  list(variable="MIN_AGE",                 importance=0.182917, rank=6,  pct_of_total=1.87,  group="agency_characteristics"),
  list(variable="MAX_AGE",                 importance=0.147964, rank=7,  pct_of_total=1.51,  group="agency_characteristics"),
  list(variable="STATE_ABBR",              importance=0.137293, rank=8,  pct_of_total=1.40,  group="geography"),
  list(variable="VENDOR",                  importance=0.124808, rank=9,  pct_of_total=1.27,  group="vendor"),
  list(variable="STAT_PROFILE_DATE_YEAR",  importance=0.072280, rank=10, pct_of_total=0.74,  group="time_trend")
)

# =============================================================================
# Build 2015 prediction population
# =============================================================================

cat("Building 2015 prediction population...\n")

wp_2015_raw <- raw |>
  filter(
    STAT_PROFILE_DATE_YEAR == 2015,
    PROD_ABBR != "COMMPOL"
  ) |>
  mutate(
    log_prev_wp   = suppressWarnings(log(PREV_WRTN_PREM_AMT    + 1)),
    log_prev_poly = suppressWarnings(log(PREV_POLY_INFORCE_QTY + 1))
  ) |>
  mutate(
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  ) |>
  filter(!is.na(log_prev_wp), !is.na(log_prev_poly))

cat(sprintf("  2015 candidate rows (after lag filter): %s\n", format(nrow(wp_2015_raw), big.mark = ",")))

unseen_state  <- setdiff(unique(as.character(wp_2015_raw$STATE_ABBR)),  alldata_state_levels)
unseen_prod   <- setdiff(unique(as.character(wp_2015_raw$PROD_ABBR)),   alldata_prod_levels)
unseen_vendor <- setdiff(unique(as.character(wp_2015_raw$VENDOR)),      alldata_vendor_levels)

if (length(unseen_state)  > 0) cat(sprintf("  WARNING: Unseen STATE_ABBR in 2015: %s\n",  paste(unseen_state,  collapse = ", ")))
if (length(unseen_prod)   > 0) cat(sprintf("  WARNING: Unseen PROD_ABBR in 2015:  %s\n",  paste(unseen_prod,  collapse = ", ")))
if (length(unseen_vendor) > 0) cat(sprintf("  WARNING: Unseen VENDOR in 2015:     %s\n",  paste(unseen_vendor, collapse = ", ")))

rows_with_unseen <- (wp_2015_raw$STATE_ABBR %in% unseen_state) |
                    (wp_2015_raw$PROD_ABBR   %in% unseen_prod)  |
                    (wp_2015_raw$VENDOR      %in% unseen_vendor)
n_2015_unseen_excluded <- sum(rows_with_unseen)
wp_2015_pred <- wp_2015_raw[!rows_with_unseen, ]

if (n_2015_unseen_excluded > 0) {
  cat(sprintf("  Excluded %d 2015 rows with unseen categorical levels.\n", n_2015_unseen_excluded))
}

wp_2015_pred <- wp_2015_pred |>
  mutate(
    STATE_ABBR = factor(STATE_ABBR, levels = alldata_state_levels),
    PROD_ABBR  = factor(PROD_ABBR,  levels = alldata_prod_levels),
    VENDOR     = factor(VENDOR,     levels = alldata_vendor_levels)
  )

na_after_factor <- sum(is.na(wp_2015_pred$STATE_ABBR)) +
                   sum(is.na(wp_2015_pred$PROD_ABBR))  +
                   sum(is.na(wp_2015_pred$VENDOR))
if (na_after_factor > 0) {
  cat(sprintf("  WARNING: %d NA factor values after leveling — excluding.\n", na_after_factor))
  wp_2015_pred <- wp_2015_pred |> filter(!is.na(STATE_ABBR), !is.na(PROD_ABBR), !is.na(VENDOR))
}

n_2015_pred <- nrow(wp_2015_pred)
cat(sprintf("  2015 eligible prediction rows: %s\n\n", format(n_2015_pred, big.mark = ",")))

# Generate predictions
cat("Generating 2015 predictions...\n")
pred_2015_logwp <- predict(rf_final, data = wp_2015_pred |> select(all_of(RF_PREDICTORS)))$predictions
pred_2015_wp    <- pmax(exp(pred_2015_logwp) - 1, 0)

n_finite_log <- sum(is.finite(pred_2015_logwp))
n_finite_wp  <- sum(is.finite(pred_2015_wp))
n_neg_wp     <- sum(pred_2015_wp < 0, na.rm = TRUE)

cat(sprintf("  Predictions: %d  |  finite pred_log_wp: %d  |  finite pred_wp: %d  |  pred_wp<0: %d\n\n",
            n_2015_pred, n_finite_log, n_finite_wp, n_neg_wp))

has_actual_wp <- "WRTN_PREM_AMT" %in% colnames(wp_2015_pred) &&
                 any(!is.na(wp_2015_pred$WRTN_PREM_AMT) & wp_2015_pred$WRTN_PREM_AMT > 0)

pred_2015_df <- wp_2015_pred |>
  mutate(
    STATE_ABBR  = as.character(STATE_ABBR),
    PROD_ABBR   = as.character(PROD_ABBR),
    VENDOR      = as.character(VENDOR),
    pred_log_wp = round(pred_2015_logwp, 6),
    pred_wp     = round(pred_2015_wp, 2)
  )

if (has_actual_wp) {
  pred_2015_df <- pred_2015_df |>
    mutate(
      actual_2015_wp_reference     = WRTN_PREM_AMT,
      actual_2015_log_wp_reference = round(log(pmax(WRTN_PREM_AMT, 0) + 1), 6)
    )
}

base_cols_2015 <- c("AGENCY_ID", "PRIMARY_AGENCY_ID",
                    "STAT_PROFILE_DATE_YEAR", "STATE_ABBR", "PROD_ABBR", "VENDOR",
                    "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                    "PREV_WRTN_PREM_AMT", "PREV_POLY_INFORCE_QTY",
                    "log_prev_wp", "log_prev_poly",
                    "pred_log_wp", "pred_wp")
if (has_actual_wp) {
  base_cols_2015 <- c(base_cols_2015, "actual_2015_wp_reference", "actual_2015_log_wp_reference")
}
pred_2015_out_cols <- intersect(base_cols_2015, colnames(pred_2015_df))
pred_2015_export   <- pred_2015_df |> select(all_of(pred_2015_out_cols))

csv_path <- file.path(output_dir, "rf_final_2015_predictions.csv")
write.csv(pred_2015_export, csv_path, row.names = FALSE)
cat(sprintf("  rf_final_2015_predictions.csv — %s rows — %.1f KB\n\n",
            format(n_2015_pred, big.mark = ","), file.size(csv_path) / 1024))

# =============================================================================
# Training-population diagnostic sample (2,000 rows)
# =============================================================================

cat("Building training-population diagnostic sample...\n")
n_years_train <- length(unique(wp_alldata$STAT_PROFILE_DATE_YEAR))
n_per_year    <- ceiling(2000L / n_years_train)
set.seed(SEED)
samp_alldata <- wp_alldata |>
  group_by(STAT_PROFILE_DATE_YEAR) |>
  slice_sample(n = n_per_year) |>
  ungroup() |>
  slice_head(n = 2000L)

samp_preds <- predict(rf_final, data = samp_alldata |> select(all_of(RF_PREDICTORS)))$predictions

samp_export <- samp_alldata |>
  mutate(
    STATE_ABBR    = as.character(STATE_ABBR),
    PROD_ABBR     = as.character(PROD_ABBR),
    VENDOR        = as.character(VENDOR),
    actual_log_wp = round(log_wp, 6),
    actual_wp     = round(exp(log_wp) - 1, 2),
    pred_log_wp   = round(samp_preds, 6),
    pred_wp       = round(pmax(exp(samp_preds) - 1, 0), 2),
    residual_log  = round(log_wp - samp_preds, 6),
    abs_error_log = round(abs(log_wp - samp_preds), 6)
  ) |>
  select(AGENCY_ID, STAT_PROFILE_DATE_YEAR, STATE_ABBR, PROD_ABBR, VENDOR,
         actual_log_wp, actual_wp, pred_log_wp, pred_wp, residual_log, abs_error_log)

cat(sprintf("  %d rows sampled from 2006-2014 training population\n\n", nrow(samp_export)))

# =============================================================================
# Save model RDS
# =============================================================================

cat("Saving RF_FINAL_ALL_DATA model object...\n")
rds_obj <- list(
  model_name             = "RF_FINAL_ALL_DATA",
  description            = paste0(
    "Refit of RF_1_SAFE_TUNED on all 2006-2014 eligible rows (n=",
    format(n_alldata, big.mark = ","), "). ",
    "For deployment/prediction use. ",
    "Holdout evidence: RF_1_SAFE_TUNED test R2=0.8839, RMSE=0.7532, MAE=0.4428 (2013-2014)."
  ),
  ranger_model           = rf_final,
  predictor_names        = RF_PREDICTORS,
  numeric_predictors     = RF_NUMERIC_PREDICTORS,
  categorical_predictors = RF_CATEGORICAL_PREDICTORS,
  factor_levels          = list(
    STATE_ABBR = alldata_state_levels,
    PROD_ABBR  = alldata_prod_levels,
    VENDOR     = alldata_vendor_levels
  ),
  target                 = "log_wp",
  target_definition      = "log(WRTN_PREM_AMT + 1)",
  inverse_transform      = "pred_wp = exp(pred_log_wp) - 1",
  training_years         = 2006:2014,
  excluded_years         = c(2005L, 2015L),
  n_training_rows        = n_alldata,
  hyperparameters        = list(
    num_trees               = FINAL_NUM_TREES,
    mtry                    = FINAL_MTRY,
    min_node_size           = FINAL_MIN_NODE_SIZE,
    sample_fraction         = FINAL_SAMPLE_FRACTION,
    replace                 = FINAL_REPLACE,
    importance              = "permutation",
    respect_unordered_factors = "order",
    seed                    = SEED
  ),
  locked_holdout_metrics = LOCKED_HOLDOUT
)

rds_path <- file.path(output_dir, "rf_final_all_data.rds")
saveRDS(rds_obj, rds_path)
cat(sprintf("  rf_final_all_data.rds — %.1f KB\n\n", file.size(rds_path) / 1024))

# =============================================================================
# Validation checks
# =============================================================================

validation_checks <- list(
  row_count = list(
    expected = n_expected,
    actual   = n_alldata,
    pass     = (n_alldata == n_expected),
    note     = if (n_alldata == n_expected) "PASS" else
                 sprintf("DIFF: actual=%d expected=%d delta=%+d",
                         n_alldata, n_expected, n_alldata - n_expected)
  ),
  year_check = list(
    training_years  = as.list(sort(unique(wp_alldata$STAT_PROFILE_DATE_YEAR))),
    excluded_2005   = !(2005L %in% unique(wp_alldata$STAT_PROFILE_DATE_YEAR)),
    excluded_2015   = !(2015L %in% unique(wp_alldata$STAT_PROFILE_DATE_YEAR)),
    pass            = (function() {
      obs <- sort(unique(as.integer(wp_alldata$STAT_PROFILE_DATE_YEAR)))
      exp <- 2006:2014
      setequal(obs, exp) && length(obs) == length(exp)
    })()
  ),
  predictor_check = list(
    final_predictors           = RF_PREDICTORS,
    n_predictors               = P,
    agency_cluster_excluded    = TRUE,
    agency_id_excluded         = TRUE,
    primary_agency_id_excluded = TRUE,
    leakage_vars_excluded      = TRUE
  ),
  factor_levels = list(
    STATE_ABBR_n_levels  = length(alldata_state_levels),
    PROD_ABBR_n_levels   = length(alldata_prod_levels),
    VENDOR_n_levels      = length(alldata_vendor_levels),
    saved_to_rds         = TRUE,
    saved_to_metadata    = TRUE
  ),
  predictions_2015 = list(
    n_candidate_rows        = nrow(wp_2015_raw),
    n_unseen_level_excluded = n_2015_unseen_excluded,
    n_eligible_rows         = n_2015_pred,
    unseen_state_levels     = as.list(unseen_state),
    unseen_prod_levels      = as.list(unseen_prod),
    unseen_vendor_levels    = as.list(unseen_vendor),
    n_finite_pred_log_wp    = n_finite_log,
    n_finite_pred_wp        = n_finite_wp,
    n_pred_wp_negative      = n_neg_wp,
    all_preds_finite        = (n_finite_log == n_2015_pred),
    pred_wp_nonneg          = (n_neg_wp == 0),
    pass                    = (n_finite_log == n_2015_pred && n_neg_wp == 0)
  ),
  artifacts = list(
    model_rds_saved         = file.exists(file.path(output_dir, "rf_final_all_data.rds")),
    csv_saved               = file.exists(csv_path),
    metadata_json_queued    = TRUE,
    template_json_queued    = TRUE
  )
)

# =============================================================================
# Assemble JSON outputs
# =============================================================================

cat("Assembling JSON outputs...\n\n")

# ---- 1. rf_final_all_data_summary.json ----
summary_warnings <- list(
  "RF_FINAL_ALL_DATA has no untouched holdout — all 2006-2014 data used for training.",
  "OOB metrics are diagnostics only; they do not replace the locked holdout evidence.",
  "2015 predictions are one-year-ahead artifacts, not evaluation results.",
  "STAT_PROFILE_DATE_YEAR=2015 is numeric; RF routes year=2015 via training-period splits.",
  if (n_2015_unseen_excluded > 0) {
    sprintf("%d 2015 rows excluded due to unseen categorical levels.", n_2015_unseen_excluded)
  } else {
    "All 2015 rows had known categorical levels — no rows excluded."
  }
)

summary_out <- list(
  purpose = paste0(
    "Final RF all-data refit for Dataset A Part 2. Model selection is complete. ",
    "RF_FINAL_ALL_DATA is the deployment/refit version of RF_1_SAFE_TUNED, ",
    "trained on all 2006-2014 eligible rows (n=", format(n_alldata, big.mark = ","), "). ",
    "The holdout evidence (2013-2014) is unchanged and carried forward."
  ),
  model_name             = "RF_FINAL_ALL_DATA",
  selected_specification = "RF_1_SAFE_TUNED",
  training_years         = "2006-2014",
  excluded_years         = list(2005L, 2015L),
  n_training_rows        = n_alldata,
  row_count_check        = validation_checks$row_count,
  predictors = list(
    numeric     = RF_NUMERIC_PREDICTORS,
    categorical = RF_CATEGORICAL_PREDICTORS,
    p           = P,
    excluded    = list("AGENCY_CLUSTER", "AGENCY_ID", "PRIMARY_AGENCY_ID",
                       "PROD_LINE", "POLY_INFORCE_QTY", "RETENTION_RATIO",
                       "LOSS_RATIO", "PREV_LOSS_RATIO", "PREV_RETENTION_RATIO")
  ),
  hyperparameters = list(
    num_trees               = FINAL_NUM_TREES,
    mtry                    = FINAL_MTRY,
    min_node_size           = FINAL_MIN_NODE_SIZE,
    sample_fraction         = FINAL_SAMPLE_FRACTION,
    replace                 = FINAL_REPLACE,
    importance              = "permutation",
    respect_unordered_factors = "order",
    seed                    = SEED
  ),
  oob_diagnostics = list(
    oob_r2   = oob_r2,
    oob_rmse = oob_rmse,
    note     = "OOB from all-data refit. NOT a holdout metric. All 2006-2014 rows used for training."
  ),
  train_within_sample = list(
    n    = n_alldata,
    r2   = train_r2,
    rmse = train_rmse,
    mae  = train_mae,
    note = "Within-sample fit. Not a test metric."
  ),
  locked_holdout_metrics = LOCKED_HOLDOUT,
  agency_cluster_sensitivity = list(
    result    = "NO_MATERIAL_GAIN_FROM_CLUSTERING",
    best_k    = 50L,
    delta_r2  = -0.0016,
    threshold = "delta seen-agency R2 < 0.005",
    conclusion = "Agency clustering does not materially improve RF. RF_1_SAFE_TUNED retained as final specification."
  ),
  feature_importance_summary = list(
    top_predictor     = imp_list_final[[1]]$variable,
    top_predictor_pct = imp_list_final[[1]]$pct_of_total,
    top_5             = sapply(head(imp_list_final, 5L), `[[`, "variable")
  ),
  predictions_2015 = list(
    n_rows    = n_2015_pred,
    year      = 2015L,
    purpose   = "One-year-ahead predictions using prior-year lag predictors and 2015 agency/product information.",
    note      = "NOT evaluation. Do not compute R2/RMSE/MAE on 2015. Do not call 2015 a test set.",
    generated = (n_2015_pred > 0)
  ),
  validation           = validation_checks,
  output_files         = list(
    "rf_final_all_data_summary.json",
    "rf_final_all_data_metadata.json",
    "rf_final_all_data_importance.json",
    "rf_final_all_data_predictions_sample.json",
    "rf_final_2015_predictions.csv",
    "rf_final_2015_predictions.json",
    "rf_final_prediction_input_template.json",
    "rf_final_all_data.rds"
  ),
  warnings_and_limitations = summary_warnings,
  final_recommendation = list(
    deployment_model        = "RF_FINAL_ALL_DATA",
    evaluated_specification = "RF_1_SAFE_TUNED",
    holdout_evidence        = "2013-2014: Test R2=0.8839, RMSE=0.7532, MAE=0.4428",
    statement = paste0(
      "RF_FINAL_ALL_DATA is the deployment version of the selected RF_1_SAFE_TUNED specification. ",
      "RF_1_SAFE_TUNED remains the evaluated final specification. ",
      "Holdout metrics from 2013-2014 remain the official evidence. ",
      "2015 predictions are one-year-ahead artifacts, not evaluation results."
    )
  )
)

# ---- 2. rf_final_all_data_metadata.json ----
metadata_out <- list(
  model_name    = "RF_FINAL_ALL_DATA",
  model_purpose = "Deployment/prediction version of RF_1_SAFE_TUNED. Trained on all 2006-2014 eligible data.",
  target_definition   = "log_wp = log(WRTN_PREM_AMT + 1)",
  inverse_transform   = "pred_wp = exp(pred_log_wp) - 1",
  training_years_used = as.list(2006:2014),
  excluded_years      = list(2005L, 2015L),
  row_filters = list(
    "STAT_PROFILE_DATE_YEAR in 2006:2014",
    "PROD_ABBR != 'COMMPOL'",
    "WRTN_PREM_AMT > 0",
    "!is.na(log_wp)",
    "!is.na(log_prev_wp)",
    "!is.na(log_prev_poly)"
  ),
  n_training_rows = n_alldata,
  predictors = list(
    all         = RF_PREDICTORS,
    numeric     = RF_NUMERIC_PREDICTORS,
    categorical = RF_CATEGORICAL_PREDICTORS,
    p           = P
  ),
  categorical_factor_levels = list(
    STATE_ABBR = alldata_state_levels,
    PROD_ABBR  = alldata_prod_levels,
    VENDOR     = alldata_vendor_levels
  ),
  lag_variable_definitions = list(
    list(field="log_prev_wp",   formula="log(PREV_WRTN_PREM_AMT + 1)",
         source="PREV_WRTN_PREM_AMT from agency_performance table"),
    list(field="log_prev_poly", formula="log(PREV_POLY_INFORCE_QTY + 1)",
         source="PREV_POLY_INFORCE_QTY from agency_performance table")
  ),
  numeric_input_validation = list(
    PREV_WRTN_PREM_AMT      = "numeric >= 0",
    PREV_POLY_INFORCE_QTY   = "numeric >= 0",
    ACTIVE_PRODUCERS        = "integer >= 0",
    AGENCY_APPOINTMENT_YEAR = "integer year",
    MAX_AGE                 = "positive numeric",
    MIN_AGE                 = "positive numeric; MIN_AGE <= MAX_AGE",
    STAT_PROFILE_DATE_YEAR  = "integer year; training range 2006-2014; 2015 supported for one-year-ahead prediction"
  ),
  frontend_input_guidance = list(
    categorical_note   = "STATE_ABBR, PROD_ABBR, VENDOR use dropdowns populated from categorical_factor_levels.",
    year_note          = "STAT_PROFILE_DATE_YEAR is numeric, not categorical. 2015 predictions are allowed.",
    unseen_levels_note = "Do not allow STATE_ABBR/PROD_ABBR/VENDOR values outside categorical_factor_levels unless backend fallback is implemented.",
    unknown_vendor     = if ("Unknown" %in% alldata_vendor_levels) {
      "VENDOR='Unknown' is a valid training-population level."
    } else {
      "VENDOR='Unknown' is NOT in training data. Do not use as a fallback."
    }
  ),
  year_numeric_note    = "STAT_PROFILE_DATE_YEAR is numeric. RF routes year=2015 via split structure on highest observed training years.",
  prediction_year_note = "2015 predictions are one-year-ahead artifacts using prior-year lag predictors. Not a multi-year forecast.",
  locked_holdout_metrics = LOCKED_HOLDOUT,
  model_notes = list(
    "RF_FINAL_ALL_DATA is the deployment/refit version of RF_1_SAFE_TUNED.",
    "RF_1_SAFE_TUNED remains the evaluated final specification.",
    "Holdout metrics from 2013-2014 are the official evidence.",
    "Use RF_FINAL_ALL_DATA for prediction; use RF_1_SAFE_TUNED holdout metrics for model-selection reporting."
  )
)

# ---- 3. rf_final_all_data_importance.json ----
importance_out <- list(
  description = paste0(
    "Feature importance for RF_FINAL_ALL_DATA (all-data refit) and RF_1_SAFE_TUNED (holdout-evaluated). ",
    "holdout_evaluated_rf_importance: locked from rf_feature_importance.json. ",
    "final_all_data_refit_importance: from RF_FINAL_ALL_DATA trained on 2006-2014."
  ),
  holdout_evaluated_rf_importance = list(
    model_id        = "RF_1_SAFE_TUNED",
    data_used       = "2006-2012 training rows (n=103,377)",
    importance_type = "permutation",
    note            = "Locked from modeling/dataset_a_part2/outputs/rf_feature_importance.json",
    importance      = imp_list_locked
  ),
  final_all_data_refit_importance = list(
    model_id        = "RF_FINAL_ALL_DATA",
    data_used       = sprintf("2006-2014 all eligible rows (n=%d)", n_alldata),
    importance_type = "permutation",
    note            = "From all-data refit. Not a holdout metric.",
    importance      = imp_list_final
  )
)

# ---- 4. rf_final_all_data_predictions_sample.json ----
pred_sample_out <- list(
  description = paste0(
    "TRAINING-POPULATION DIAGNOSTIC SAMPLE. NOT HOLDOUT EVALUATION. ",
    nrow(samp_export), " rows sampled proportionally from 2006-2014 training population. ",
    "Low residuals reflect within-sample fit, not out-of-sample performance. ",
    "Official holdout: RF_1_SAFE_TUNED test R2=0.8839, RMSE=0.7532, MAE=0.4428 (2013-2014)."
  ),
  n_rows      = nrow(samp_export),
  predictions = lapply(seq_len(nrow(samp_export)), function(i) {
    r <- samp_export[i, ]
    list(AGENCY_ID              = r$AGENCY_ID,
         STAT_PROFILE_DATE_YEAR = r$STAT_PROFILE_DATE_YEAR,
         STATE_ABBR             = r$STATE_ABBR,
         PROD_ABBR              = r$PROD_ABBR,
         VENDOR                 = r$VENDOR,
         actual_log_wp          = r$actual_log_wp,
         actual_wp              = r$actual_wp,
         pred_log_wp            = r$pred_log_wp,
         pred_wp                = r$pred_wp,
         residual_log           = r$residual_log,
         abs_error_log          = r$abs_error_log)
  })
)

# ---- 5. rf_final_2015_predictions.json ----
n_json_2015      <- n_2015_pred
sampled_for_json <- FALSE
pred_2015_json_note <- sprintf(
  "All %d 2015 prediction rows included in JSON and in rf_final_2015_predictions.csv.",
  n_2015_pred
)

json_2015_df <- pred_2015_export

pred_2015_json_out <- list(
  description = paste0(
    "2015 one-year-ahead Written Premium predictions from RF_FINAL_ALL_DATA. ",
    "NOT evaluation — 2015 WRTN_PREM_AMT is the quantity being predicted. ",
    "Do not compute R2/RMSE/MAE on 2015. Do not call 2015 a test set. ",
    pred_2015_json_note
  ),
  model                    = "RF_FINAL_ALL_DATA",
  prediction_year          = 2015L,
  n_rows_in_json           = n_json_2015,
  n_rows_total             = n_2015_pred,
  sampled                  = FALSE,
  n_excluded_unseen_levels = n_2015_unseen_excluded,
  unseen_levels_detail     = list(
    STATE_ABBR = as.list(unseen_state),
    PROD_ABBR  = as.list(unseen_prod),
    VENDOR     = as.list(unseen_vendor)
  ),
  has_actual_wp_reference = has_actual_wp,
  actual_wp_reference_note = if (has_actual_wp) {
    "actual_2015_wp_reference: audit/reference only — not used for evaluation."
  } else {
    "No 2015 actual WRTN_PREM_AMT available in database for reference."
  },
  predictions = lapply(seq_len(nrow(json_2015_df)), function(i) {
    r   <- json_2015_df[i, ]
    out <- list(
      AGENCY_ID               = r$AGENCY_ID,
      STAT_PROFILE_DATE_YEAR  = r$STAT_PROFILE_DATE_YEAR,
      STATE_ABBR              = r$STATE_ABBR,
      PROD_ABBR               = r$PROD_ABBR,
      VENDOR                  = r$VENDOR,
      ACTIVE_PRODUCERS        = r$ACTIVE_PRODUCERS,
      AGENCY_APPOINTMENT_YEAR = r$AGENCY_APPOINTMENT_YEAR,
      MAX_AGE                 = r$MAX_AGE,
      MIN_AGE                 = r$MIN_AGE,
      PREV_WRTN_PREM_AMT      = r$PREV_WRTN_PREM_AMT,
      PREV_POLY_INFORCE_QTY   = r$PREV_POLY_INFORCE_QTY,
      log_prev_wp             = r$log_prev_wp,
      log_prev_poly           = r$log_prev_poly,
      pred_log_wp             = r$pred_log_wp,
      pred_wp                 = r$pred_wp
    )
    if ("PRIMARY_AGENCY_ID" %in% colnames(json_2015_df)) out$PRIMARY_AGENCY_ID <- r$PRIMARY_AGENCY_ID
    if (has_actual_wp && "actual_2015_wp_reference" %in% colnames(json_2015_df)) {
      out$actual_2015_wp_reference     <- r$actual_2015_wp_reference
      out$actual_2015_log_wp_reference <- r$actual_2015_log_wp_reference
    }
    out
  })
)

# ---- 6. rf_final_prediction_input_template.json ----
vendor_unknown_note <- if ("Unknown" %in% alldata_vendor_levels) {
  "'Unknown' is a valid VENDOR level in the training population."
} else {
  paste0("'Unknown' is NOT a valid VENDOR level. Using '", alldata_vendor_levels[1],
         "' in example. Valid levels: ", paste(alldata_vendor_levels, collapse = ", "))
}

input_template <- list(
  description = paste0(
    "User-input prediction template for RF_FINAL_ALL_DATA. ",
    "Defines fields a user must provide to generate a Written Premium prediction. ",
    "Categorical inputs use dropdowns from allowed_values. ",
    "Do not allow STATE_ABBR/PROD_ABBR/VENDOR values outside allowed_values."
  ),
  model              = "RF_FINAL_ALL_DATA",
  target             = "log_wp = log(WRTN_PREM_AMT + 1)",
  inverse_transform  = "pred_wp = exp(pred_log_wp) - 1",
  prediction_outputs = list(
    list(field="pred_log_wp", type="numeric",
         description="Predicted log(WRTN_PREM_AMT + 1)"),
    list(field="pred_wp",     type="numeric",
         description="Predicted written premium in dollars: exp(pred_log_wp) - 1")
  ),
  user_input_fields = list(
    list(field="PREV_WRTN_PREM_AMT",      type="numeric",     required=TRUE,
         description="Prior-year written premium amount (dollars).",
         validation="PREV_WRTN_PREM_AMT >= 0",
         transform="log_prev_wp = log(PREV_WRTN_PREM_AMT + 1)"),
    list(field="PREV_POLY_INFORCE_QTY",   type="numeric",     required=TRUE,
         description="Prior-year policies in force count.",
         validation="PREV_POLY_INFORCE_QTY >= 0",
         transform="log_prev_poly = log(PREV_POLY_INFORCE_QTY + 1)"),
    list(field="ACTIVE_PRODUCERS",        type="numeric",     required=TRUE,
         description="Number of active producers at the agency.",
         validation="Integer >= 0"),
    list(field="AGENCY_APPOINTMENT_YEAR", type="numeric",     required=TRUE,
         description="Year agency was appointed (4-digit year).",
         validation="Integer year, e.g. 1990-2015"),
    list(field="MAX_AGE",                 type="numeric",     required=TRUE,
         description="Maximum producer age at agency.",
         validation="Positive numeric"),
    list(field="MIN_AGE",                 type="numeric",     required=TRUE,
         description="Minimum producer age at agency.",
         validation="Positive numeric; MIN_AGE <= MAX_AGE"),
    list(field="STAT_PROFILE_DATE_YEAR",  type="numeric",     required=TRUE,
         description=paste0(
           "Prediction year (numeric, not categorical). Training range: 2006-2014. ",
           "2015 supported for one-year-ahead prediction."),
         validation="Integer year; typically 2006-2015 for this model"),
    list(field="STATE_ABBR",  type="categorical", required=TRUE,
         description="Two-letter state abbreviation.",
         allowed_values=alldata_state_levels,
         ui_note="Dropdown from allowed_values. Unseen values are rejected."),
    list(field="PROD_ABBR",   type="categorical", required=TRUE,
         description="Product abbreviation code.",
         allowed_values=alldata_prod_levels,
         ui_note="Dropdown from allowed_values. COMMPOL excluded from model population."),
    list(field="VENDOR",      type="categorical", required=TRUE,
         description="Vendor identifier.",
         allowed_values=alldata_vendor_levels,
         ui_note="Dropdown from allowed_values.")
  ),
  backend_computed_fields = list(
    list(field="log_prev_wp",   formula="log(PREV_WRTN_PREM_AMT + 1)",
         description="Log-transformed prior written premium. Computed by backend before prediction."),
    list(field="log_prev_poly", formula="log(PREV_POLY_INFORCE_QTY + 1)",
         description="Log-transformed prior policies in force. Computed by backend before prediction.")
  ),
  example_inputs = list(
    list(
      label                   = "Generic valid example (year 2013)",
      PREV_WRTN_PREM_AMT      = 150000,
      PREV_POLY_INFORCE_QTY   = 320,
      ACTIVE_PRODUCERS        = 5L,
      AGENCY_APPOINTMENT_YEAR = 2001L,
      MAX_AGE                 = 55,
      MIN_AGE                 = 38,
      STAT_PROFILE_DATE_YEAR  = 2013L,
      STATE_ABBR              = if ("IL" %in% alldata_state_levels) "IL" else alldata_state_levels[1],
      PROD_ABBR               = alldata_prod_levels[1],
      VENDOR                  = alldata_vendor_levels[1],
      backend_computed        = list(
        log_prev_wp   = round(log(150000 + 1), 4),
        log_prev_poly = round(log(320 + 1), 4)
      )
    ),
    list(
      label                   = "2015 prediction example",
      PREV_WRTN_PREM_AMT      = 200000,
      PREV_POLY_INFORCE_QTY   = 480,
      ACTIVE_PRODUCERS        = 8L,
      AGENCY_APPOINTMENT_YEAR = 1998L,
      MAX_AGE                 = 60,
      MIN_AGE                 = 42,
      STAT_PROFILE_DATE_YEAR  = 2015L,
      STATE_ABBR              = if ("TX" %in% alldata_state_levels) "TX" else alldata_state_levels[1],
      PROD_ABBR               = alldata_prod_levels[1],
      VENDOR                  = alldata_vendor_levels[1],
      note                    = paste0(
        "STAT_PROFILE_DATE_YEAR=2015 is numeric. ",
        "RF routes year=2015 via split structure on highest training-period year values. ",
        "One-year-ahead prediction only — not a multi-year forecast."
      ),
      backend_computed        = list(
        log_prev_wp   = round(log(200000 + 1), 4),
        log_prev_poly = round(log(480 + 1), 4)
      )
    ),
    list(
      label                   = "Example with VENDOR = 'Unknown'",
      PREV_WRTN_PREM_AMT      = 75000,
      PREV_POLY_INFORCE_QTY   = 180,
      ACTIVE_PRODUCERS        = 3L,
      AGENCY_APPOINTMENT_YEAR = 2005L,
      MAX_AGE                 = 48,
      MIN_AGE                 = 35,
      STAT_PROFILE_DATE_YEAR  = 2015L,
      STATE_ABBR              = alldata_state_levels[1],
      PROD_ABBR               = alldata_prod_levels[1],
      VENDOR                  = if ("Unknown" %in% alldata_vendor_levels) "Unknown" else alldata_vendor_levels[1],
      note                    = vendor_unknown_note,
      backend_computed        = list(
        log_prev_wp   = round(log(75000 + 1), 4),
        log_prev_poly = round(log(180 + 1), 4)
      )
    )
  )
)

# =============================================================================
# Write all outputs
# =============================================================================

cat("Writing output files...\n")
write_json_out(summary_out,        "rf_final_all_data_summary.json")
write_json_out(metadata_out,       "rf_final_all_data_metadata.json")
write_json_out(importance_out,     "rf_final_all_data_importance.json")
write_json_out(pred_sample_out,    "rf_final_all_data_predictions_sample.json")
write_json_out(pred_2015_json_out, "rf_final_2015_predictions.json")
write_json_out(input_template,     "rf_final_prediction_input_template.json")

cat(sprintf("\n=== RF Final All-Data Refit complete ===\n"))
cat(sprintf("Outputs in: %s\n\n", output_dir))
cat(sprintf("--- Summary ---\n"))
cat(sprintf("Final training rows    : %s (expected %s) — %s\n",
            format(n_alldata, big.mark = ","),
            format(n_expected, big.mark = ","),
            if (n_alldata == n_expected) "PASS" else "DIFF"))
cat(sprintf("OOB R2 (diagnostic)    : %.4f\n", oob_r2))
cat(sprintf("OOB RMSE (diagnostic)  : %.4f\n", oob_rmse))
cat(sprintf("Locked holdout R2      : %.4f  (2013-2014, RF_1_SAFE_TUNED)\n", LOCKED_HOLDOUT$test_r2))
cat(sprintf("Locked holdout RMSE    : %.4f\n", LOCKED_HOLDOUT$test_rmse))
cat(sprintf("Locked holdout MAE     : %.4f\n", LOCKED_HOLDOUT$test_mae))
cat(sprintf("2015 pred rows         : %s\n", format(n_2015_pred, big.mark = ",")))
cat(sprintf("2015 unseen excluded   : %d\n", n_2015_unseen_excluded))
cat(sprintf("2015 preds finite      : %s\n", if (n_finite_log == n_2015_pred) "PASS" else "FAIL"))
cat(sprintf("pred_wp >= 0           : %s\n", if (n_neg_wp == 0) "PASS" else "FAIL"))
cat(sprintf("Model RDS saved        : %s\n", if (file.exists(rds_path)) "YES" else "FAIL"))
cat(sprintf("Factor levels saved    : STATE=%d  PROD=%d  VENDOR=%d\n",
            length(alldata_state_levels), length(alldata_prod_levels), length(alldata_vendor_levels)))
cat(sprintf("\nFinal recommendation   : RF_FINAL_ALL_DATA is the deployment version of RF_1_SAFE_TUNED.\n"))
cat(sprintf("                         Holdout evidence (2013-2014) remains the official evaluation.\n"))
cat(sprintf("                         2015 predictions are one-year-ahead artifacts, not evaluation.\n"))
