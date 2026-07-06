# =============================================================================
# Dataset A Part 2 — Written Premium Random Forest
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_wp_rf.R
#
# Outputs: modeling/dataset_a_part2/outputs/
#   rf_summary.json
#   rf_tuning_results.json
#   rf_feature_importance.json
#   rf_model_comparison.json
#   rf_predictions_sample.json
#
# Required packages: DBI, RSQLite, dplyr, ranger, jsonlite
# Benchmark: OLS_ADDITIVE_FINAL (locked results from ols_model_comparison.json)
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(ranger); library(jsonlite)
})

SEED          <- 42L
db_path       <- "insurance.db"
output_dir    <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

RF_PREDICTORS <- c("log_prev_wp", "log_prev_poly",
                   "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR",
                   "MAX_AGE", "MIN_AGE", "STAT_PROFILE_DATE_YEAR",
                   "STATE_ABBR", "PROD_ABBR", "VENDOR")
P <- length(RF_PREDICTORS)  # 10

cat("=== Dataset A Part 2 — WP Random Forest ===\n\n")

# =============================================================================
# Load raw data
# =============================================================================

cat("Connecting to database...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n", format(nrow(raw), big.mark = ","), ncol(raw)))

# =============================================================================
# Build modeling dataset (same pipeline as OLS)
# =============================================================================

cat("Building modeling dataset...\n")

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
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  ) |>
  mutate(
    # Set factor levels from full 2006-2014 population so train and test share levels
    STATE_ABBR = factor(STATE_ABBR),
    PROD_ABBR  = factor(PROD_ABBR),
    VENDOR     = factor(VENDOR)
  )

# Candidate population counts (before NA exclusion — matches OLS candidate population)
n_cand_train <- sum(wp_base$STAT_PROFILE_DATE_YEAR %in% 2006:2012)
n_cand_test  <- sum(wp_base$STAT_PROFILE_DATE_YEAR %in% 2013:2014)

# Effective RF population: drop rows where lag predictors or target are NA.
# Matches OLS lm() na.omit behavior — same rows as OLS effective evaluated rows.
wp_rf <- wp_base |>
  filter(!is.na(log_prev_wp), !is.na(log_prev_poly), !is.na(log_wp))

wp_train_rf <- wp_rf |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_test_rf  <- wp_rf |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)
n_eval_train <- nrow(wp_train_rf)
n_eval_test  <- nrow(wp_test_rf)
n_drop_train <- n_cand_train - n_eval_train
n_drop_test  <- n_cand_test  - n_eval_test

cat(sprintf("  Candidate population  : %s train / %s test\n",
            format(n_cand_train, big.mark = ","), format(n_cand_test, big.mark = ",")))
cat(sprintf("  Effective RF rows     : %s train / %s test\n",
            format(n_eval_train, big.mark = ","), format(n_eval_test, big.mark = ",")))
cat(sprintf("  Dropped (NA lag vars) : %d train / %d test\n\n",
            n_drop_train, n_drop_test))

if (n_eval_train != 103377L || n_eval_test != 30981L) {
  cat(sprintf("  NOTE: RF row counts differ from OLS documented counts (103,377 / 30,981).\n\n"))
}

# RF input data frames (10 predictors + target)
rf_train <- wp_train_rf |> select(all_of(c(RF_PREDICTORS, "log_wp")))
rf_test  <- wp_test_rf  |> select(all_of(c(RF_PREDICTORS, "log_wp")))

# =============================================================================
# Helper functions
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

eval_rf <- function(model, train_df, test_df) {
  pred_tr <- predict(model, data = train_df)$predictions
  pred_te <- predict(model, data = test_df)$predictions
  y_tr    <- train_df$log_wp
  y_te    <- test_df$log_wp
  list(
    n_train = sum(!is.na(pred_tr) & !is.na(y_tr)),
    n_test  = sum(!is.na(pred_te) & !is.na(y_te)),
    train   = list(r2   = r2_score(y_tr, pred_tr),
                   rmse = rmse_fn(y_tr, pred_tr),
                   mae  = mae_fn(y_tr,  pred_tr)),
    test    = list(r2   = r2_score(y_te, pred_te),
                   rmse = rmse_fn(y_te, pred_te),
                   mae  = mae_fn(y_te,  pred_te))
  )
}

write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
  cat(sprintf("  %-45s %.1f KB\n", filename, file.size(path) / 1024))
}

# =============================================================================
# OLS benchmark metrics (locked from OLS outputs)
# =============================================================================

ols_metrics <- list(
  model  = "OLS_ADDITIVE_FINAL",
  source = "locked — modeling/dataset_a_part2/outputs/ols_model_comparison.json",
  n_eval_train = 103377L,
  n_eval_test  = 30981L,
  train  = list(r2 = 0.7810, rmse = 1.0238, mae = 0.6468),
  test   = list(r2 = 0.7552, rmse = 1.0937, mae = 0.6990)
)

# =============================================================================
# OLS fit for prediction sample columns
# =============================================================================

cat("Fitting OLS_ADDITIVE_FINAL on RF population (for prediction sample)...\n")
f_ols <- log_wp ~ log_prev_wp + log_prev_poly +
                  ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                  STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR
m_ols <- lm(f_ols, data = rf_train)
cat("  Done\n\n")

# =============================================================================
# Part D — RF_0_SAFE_DEFAULT
# =============================================================================

cat("Fitting RF_0_SAFE_DEFAULT...\n")
mtry_default <- max(floor(P / 3L), 1L)  # ranger regression default: floor(p/3) = 3

t0 <- proc.time()
rf0 <- ranger(
  log_wp ~ .,
  data                    = rf_train,
  num.trees               = 500L,
  mtry                    = mtry_default,
  min.node.size           = 5L,
  replace                 = TRUE,
  sample.fraction         = 1.0,
  importance              = "permutation",
  respect.unordered.factors = "order",
  seed                    = SEED
)
rf0_time     <- round((proc.time() - t0)["elapsed"], 1)
oob_rmse_rf0 <- round(sqrt(rf0$prediction.error), 4)
oob_r2_rf0   <- round(rf0$r.squared, 4)
perf_rf0     <- eval_rf(rf0, rf_train, rf_test)

cat(sprintf("  RF_0 done in %.1fs\n", rf0_time))
cat(sprintf("  OOB RMSE: %.4f  OOB R²: %.4f  |  train R²: %.4f  test R²: %.4f\n\n",
            oob_rmse_rf0, oob_r2_rf0, perf_rf0$train$r2, perf_rf0$test$r2))

# =============================================================================
# Part E — Tuning grid (OOB-based, test set not touched)
# =============================================================================

cat("Running tuning grid...\n")
mtry_grid <- c(3L, 5L, 7L, 10L)
mns_grid  <- c(5L, 20L, 50L, 100L)
sf_grid   <- c(0.6, 0.8, 1.0)
grid      <- expand.grid(mtry = mtry_grid, min_node_size = mns_grid,
                         sample_fraction = sf_grid, stringsAsFactors = FALSE)
n_grid    <- nrow(grid)
cat(sprintf("  %d candidates: mtry {%s} × min.node.size {%s} × sample.fraction {%s}\n",
            n_grid,
            paste(mtry_grid, collapse = ","),
            paste(mns_grid,  collapse = ","),
            paste(sf_grid,   collapse = ",")))
cat(sprintf("  importance='none' during grid search for speed\n\n"))

grid_results <- vector("list", n_grid)
for (i in seq_len(n_grid)) {
  g   <- grid[i, ]
  t_i <- proc.time()
  m_i <- ranger(
    log_wp ~ .,
    data                    = rf_train,
    num.trees               = 500L,
    mtry                    = g$mtry,
    min.node.size           = g$min_node_size,
    replace                 = (g$sample_fraction == 1.0),
    sample.fraction         = g$sample_fraction,
    importance              = "none",
    respect.unordered.factors = "order",
    seed                    = SEED + i
  )
  rt_i <- round((proc.time() - t_i)["elapsed"], 2)
  grid_results[[i]] <- list(
    model_id        = sprintf("GRID_%02d", i),
    num_trees       = 500L,
    mtry            = g$mtry,
    min_node_size   = g$min_node_size,
    sample_fraction = g$sample_fraction,
    replace         = (g$sample_fraction == 1.0),
    oob_rmse        = round(sqrt(m_i$prediction.error), 4),
    oob_r2          = round(m_i$r.squared, 4),
    runtime_s       = rt_i
  )
  if (i %% 16 == 0 || i == n_grid) {
    cat(sprintf("  [%2d/%d] mtry=%2d mns=%3d sf=%.1f | OOB RMSE=%.4f OOB R²=%.4f | %.1fs\n",
                i, n_grid,
                g$mtry, g$min_node_size, g$sample_fraction,
                grid_results[[i]]$oob_rmse, grid_results[[i]]$oob_r2, rt_i))
  }
}

best_idx    <- which.min(sapply(grid_results, `[[`, "oob_rmse"))
best_g      <- grid[best_idx, ]
best_result <- grid_results[[best_idx]]
cat(sprintf("\n  Best: mtry=%d  min.node.size=%d  sample.fraction=%.1f  OOB RMSE=%.4f\n\n",
            best_g$mtry, best_g$min_node_size, best_g$sample_fraction, best_result$oob_rmse))

# =============================================================================
# Part E (continued) — RF_1_SAFE_TUNED (best params, full importance)
# =============================================================================

cat("Fitting RF_1_SAFE_TUNED with best hyperparameters...\n")
t1 <- proc.time()
rf1 <- ranger(
  log_wp ~ .,
  data                    = rf_train,
  num.trees               = 500L,
  mtry                    = best_g$mtry,
  min.node.size           = best_g$min_node_size,
  replace                 = (best_g$sample_fraction == 1.0),
  sample.fraction         = best_g$sample_fraction,
  importance              = "permutation",
  respect.unordered.factors = "order",
  seed                    = SEED
)
rf1_time     <- round((proc.time() - t1)["elapsed"], 1)
oob_rmse_rf1 <- round(sqrt(rf1$prediction.error), 4)
oob_r2_rf1   <- round(rf1$r.squared, 4)
perf_rf1     <- eval_rf(rf1, rf_train, rf_test)

cat(sprintf("  RF_1 done in %.1fs\n", rf1_time))
cat(sprintf("  OOB RMSE: %.4f  OOB R²: %.4f  |  train R²: %.4f  test R²: %.4f\n\n",
            oob_rmse_rf1, oob_r2_rf1, perf_rf1$train$r2, perf_rf1$test$r2))

# =============================================================================
# Part G — Feature importance (RF_1_SAFE_TUNED, permutation)
# =============================================================================

imp_raw   <- sort(rf1$variable.importance, decreasing = TRUE)
imp_total <- sum(abs(imp_raw))

group_of <- function(v) {
  switch(v,
    log_prev_wp            = "prior_performance_lag",
    log_prev_poly          = "prior_performance_lag",
    ACTIVE_PRODUCERS       = "agency_characteristics",
    AGENCY_APPOINTMENT_YEAR = "agency_characteristics",
    MAX_AGE                = "agency_characteristics",
    MIN_AGE                = "agency_characteristics",
    STATE_ABBR             = "geography",
    PROD_ABBR              = "product",
    VENDOR                 = "vendor",
    STAT_PROFILE_DATE_YEAR = "time_trend",
    "other"
  )
}

imp_list <- lapply(seq_along(imp_raw), function(i) {
  v <- names(imp_raw)[i]
  list(
    variable               = v,
    permutation_importance = round(imp_raw[[i]], 6),
    rank                   = i,
    normalized_pct         = round(imp_raw[[i]] / imp_total * 100, 2),
    group                  = group_of(v)
  )
})

# Group summaries
imp_df    <- do.call(rbind, lapply(imp_list, as.data.frame, stringsAsFactors = FALSE))
grp_sums  <- sort(tapply(imp_df$normalized_pct, imp_df$group, sum), decreasing = TRUE)
grp_table <- lapply(names(grp_sums), function(g) {
  list(group = g, total_normalized_pct = round(grp_sums[[g]], 2))
})

lag_pct      <- grp_sums["prior_performance_lag"]
prod_pct     <- imp_df$normalized_pct[imp_df$variable == "PROD_ABBR"]
state_pct    <- imp_df$normalized_pct[imp_df$variable == "STATE_ABBR"]
vendor_pct   <- imp_df$normalized_pct[imp_df$variable == "VENDOR"]

# =============================================================================
# Part H — Prediction sample (up to 2,000 test rows, stratified by year)
# =============================================================================

cat("Building prediction sample...\n")
n_per_year <- ceiling(min(2000L, n_eval_test) / length(unique(wp_test_rf$STAT_PROFILE_DATE_YEAR)))
set.seed(SEED)
samp <- wp_test_rf |>
  group_by(STAT_PROFILE_DATE_YEAR) |>
  slice_sample(n = n_per_year) |>
  ungroup() |>
  slice_head(n = 2000L)

ols_pred_samp  <- suppressWarnings(predict(m_ols, newdata = samp))
rf0_pred_samp  <- predict(rf0, data = samp |> select(all_of(RF_PREDICTORS)))$predictions
rf1_pred_samp  <- predict(rf1, data = samp |> select(all_of(RF_PREDICTORS)))$predictions

pred_sample_rows <- samp |>
  mutate(
    STATE_ABBR  = as.character(STATE_ABBR),
    PROD_ABBR   = as.character(PROD_ABBR),
    VENDOR      = as.character(VENDOR),
    actual_log_wp          = round(log_wp, 6),
    actual_wp              = round(exp(log_wp) - 1, 2),
    ols_pred_log_wp        = round(ols_pred_samp,  6),
    ols_pred_wp            = round(pmax(exp(ols_pred_samp)  - 1, 0), 2),
    rf_default_pred_log_wp = round(rf0_pred_samp,  6),
    rf_default_pred_wp     = round(pmax(exp(rf0_pred_samp)  - 1, 0), 2),
    rf_tuned_pred_log_wp   = round(rf1_pred_samp,  6),
    rf_tuned_pred_wp       = round(pmax(exp(rf1_pred_samp)  - 1, 0), 2),
    rf_tuned_residual_log  = round(log_wp - rf1_pred_samp, 6),
    rf_tuned_abs_error_log = round(abs(log_wp - rf1_pred_samp), 6)
  ) |>
  select(AGENCY_ID, STATE_ABBR, PROD_ABBR, VENDOR, STAT_PROFILE_DATE_YEAR,
         actual_log_wp, actual_wp,
         ols_pred_log_wp, ols_pred_wp,
         rf_default_pred_log_wp, rf_default_pred_wp,
         rf_tuned_pred_log_wp, rf_tuned_pred_wp,
         rf_tuned_residual_log, rf_tuned_abs_error_log)

cat(sprintf("  %d rows sampled from 2013-2014 test set\n\n", nrow(pred_sample_rows)))

# =============================================================================
# Assemble JSONs
# =============================================================================

delta_r2_rf0   <- round(perf_rf0$test$r2   - ols_metrics$test$r2,   4)
delta_rmse_rf0 <- round(perf_rf0$test$rmse - ols_metrics$test$rmse, 4)
delta_mae_rf0  <- round(perf_rf0$test$mae  - ols_metrics$test$mae,  4)
delta_r2_rf1   <- round(perf_rf1$test$r2   - ols_metrics$test$r2,   4)
delta_rmse_rf1 <- round(perf_rf1$test$rmse - ols_metrics$test$rmse, 4)
delta_mae_rf1  <- round(perf_rf1$test$mae  - ols_metrics$test$mae,  4)

row_count_note <- paste0(
  "Candidate population: ", format(n_cand_train, big.mark = ","), " train / ",
  format(n_cand_test, big.mark = ","), " test ",
  "(rows after WP modeling filters: PROD_ABBR != COMMPOL, WRTN_PREM_AMT > 0, years 2006-2014). ",
  "Effective RF rows: ", format(n_eval_train, big.mark = ","), " train / ",
  format(n_eval_test, big.mark = ","), " test ",
  "(rows with non-NA log_prev_wp and log_prev_poly). ",
  "Rows dropped: ", n_drop_train, " train, ", n_drop_test, " test. ",
  "Matches OLS effective evaluated row counts."
)

rf_recommendation <- if (perf_rf1$test$r2 > ols_metrics$test$r2 + 0.02) {
  list(
    decision = "ADVANCE_RF",
    justification = sprintf(
      paste0(
        "RF_1_SAFE_TUNED test R² = %.4f vs OLS_ADDITIVE_FINAL test R² = %.4f (delta = %+.4f). ",
        "RF materially improves out-of-sample predictive performance on the 2013-2014 holdout. ",
        "Advance RF as the primary predictive model branch."
      ),
      perf_rf1$test$r2, ols_metrics$test$r2, delta_r2_rf1
    )
  )
} else if (perf_rf1$test$r2 > ols_metrics$test$r2 + 0.005) {
  list(
    decision = "ADVANCE_RF",
    justification = sprintf(
      paste0(
        "RF_1_SAFE_TUNED test R² = %.4f vs OLS_ADDITIVE_FINAL test R² = %.4f (delta = %+.4f). ",
        "RF improves out-of-sample performance. ",
        "Advance RF as the primary predictive model branch. ",
        "Retain OLS_ADDITIVE_FINAL for coefficient interpretability."
      ),
      perf_rf1$test$r2, ols_metrics$test$r2, delta_r2_rf1
    )
  )
} else if (perf_rf1$test$r2 > ols_metrics$test$r2) {
  list(
    decision = "MODEST_RF_IMPROVEMENT",
    justification = sprintf(
      paste0(
        "RF_1_SAFE_TUNED test R² = %.4f vs OLS_ADDITIVE_FINAL test R² = %.4f (delta = %+.4f). ",
        "RF improvement is marginal. ",
        "Consider advancing RF for its nonlinear/interaction insights while retaining OLS as the primary interpretable specification."
      ),
      perf_rf1$test$r2, ols_metrics$test$r2, delta_r2_rf1
    )
  )
} else {
  list(
    decision = "RETAIN_OLS_BASELINE",
    justification = sprintf(
      paste0(
        "RF_1_SAFE_TUNED test R² = %.4f vs OLS_ADDITIVE_FINAL test R² = %.4f (delta = %+.4f). ",
        "RF does not improve over OLS on the 2013-2014 holdout. ",
        "Retain OLS_ADDITIVE_FINAL as the primary specification. ",
        "RF feature importance remains valuable for understanding nonlinear effects and variable relevance."
      ),
      perf_rf1$test$r2, ols_metrics$test$r2, delta_r2_rf1
    )
  )
}

# ---------- rf_summary.json ----------
rf_summary_out <- list(
  description = paste0(
    "Random Forest Written Premium modeling for Dataset A Part 2. ",
    "Target: log(WRTN_PREM_AMT + 1). Train: 2006-2012. Test: 2013-2014. ",
    "Same safe predictor set as OLS_ADDITIVE_FINAL (7 numeric + 3 categorical). ",
    "Model class question: does RF improve out-of-sample performance over the additive OLS baseline?"
  ),
  row_count_note  = row_count_note,
  modeling_population = list(
    target            = "log_wp = log(WRTN_PREM_AMT + 1)",
    train_years       = "2006-2012",
    test_years        = "2013-2014",
    n_candidate_train = n_cand_train,
    n_candidate_test  = n_cand_test,
    n_eval_train      = n_eval_train,
    n_eval_test       = n_eval_test
  ),
  predictors = list(
    numeric     = c("log_prev_wp", "log_prev_poly", "ACTIVE_PRODUCERS",
                    "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE", "STAT_PROFILE_DATE_YEAR"),
    categorical = c("STATE_ABBR", "PROD_ABBR", "VENDOR"),
    p           = P
  ),
  ols_benchmark = ols_metrics,
  RF_0_SAFE_DEFAULT = list(
    hyperparameters = list(
      num_trees       = 500L,
      mtry            = mtry_default,
      min_node_size   = 5L,
      replace         = TRUE,
      sample_fraction = 1.0,
      importance      = "permutation",
      respect_unordered_factors = "order"
    ),
    oob_rmse       = oob_rmse_rf0,
    oob_r2         = oob_r2_rf0,
    train          = perf_rf0$train,
    test           = perf_rf0$test,
    runtime_s      = rf0_time,
    delta_vs_ols   = list(test_r2 = delta_r2_rf0, test_rmse = delta_rmse_rf0, test_mae = delta_mae_rf0)
  ),
  RF_1_SAFE_TUNED = list(
    hyperparameters = list(
      num_trees       = 500L,
      mtry            = best_g$mtry,
      min_node_size   = best_g$min_node_size,
      replace         = (best_g$sample_fraction == 1.0),
      sample_fraction = best_g$sample_fraction,
      importance      = "permutation",
      respect_unordered_factors = "order",
      selected_by     = "minimum OOB RMSE across 48-model grid"
    ),
    oob_rmse       = oob_rmse_rf1,
    oob_r2         = oob_r2_rf1,
    train          = perf_rf1$train,
    test           = perf_rf1$test,
    runtime_s      = rf1_time,
    delta_vs_ols   = list(test_r2 = delta_r2_rf1, test_rmse = delta_rmse_rf1, test_mae = delta_mae_rf1)
  ),
  recommendation = rf_recommendation
)

# ---------- rf_tuning_results.json ----------
rf_tuning_out <- list(
  description = paste0(
    "RF tuning grid. 4 × 4 × 3 = 48 candidates. ",
    "Fixed: num.trees=500, importance='none'. ",
    "Varied: mtry {", paste(mtry_grid, collapse = ","), "}, ",
    "min.node.size {", paste(mns_grid, collapse = ","), "}, ",
    "sample.fraction {", paste(sf_grid, collapse = ","), "}. ",
    "Selection criterion: OOB RMSE (test set not used). ",
    "sample.fraction=1.0 uses replace=TRUE (bootstrap); others use replace=FALSE."
  ),
  n_candidates = n_grid,
  grid_results = grid_results,
  best = list(
    model_id        = best_result$model_id,
    mtry            = best_g$mtry,
    min_node_size   = best_g$min_node_size,
    sample_fraction = best_g$sample_fraction,
    replace         = (best_g$sample_fraction == 1.0),
    oob_rmse        = best_result$oob_rmse,
    oob_r2          = best_result$oob_r2
  )
)

# ---------- rf_feature_importance.json ----------
rf_fi_out <- list(
  description = paste0(
    "Permutation feature importance for RF_1_SAFE_TUNED. ",
    "Permutation importance = decrease in OOB accuracy when a variable is randomly permuted. ",
    "Higher = more important. Computed by ranger (importance = 'permutation')."
  ),
  model          = "RF_1_SAFE_TUNED",
  importance_type = "permutation",
  all_variables  = imp_list,
  group_summary  = grp_table,
  top_5          = head(sapply(imp_list, `[[`, "variable"), 5L),
  interpretation = list(
    top_predictor              = imp_list[[1]]$variable,
    lag_vars_pct               = round(lag_pct, 2),
    lag_vars_dominate          = (lag_pct > 40),
    product_pct                = round(prod_pct, 2),
    product_still_matters      = (prod_pct > 5),
    state_pct                  = round(state_pct, 2),
    state_still_matters        = (state_pct > 3),
    vendor_pct                 = round(vendor_pct, 2),
    note = paste0(
      "Prior performance lags (log_prev_wp + log_prev_poly) account for ",
      round(lag_pct, 1), "% of total permutation importance. ",
      "PROD_ABBR = ", round(prod_pct, 1), "%, ",
      "STATE_ABBR = ", round(state_pct, 1), "%, ",
      "VENDOR = ", round(vendor_pct, 1), "%. ",
      if (lag_pct > 50) {
        "The RF story is consistent with OLS: prior premium is by far the strongest predictor, "
      } else {
        "Lag variables dominate but categorical structure contributes meaningfully. "
      },
      "RF captures nonlinear interactions that OLS cannot represent additively."
    )
  )
)

# ---------- rf_model_comparison.json ----------
rf_comparison_out <- list(
  description = paste0(
    "OLS vs RF model comparison on the same target, predictor set, and train/test split. ",
    "OLS metrics from locked OLS outputs. ",
    "RF metrics from full ", format(n_eval_train, big.mark = ","), " train / ",
    format(n_eval_test, big.mark = ","), " test effective population."
  ),
  row_count_note = row_count_note,
  models = list(
    ols_additive_final = list(
      model          = "OLS_ADDITIVE_FINAL",
      n_coefficients = 48L,
      n_aliased      = 0L,
      train          = ols_metrics$train,
      test           = ols_metrics$test
    ),
    rf_0_safe_default = list(
      model  = "RF_0_SAFE_DEFAULT",
      hyperparameters = list(
        num_trees = 500L, mtry = mtry_default, min_node_size = 5L,
        replace = TRUE, sample_fraction = 1.0
      ),
      oob_rmse     = oob_rmse_rf0,
      oob_r2       = oob_r2_rf0,
      train        = perf_rf0$train,
      test         = perf_rf0$test,
      delta_vs_ols = list(test_r2 = delta_r2_rf0, test_rmse = delta_rmse_rf0, test_mae = delta_mae_rf0)
    ),
    rf_1_safe_tuned = list(
      model  = "RF_1_SAFE_TUNED",
      hyperparameters = list(
        num_trees = 500L,
        mtry      = best_g$mtry,
        min_node_size   = best_g$min_node_size,
        replace         = (best_g$sample_fraction == 1.0),
        sample_fraction = best_g$sample_fraction
      ),
      oob_rmse     = oob_rmse_rf1,
      oob_r2       = oob_r2_rf1,
      train        = perf_rf1$train,
      test         = perf_rf1$test,
      delta_vs_ols = list(test_r2 = delta_r2_rf1, test_rmse = delta_rmse_rf1, test_mae = delta_mae_rf1)
    )
  )
)

# ---------- rf_predictions_sample.json ----------
rf_pred_sample_out <- list(
  description = paste0(
    "Prediction sample from the 2013-2014 test set for frontend/reporting visualization. ",
    nrow(pred_sample_rows), " rows sampled proportionally by year (", n_per_year, " per year). ",
    "OLS predictions from OLS_ADDITIVE_FINAL fit on the effective RF training population. ",
    "IMPORTANT: this sample is for visualization only. ",
    "All official metrics are computed on the full ", format(n_eval_test, big.mark = ","),
    "-row test set."
  ),
  n_rows      = nrow(pred_sample_rows),
  predictions = lapply(seq_len(nrow(pred_sample_rows)), function(i) {
    r <- pred_sample_rows[i, ]
    list(
      AGENCY_ID              = r$AGENCY_ID,
      STATE_ABBR             = r$STATE_ABBR,
      PROD_ABBR              = r$PROD_ABBR,
      VENDOR                 = r$VENDOR,
      STAT_PROFILE_DATE_YEAR = r$STAT_PROFILE_DATE_YEAR,
      actual_log_wp          = r$actual_log_wp,
      actual_wp              = r$actual_wp,
      ols_pred_log_wp        = r$ols_pred_log_wp,
      ols_pred_wp            = r$ols_pred_wp,
      rf_default_pred_log_wp = r$rf_default_pred_log_wp,
      rf_default_pred_wp     = r$rf_default_pred_wp,
      rf_tuned_pred_log_wp   = r$rf_tuned_pred_log_wp,
      rf_tuned_pred_wp       = r$rf_tuned_pred_wp,
      rf_tuned_residual_log  = r$rf_tuned_residual_log,
      rf_tuned_abs_error_log = r$rf_tuned_abs_error_log
    )
  })
)

# =============================================================================
# Write outputs
# =============================================================================

cat("Writing outputs...\n")
write_json_out(rf_summary_out,      "rf_summary.json")
write_json_out(rf_tuning_out,       "rf_tuning_results.json")
write_json_out(rf_fi_out,           "rf_feature_importance.json")
write_json_out(rf_comparison_out,   "rf_model_comparison.json")
write_json_out(rf_pred_sample_out,  "rf_predictions_sample.json")

cat(sprintf("\n=== RF modeling complete ===\nOutputs in: %s\n", output_dir))
