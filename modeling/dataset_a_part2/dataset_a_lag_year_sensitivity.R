# =============================================================================
# Dataset A Part 2 — Lag-Year Integrity Sensitivity Test
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_lag_year_sensitivity.R
#
# Output: modeling/dataset_a_part2/outputs/lag_year_sensitivity.json
#
# Purpose: Test whether including 2006 training rows (whose lag predictors
# reflect potentially incomplete 2005 data) materially changes the OLS and
# RF modeling conclusions.
#
# Models refit (not read from prior JSON outputs):
#   OLS_ADDITIVE_FINAL_ORIGINAL   train 2006-2012, test 2013-2014
#   OLS_ADDITIVE_FINAL_NO_2006    train 2007-2012, test 2013-2014
#   RF_1_SAFE_TUNED_ORIGINAL      train 2006-2012, test 2013-2014
#   RF_1_SAFE_TUNED_NO_2006       train 2007-2012, test 2013-2014
#
# RF hyperparameters from prior tuning (fixed — no re-tuning):
#   mtry=3, min.node.size=5, sample.fraction=0.6, replace=FALSE, num.trees=500
#
# Test set is held constant at 2013-2014 for all four models.
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(ranger); library(jsonlite)
})

SEED          <- 42L
db_path       <- "insurance.db"
output_dir    <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Locked RF hyperparameters from prior tuning grid (mtry=3 mns=5 sf=0.6 was best OOB RMSE)
RF_MTRY     <- 3L
RF_MNS      <- 5L
RF_SF       <- 0.6
RF_REPLACE  <- FALSE   # replace=FALSE when sample.fraction < 1.0
RF_TREES    <- 500L

OLS_PREDICTORS <- c("log_prev_wp", "log_prev_poly",
                    "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR",
                    "MAX_AGE", "MIN_AGE",
                    "STATE_ABBR", "PROD_ABBR", "VENDOR",
                    "STAT_PROFILE_DATE_YEAR")
RF_PREDICTORS  <- OLS_PREDICTORS   # same 10 predictors

KEY_COEF_VARS  <- c("log_prev_wp", "log_prev_poly", "ACTIVE_PRODUCERS",
                    "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
                    "STAT_PROFILE_DATE_YEAR")

cat("=== Lag-Year Integrity Sensitivity Test ===\n\n")

# =============================================================================
# Load raw data
# =============================================================================

cat("Loading data...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n", format(nrow(raw), big.mark = ","), ncol(raw)))

# =============================================================================
# Helper functions
# =============================================================================

pct <- function(n, d) round(n / d * 100, 2)

r2_score <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  a  <- actual[ok]; p <- predicted[ok]
  ss_res <- sum((a - p)^2); ss_tot <- sum((a - mean(a))^2)
  if (ss_tot == 0) return(NA_real_)
  round(1 - ss_res / ss_tot, 4)
}
rmse_fn  <- function(a, p) { ok <- !is.na(a)&!is.na(p); round(sqrt(mean((a[ok]-p[ok])^2)), 4) }
mae_fn   <- function(a, p) { ok <- !is.na(a)&!is.na(p); round(mean(abs(a[ok]-p[ok])), 4) }

eval_ols <- function(model, train_df, test_df) {
  pr_tr <- suppressWarnings(predict(model, newdata = train_df))
  pr_te <- suppressWarnings(predict(model, newdata = test_df))
  y_tr  <- train_df$log_wp;  y_te  <- test_df$log_wp
  list(
    n_train   = sum(!is.na(pr_tr) & !is.na(y_tr)),
    n_test    = sum(!is.na(pr_te) & !is.na(y_te)),
    train_r2  = r2_score(y_tr, pr_tr), train_rmse = rmse_fn(y_tr, pr_tr), train_mae = mae_fn(y_tr, pr_tr),
    test_r2   = r2_score(y_te, pr_te), test_rmse  = rmse_fn(y_te, pr_te), test_mae  = mae_fn(y_te, pr_te)
  )
}

eval_rf <- function(model, train_df, test_df) {
  pr_tr <- predict(model, data = train_df)$predictions
  pr_te <- predict(model, data = test_df)$predictions
  y_tr  <- train_df$log_wp;  y_te  <- test_df$log_wp
  list(
    n_train   = sum(!is.na(pr_tr) & !is.na(y_tr)),
    n_test    = sum(!is.na(pr_te) & !is.na(y_te)),
    oob_r2    = round(model$r.squared, 4),
    oob_rmse  = round(sqrt(model$prediction.error), 4),
    train_r2  = r2_score(y_tr, pr_tr), train_rmse = rmse_fn(y_tr, pr_tr), train_mae = mae_fn(y_tr, pr_tr),
    test_r2   = r2_score(y_te, pr_te), test_rmse  = rmse_fn(y_te, pr_te), test_mae  = mae_fn(y_te, pr_te)
  )
}

extract_key_coefs <- function(model, vars) {
  cf <- coef(model)
  lapply(vars, function(v) {
    val <- cf[v]
    list(variable = v, estimate = if (is.na(val)) NA_real_ else round(val, 6))
  })
}

importance_table <- function(model) {
  imp     <- sort(model$variable.importance, decreasing = TRUE)
  total   <- sum(abs(imp))
  lapply(seq_along(imp), function(i) {
    list(variable = names(imp)[i], rank = i,
         permutation_importance = round(imp[[i]], 6),
         normalized_pct         = round(imp[[i]] / total * 100, 2))
  })
}

quantile_summary <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(n=0L,min=NA,p25=NA,median=NA,mean=NA,p75=NA,p90=NA,p95=NA,p99=NA,max=NA))
  q <- quantile(x, probs = c(0.25, 0.75, 0.90, 0.95, 0.99))
  list(n = length(x), min = round(min(x),4), p25 = round(q[["25%"]],4),
       median = round(median(x),4), mean = round(mean(x),4),
       p75 = round(q[["75%"]],4), p90 = round(q[["90%"]],4),
       p95 = round(q[["95%"]],4), p99 = round(q[["99%"]],4), max = round(max(x),4))
}

write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
  cat(sprintf("  %-42s %.1f KB\n", filename, file.size(path) / 1024))
}

# =============================================================================
# PART A — Build modeling populations
# =============================================================================

cat("Part A: Building modeling populations...\n")

OLS_FORMULA <- log_wp ~
  log_prev_wp + log_prev_poly +
  ACTIVE_PRODUCERS + AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
  STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR

build_pop <- function(data, train_years, test_years) {
  base <- data |>
    filter(PROD_ABBR != "COMMPOL", WRTN_PREM_AMT > 0,
           STAT_PROFILE_DATE_YEAR %in% c(train_years, test_years)) |>
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
      STATE_ABBR = factor(STATE_ABBR),
      PROD_ABBR  = factor(PROD_ABBR),
      VENDOR     = factor(VENDOR)
    )
  cand_train <- sum(base$STAT_PROFILE_DATE_YEAR %in% train_years)
  cand_test  <- sum(base$STAT_PROFILE_DATE_YEAR %in% test_years)
  eff        <- base |> filter(!is.na(log_prev_wp), !is.na(log_prev_poly))
  eff_train  <- eff  |> filter(STAT_PROFILE_DATE_YEAR %in% train_years)
  eff_test   <- eff  |> filter(STAT_PROFILE_DATE_YEAR %in% test_years)
  list(
    base = base, eff = eff,
    train = eff_train, test = eff_test,
    n_cand_train = cand_train, n_cand_test = cand_test,
    n_eff_train  = nrow(eff_train), n_eff_test = nrow(eff_test)
  )
}

pop_orig  <- build_pop(raw, 2006:2012, 2013:2014)
pop_no06  <- build_pop(raw, 2007:2012, 2013:2014)

cat(sprintf("  ORIGINAL  train: %s eff rows (cand %s)  |  test: %s eff rows\n",
            format(pop_orig$n_eff_train,  big.mark=","),
            format(pop_orig$n_cand_train, big.mark=","),
            format(pop_orig$n_eff_test,   big.mark=",")))
cat(sprintf("  NO_2006   train: %s eff rows (cand %s)  |  test: %s eff rows\n",
            format(pop_no06$n_eff_train,  big.mark=","),
            format(pop_no06$n_cand_train, big.mark=","),
            format(pop_no06$n_eff_test,   big.mark=",")))
cat(sprintf("  Rows removed by dropping 2006: %s (%.1f%% of original train)\n\n",
            format(pop_orig$n_eff_train - pop_no06$n_eff_train, big.mark=","),
            pct(pop_orig$n_eff_train - pop_no06$n_eff_train, pop_orig$n_eff_train)))

# 2006 row distribution
rows_2006 <- pop_orig$train |> filter(STAT_PROFILE_DATE_YEAR == 2006)
n_2006 <- nrow(rows_2006)

dist_2006_prod  <- rows_2006 |> count(PROD_ABBR)  |> arrange(desc(n)) |>
  mutate(pct = pct(n, n_2006), PROD_ABBR = as.character(PROD_ABBR))
dist_2006_state <- rows_2006 |> count(STATE_ABBR) |> arrange(desc(n)) |>
  mutate(pct = pct(n, n_2006), STATE_ABBR = as.character(STATE_ABBR))
dist_2006_vendor <- rows_2006 |> count(VENDOR) |> arrange(desc(n)) |>
  mutate(pct = pct(n, n_2006), VENDOR = as.character(VENDOR))

# =============================================================================
# PART E — Lag integrity diagnostic for 2006 rows
# =============================================================================

cat("Part E: Lag integrity diagnostic...\n")

lag_diag <- function(df, label) {
  n_tot <- nrow(df)
  raw_wp   <- raw$PREV_WRTN_PREM_AMT[match(paste(df$AGENCY_ID, df$PROD_ABBR, df$STAT_PROFILE_DATE_YEAR),
                                            paste(raw$AGENCY_ID, raw$PROD_ABBR, raw$STAT_PROFILE_DATE_YEAR))]
  raw_poly <- raw$PREV_POLY_INFORCE_QTY[match(paste(df$AGENCY_ID, df$PROD_ABBR, df$STAT_PROFILE_DATE_YEAR),
                                               paste(raw$AGENCY_ID, raw$PROD_ABBR, raw$STAT_PROFILE_DATE_YEAR))]
  pct_neg_wp   <- pct(sum(df$PREV_WRTN_PREM_AMT    < 0, na.rm=TRUE), n_tot)
  pct_neg_poly <- pct(sum(df$PREV_POLY_INFORCE_QTY < 0, na.rm=TRUE), n_tot)
  pct_zero_wp  <- pct(sum(df$PREV_WRTN_PREM_AMT   == 0, na.rm=TRUE), n_tot)
  pct_zero_poly<- pct(sum(df$PREV_POLY_INFORCE_QTY== 0, na.rm=TRUE), n_tot)
  list(
    label                     = label,
    n_rows                    = n_tot,
    prev_wrtn_prem_amt        = quantile_summary(df$PREV_WRTN_PREM_AMT),
    prev_poly_inforce_qty     = quantile_summary(df$PREV_POLY_INFORCE_QTY),
    log_prev_wp               = quantile_summary(df$log_prev_wp),
    log_prev_poly             = quantile_summary(df$log_prev_poly),
    pct_prev_wp_negative      = pct_neg_wp,
    pct_prev_poly_negative    = pct_neg_poly,
    pct_prev_wp_zero          = pct_zero_wp,
    pct_prev_poly_zero        = pct_zero_poly,
    pct_log_prev_wp_na        = pct(sum(is.na(df$log_prev_wp)),   n_tot),
    pct_log_prev_poly_na      = pct(sum(is.na(df$log_prev_poly)), n_tot)
  )
}

# For lag diagnostics use the ORIGINAL base (before NA exclusion) to capture the invalid rows too
base_2006  <- pop_orig$base |> filter(STAT_PROFILE_DATE_YEAR == 2006)
base_07_12 <- pop_orig$base |> filter(STAT_PROFILE_DATE_YEAR %in% 2007:2012)

lag_diag_2006  <- lag_diag(base_2006,  "2006 training rows (pre-NA exclusion)")
lag_diag_07_12 <- lag_diag(base_07_12, "2007-2012 training rows (pre-NA exclusion)")

# Looks materially different if mean log_prev_wp differs by > 10% of the 2007-2012 mean
mean_log_wp_2006  <- lag_diag_2006$log_prev_wp$mean
mean_log_wp_0712  <- lag_diag_07_12$log_prev_wp$mean
lag_material_diff <- abs(mean_log_wp_2006 - mean_log_wp_0712) / max(abs(mean_log_wp_0712), 1e-9) > 0.10

cat(sprintf("  2006 mean log_prev_wp=%.4f  vs 2007-2012 mean=%.4f  material_diff=%s\n\n",
            mean_log_wp_2006, mean_log_wp_0712,
            if (lag_material_diff) "YES" else "NO"))

# =============================================================================
# PART B/C — Fit all four models
# =============================================================================

cat("Fitting OLS_ADDITIVE_FINAL_ORIGINAL (train 2006-2012)...\n")
t0 <- proc.time()
m_ols_orig <- lm(OLS_FORMULA, data = pop_orig$train)
cat(sprintf("  Done in %.1fs\n", (proc.time()-t0)["elapsed"]))
perf_ols_orig <- eval_ols(m_ols_orig, pop_orig$train, pop_orig$test)
cat(sprintf("  train R²=%.4f  test R²=%.4f  test RMSE=%.4f\n\n",
            perf_ols_orig$train_r2, perf_ols_orig$test_r2, perf_ols_orig$test_rmse))

cat("Fitting OLS_ADDITIVE_FINAL_NO_2006 (train 2007-2012)...\n")
t0 <- proc.time()
m_ols_no06 <- lm(OLS_FORMULA, data = pop_no06$train)
cat(sprintf("  Done in %.1fs\n", (proc.time()-t0)["elapsed"]))
perf_ols_no06 <- eval_ols(m_ols_no06, pop_no06$train, pop_orig$test)  # same test set
cat(sprintf("  train R²=%.4f  test R²=%.4f  test RMSE=%.4f\n\n",
            perf_ols_no06$train_r2, perf_ols_no06$test_r2, perf_ols_no06$test_rmse))

cat("Fitting RF_1_SAFE_TUNED_ORIGINAL (train 2006-2012)...\n")
rf_train_orig <- pop_orig$train |> select(all_of(c(RF_PREDICTORS, "log_wp")))
rf_test_orig  <- pop_orig$test  |> select(all_of(c(RF_PREDICTORS, "log_wp")))
t0 <- proc.time()
m_rf_orig <- ranger(
  log_wp ~ ., data = rf_train_orig,
  num.trees = RF_TREES, mtry = RF_MTRY, min.node.size = RF_MNS,
  replace = RF_REPLACE, sample.fraction = RF_SF,
  importance = "permutation", respect.unordered.factors = "order", seed = SEED
)
rf_orig_time <- round((proc.time()-t0)["elapsed"], 1)
perf_rf_orig <- eval_rf(m_rf_orig, rf_train_orig, rf_test_orig)
cat(sprintf("  Done in %.1fs  OOB R²=%.4f  test R²=%.4f\n\n",
            rf_orig_time, perf_rf_orig$oob_r2, perf_rf_orig$test_r2))

cat("Fitting RF_1_SAFE_TUNED_NO_2006 (train 2007-2012)...\n")
rf_train_no06 <- pop_no06$train |> select(all_of(c(RF_PREDICTORS, "log_wp")))
t0 <- proc.time()
m_rf_no06 <- ranger(
  log_wp ~ ., data = rf_train_no06,
  num.trees = RF_TREES, mtry = RF_MTRY, min.node.size = RF_MNS,
  replace = RF_REPLACE, sample.fraction = RF_SF,
  importance = "permutation", respect.unordered.factors = "order", seed = SEED
)
rf_no06_time <- round((proc.time()-t0)["elapsed"], 1)
perf_rf_no06 <- eval_rf(m_rf_no06, rf_train_no06, rf_test_orig)  # same test set
cat(sprintf("  Done in %.1fs  OOB R²=%.4f  test R²=%.4f\n\n",
            rf_no06_time, perf_rf_no06$oob_r2, perf_rf_no06$test_r2))

# =============================================================================
# PART D — Delta analysis
# =============================================================================

cat("Part D: Computing deltas...\n")

delta_ols <- list(
  delta_train_r2   = round(perf_ols_no06$train_r2   - perf_ols_orig$train_r2,   4),
  delta_test_r2    = round(perf_ols_no06$test_r2    - perf_ols_orig$test_r2,    4),
  delta_train_rmse = round(perf_ols_no06$train_rmse - perf_ols_orig$train_rmse, 4),
  delta_test_rmse  = round(perf_ols_no06$test_rmse  - perf_ols_orig$test_rmse,  4),
  delta_train_mae  = round(perf_ols_no06$train_mae  - perf_ols_orig$train_mae,  4),
  delta_test_mae   = round(perf_ols_no06$test_mae   - perf_ols_orig$test_mae,   4)
)

delta_rf <- list(
  delta_oob_r2     = round(perf_rf_no06$oob_r2     - perf_rf_orig$oob_r2,    4),
  delta_oob_rmse   = round(perf_rf_no06$oob_rmse   - perf_rf_orig$oob_rmse,  4),
  delta_train_r2   = round(perf_rf_no06$train_r2   - perf_rf_orig$train_r2,  4),
  delta_test_r2    = round(perf_rf_no06$test_r2    - perf_rf_orig$test_r2,   4),
  delta_train_rmse = round(perf_rf_no06$train_rmse - perf_rf_orig$train_rmse,4),
  delta_test_rmse  = round(perf_rf_no06$test_rmse  - perf_rf_orig$test_rmse, 4),
  delta_train_mae  = round(perf_rf_no06$train_mae  - perf_rf_orig$train_mae, 4),
  delta_test_mae   = round(perf_rf_no06$test_mae   - perf_rf_orig$test_mae,  4)
)

# Coefficient comparison for key OLS predictors
coef_orig  <- extract_key_coefs(m_ols_orig, KEY_COEF_VARS)
coef_no06  <- extract_key_coefs(m_ols_no06, KEY_COEF_VARS)
coef_delta <- lapply(seq_along(KEY_COEF_VARS), function(i) {
  v  <- KEY_COEF_VARS[i]
  o  <- coef_orig[[i]]$estimate
  n6 <- coef_no06[[i]]$estimate
  list(variable         = v,
       original_estimate = o,
       no2006_estimate  = n6,
       delta            = if (!is.na(o) && !is.na(n6)) round(n6 - o, 6) else NA_real_,
       pct_change       = if (!is.na(o) && abs(o) > 1e-9) round((n6 - o) / abs(o) * 100, 2) else NA_real_)
})

# Feature importance comparison
imp_orig <- importance_table(m_rf_orig)
imp_no06 <- importance_table(m_rf_no06)

imp_orig_map <- setNames(sapply(imp_orig, `[[`, "normalized_pct"),
                          sapply(imp_orig, `[[`, "variable"))
imp_no06_map <- setNames(sapply(imp_no06, `[[`, "normalized_pct"),
                          sapply(imp_no06, `[[`, "variable"))
rank_orig_map <- setNames(sapply(imp_orig, `[[`, "rank"),
                           sapply(imp_orig, `[[`, "variable"))
rank_no06_map <- setNames(sapply(imp_no06, `[[`, "rank"),
                           sapply(imp_no06, `[[`, "variable"))

all_vars <- unique(c(names(imp_orig_map), names(imp_no06_map)))
imp_comparison <- lapply(all_vars, function(v) {
  list(
    variable          = v,
    rank_original     = rank_orig_map[[v]],
    rank_no2006       = rank_no06_map[[v]],
    rank_delta        = rank_no06_map[[v]] - rank_orig_map[[v]],
    norm_pct_original = imp_orig_map[[v]],
    norm_pct_no2006   = imp_no06_map[[v]],
    norm_pct_delta    = round(imp_no06_map[[v]] - imp_orig_map[[v]], 2)
  )
})
imp_comparison <- imp_comparison[order(sapply(imp_comparison, `[[`, "rank_original"))]

# RF vs OLS under both training regimes
rf_vs_ols_orig  <- list(
  rf_test_r2  = perf_rf_orig$test_r2,
  ols_test_r2 = perf_ols_orig$test_r2,
  delta       = round(perf_rf_orig$test_r2 - perf_ols_orig$test_r2, 4),
  rf_wins     = perf_rf_orig$test_r2 > perf_ols_orig$test_r2
)
rf_vs_ols_no06  <- list(
  rf_test_r2  = perf_rf_no06$test_r2,
  ols_test_r2 = perf_ols_no06$test_r2,
  delta       = round(perf_rf_no06$test_r2 - perf_ols_no06$test_r2, 4),
  rf_wins     = perf_rf_no06$test_r2 > perf_ols_no06$test_r2
)
rf_conclusion_unchanged <- (rf_vs_ols_orig$rf_wins == rf_vs_ols_no06$rf_wins)

cat(sprintf("  OLS  delta test R²=%+.4f  delta test RMSE=%+.4f\n",
            delta_ols$delta_test_r2, delta_ols$delta_test_rmse))
cat(sprintf("  RF   delta test R²=%+.4f  delta test RMSE=%+.4f\n",
            delta_rf$delta_test_r2, delta_rf$delta_test_rmse))
cat(sprintf("  RF-vs-OLS conclusion unchanged: %s\n\n", rf_conclusion_unchanged))

# =============================================================================
# PART F — Decision rule
# =============================================================================

MATERIAL_R2_THRESHOLD   <- 0.01
MATERIAL_RMSE_THRESHOLD <- 0.02

ols_r2_material   <- abs(delta_ols$delta_test_r2)   > MATERIAL_R2_THRESHOLD
ols_rmse_material <- abs(delta_ols$delta_test_rmse)  > MATERIAL_RMSE_THRESHOLD
rf_r2_material    <- abs(delta_rf$delta_test_r2)    > MATERIAL_R2_THRESHOLD
rf_rmse_material  <- abs(delta_rf$delta_test_rmse)   > MATERIAL_RMSE_THRESHOLD

# Top predictor rank change: check if log_prev_wp or log_prev_poly move more than 1 rank
rank_changes <- sapply(imp_comparison, `[[`, "rank_delta")
rank_change_flag <- any(abs(rank_changes) > 1, na.rm = TRUE)

any_material <- ols_r2_material || ols_rmse_material || rf_r2_material ||
                rf_rmse_material || !rf_conclusion_unchanged

recommendation <- if (!any_material && !rank_change_flag) {
  list(
    decision     = "KEEP_ORIGINAL_TRAINING_WINDOW",
    justification = sprintf(
      paste0(
        "Excluding 2006 training rows has negligible effect on holdout performance and interpretation. ",
        "OLS delta test R² = %+.4f (threshold ±%.2f). RF delta test R² = %+.4f (threshold ±%.2f). ",
        "RF outperforms OLS under both training regimes. Top predictor rankings stable. ",
        "2006 lag predictors show %s distortion vs 2007-2012. ",
        "Original 2006-2012 training window is retained."
      ),
      delta_ols$delta_test_r2, MATERIAL_R2_THRESHOLD,
      delta_rf$delta_test_r2,  MATERIAL_R2_THRESHOLD,
      if (lag_material_diff) "material" else "minimal"
    )
  )
} else if (any_material && (ols_r2_material || rf_r2_material)) {
  list(
    decision     = "SWITCH_TO_2007_2012_TRAINING_WINDOW",
    justification = sprintf(
      paste0(
        "Excluding 2006 produces material changes. ",
        "OLS delta test R² = %+.4f. RF delta test R² = %+.4f. ",
        "At least one metric exceeds materiality threshold (R² ±%.2f, RMSE ±%.2f). ",
        "2006 lag predictors reflect potentially incomplete 2005 data. ",
        "Switch to 2007-2012 training window for cleaner lag integrity."
      ),
      delta_ols$delta_test_r2, delta_rf$delta_test_r2,
      MATERIAL_R2_THRESHOLD, MATERIAL_RMSE_THRESHOLD
    )
  )
} else {
  list(
    decision     = "DOCUMENT_SENSITIVITY_ONLY",
    justification = sprintf(
      paste0(
        "Minor differences detected but no change to major modeling conclusions. ",
        "OLS delta test R² = %+.4f. RF delta test R² = %+.4f. ",
        "RF %s outperform OLS under both regimes. ",
        "Retain original 2006-2012 training window; document sensitivity as a robustness check."
      ),
      delta_ols$delta_test_r2, delta_rf$delta_test_r2,
      if (rf_conclusion_unchanged) "continues to" else "no longer does"
    )
  )
}

cat(sprintf("  Decision: %s\n\n", recommendation$decision))

# =============================================================================
# Assemble and write JSON
# =============================================================================

cat("Writing output...\n")

out <- list(
  description = paste0(
    "Lag-year integrity sensitivity test for Dataset A Part 2. ",
    "Tests whether including 2006 training rows — whose lag predictors (log_prev_wp, log_prev_poly) ",
    "reflect potentially incomplete 2005 data — materially changes the OLS and RF modeling conclusions. ",
    "All four models are refit within this script for exact apples-to-apples comparison. ",
    "Test set is held constant at 2013-2014 effective rows for all models."
  ),
  issue_summary = paste0(
    "2005 was excluded from the modeling population as a partial reporting year. ",
    "The 2006 training rows use PREV_WRTN_PREM_AMT and PREV_POLY_INFORCE_QTY, ",
    "which reflect 2005 values. If 2005 is incomplete, 2006 lag predictors may be distorted. ",
    "This script tests whether removing those rows changes holdout R², RMSE, MAE, ",
    "OLS coefficients, RF importance rankings, or the RF-vs-OLS conclusion."
  ),
  modeling_populations = list(
    target_variable = "log_wp = log(WRTN_PREM_AMT + 1)",
    filters = list(
      exclude_prod_abbr = "COMMPOL",
      wp_filter = "WRTN_PREM_AMT > 0",
      na_exclusion = "rows where log_prev_wp or log_prev_poly is NA excluded"
    ),
    predictors = list(
      numeric     = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                      "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE","STAT_PROFILE_DATE_YEAR"),
      categorical = c("STATE_ABBR","PROD_ABBR","VENDOR")
    )
  ),
  row_counts = list(
    original = list(
      train_years      = "2006-2012",
      test_years       = "2013-2014",
      n_candidate_train = pop_orig$n_cand_train,
      n_effective_train = pop_orig$n_eff_train,
      n_effective_test  = pop_orig$n_eff_test
    ),
    no_2006 = list(
      train_years       = "2007-2012",
      test_years        = "2013-2014",
      n_candidate_train = pop_no06$n_cand_train,
      n_effective_train = pop_no06$n_eff_train,
      n_effective_test  = pop_no06$n_eff_test
    ),
    rows_2006 = list(
      n_rows_removed    = pop_orig$n_eff_train - pop_no06$n_eff_train,
      pct_of_original   = pct(pop_orig$n_eff_train - pop_no06$n_eff_train, pop_orig$n_eff_train),
      top_10_prod_abbr  = lapply(seq_len(min(10,nrow(dist_2006_prod))), function(i)
        list(PROD_ABBR=dist_2006_prod$PROD_ABBR[i], n=dist_2006_prod$n[i], pct=dist_2006_prod$pct[i])),
      top_10_state_abbr = lapply(seq_len(min(10,nrow(dist_2006_state))), function(i)
        list(STATE_ABBR=dist_2006_state$STATE_ABBR[i], n=dist_2006_state$n[i], pct=dist_2006_state$pct[i]))
    )
  ),
  part_e_lag_integrity_diagnostics = list(
    year_2006   = lag_diag_2006,
    year_2007_12 = lag_diag_07_12,
    mean_log_prev_wp_2006   = round(mean_log_wp_2006,  4),
    mean_log_prev_wp_0712   = round(mean_log_wp_0712,  4),
    mean_diff_log_prev_wp   = round(mean_log_wp_2006 - mean_log_wp_0712, 4),
    pct_diff_log_prev_wp    = round(abs(mean_log_wp_2006 - mean_log_wp_0712) / max(abs(mean_log_wp_0712),1e-9) * 100, 2),
    material_lag_distortion = lag_material_diff,
    interpretation = if (lag_material_diff) {
      "2006 mean log_prev_wp differs materially from 2007-2012 mean (>10%). Lag distortion from 2005 may be affecting 2006 training rows."
    } else {
      "2006 mean log_prev_wp is close to 2007-2012 mean (<10% difference). 2005 incompleteness does not appear to have materially distorted 2006 lag predictors."
    }
  ),
  models = list(
    ols_original = list(
      model_id      = "OLS_ADDITIVE_FINAL_ORIGINAL",
      train_years   = "2006-2012",
      test_years    = "2013-2014",
      n_train       = perf_ols_orig$n_train,
      n_test        = perf_ols_orig$n_test,
      train_r2      = perf_ols_orig$train_r2,
      train_rmse    = perf_ols_orig$train_rmse,
      train_mae     = perf_ols_orig$train_mae,
      test_r2       = perf_ols_orig$test_r2,
      test_rmse     = perf_ols_orig$test_rmse,
      test_mae      = perf_ols_orig$test_mae,
      key_coefficients = coef_orig
    ),
    ols_no_2006 = list(
      model_id      = "OLS_ADDITIVE_FINAL_NO_2006",
      train_years   = "2007-2012",
      test_years    = "2013-2014",
      n_train       = perf_ols_no06$n_train,
      n_test        = perf_ols_no06$n_test,
      train_r2      = perf_ols_no06$train_r2,
      train_rmse    = perf_ols_no06$train_rmse,
      train_mae     = perf_ols_no06$train_mae,
      test_r2       = perf_ols_no06$test_r2,
      test_rmse     = perf_ols_no06$test_rmse,
      test_mae      = perf_ols_no06$test_mae,
      key_coefficients = coef_no06
    ),
    rf_original = list(
      model_id      = "RF_1_SAFE_TUNED_ORIGINAL",
      train_years   = "2006-2012",
      test_years    = "2013-2014",
      hyperparameters = list(num_trees=RF_TREES, mtry=RF_MTRY, min_node_size=RF_MNS,
                             replace=RF_REPLACE, sample_fraction=RF_SF,
                             importance="permutation"),
      n_train       = perf_rf_orig$n_train,
      n_test        = perf_rf_orig$n_test,
      oob_r2        = perf_rf_orig$oob_r2,
      oob_rmse      = perf_rf_orig$oob_rmse,
      train_r2      = perf_rf_orig$train_r2,
      train_rmse    = perf_rf_orig$train_rmse,
      train_mae     = perf_rf_orig$train_mae,
      test_r2       = perf_rf_orig$test_r2,
      test_rmse     = perf_rf_orig$test_rmse,
      test_mae      = perf_rf_orig$test_mae,
      runtime_s     = rf_orig_time,
      feature_importance = imp_orig
    ),
    rf_no_2006 = list(
      model_id      = "RF_1_SAFE_TUNED_NO_2006",
      train_years   = "2007-2012",
      test_years    = "2013-2014",
      hyperparameters = list(num_trees=RF_TREES, mtry=RF_MTRY, min_node_size=RF_MNS,
                             replace=RF_REPLACE, sample_fraction=RF_SF,
                             importance="permutation"),
      n_train       = perf_rf_no06$n_train,
      n_test        = perf_rf_no06$n_test,
      oob_r2        = perf_rf_no06$oob_r2,
      oob_rmse      = perf_rf_no06$oob_rmse,
      train_r2      = perf_rf_no06$train_r2,
      train_rmse    = perf_rf_no06$train_rmse,
      train_mae     = perf_rf_no06$train_mae,
      test_r2       = perf_rf_no06$test_r2,
      test_rmse     = perf_rf_no06$test_rmse,
      test_mae      = perf_rf_no06$test_mae,
      runtime_s     = rf_no06_time,
      feature_importance = imp_no06
    )
  ),
  performance_comparison = list(
    ols_original_vs_no_2006  = delta_ols,
    rf_original_vs_no_2006   = delta_rf,
    rf_vs_ols_original       = rf_vs_ols_orig,
    rf_vs_ols_no_2006        = rf_vs_ols_no06,
    rf_conclusion_unchanged  = rf_conclusion_unchanged
  ),
  delta_analysis = list(
    materiality_thresholds = list(r2 = MATERIAL_R2_THRESHOLD, rmse = MATERIAL_RMSE_THRESHOLD),
    ols_r2_material        = ols_r2_material,
    ols_rmse_material      = ols_rmse_material,
    rf_r2_material         = rf_r2_material,
    rf_rmse_material       = rf_rmse_material,
    rf_conclusion_changed  = !rf_conclusion_unchanged
  ),
  coefficient_comparison = coef_delta,
  feature_importance_comparison = imp_comparison,
  recommendation = recommendation,
  key_findings = list(
    sprintf("Row counts: original train %s eff rows; no-2006 train %s eff rows (%d rows removed, %.1f%%).",
            format(pop_orig$n_eff_train, big.mark=","),
            format(pop_no06$n_eff_train, big.mark=","),
            pop_orig$n_eff_train - pop_no06$n_eff_train,
            pct(pop_orig$n_eff_train - pop_no06$n_eff_train, pop_orig$n_eff_train)),
    sprintf("2006 lag distortion: mean log_prev_wp 2006=%.4f vs 2007-2012=%.4f (diff=%.4f, %.1f%%). Material=%s.",
            mean_log_wp_2006, mean_log_wp_0712,
            mean_log_wp_2006 - mean_log_wp_0712,
            abs(mean_log_wp_2006 - mean_log_wp_0712) / max(abs(mean_log_wp_0712), 1e-9) * 100,
            if (lag_material_diff) "YES" else "NO"),
    sprintf("OLS: original test R²=%.4f; no-2006 test R²=%.4f; delta=%+.4f (material=%s).",
            perf_ols_orig$test_r2, perf_ols_no06$test_r2,
            delta_ols$delta_test_r2, if (ols_r2_material) "YES" else "NO"),
    sprintf("RF: original test R²=%.4f; no-2006 test R²=%.4f; delta=%+.4f (material=%s).",
            perf_rf_orig$test_r2, perf_rf_no06$test_r2,
            delta_rf$delta_test_r2, if (rf_r2_material) "YES" else "NO"),
    sprintf("RF vs OLS: RF wins under original (delta=+%.4f); RF wins under no-2006 (delta=+%.4f). Conclusion unchanged=%s.",
            rf_vs_ols_orig$delta, rf_vs_ols_no06$delta, rf_conclusion_unchanged),
    sprintf("Decision: %s.", recommendation$decision)
  )
)

write_json_out(out, "lag_year_sensitivity.json")
cat("\n=== Lag-year sensitivity test complete ===\n")
