# =============================================================================
# Dataset A Part 2 — OLS and LMM Final All-Data Refit Artifacts
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_wp_ols_lmm_final.R
#
# Purpose: Refit OLS_ADDITIVE_FINAL and AGENCY_LMM_FULL on all 2006-2014 eligible
#          rows. These are supporting all-data refit artifacts for completeness,
#          interpretability, and reference. Model selection is COMPLETE.
#          RF_FINAL_ALL_DATA remains the primary deployment/prediction artifact.
#
# Outputs: modeling/dataset_a_part2/outputs/
#   ols_lmm_final_all_data_summary.json
#   ols_additive_final_all_data_coefficients.json
#   ols_additive_final_all_data_predictions_sample.json
#   ols_additive_final_all_data.rds
#   agency_lmm_full_all_data_summary.json
#   agency_lmm_full_all_data_fixed_effects.json
#   agency_lmm_full_all_data_random_effects_summary.json
#   agency_lmm_full_all_data_variance_components.json
#   agency_lmm_full_all_data_predictions_sample.json
#   agency_lmm_full_all_data.rds
#   ols_lmm_final_all_data_metadata.json  (optional)
#
# Do NOT modify: dataset_a_wp_ols.R, dataset_a_wp_lmm.R, dataset_a_wp_rf.R,
#   dataset_a_wp_rf_final.R, dataset_a_wp_rf_agency_clusters.R,
#   dataset_a_modeling.R, server.js
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(lme4); library(jsonlite)
})

SEED       <- 42L
db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

LMM_CTRL <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

# =============================================================================
# Locked evaluated metrics from model-selection phase (carry forward unchanged)
# =============================================================================

LOCKED_OLS <- list(
  model      = "OLS_ADDITIVE_FINAL",
  train_years = "2006-2012",
  test_years  = "2013-2014",
  n_train     = 103377L,
  n_test      = 30981L,
  train_r2    = 0.7810, train_rmse = 1.0238, train_mae = 0.6468,
  test_r2     = 0.7552, test_rmse  = 1.0937, test_mae  = 0.6990,
  source      = "locked from modeling/dataset_a_part2/outputs/ols_model_comparison.json"
)

LOCKED_LMM <- list(
  model      = "AGENCY_LMM_FULL",
  train_years = "2006-2012",
  test_years  = "2013-2014",
  n_train     = 103377L,
  n_test      = 30981L,
  conditional = list(test_r2 = 0.7607, test_rmse = 1.0814, test_mae = 0.7071),
  marginal    = list(test_r2 = 0.7345, test_rmse = 1.1389, test_mae = 0.7768),
  variance_components = list(
    agency_variance   = 0.303105,
    residual_variance = 0.948602,
    total_variance    = 1.251707,
    agency_icc        = 0.2422
  ),
  source = "locked from modeling/dataset_a_part2/outputs/lmm_variance_components.json"
)

LOCKED_RF <- list(
  model     = "RF_1_SAFE_TUNED",
  test_years = "2013-2014",
  test_r2   = 0.8839, test_rmse = 0.7532, test_mae = 0.4428,
  source    = "locked from modeling/dataset_a_part2/outputs/rf_summary.json"
)

cat("=== Dataset A Part 2 — OLS and LMM Final All-Data Refit ===\n\n")
cat("Locked OLS test R²   : 0.7552  (OLS_ADDITIVE_FINAL, 2013-2014)\n")
cat("Locked LMM cond test R²: 0.7607  (AGENCY_LMM_FULL, 2013-2014)\n")
cat("Locked RF test R²    : 0.8839  (RF_1_SAFE_TUNED, 2013-2014)\n\n")

# =============================================================================
# Helpers
# =============================================================================

r2_score <- function(a, p) {
  ok <- !is.na(a) & !is.na(p)
  ss_res <- sum((a[ok] - p[ok])^2); ss_tot <- sum((a[ok] - mean(a[ok]))^2)
  if (ss_tot == 0) return(NA_real_)
  round(1 - ss_res / ss_tot, 4)
}
rmse_fn <- function(a, p) { ok <- !is.na(a) & !is.na(p); round(sqrt(mean((a[ok]-p[ok])^2)), 4) }
mae_fn  <- function(a, p) { ok <- !is.na(a) & !is.na(p); round(mean(abs(a[ok]-p[ok])),     4) }

write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
  cat(sprintf("  %-60s %.1f KB\n", filename, file.size(path) / 1024))
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
# Build 2006-2014 all-data population
# =============================================================================

cat("Building 2006-2014 all-data population...\n")

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
    VENDOR     = factor(VENDOR,     levels = alldata_vendor_levels),
    AGENCY_ID  = factor(AGENCY_ID)
  )

n_alldata  <- nrow(wp_alldata)
n_expected <- 134358L
n_agencies <- n_distinct(wp_alldata$AGENCY_ID)

# Type-safe year check
obs_years <- sort(unique(as.integer(wp_alldata$STAT_PROFILE_DATE_YEAR)))
exp_years <- 2006:2014
year_check_pass <- setequal(obs_years, exp_years) && length(obs_years) == length(exp_years)

cat(sprintf("  Expected rows     : %s\n", format(n_expected, big.mark = ",")))
cat(sprintf("  Actual rows       : %s\n", format(n_alldata,  big.mark = ",")))
cat(sprintf("  Row count check   : %s\n",
            if (n_alldata == n_expected) "PASS" else sprintf("DIFF (%+d)", n_alldata - n_expected)))
cat(sprintf("  Training years    : %s\n", paste(obs_years, collapse = ", ")))
cat(sprintf("  Year check        : %s\n", if (year_check_pass) "PASS" else "FAIL"))
cat(sprintf("  Agencies          : %s\n", format(n_agencies, big.mark = ",")))
cat(sprintf("  STATE_ABBR levels : %d\n", length(alldata_state_levels)))
cat(sprintf("  PROD_ABBR levels  : %d\n", length(alldata_prod_levels)))
cat(sprintf("  VENDOR levels     : %d\n\n", length(alldata_vendor_levels)))

validation <- list(
  row_count = list(
    expected = n_expected,
    actual   = n_alldata,
    pass     = (n_alldata == n_expected),
    note     = if (n_alldata == n_expected) "PASS" else
                 sprintf("DIFF: actual=%d expected=%d delta=%+d",
                         n_alldata, n_expected, n_alldata - n_expected)
  ),
  year_check = list(
    training_years = as.list(obs_years),
    excluded_2005  = !(2005L %in% obs_years),
    excluded_2015  = !(2015L %in% obs_years),
    pass           = year_check_pass,
    method         = "setequal(sort(unique(as.integer(...))), 2006:2014)"
  ),
  predictor_check = list(
    ols_predictors    = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                          "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE",
                          "STATE_ABBR","PROD_ABBR","VENDOR","STAT_PROFILE_DATE_YEAR"),
    lmm_fixed_effects = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                          "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE",
                          "STATE_ABBR","PROD_ABBR","VENDOR","STAT_PROFILE_DATE_YEAR"),
    lmm_random_effect = "(1 | AGENCY_ID)",
    agency_cluster_excluded     = TRUE,
    agency_id_as_fixed_excluded = TRUE,
    primary_agency_id_excluded  = TRUE,
    leakage_vars_excluded       = TRUE
  )
)

# =============================================================================
# Formulas
# =============================================================================

F_OLS <- log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS +
                  AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                  STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR

F_LMM <- log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS +
                  AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                  STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR +
                  (1 | AGENCY_ID)

# =============================================================================
# OLS_ADDITIVE_FINAL_ALL_DATA
# =============================================================================

cat("Fitting OLS_ADDITIVE_FINAL_ALL_DATA on 2006-2014 all eligible data...\n")
t0 <- proc.time()
ols_final <- lm(F_OLS, data = wp_alldata)
ols_time  <- round((proc.time() - t0)["elapsed"], 1)

ols_train_preds <- predict(ols_final, newdata = wp_alldata)
ols_train_r2    <- r2_score(wp_alldata$log_wp, ols_train_preds)
ols_train_rmse  <- rmse_fn(wp_alldata$log_wp, ols_train_preds)
ols_train_mae   <- mae_fn(wp_alldata$log_wp,  ols_train_preds)

cat(sprintf("  Done in %.1fs\n", ols_time))
cat(sprintf("  Within-sample R²: %.4f  RMSE: %.4f  MAE: %.4f  (NOT holdout)\n\n",
            ols_train_r2, ols_train_rmse, ols_train_mae))

# OLS coefficient table
ols_coef_raw <- summary(ols_final)$coefficients
ols_coef_list <- lapply(seq_len(nrow(ols_coef_raw)), function(i) {
  list(
    term     = rownames(ols_coef_raw)[i],
    estimate = round(ols_coef_raw[i, "Estimate"],   6),
    std_err  = round(ols_coef_raw[i, "Std. Error"], 6),
    t_stat   = round(ols_coef_raw[i, "t value"],    4),
    p_value  = round(ols_coef_raw[i, "Pr(>|t|)"],   6),
    sig      = if (ols_coef_raw[i, "Pr(>|t|)"] < 0.001) "***"
               else if (ols_coef_raw[i, "Pr(>|t|)"] < 0.01) "**"
               else if (ols_coef_raw[i, "Pr(>|t|)"] < 0.05) "*"
               else if (ols_coef_raw[i, "Pr(>|t|)"] < 0.10) "."
               else ""
  )
})

# OLS prediction sample
set.seed(SEED)
n_years_all <- length(unique(wp_alldata$STAT_PROFILE_DATE_YEAR))
n_per_yr    <- ceiling(2000L / n_years_all)
ols_samp <- wp_alldata |>
  group_by(STAT_PROFILE_DATE_YEAR) |>
  slice_sample(n = n_per_yr) |>
  ungroup() |>
  slice_head(n = 2000L)

ols_samp_preds <- predict(ols_final, newdata = ols_samp)

ols_samp_out <- lapply(seq_len(nrow(ols_samp)), function(i) {
  r <- ols_samp[i, ]
  p <- ols_samp_preds[i]
  list(
    AGENCY_ID              = r$AGENCY_ID,
    STAT_PROFILE_DATE_YEAR = r$STAT_PROFILE_DATE_YEAR,
    STATE_ABBR             = as.character(r$STATE_ABBR),
    PROD_ABBR              = as.character(r$PROD_ABBR),
    VENDOR                 = as.character(r$VENDOR),
    actual_log_wp          = round(r$log_wp, 6),
    actual_wp              = round(exp(r$log_wp) - 1, 2),
    pred_log_wp            = round(p, 6),
    pred_wp                = round(pmax(exp(p) - 1, 0), 2),
    residual_log           = round(r$log_wp - p, 6),
    abs_error_log          = round(abs(r$log_wp - p), 6)
  )
})

# OLS RDS
ols_rds_obj <- list(
  model_name        = "OLS_ADDITIVE_FINAL_ALL_DATA",
  description       = paste0(
    "Refit of OLS_ADDITIVE_FINAL on all 2006-2014 eligible rows (n=",
    format(n_alldata, big.mark=","), "). ",
    "Supporting reference artifact. ",
    "Holdout evidence: OLS_ADDITIVE_FINAL test R2=0.7552, RMSE=1.0937, MAE=0.6990 (2013-2014). ",
    "RF_FINAL_ALL_DATA remains the primary deployment artifact."
  ),
  lm_model          = ols_final,
  formula           = deparse(F_OLS),
  predictor_names   = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                        "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE",
                        "STATE_ABBR","PROD_ABBR","VENDOR","STAT_PROFILE_DATE_YEAR"),
  factor_levels     = list(
    STATE_ABBR = alldata_state_levels,
    PROD_ABBR  = alldata_prod_levels,
    VENDOR     = alldata_vendor_levels
  ),
  target            = "log_wp",
  target_definition = "log(WRTN_PREM_AMT + 1)",
  inverse_transform = "pred_wp = exp(pred_log_wp) - 1",
  training_filters  = list(
    "STAT_PROFILE_DATE_YEAR in 2006:2014", "PROD_ABBR != 'COMMPOL'",
    "WRTN_PREM_AMT > 0", "!is.na(log_prev_wp)", "!is.na(log_prev_poly)"
  ),
  n_training_rows   = n_alldata,
  n_coefficients    = length(coef(ols_final)),
  locked_holdout_metrics = LOCKED_OLS
)

rds_ols_path <- file.path(output_dir, "ols_additive_final_all_data.rds")
saveRDS(ols_rds_obj, rds_ols_path)
cat(sprintf("  ols_additive_final_all_data.rds — %.1f KB\n\n",
            file.size(rds_ols_path) / 1024))

# =============================================================================
# AGENCY_LMM_FULL_ALL_DATA
# =============================================================================

cat("Fitting AGENCY_LMM_FULL_ALL_DATA on 2006-2014 all eligible data...\n")
cat("  REML=TRUE  optimizer=bobyqa  maxfun=200000\n")

lmm_warns <- character(0)
t0 <- proc.time()
lmm_final <- withCallingHandlers(
  lmer(F_LMM, data = wp_alldata, REML = TRUE, control = LMM_CTRL),
  warning = function(w) {
    lmm_warns <<- c(lmm_warns, conditionMessage(w))
    invokeRestart("muffleWarning")
  }
)
lmm_time     <- round((proc.time() - t0)["elapsed"], 1)
lmm_singular <- isSingular(lmm_final)

cat(sprintf("  Done in %.1fs  singular=%s  warnings=%d\n",
            lmm_time, lmm_singular, length(lmm_warns)))
if (length(lmm_warns) > 0) cat(sprintf("  Warning: %s\n", lmm_warns[1]))

# Within-sample predictions
lmm_train_cond <- predict(lmm_final, newdata = wp_alldata, re.form = NULL,
                          allow.new.levels = TRUE)
lmm_train_marg <- predict(lmm_final, newdata = wp_alldata, re.form = NA,
                          allow.new.levels = TRUE)

lmm_train_cond_r2   <- r2_score(wp_alldata$log_wp, lmm_train_cond)
lmm_train_cond_rmse <- rmse_fn(wp_alldata$log_wp, lmm_train_cond)
lmm_train_cond_mae  <- mae_fn(wp_alldata$log_wp,  lmm_train_cond)
lmm_train_marg_r2   <- r2_score(wp_alldata$log_wp, lmm_train_marg)
lmm_train_marg_rmse <- rmse_fn(wp_alldata$log_wp, lmm_train_marg)
lmm_train_marg_mae  <- mae_fn(wp_alldata$log_wp,  lmm_train_marg)

cat(sprintf("  Within-sample cond R²: %.4f  RMSE: %.4f  MAE: %.4f  (NOT holdout)\n",
            lmm_train_cond_r2, lmm_train_cond_rmse, lmm_train_cond_mae))
cat(sprintf("  Within-sample marg R²: %.4f  RMSE: %.4f  MAE: %.4f\n\n",
            lmm_train_marg_r2, lmm_train_marg_rmse, lmm_train_marg_mae))

# Variance components
vc_df    <- as.data.frame(VarCorr(lmm_final))
res_var  <- vc_df[vc_df$grp == "Residual", "vcov"]
ag_var   <- vc_df[vc_df$grp == "AGENCY_ID", "vcov"]
tot_var  <- ag_var + res_var
ag_icc   <- round(ag_var / tot_var, 4)

cat(sprintf("  Agency variance : %.6f\n", ag_var))
cat(sprintf("  Residual variance: %.6f\n", res_var))
cat(sprintf("  Total variance  : %.6f\n", tot_var))
cat(sprintf("  Agency ICC      : %.4f  (vs locked 0.2422 from 2006-2012 only)\n\n", ag_icc))

# Fixed effects table
fe_raw   <- summary(lmm_final)$coefficients
fe_list  <- lapply(seq_len(nrow(fe_raw)), function(i) {
  out <- list(
    term     = rownames(fe_raw)[i],
    estimate = round(fe_raw[i, "Estimate"],   6),
    std_err  = round(fe_raw[i, "Std. Error"], 6),
    t_value  = round(fe_raw[i, "t value"],    4)
  )
  if ("Pr(>|t|)" %in% colnames(fe_raw)) {
    out$p_value <- round(fe_raw[i, "Pr(>|t|)"], 6)
  } else {
    out$p_value_note <- "lme4 does not report p-values for lmer by default. Use lmerTest or Satterthwaite approximation if needed."
  }
  out
})

# Agency random effects summary
re_ag     <- ranef(lmm_final)$AGENCY_ID[[1]]   # vector of agency random intercepts
n_re_ag   <- length(re_ag)
re_sorted <- sort(re_ag)

quantile_safe <- function(x, p) round(as.numeric(quantile(x, p, na.rm = TRUE)), 6)

re_summary <- list(
  n_agencies = n_re_ag,
  min        = quantile_safe(re_ag, 0.00),
  p5         = quantile_safe(re_ag, 0.05),
  p25        = quantile_safe(re_ag, 0.25),
  median     = quantile_safe(re_ag, 0.50),
  mean       = round(mean(re_ag, na.rm = TRUE), 6),
  p75        = quantile_safe(re_ag, 0.75),
  p95        = quantile_safe(re_ag, 0.95),
  max        = quantile_safe(re_ag, 1.00),
  std_dev    = round(sd(re_ag, na.rm = TRUE), 6),
  note       = paste0(
    "Fitted agency random intercepts from AGENCY_LMM_FULL_ALL_DATA (all 2006-2014 data). ",
    "These are EBLUPs (Empirical Best Linear Unbiased Predictors). ",
    "Do not interpret as final performance rankings — they reflect fitted training-period effects."
  )
)

# Top/bottom 20 agencies by random effect
re_df     <- data.frame(
  AGENCY_ID  = rownames(ranef(lmm_final)$AGENCY_ID),
  re_value   = re_ag,
  stringsAsFactors = FALSE
)
re_df_srt <- re_df[order(re_df$re_value), ]

top_bottom_n <- min(20L, floor(n_re_ag / 2))
re_bottom20  <- lapply(seq_len(top_bottom_n), function(i) {
  list(AGENCY_ID = re_df_srt$AGENCY_ID[i],
       random_intercept = round(re_df_srt$re_value[i], 6))
})
re_top20 <- lapply(seq(nrow(re_df_srt) - top_bottom_n + 1, nrow(re_df_srt)), function(i) {
  list(AGENCY_ID = re_df_srt$AGENCY_ID[i],
       random_intercept = round(re_df_srt$re_value[i], 6))
})

re_summary$bottom_agencies_by_re <- re_bottom20
re_summary$top_agencies_by_re    <- rev(re_top20)

# LMM prediction sample (same 2000 rows as OLS sample for easy comparison)
set.seed(SEED)
lmm_samp <- wp_alldata |>
  group_by(STAT_PROFILE_DATE_YEAR) |>
  slice_sample(n = n_per_yr) |>
  ungroup() |>
  slice_head(n = 2000L)

lmm_samp_cond <- predict(lmm_final, newdata = lmm_samp, re.form = NULL,
                         allow.new.levels = TRUE)
lmm_samp_marg <- predict(lmm_final, newdata = lmm_samp, re.form = NA,
                         allow.new.levels = TRUE)

lmm_samp_out <- lapply(seq_len(nrow(lmm_samp)), function(i) {
  r <- lmm_samp[i, ]
  pc <- lmm_samp_cond[i]; pm <- lmm_samp_marg[i]
  list(
    AGENCY_ID                  = as.character(r$AGENCY_ID),
    STAT_PROFILE_DATE_YEAR     = r$STAT_PROFILE_DATE_YEAR,
    STATE_ABBR                 = as.character(r$STATE_ABBR),
    PROD_ABBR                  = as.character(r$PROD_ABBR),
    VENDOR                     = as.character(r$VENDOR),
    actual_log_wp              = round(r$log_wp, 6),
    actual_wp                  = round(exp(r$log_wp) - 1, 2),
    pred_log_wp_conditional    = round(pc, 6),
    pred_wp_conditional        = round(pmax(exp(pc) - 1, 0), 2),
    pred_log_wp_marginal       = round(pm, 6),
    pred_wp_marginal           = round(pmax(exp(pm) - 1, 0), 2),
    residual_log_conditional   = round(r$log_wp - pc, 6),
    residual_log_marginal      = round(r$log_wp - pm, 6)
  )
})

# Check prediction sanity
n_finite_cond <- sum(is.finite(lmm_samp_cond))
n_finite_marg <- sum(is.finite(lmm_samp_marg))
n_neg_cond    <- sum(pmax(exp(lmm_samp_cond) - 1, 0) < 0)
n_neg_marg    <- sum(pmax(exp(lmm_samp_marg) - 1, 0) < 0)

validation$prediction_checks <- list(
  ols_sample_n           = nrow(ols_samp),
  ols_finite_pred_log_wp = sum(is.finite(ols_samp_preds)),
  ols_pred_wp_nonneg     = sum(pmax(exp(ols_samp_preds) - 1, 0) < 0) == 0,
  lmm_sample_n           = nrow(lmm_samp),
  lmm_cond_finite        = n_finite_cond,
  lmm_marg_finite        = n_finite_marg,
  lmm_cond_pred_wp_nonneg = (n_neg_cond == 0),
  lmm_marg_pred_wp_nonneg = (n_neg_marg == 0)
)
validation$model_objects <- list(
  ols_rds_saved = file.exists(rds_ols_path),
  lmm_rds_saved = TRUE   # set to TRUE after saving below
)

# LMM RDS
lmm_rds_obj <- list(
  model_name             = "AGENCY_LMM_FULL_ALL_DATA",
  description            = paste0(
    "Refit of AGENCY_LMM_FULL on all 2006-2014 eligible rows (n=",
    format(n_alldata, big.mark=","), "). ",
    "Supporting reference artifact. ",
    "RF_FINAL_ALL_DATA remains the primary deployment artifact."
  ),
  lmer_model             = lmm_final,
  formula                = deparse(F_LMM),
  fixed_effect_predictors = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                               "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE",
                               "STATE_ABBR","PROD_ABBR","VENDOR","STAT_PROFILE_DATE_YEAR"),
  random_effect_grouping = "AGENCY_ID",
  factor_levels          = list(
    STATE_ABBR = alldata_state_levels,
    PROD_ABBR  = alldata_prod_levels,
    VENDOR     = alldata_vendor_levels
  ),
  target                 = "log_wp",
  target_definition      = "log(WRTN_PREM_AMT + 1)",
  inverse_transform      = "pred_wp = exp(pred_log_wp) - 1",
  training_filters       = list(
    "STAT_PROFILE_DATE_YEAR in 2006:2014", "PROD_ABBR != 'COMMPOL'",
    "WRTN_PREM_AMT > 0", "!is.na(log_prev_wp)", "!is.na(log_prev_poly)"
  ),
  n_training_rows        = n_alldata,
  n_agencies             = n_re_ag,
  singular               = lmm_singular,
  convergence_warnings   = lmm_warns,
  variance_components    = list(
    agency_variance   = round(ag_var, 6),
    residual_variance = round(res_var, 6),
    total_variance    = round(tot_var, 6),
    agency_icc        = ag_icc
  ),
  locked_holdout_metrics = LOCKED_LMM
)

rds_lmm_path <- file.path(output_dir, "agency_lmm_full_all_data.rds")
saveRDS(lmm_rds_obj, rds_lmm_path)
cat(sprintf("  agency_lmm_full_all_data.rds — %.1f KB\n\n",
            file.size(rds_lmm_path) / 1024))
validation$model_objects$lmm_rds_saved <- file.exists(rds_lmm_path)

# =============================================================================
# Assemble and write JSON outputs
# =============================================================================

cat("Writing JSON outputs...\n")

# ---- 1. ols_lmm_final_all_data_summary.json ----
summary_out <- list(
  purpose = paste0(
    "Final OLS and LMM all-data refit artifacts for Dataset A Part 2. ",
    "Model selection is complete. ",
    "OLS_ADDITIVE_FINAL_ALL_DATA and AGENCY_LMM_FULL_ALL_DATA are supporting reference artifacts. ",
    "RF_FINAL_ALL_DATA remains the primary deployment/prediction artifact."
  ),
  models_fit         = list("OLS_ADDITIVE_FINAL_ALL_DATA", "AGENCY_LMM_FULL_ALL_DATA"),
  training_years     = "2006-2014",
  excluded_years     = list(2005L, 2015L),
  n_training_rows    = n_alldata,
  n_agencies         = n_re_ag,
  row_count_check    = validation$row_count,
  year_check         = validation$year_check,
  predictor_check    = validation$predictor_check,
  ols_all_data = list(
    model_name      = "OLS_ADDITIVE_FINAL_ALL_DATA",
    formula         = deparse(F_OLS),
    n_coefficients  = length(coef(ols_final)),
    runtime_s       = ols_time,
    within_sample_diagnostics = list(
      note = "WITHIN-SAMPLE FIT. NOT holdout evaluation. All 2006-2014 data used for training.",
      r2   = ols_train_r2,
      rmse = ols_train_rmse,
      mae  = ols_train_mae,
      n    = n_alldata
    ),
    locked_holdout_metrics = LOCKED_OLS
  ),
  lmm_all_data = list(
    model_name      = "AGENCY_LMM_FULL_ALL_DATA",
    formula         = deparse(F_LMM),
    optimizer       = "bobyqa",
    reml            = TRUE,
    singular        = lmm_singular,
    convergence_warnings = lmm_warns,
    runtime_s       = lmm_time,
    n_agencies_fitted = n_re_ag,
    within_sample_diagnostics = list(
      note = "WITHIN-SAMPLE FIT. NOT holdout evaluation. Conditional predictions use training EBLUPs.",
      conditional = list(r2=lmm_train_cond_r2, rmse=lmm_train_cond_rmse, mae=lmm_train_cond_mae, n=n_alldata),
      marginal    = list(r2=lmm_train_marg_r2, rmse=lmm_train_marg_rmse, mae=lmm_train_marg_mae, n=n_alldata)
    ),
    variance_components = list(
      agency_variance   = round(ag_var, 6),
      residual_variance = round(res_var, 6),
      total_variance    = round(tot_var, 6),
      agency_icc        = ag_icc,
      comparison_to_locked = list(
        locked_agency_variance   = LOCKED_LMM$variance_components$agency_variance,
        locked_residual_variance = LOCKED_LMM$variance_components$residual_variance,
        locked_agency_icc        = LOCKED_LMM$variance_components$agency_icc,
        note = "Locked values from AGENCY_LMM_FULL fit on 2006-2012 training data only."
      )
    ),
    locked_holdout_metrics = LOCKED_LMM
  ),
  locked_rf_reference   = LOCKED_RF,
  prediction_checks     = validation$prediction_checks,
  model_objects         = validation$model_objects,
  inverse_transform_note = paste0(
    "Predictions generated on log scale. Dollar values use direct inverse transform: exp(pred_log_wp) - 1. ",
    "No lognormal bias correction or smearing correction applied. ",
    "Official evaluation remains on the log target scale."
  ),
  final_interpretation = list(
    ols   = "OLS_ADDITIVE_FINAL_ALL_DATA is the all-data refit of the final evaluated OLS benchmark.",
    lmm   = "AGENCY_LMM_FULL_ALL_DATA is the all-data refit of the final evaluated agency LMM benchmark.",
    use   = "These artifacts are for completeness, interpretability, and reference. They do not replace the locked holdout metrics from model selection.",
    rf    = "RF_FINAL_ALL_DATA remains the primary final prediction artifact because RF_1_SAFE_TUNED was the best evaluated predictive model.",
    holdout_note = "Holdout evidence from 2013-2014 is the official evaluation for all three specifications."
  )
)
write_json_out(summary_out, "ols_lmm_final_all_data_summary.json")

# ---- 2. ols_additive_final_all_data_coefficients.json ----
ols_coef_out <- list(
  description = paste0(
    "OLS coefficients from OLS_ADDITIVE_FINAL_ALL_DATA (all 2006-2014 data, n=",
    format(n_alldata, big.mark=","), "). ",
    "These reflect the all-data refit and are NOT the source of holdout evaluation metrics. ",
    "Holdout evidence: OLS_ADDITIVE_FINAL test R2=0.7552, RMSE=1.0937 (2013-2014)."
  ),
  model_name       = "OLS_ADDITIVE_FINAL_ALL_DATA",
  n_training_rows  = n_alldata,
  n_coefficients   = length(ols_coef_list),
  all_data_refit_note = paste0(
    "Coefficients from the all-data refit. The evaluated OLS specification is OLS_ADDITIVE_FINAL ",
    "(fit on 2006-2012 only). Coefficient magnitudes and significance may differ slightly ",
    "between the 2006-2012 fit and this 2006-2014 refit."
  ),
  locked_holdout_metrics = LOCKED_OLS,
  coefficients     = ols_coef_list
)
write_json_out(ols_coef_out, "ols_additive_final_all_data_coefficients.json")

# ---- 3. ols_additive_final_all_data_predictions_sample.json ----
ols_samp_json <- list(
  description = paste0(
    "TRAINING-POPULATION DIAGNOSTIC SAMPLE. NOT HOLDOUT EVALUATION. ",
    length(ols_samp_out), " rows sampled proportionally from 2006-2014 training population. ",
    "Official holdout: OLS_ADDITIVE_FINAL test R2=0.7552, RMSE=1.0937, MAE=0.6990 (2013-2014). ",
    "pred_wp = exp(pred_log_wp) - 1 (no bias correction)."
  ),
  model_name  = "OLS_ADDITIVE_FINAL_ALL_DATA",
  n_rows      = length(ols_samp_out),
  sample_note = "Training-population diagnostic sample. Not holdout evaluation.",
  predictions = ols_samp_out
)
write_json_out(ols_samp_json, "ols_additive_final_all_data_predictions_sample.json")

# ---- 4. agency_lmm_full_all_data_summary.json ----
lmm_summary_out <- list(
  model_name   = "AGENCY_LMM_FULL_ALL_DATA",
  purpose      = paste0(
    "All-data refit of AGENCY_LMM_FULL on 2006-2014 eligible rows. ",
    "Supporting reference artifact for agency-credibility interpretability. ",
    "RF_FINAL_ALL_DATA remains the primary deployment artifact."
  ),
  formula      = deparse(F_LMM),
  training_years  = "2006-2014",
  excluded_years  = list(2005L, 2015L),
  row_filters     = list("STAT_PROFILE_DATE_YEAR in 2006:2014", "PROD_ABBR != 'COMMPOL'",
                         "WRTN_PREM_AMT > 0", "!is.na(log_prev_wp)", "!is.na(log_prev_poly)"),
  n_training_rows = n_alldata,
  n_agencies      = n_re_ag,
  optimizer       = "bobyqa",
  reml            = TRUE,
  convergence_status = list(
    singular            = lmm_singular,
    n_warnings          = length(lmm_warns),
    warnings            = if (length(lmm_warns) > 0) lmm_warns else list(),
    converged           = (length(lmm_warns) == 0 && !lmm_singular)
  ),
  variance_components = list(
    agency_variance   = round(ag_var, 6),
    residual_variance = round(res_var, 6),
    total_variance    = round(tot_var, 6),
    agency_icc        = ag_icc
  ),
  within_sample_diagnostics = list(
    note        = "WITHIN-SAMPLE FIT. NOT holdout evaluation.",
    conditional = list(r2=lmm_train_cond_r2, rmse=lmm_train_cond_rmse, mae=lmm_train_cond_mae),
    marginal    = list(r2=lmm_train_marg_r2, rmse=lmm_train_marg_rmse, mae=lmm_train_marg_mae)
  ),
  locked_evaluated_conditional = LOCKED_LMM$conditional,
  locked_evaluated_marginal    = LOCKED_LMM$marginal,
  locked_rf_comparison         = list(
    model    = LOCKED_RF$model,
    test_r2  = LOCKED_RF$test_r2,
    test_rmse = LOCKED_RF$test_rmse,
    test_mae  = LOCKED_RF$test_mae,
    note = "RF_1_SAFE_TUNED is the leading evaluated predictive model. RF_FINAL_ALL_DATA is the primary deployment artifact."
  ),
  all_data_refit_note = paste0(
    "This all-data refit has no untouched test set. ",
    "OOB/within-sample diagnostics do not replace the locked holdout evidence. ",
    "The evaluated specification remains AGENCY_LMM_FULL (fit on 2006-2012 only)."
  )
)
write_json_out(lmm_summary_out, "agency_lmm_full_all_data_summary.json")

# ---- 5. agency_lmm_full_all_data_fixed_effects.json ----
lmm_fe_out <- list(
  description = paste0(
    "Fixed effect estimates from AGENCY_LMM_FULL_ALL_DATA (all 2006-2014 data, n=",
    format(n_alldata, big.mark=","), "). ",
    "All-data refit — not the source of holdout evaluation metrics. ",
    "lme4 does not provide p-values for lmer by default (no denominator df for F-tests). ",
    "t-values > 2 are generally considered significant in large-n contexts."
  ),
  model_name      = "AGENCY_LMM_FULL_ALL_DATA",
  n_training_rows = n_alldata,
  all_data_refit_note = paste0(
    "Fixed effects from the all-data refit. The evaluated LMM is AGENCY_LMM_FULL ",
    "(fit on 2006-2012 only)."
  ),
  p_value_note = paste0(
    "lme4::lmer does not report p-values. t-values shown. ",
    "For Satterthwaite or Kenward-Roger approximate p-values, use the lmerTest package."
  ),
  fixed_effects = fe_list
)
write_json_out(lmm_fe_out, "agency_lmm_full_all_data_fixed_effects.json")

# ---- 6. agency_lmm_full_all_data_random_effects_summary.json ----
lmm_re_out <- list(
  description = paste0(
    "Agency random intercept distribution from AGENCY_LMM_FULL_ALL_DATA. ",
    "EBLUPs (Empirical Best Linear Unbiased Predictors) for ", n_re_ag, " agencies. ",
    "Training-period fitted effects — do not interpret as final performance rankings. ",
    "Positive values indicate agencies with above-average WP after adjusting for fixed effects."
  ),
  model_name           = "AGENCY_LMM_FULL_ALL_DATA",
  grouping_variable    = "AGENCY_ID",
  n_agencies           = n_re_ag,
  distribution_summary = re_summary,
  agency_variance      = round(ag_var, 6),
  agency_std_dev       = round(sqrt(ag_var), 6),
  note = paste0(
    "These are fitted training-period effects and should not be used as out-of-sample agency rankings. ",
    "Agencies with more observations contribute more information to their EBLUP estimate."
  )
)
write_json_out(lmm_re_out, "agency_lmm_full_all_data_random_effects_summary.json")

# ---- 7. agency_lmm_full_all_data_variance_components.json ----
lmm_vc_out <- list(
  description = paste0(
    "Variance components from AGENCY_LMM_FULL_ALL_DATA (all 2006-2014 data). ",
    "Agency ICC reflects residual agency clustering after adjusting for all fixed effects. ",
    "Comparison to locked AGENCY_LMM_FULL (2006-2012 only) provided for reference."
  ),
  model_name = "AGENCY_LMM_FULL_ALL_DATA",
  all_data_refit_variance_components = list(
    agency_variance      = round(ag_var, 6),
    agency_std_dev       = round(sqrt(ag_var), 6),
    agency_variance_pct  = round(ag_var / tot_var * 100, 2),
    residual_variance    = round(res_var, 6),
    residual_std_dev     = round(sqrt(res_var), 6),
    residual_variance_pct = round(res_var / tot_var * 100, 2),
    total_variance       = round(tot_var, 6),
    agency_icc           = ag_icc,
    singular             = lmm_singular
  ),
  locked_agency_lmm_full_variance_components = list(
    note              = "From AGENCY_LMM_FULL fit on 2006-2012 training data only.",
    agency_variance   = LOCKED_LMM$variance_components$agency_variance,
    residual_variance = LOCKED_LMM$variance_components$residual_variance,
    total_variance    = LOCKED_LMM$variance_components$total_variance,
    agency_icc        = LOCKED_LMM$variance_components$agency_icc
  ),
  interpretation = paste0(
    "Agency ICC = ", ag_icc, " in the all-data refit. ",
    "This indicates that agency identity accounts for approximately ",
    round(ag_icc * 100, 1),
    "% of total variance in log_wp after adjusting for all fixed effects. ",
    "Comparison locked value (2006-2012 only): ICC=0.2422. ",
    "Differences reflect training-period composition and should not be over-interpreted."
  )
)
write_json_out(lmm_vc_out, "agency_lmm_full_all_data_variance_components.json")

# ---- 8. agency_lmm_full_all_data_predictions_sample.json ----
lmm_samp_json <- list(
  description = paste0(
    "TRAINING-POPULATION DIAGNOSTIC SAMPLE. NOT HOLDOUT EVALUATION. ",
    length(lmm_samp_out), " rows sampled proportionally from 2006-2014 training population. ",
    "Conditional predictions use EBLUPs from training — low residuals reflect within-sample fit. ",
    "Official holdout: AGENCY_LMM_FULL cond test R2=0.7607, RMSE=1.0814 (2013-2014). ",
    "pred_wp = exp(pred_log_wp) - 1 (no bias correction)."
  ),
  model_name  = "AGENCY_LMM_FULL_ALL_DATA",
  n_rows      = length(lmm_samp_out),
  sample_note = "Training-population diagnostic sample. Not holdout evaluation.",
  conditional_note = "Conditional predictions use agency EBLUPs from training (re.form=NULL).",
  marginal_note    = "Marginal predictions use fixed effects only (re.form=NA).",
  predictions = lmm_samp_out
)
write_json_out(lmm_samp_json, "agency_lmm_full_all_data_predictions_sample.json")

# ---- 9. ols_lmm_final_all_data_metadata.json (optional) ----
metadata_out <- list(
  description = paste0(
    "Combined metadata for OLS_ADDITIVE_FINAL_ALL_DATA and AGENCY_LMM_FULL_ALL_DATA. ",
    "Supporting all-data refit artifacts. Model selection is complete. ",
    "RF_FINAL_ALL_DATA is the primary deployment artifact."
  ),
  model_selection_complete = TRUE,
  primary_deployment_model = "RF_FINAL_ALL_DATA",
  supporting_reference_models = list("OLS_ADDITIVE_FINAL_ALL_DATA", "AGENCY_LMM_FULL_ALL_DATA"),
  training_years      = as.list(2006:2014),
  excluded_years      = list(2005L, 2015L),
  n_training_rows     = n_alldata,
  target              = "log_wp = log(WRTN_PREM_AMT + 1)",
  inverse_transform   = "pred_wp = exp(pred_log_wp) - 1",
  bias_correction     = "none applied — direct inverse transform only",
  row_filters         = list(
    "STAT_PROFILE_DATE_YEAR in 2006:2014", "PROD_ABBR != 'COMMPOL'",
    "WRTN_PREM_AMT > 0", "!is.na(log_prev_wp)", "!is.na(log_prev_poly)"
  ),
  shared_predictors   = list(
    numeric     = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                    "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE","STAT_PROFILE_DATE_YEAR"),
    categorical = c("STATE_ABBR","PROD_ABBR","VENDOR")
  ),
  lmm_additional      = list(random_effect = "(1 | AGENCY_ID)", reml = TRUE, optimizer = "bobyqa"),
  categorical_factor_levels = list(
    STATE_ABBR = alldata_state_levels,
    PROD_ABBR  = alldata_prod_levels,
    VENDOR     = alldata_vendor_levels
  ),
  locked_holdout_metrics = list(
    OLS_ADDITIVE_FINAL = LOCKED_OLS,
    AGENCY_LMM_FULL    = LOCKED_LMM,
    RF_1_SAFE_TUNED    = LOCKED_RF
  ),
  validation = validation,
  output_files = list(
    "ols_lmm_final_all_data_summary.json",
    "ols_additive_final_all_data_coefficients.json",
    "ols_additive_final_all_data_predictions_sample.json",
    "ols_additive_final_all_data.rds",
    "agency_lmm_full_all_data_summary.json",
    "agency_lmm_full_all_data_fixed_effects.json",
    "agency_lmm_full_all_data_random_effects_summary.json",
    "agency_lmm_full_all_data_variance_components.json",
    "agency_lmm_full_all_data_predictions_sample.json",
    "agency_lmm_full_all_data.rds",
    "ols_lmm_final_all_data_metadata.json"
  )
)
write_json_out(metadata_out, "ols_lmm_final_all_data_metadata.json")

cat(sprintf("\n=== OLS and LMM Final All-Data Refit complete ===\n"))
cat(sprintf("Outputs in: %s\n\n", output_dir))
cat("--- Summary ---\n")
cat(sprintf("Final training rows         : %s — %s\n",
            format(n_alldata, big.mark=","),
            if (n_alldata == n_expected) "PASS" else "DIFF"))
cat(sprintf("Year check                  : %s\n", if (year_check_pass) "PASS" else "FAIL"))
cat(sprintf("OLS fitted                  : %d coefficients  %.1fs\n",
            length(coef(ols_final)), ols_time))
cat(sprintf("OLS within-sample R²        : %.4f (NOT holdout)\n", ols_train_r2))
cat(sprintf("OLS locked holdout R²       : %.4f (2013-2014, OLS_ADDITIVE_FINAL)\n", LOCKED_OLS$test_r2))
cat(sprintf("OLS RDS saved               : %s\n", if (file.exists(rds_ols_path)) "YES" else "FAIL"))
cat(sprintf("LMM fitted                  : %d agencies  %.1fs\n", n_re_ag, lmm_time))
cat(sprintf("LMM singular                : %s\n", lmm_singular))
cat(sprintf("LMM convergence warnings    : %d\n", length(lmm_warns)))
cat(sprintf("LMM agency ICC (all-data)   : %.4f (vs locked 0.2422)\n", ag_icc))
cat(sprintf("LMM within-sample cond R²   : %.4f (NOT holdout)\n", lmm_train_cond_r2))
cat(sprintf("LMM locked cond holdout R²  : %.4f (2013-2014, AGENCY_LMM_FULL)\n", LOCKED_LMM$conditional$test_r2))
cat(sprintf("LMM RDS saved               : %s\n", if (file.exists(rds_lmm_path)) "YES" else "FAIL"))
cat(sprintf("\nRF_FINAL_ALL_DATA           : primary deployment artifact (unchanged)\n"))
cat(sprintf("OLS_ADDITIVE_FINAL_ALL_DATA : supporting reference artifact\n"))
cat(sprintf("AGENCY_LMM_FULL_ALL_DATA    : supporting reference artifact\n"))
