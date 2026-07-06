# =============================================================================
# Dataset A Part 2 — Written Premium LMM Modeling
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_wp_lmm.R
#
# Outputs (modeling/dataset_a_part2/outputs/):
#   lmm_summary.json
#   lmm_model_comparison.json
#   lmm_variance_components.json
#   lmm_predictions_sample.json
#
# Models fit (10 total):
#   Null models  : AGENCY_NULL_FULL, AGENCY_NULL_PARENT_CC, NESTED_NULL_PARENT_CC,
#                  AGENCY_NULL_TRUE_PARENT, NESTED_NULL_TRUE_PARENT
#   Full LMMs    : AGENCY_LMM_FULL, AGENCY_LMM_PARENT_CC, NESTED_LMM_PARENT_CC,
#                  AGENCY_LMM_TRUE_PARENT, NESTED_LMM_TRUE_PARENT
#
# Benchmarks (locked):
#   OLS_ADDITIVE_FINAL : test R²=0.7552  RMSE=1.0937  MAE=0.6990
#   RF_1_SAFE_TUNED    : test R²=0.8839  RMSE=0.7532  MAE=0.4428
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(lme4); library(jsonlite)
})

SEED       <- 42L
db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

SENTINEL_PAI <- 99999L

OLS_BENCHMARKS <- list(
  model      = "OLS_ADDITIVE_FINAL",
  test_r2    = 0.7552, test_rmse = 1.0937, test_mae  = 0.6990,
  train_r2   = 0.7810, train_rmse = 1.0238, train_mae = 0.6468,
  source     = "locked from ols_model_comparison.json"
)
RF_BENCHMARKS <- list(
  model      = "RF_1_SAFE_TUNED",
  test_r2    = 0.8839, test_rmse  = 0.7532, test_mae  = 0.4428,
  source     = "locked from rf_model_comparison.json"
)

LMM_CTRL <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 2e5))

cat("=== Written Premium LMM Modeling ===\n\n")

# =============================================================================
# Load and build effective modeling population
# =============================================================================

cat("Loading data...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n", format(nrow(raw), big.mark = ","), ncol(raw)))

cat("Building effective population (same filters as OLS/RF)...\n")

wp_base <- raw |>
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
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly),
    STATE_ABBR    = factor(STATE_ABBR),
    PROD_ABBR     = factor(PROD_ABBR),
    VENDOR        = factor(VENDOR)
  )

wp_eff <- wp_base |> filter(!is.na(log_prev_wp), !is.na(log_prev_poly))

wp_full_train <- wp_eff |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_full_test  <- wp_eff |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)

n_full_train <- nrow(wp_full_train); n_full_test <- nrow(wp_full_test)

if (n_full_train == 103377L && n_full_test == 30981L) {
  cat(sprintf("  Full effective: %s train / %s test  [matches expected]\n",
              format(n_full_train, big.mark=","), format(n_full_test, big.mark=",")))
} else {
  cat(sprintf("  Full effective: %s train / %s test  [MISMATCH: expected 103,377 / 30,981]\n",
              format(n_full_train, big.mark=","), format(n_full_test, big.mark=",")))
}

# =============================================================================
# PART B — Three population definitions
# =============================================================================

# PARENT_CC: exclude PRIMARY_AGENCY_ID = 99999
wp_pcc_train <- wp_full_train |> filter(PRIMARY_AGENCY_ID != SENTINEL_PAI)
wp_pcc_test  <- wp_full_test  |> filter(PRIMARY_AGENCY_ID != SENTINEL_PAI)

# TRUE_PARENT: exclude sentinel AND rows where PAI == AID (row-level)
wp_tp_train  <- wp_full_train |>
  filter(PRIMARY_AGENCY_ID != SENTINEL_PAI,
         PRIMARY_AGENCY_ID != AGENCY_ID)
wp_tp_test   <- wp_full_test  |>
  filter(PRIMARY_AGENCY_ID != SENTINEL_PAI,
         PRIMARY_AGENCY_ID != AGENCY_ID)

cat(sprintf("\n  PARENT_CC   : %s train / %s test\n",
            format(nrow(wp_pcc_train), big.mark=","), format(nrow(wp_pcc_test), big.mark=",")))
cat(sprintf("  TRUE_PARENT : %s train / %s test\n\n",
            format(nrow(wp_tp_train),  big.mark=","), format(nrow(wp_tp_test),  big.mark=",")))

# Population summary helper
pop_summary <- function(train, test, label, include_parent = FALSE) {
  n_tr_ag  <- n_distinct(train$AGENCY_ID)
  n_te_ag  <- n_distinct(test$AGENCY_ID)
  seen_ag  <- intersect(unique(test$AGENCY_ID), unique(train$AGENCY_ID))
  out <- list(
    label             = label,
    n_train_rows      = nrow(train),
    n_test_rows       = nrow(test),
    n_train_agencies  = n_tr_ag,
    n_test_agencies   = n_te_ag,
    n_test_agencies_seen_in_train = length(seen_ag),
    pct_test_agencies_seen        = round(length(seen_ag) / n_te_ag * 100, 2),
    pct_test_rows_seen_agency     = round(sum(test$AGENCY_ID %in% unique(train$AGENCY_ID)) / nrow(test) * 100, 2)
  )
  if (include_parent) {
    n_tr_p   <- n_distinct(train$PRIMARY_AGENCY_ID)
    n_te_p   <- n_distinct(test$PRIMARY_AGENCY_ID)
    seen_p   <- intersect(unique(test$PRIMARY_AGENCY_ID), unique(train$PRIMARY_AGENCY_ID))
    out$n_train_parents              <- n_tr_p
    out$n_test_parents               <- n_te_p
    out$n_test_parents_seen_in_train <- length(seen_p)
    out$pct_test_parents_seen        <- round(length(seen_p) / n_te_p * 100, 2)
    out$pct_test_rows_seen_parent    <- round(sum(test$PRIMARY_AGENCY_ID %in% unique(train$PRIMARY_AGENCY_ID)) / nrow(test) * 100, 2)
  }
  out
}

pop_defs <- list(
  FULL        = pop_summary(wp_full_train, wp_full_test, "FULL",        include_parent = FALSE),
  PARENT_CC   = pop_summary(wp_pcc_train,  wp_pcc_test,  "PARENT_CC",   include_parent = TRUE),
  TRUE_PARENT = pop_summary(wp_tp_train,   wp_tp_test,   "TRUE_PARENT", include_parent = TRUE)
)

# =============================================================================
# Helper functions
# =============================================================================

pct <- function(n, d) round(n / d * 100, 2)

r2_score <- function(a, p) {
  ok <- !is.na(a) & !is.na(p)
  ss_res <- sum((a[ok]-p[ok])^2); ss_tot <- sum((a[ok]-mean(a[ok]))^2)
  if (ss_tot == 0) return(NA_real_)
  round(1 - ss_res/ss_tot, 4)
}
rmse_fn <- function(a, p) { ok <- !is.na(a)&!is.na(p); round(sqrt(mean((a[ok]-p[ok])^2)), 4) }
mae_fn  <- function(a, p) { ok <- !is.na(a)&!is.na(p); round(mean(abs(a[ok]-p[ok])),     4) }

# Recode test-data factor levels to exactly what was estimated in the model.
# Uses names(fixef(model)) as ground truth: levels absent from fixef get
# recoded to the reference level (contributing 0 to the fixed-effect sum).
align_to_model <- function(newdata, model) {
  fe_names  <- names(fixef(model))
  cat_vars  <- c("STATE_ABBR", "PROD_ABBR", "VENDOR")  # fixed categorical predictors
  for (v in cat_vars) {
    if (!v %in% names(newdata)) next
    # Coefficient names for this variable look like "PROD_ABBRHome", "STATE_ABBRNY", etc.
    matched  <- fe_names[startsWith(fe_names, v)]
    non_ref  <- sub(paste0("^", v), "", matched)        # strip variable prefix
    all_tr   <- levels(droplevels(model@frame[[v]]))    # levels actually in training
    ref_lvl  <- setdiff(all_tr, non_ref)
    if (length(ref_lvl) == 0) ref_lvl <- all_tr[1]     # fallback
    active   <- c(ref_lvl[1], non_ref)
    vals     <- as.character(newdata[[v]])
    vals[!vals %in% active] <- ref_lvl[1]               # unseen → reference
    newdata[[v]] <- factor(vals, levels = active)
  }
  newdata
}

fit_lmm <- function(formula, data, label) {
  cat(sprintf("  Fitting %-40s", label))
  data  <- droplevels(data)
  warns <- character(0)
  t0 <- proc.time()
  model <- withCallingHandlers(
    lmer(formula, data = data, REML = TRUE, control = LMM_CTRL),
    warning = function(w) {
      warns <<- c(warns, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  elapsed <- round((proc.time() - t0)["elapsed"], 1)
  sing    <- isSingular(model)
  cat(sprintf("  %.1fs  singular=%s  warnings=%d\n", elapsed, sing, length(warns)))
  list(model = model, label = label, warnings = warns, singular = sing, runtime_s = elapsed)
}

extract_vc <- function(fit_result) {
  model <- fit_result$model
  vc    <- as.data.frame(VarCorr(model))
  res   <- vc[vc$grp == "Residual", "vcov"]
  re    <- vc[vc$grp != "Residual", c("grp", "vcov")]
  total <- sum(re$vcov) + res

  re_list <- lapply(seq_len(nrow(re)), function(i) {
    v <- re$vcov[i]
    list(group          = re$grp[i],
         variance       = round(v,   6),
         std_dev        = round(sqrt(v), 6),
         variance_share = round(v / total * 100, 2))
  })

  result <- list(
    model_id             = fit_result$label,
    singular             = fit_result$singular,
    convergence_warnings = fit_result$warnings,
    random_effects       = re_list,
    residual_variance    = round(res,        6),
    residual_std_dev     = round(sqrt(res),  6),
    residual_variance_share = round(res / total * 100, 2),
    total_variance       = round(total, 6)
  )

  # ICC values
  re_named <- setNames(re$vcov, re$grp)
  if ("AGENCY_ID" %in% names(re_named) && !("PRIMARY_AGENCY_ID" %in% names(re_named))) {
    result$icc_agency <- round(re_named["AGENCY_ID"] / total, 4)
  }
  if ("AGENCY_ID" %in% names(re_named) && "PRIMARY_AGENCY_ID" %in% names(re_named)) {
    result$icc_agency     <- round(re_named["AGENCY_ID"]         / total, 4)
    result$icc_parent     <- round(re_named["PRIMARY_AGENCY_ID"] / total, 4)
    result$icc_hierarchy  <- round((re_named["AGENCY_ID"] + re_named["PRIMARY_AGENCY_ID"]) / total, 4)
  }

  # Group counts
  grp_names <- re$grp
  result$group_counts <- lapply(grp_names, function(g) {
    cnts <- table(model@frame[[g]])
    list(grouping_variable = g,
         n_groups_in_fit   = length(cnts),
         rows_per_group    = list(min    = as.integer(min(cnts)),
                                  median = as.numeric(median(cnts)),
                                  mean   = round(mean(cnts), 1),
                                  max    = as.integer(max(cnts))))
  })
  result
}

eval_lmm_full <- function(fit_result, train_df, test_df) {
  model     <- fit_result$model
  # Align factor levels to what the model was trained on (avoids non-conformable X %*% fixef)
  train_ali <- align_to_model(train_df, model)
  test_ali  <- align_to_model(test_df,  model)
  p_cond_tr <- predict(model, newdata = train_ali, re.form = NULL, allow.new.levels = TRUE)
  p_cond_te <- predict(model, newdata = test_ali,  re.form = NULL, allow.new.levels = TRUE)
  p_marg_tr <- predict(model, newdata = train_ali, re.form = NA,   allow.new.levels = TRUE)
  p_marg_te <- predict(model, newdata = test_ali,  re.form = NA,   allow.new.levels = TRUE)
  y_tr <- train_df$log_wp; y_te <- test_df$log_wp

  seen_ag <- unique(train_df$AGENCY_ID)
  n_seen  <- sum(test_df$AGENCY_ID %in% seen_ag)
  n_new   <- nrow(test_df) - n_seen

  nl <- list(
    n_test_rows_seen_agency  = n_seen,
    n_test_rows_new_agency   = n_new,
    pct_test_rows_seen_agency = round(n_seen / nrow(test_df) * 100, 2)
  )

  re_names <- names(ranef(model))
  if ("PRIMARY_AGENCY_ID" %in% re_names) {
    seen_p  <- unique(train_df$PRIMARY_AGENCY_ID)
    n_sp    <- sum(test_df$PRIMARY_AGENCY_ID %in% seen_p, na.rm = TRUE)
    nl$n_test_rows_seen_parent  <- n_sp
    nl$n_test_rows_new_parent   <- nrow(test_df) - n_sp
    nl$pct_test_rows_seen_parent <- round(n_sp / nrow(test_df) * 100, 2)
  }

  list(
    model_id    = fit_result$label,
    n_train     = nrow(train_df),
    n_test      = nrow(test_df),
    singular    = fit_result$singular,
    convergence_warnings = fit_result$warnings,
    conditional = list(
      train_r2   = r2_score(y_tr, p_cond_tr), train_rmse = rmse_fn(y_tr, p_cond_tr),
      train_mae  = mae_fn(y_tr,   p_cond_tr),
      test_r2    = r2_score(y_te, p_cond_te), test_rmse  = rmse_fn(y_te, p_cond_te),
      test_mae   = mae_fn(y_te,   p_cond_te)
    ),
    marginal = list(
      train_r2   = r2_score(y_tr, p_marg_tr), train_rmse = rmse_fn(y_tr, p_marg_tr),
      train_mae  = mae_fn(y_tr,   p_marg_tr),
      test_r2    = r2_score(y_te, p_marg_te), test_rmse  = rmse_fn(y_te, p_marg_te),
      test_mae   = mae_fn(y_te,   p_marg_te)
    ),
    new_level_counts = nl
  )
}

write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
  cat(sprintf("  %-40s %.1f KB\n", filename, file.size(path) / 1024))
}

# =============================================================================
# OLS refit on FULL population (for prediction sample columns)
# =============================================================================

cat("Fitting OLS_ADDITIVE_FINAL (FULL, for prediction sample)...\n")
f_ols <- log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS +
                  AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                  STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR
m_ols <- lm(f_ols, data = wp_full_train)
cat("  Done\n\n")

# =============================================================================
# Formulas
# =============================================================================

FIXED_FX <- log_wp ~ log_prev_wp + log_prev_poly + ACTIVE_PRODUCERS +
                     AGENCY_APPOINTMENT_YEAR + MAX_AGE + MIN_AGE +
                     STATE_ABBR + PROD_ABBR + VENDOR + STAT_PROFILE_DATE_YEAR

f_null_ag     <- log_wp ~ 1 + (1 | AGENCY_ID)
f_null_nested <- log_wp ~ 1 + (1 | PRIMARY_AGENCY_ID) + (1 | AGENCY_ID)
f_lmm_ag      <- update(FIXED_FX, . ~ . + (1 | AGENCY_ID))
f_lmm_nested  <- update(FIXED_FX, . ~ . + (1 | PRIMARY_AGENCY_ID) + (1 | AGENCY_ID))

# =============================================================================
# PART C — Null models
# =============================================================================

cat("Fitting null models...\n")
null_agency_full    <- fit_lmm(f_null_ag,     wp_full_train,   "AGENCY_NULL_FULL")
null_agency_pcc     <- fit_lmm(f_null_ag,     wp_pcc_train,    "AGENCY_NULL_PARENT_CC")
null_nested_pcc     <- fit_lmm(f_null_nested, wp_pcc_train,    "NESTED_NULL_PARENT_CC")
null_agency_tp      <- fit_lmm(f_null_ag,     wp_tp_train,     "AGENCY_NULL_TRUE_PARENT")
null_nested_tp      <- fit_lmm(f_null_nested, wp_tp_train,     "NESTED_NULL_TRUE_PARENT")
cat("\n")

# =============================================================================
# PART C — Full fixed-effect LMMs
# =============================================================================

cat("Fitting full LMMs...\n")
lmm_agency_full     <- fit_lmm(f_lmm_ag,      wp_full_train,   "AGENCY_LMM_FULL")
lmm_agency_pcc      <- fit_lmm(f_lmm_ag,      wp_pcc_train,    "AGENCY_LMM_PARENT_CC")
lmm_nested_pcc      <- fit_lmm(f_lmm_nested,  wp_pcc_train,    "NESTED_LMM_PARENT_CC")
lmm_agency_tp       <- fit_lmm(f_lmm_ag,      wp_tp_train,     "AGENCY_LMM_TRUE_PARENT")
lmm_nested_tp       <- fit_lmm(f_lmm_nested,  wp_tp_train,     "NESTED_LMM_TRUE_PARENT")
cat("\n")

# =============================================================================
# PART D — Performance metrics for full LMMs
# =============================================================================

cat("Computing performance metrics...\n")
perf_lmm_ag_full  <- eval_lmm_full(lmm_agency_full,  wp_full_train, wp_full_test)
perf_lmm_ag_pcc   <- eval_lmm_full(lmm_agency_pcc,   wp_pcc_train,  wp_pcc_test)
perf_lmm_ne_pcc   <- eval_lmm_full(lmm_nested_pcc,   wp_pcc_train,  wp_pcc_test)
perf_lmm_ag_tp    <- eval_lmm_full(lmm_agency_tp,    wp_tp_train,   wp_tp_test)
perf_lmm_ne_tp    <- eval_lmm_full(lmm_nested_tp,    wp_tp_train,   wp_tp_test)

cat(sprintf("  AGENCY_LMM_FULL  : cond test R²=%.4f  marg test R²=%.4f\n",
            perf_lmm_ag_full$conditional$test_r2,
            perf_lmm_ag_full$marginal$test_r2))
cat(sprintf("  AGENCY_LMM_PCC   : cond test R²=%.4f  marg test R²=%.4f\n",
            perf_lmm_ag_pcc$conditional$test_r2,
            perf_lmm_ag_pcc$marginal$test_r2))
cat(sprintf("  NESTED_LMM_PCC   : cond test R²=%.4f  marg test R²=%.4f\n",
            perf_lmm_ne_pcc$conditional$test_r2,
            perf_lmm_ne_pcc$marginal$test_r2))
cat(sprintf("  AGENCY_LMM_TP    : cond test R²=%.4f  marg test R²=%.4f\n",
            perf_lmm_ag_tp$conditional$test_r2,
            perf_lmm_ag_tp$marginal$test_r2))
cat(sprintf("  NESTED_LMM_TP    : cond test R²=%.4f  marg test R²=%.4f\n\n",
            perf_lmm_ne_tp$conditional$test_r2,
            perf_lmm_ne_tp$marginal$test_r2))

# =============================================================================
# PART E — Variance components
# =============================================================================

cat("Extracting variance components...\n")
vc_null_ag_full  <- extract_vc(null_agency_full)
vc_null_ag_pcc   <- extract_vc(null_agency_pcc)
vc_null_ne_pcc   <- extract_vc(null_nested_pcc)
vc_null_ag_tp    <- extract_vc(null_agency_tp)
vc_null_ne_tp    <- extract_vc(null_nested_tp)

vc_lmm_ag_full   <- extract_vc(lmm_agency_full)
vc_lmm_ag_pcc    <- extract_vc(lmm_agency_pcc)
vc_lmm_ne_pcc    <- extract_vc(lmm_nested_pcc)
vc_lmm_ag_tp     <- extract_vc(lmm_agency_tp)
vc_lmm_ne_tp     <- extract_vc(lmm_nested_tp)

cat(sprintf("  NULL AGENCY_FULL  : ICC agency=%.4f\n",  vc_null_ag_full$icc_agency))
cat(sprintf("  NULL NESTED_PCC   : ICC parent=%.4f  ICC agency=%.4f\n",
            vc_null_ne_pcc$icc_parent, vc_null_ne_pcc$icc_agency))
cat(sprintf("  NULL NESTED_TP    : ICC parent=%.4f  ICC agency=%.4f\n",
            vc_null_ne_tp$icc_parent, vc_null_ne_tp$icc_agency))
cat(sprintf("  LMM AGENCY_FULL   : residual ICC agency=%.4f\n",  vc_lmm_ag_full$icc_agency))
cat(sprintf("  LMM NESTED_PCC    : residual ICC parent=%.4f  ICC agency=%.4f\n",
            vc_lmm_ne_pcc$icc_parent, vc_lmm_ne_pcc$icc_agency))
cat("\n")

# =============================================================================
# PART F — Model comparison vs benchmarks
# =============================================================================

delta_vs_ols <- function(test_r2, test_rmse, test_mae) {
  list(
    delta_test_r2   = round(test_r2   - OLS_BENCHMARKS$test_r2,   4),
    delta_test_rmse = round(test_rmse - OLS_BENCHMARKS$test_rmse, 4),
    delta_test_mae  = round(test_mae  - OLS_BENCHMARKS$test_mae,  4)
  )
}

delta_nested_vs_agency <- function(nested_perf, agency_perf, type = "conditional") {
  list(
    delta_test_r2   = round(nested_perf[[type]]$test_r2   - agency_perf[[type]]$test_r2,   4),
    delta_test_rmse = round(nested_perf[[type]]$test_rmse - agency_perf[[type]]$test_rmse, 4),
    delta_test_mae  = round(nested_perf[[type]]$test_mae  - agency_perf[[type]]$test_mae,  4)
  )
}

# =============================================================================
# PART G — Prediction sample (FULL test population)
# =============================================================================

cat("Building prediction sample...\n")
set.seed(SEED)
n_per_yr   <- ceiling(min(2000L, nrow(wp_full_test)) / 2L)
samp_rows  <- wp_full_test |>
  group_by(STAT_PROFILE_DATE_YEAR) |>
  slice_sample(n = n_per_yr) |>
  ungroup() |>
  slice_head(n = 2000L)

ols_pred_samp  <- suppressWarnings(predict(m_ols, newdata = samp_rows))

samp_ali    <- align_to_model(samp_rows, lmm_agency_full$model)
p_cond_samp <- predict(lmm_agency_full$model, newdata = samp_ali,
                       re.form = NULL, allow.new.levels = TRUE)
p_marg_samp <- predict(lmm_agency_full$model, newdata = samp_ali,
                       re.form = NA, allow.new.levels = TRUE)

seen_ag_full  <- unique(wp_full_train$AGENCY_ID)
seen_pai_full <- unique(wp_full_train$PRIMARY_AGENCY_ID)

pred_sample <- lapply(seq_len(nrow(samp_rows)), function(i) {
  r <- samp_rows[i, ]
  list(
    AGENCY_ID              = r$AGENCY_ID,
    PRIMARY_AGENCY_ID      = r$PRIMARY_AGENCY_ID,
    STATE_ABBR             = as.character(r$STATE_ABBR),
    PROD_ABBR              = as.character(r$PROD_ABBR),
    VENDOR                 = as.character(r$VENDOR),
    STAT_PROFILE_DATE_YEAR = r$STAT_PROFILE_DATE_YEAR,
    actual_log_wp          = round(r$log_wp, 6),
    actual_wp              = round(exp(r$log_wp) - 1, 2),
    ols_pred_log_wp        = round(ols_pred_samp[i], 6),
    ols_pred_wp            = round(pmax(exp(ols_pred_samp[i]) - 1, 0), 2),
    agency_lmm_full_cond_pred_log_wp = round(p_cond_samp[i], 6),
    agency_lmm_full_cond_pred_wp     = round(pmax(exp(p_cond_samp[i]) - 1, 0), 2),
    agency_lmm_full_marg_pred_log_wp = round(p_marg_samp[i], 6),
    agency_lmm_full_marg_pred_wp     = round(pmax(exp(p_marg_samp[i]) - 1, 0), 2),
    residual_cond_log      = round(r$log_wp - p_cond_samp[i], 6),
    abs_error_cond_log     = round(abs(r$log_wp - p_cond_samp[i]), 6),
    agency_seen_in_train   = r$AGENCY_ID %in% seen_ag_full,
    parent_seen_in_train   = r$PRIMARY_AGENCY_ID %in% seen_pai_full
  )
})
cat(sprintf("  %d rows sampled from FULL test set\n\n", length(pred_sample)))

# =============================================================================
# PART I — Recommendation logic
# =============================================================================

lmm_full_cond_test_r2 <- perf_lmm_ag_full$conditional$test_r2
lmm_full_marg_test_r2 <- perf_lmm_ag_full$marginal$test_r2
residual_icc_full     <- vc_lmm_ag_full$icc_agency
any_full_warn         <- length(lmm_agency_full$warnings) > 0
full_singular         <- lmm_agency_full$singular

agency_lmm_rec <- if (!full_singular && !any_full_warn &&
                      lmm_full_cond_test_r2 > OLS_BENCHMARKS$test_r2 + 0.01 &&
                      residual_icc_full > 0.05) {
  list(
    decision = "ADVANCE_AGENCY_LMM",
    justification = sprintf(
      paste0(
        "AGENCY_LMM_FULL conditional test R² = %.4f vs OLS %.4f (delta = %+.4f > threshold 0.01). ",
        "Residual agency ICC = %.4f (> threshold 0.05). ",
        "No convergence warnings. Not singular. ",
        "Agency random intercepts add meaningful predictive and structural value."
      ),
      lmm_full_cond_test_r2, OLS_BENCHMARKS$test_r2,
      lmm_full_cond_test_r2 - OLS_BENCHMARKS$test_r2,
      residual_icc_full
    )
  )
} else if (!full_singular && (lmm_full_cond_test_r2 > OLS_BENCHMARKS$test_r2 || residual_icc_full > 0.02)) {
  list(
    decision = "KEEP_AS_STRUCTURAL_DIAGNOSTIC_ONLY",
    justification = sprintf(
      paste0(
        "AGENCY_LMM_FULL conditional test R² = %.4f (delta vs OLS = %+.4f). ",
        "Residual agency ICC = %.4f. Singular = %s. Warnings = %d. ",
        "Agency structure is visible in variance components but predictive gain is below threshold or convergence uncertain."
      ),
      lmm_full_cond_test_r2,
      lmm_full_cond_test_r2 - OLS_BENCHMARKS$test_r2,
      residual_icc_full, full_singular, length(lmm_agency_full$warnings)
    )
  )
} else {
  list(
    decision = "DO_NOT_ADVANCE",
    justification = sprintf(
      "AGENCY_LMM_FULL conditional test R² = %.4f (delta vs OLS = %+.4f). Residual ICC = %.4f. No material improvement.",
      lmm_full_cond_test_r2, lmm_full_cond_test_r2 - OLS_BENCHMARKS$test_r2, residual_icc_full
    )
  )
}

# Nested recommendation: use PARENT_CC (primary comparison)
delta_ne_pcc_vs_ag_pcc <- perf_lmm_ne_pcc$conditional$test_r2 - perf_lmm_ag_pcc$conditional$test_r2
parent_var_share_pcc   <- vc_lmm_ne_pcc$icc_parent
nested_singular_pcc    <- lmm_nested_pcc$singular

nested_lmm_rec <- if (!nested_singular_pcc &&
                      delta_ne_pcc_vs_ag_pcc > 0.005 &&
                      !is.null(parent_var_share_pcc) &&
                      parent_var_share_pcc > 0.01) {
  list(
    decision = "ADVANCE_NESTED_LMM",
    justification = sprintf(
      paste0(
        "NESTED_LMM_PARENT_CC improves over AGENCY_LMM_PARENT_CC by delta test R² = %+.4f. ",
        "Parent variance share = %.4f (> 1%%). Not singular. ",
        "Parent-level random effects add meaningful structure beyond agency effects."
      ),
      delta_ne_pcc_vs_ag_pcc, parent_var_share_pcc
    )
  )
} else if (!nested_singular_pcc &&
           !is.null(parent_var_share_pcc) &&
           parent_var_share_pcc > 0.005) {
  list(
    decision = "KEEP_AS_STRUCTURAL_DIAGNOSTIC_ONLY",
    justification = sprintf(
      paste0(
        "NESTED_LMM_PARENT_CC delta test R² vs agency-only = %+.4f (below threshold 0.005). ",
        "Parent variance share = %.4f. Not singular. ",
        "Parent structure exists in data but adds negligible predictive value beyond agency. ",
        "Retain nested model as structural diagnostic only."
      ),
      delta_ne_pcc_vs_ag_pcc, if (!is.null(parent_var_share_pcc)) parent_var_share_pcc else 0
    )
  )
} else {
  list(
    decision = "DO_NOT_ADVANCE",
    justification = sprintf(
      paste0(
        "NESTED_LMM_PARENT_CC delta test R² = %+.4f. ",
        "Singular = %s. Parent variance share = %s. ",
        "Parent random effects add no meaningful structure beyond agency level."
      ),
      delta_ne_pcc_vs_ag_pcc,
      nested_singular_pcc,
      if (!is.null(parent_var_share_pcc)) sprintf("%.4f", parent_var_share_pcc) else "NA (singular)"
    )
  )
}

rf_still_leads <- RF_BENCHMARKS$test_r2 > lmm_full_cond_test_r2

cat(sprintf("  Agency LMM rec  : %s\n", agency_lmm_rec$decision))
cat(sprintf("  Nested LMM rec  : %s\n", nested_lmm_rec$decision))
cat(sprintf("  RF still leads  : %s (RF %.4f vs LMM cond %.4f)\n\n",
            rf_still_leads, RF_BENCHMARKS$test_r2, lmm_full_cond_test_r2))

# =============================================================================
# Assemble key_findings
# =============================================================================

key_findings <- list(
  sprintf("AGENCY_NULL_FULL unconditional ICC = %.4f (agency accounts for %.1f%% of total log_wp variance).",
          vc_null_ag_full$icc_agency, vc_null_ag_full$icc_agency * 100),
  sprintf("After OLS_ADDITIVE_FINAL fixed effects, residual agency ICC = %.4f (%.1f%% of residual variance).",
          vc_lmm_ag_full$icc_agency, vc_lmm_ag_full$icc_agency * 100),
  sprintf("AGENCY_LMM_FULL: conditional test R² = %.4f (OLS = %.4f, delta = %+.4f); marginal test R² = %.4f.",
          lmm_full_cond_test_r2, OLS_BENCHMARKS$test_r2,
          lmm_full_cond_test_r2 - OLS_BENCHMARKS$test_r2,
          lmm_full_marg_test_r2),
  sprintf("RF_1_SAFE_TUNED test R² = %.4f — %s AGENCY_LMM_FULL conditional (%.4f).",
          RF_BENCHMARKS$test_r2,
          if (rf_still_leads) "still leads" else "no longer leads",
          lmm_full_cond_test_r2),
  sprintf("NESTED_NULL_PARENT_CC: parent ICC = %.4f, agency ICC = %.4f, residual = %.1f%%.",
          vc_null_ne_pcc$icc_parent, vc_null_ne_pcc$icc_agency,
          vc_null_ne_pcc$residual_variance_share),
  sprintf("NESTED_LMM_PARENT_CC residual: parent ICC = %.4f, agency ICC = %.4f. Singular = %s.",
          vc_lmm_ne_pcc$icc_parent, vc_lmm_ne_pcc$icc_agency, nested_singular_pcc),
  sprintf("NESTED_LMM_PARENT_CC vs AGENCY_LMM_PARENT_CC: delta conditional test R² = %+.4f.",
          delta_ne_pcc_vs_ag_pcc),
  sprintf("TRUE_PARENT nested: parent ICC (null) = %.4f; (full) = %.4f. Singular = %s.",
          vc_null_ne_tp$icc_parent, vc_lmm_ne_tp$icc_parent, lmm_nested_tp$singular),
  sprintf("Agency LMM recommendation: %s.", agency_lmm_rec$decision),
  sprintf("Nested LMM recommendation: %s.", nested_lmm_rec$decision)
)

# =============================================================================
# Assemble and write JSON files
# =============================================================================

cat("Writing JSON outputs...\n")

# --- lmm_summary.json ---
lmm_summary_out <- list(
  description = paste0(
    "Written Premium LMM modeling for Dataset A Part 2. ",
    "Evaluates agency-level (AGENCY_ID) and nested parent-agency (PRIMARY_AGENCY_ID/AGENCY_ID) ",
    "random intercepts. Three controlled populations: FULL, PARENT_CC (excludes PAI=99999), ",
    "TRUE_PARENT (excludes PAI=99999 and self-parent rows). REML=TRUE, bobyqa optimizer. ",
    "Benchmarks: OLS_ADDITIVE_FINAL (test R²=0.7552) and RF_1_SAFE_TUNED (test R²=0.8839)."
  ),
  modeling_population = list(
    target          = "log_wp = log(WRTN_PREM_AMT + 1)",
    train_years     = "2006-2012",
    test_years      = "2013-2014",
    filters         = list(
      exclude_prod_abbr = "COMMPOL",
      wp_filter         = "WRTN_PREM_AMT > 0",
      na_exclusion      = "log_prev_wp or log_prev_poly NA rows excluded"
    ),
    fixed_effects   = list(
      numeric     = c("log_prev_wp","log_prev_poly","ACTIVE_PRODUCERS",
                      "AGENCY_APPOINTMENT_YEAR","MAX_AGE","MIN_AGE","STAT_PROFILE_DATE_YEAR"),
      categorical = c("STATE_ABBR","PROD_ABBR","VENDOR")
    ),
    random_effects  = "random intercept only — no random slopes"
  ),
  population_definitions = pop_defs,
  model_list = list(
    null_models = c("AGENCY_NULL_FULL","AGENCY_NULL_PARENT_CC","NESTED_NULL_PARENT_CC",
                    "AGENCY_NULL_TRUE_PARENT","NESTED_NULL_TRUE_PARENT"),
    full_lmms   = c("AGENCY_LMM_FULL","AGENCY_LMM_PARENT_CC","NESTED_LMM_PARENT_CC",
                    "AGENCY_LMM_TRUE_PARENT","NESTED_LMM_TRUE_PARENT"),
    optimizer   = "bobyqa",
    reml        = TRUE
  ),
  key_findings  = key_findings,
  recommendation = list(
    agency_lmm = agency_lmm_rec,
    nested_lmm = nested_lmm_rec,
    final_model_branch_status = list(
      ols_role   = "OLS_ADDITIVE_FINAL remains the interpretability benchmark (48 coefficients, locked specification).",
      rf_role    = sprintf("RF_1_SAFE_TUNED remains the leading predictive model (test R² = %.4f).", RF_BENCHMARKS$test_r2),
      lmm_role   = sprintf(
        "AGENCY_LMM_FULL adds credibility/variance-decomposition interpretation (residual ICC = %.4f). %s",
        vc_lmm_ag_full$icc_agency, agency_lmm_rec$decision
      ),
      nested_role = sprintf(
        "Nested LMM tests parent-agency structure. Parent variance share = %.4f. %s",
        if (!is.null(vc_lmm_ne_pcc$icc_parent)) vc_lmm_ne_pcc$icc_parent else 0,
        nested_lmm_rec$decision
      )
    )
  )
)

write_json_out(lmm_summary_out, "lmm_summary.json")

# --- lmm_model_comparison.json ---
lmm_comparison_out <- list(
  description = paste0(
    "LMM prediction performance comparison. OLS and RF benchmark metrics are locked. ",
    "LMM metrics are computed within this script on the applicable test population. ",
    "Conditional predictions use training EBLUPs; unseen groups get random effect = 0. ",
    "Marginal predictions use fixed effects only."
  ),
  benchmark_metrics = list(
    OLS_ADDITIVE_FINAL = OLS_BENCHMARKS,
    RF_1_SAFE_TUNED    = RF_BENCHMARKS
  ),
  full_population_comparison = list(
    population    = "FULL (103,377 train / 30,981 test)",
    OLS_ADDITIVE_FINAL = list(
      test_r2 = OLS_BENCHMARKS$test_r2, test_rmse = OLS_BENCHMARKS$test_rmse,
      test_mae = OLS_BENCHMARKS$test_mae
    ),
    RF_1_SAFE_TUNED = list(
      test_r2 = RF_BENCHMARKS$test_r2, test_rmse = RF_BENCHMARKS$test_rmse,
      test_mae = RF_BENCHMARKS$test_mae
    ),
    AGENCY_LMM_FULL_conditional = perf_lmm_ag_full$conditional,
    AGENCY_LMM_FULL_marginal    = perf_lmm_ag_full$marginal,
    delta_conditional_vs_ols    = delta_vs_ols(perf_lmm_ag_full$conditional$test_r2,
                                               perf_lmm_ag_full$conditional$test_rmse,
                                               perf_lmm_ag_full$conditional$test_mae),
    delta_marginal_vs_ols       = delta_vs_ols(perf_lmm_ag_full$marginal$test_r2,
                                               perf_lmm_ag_full$marginal$test_rmse,
                                               perf_lmm_ag_full$marginal$test_mae),
    new_level_counts            = perf_lmm_ag_full$new_level_counts
  ),
  parent_cc_comparison = list(
    population = sprintf("PARENT_CC (%s train / %s test)",
                         format(nrow(wp_pcc_train), big.mark=","),
                         format(nrow(wp_pcc_test), big.mark=",")),
    AGENCY_LMM_PARENT_CC_conditional  = perf_lmm_ag_pcc$conditional,
    AGENCY_LMM_PARENT_CC_marginal     = perf_lmm_ag_pcc$marginal,
    NESTED_LMM_PARENT_CC_conditional  = perf_lmm_ne_pcc$conditional,
    NESTED_LMM_PARENT_CC_marginal     = perf_lmm_ne_pcc$marginal,
    delta_nested_vs_agency_conditional = delta_nested_vs_agency(perf_lmm_ne_pcc, perf_lmm_ag_pcc, "conditional"),
    delta_nested_vs_agency_marginal    = delta_nested_vs_agency(perf_lmm_ne_pcc, perf_lmm_ag_pcc, "marginal"),
    agency_new_level_counts = perf_lmm_ag_pcc$new_level_counts,
    nested_new_level_counts = perf_lmm_ne_pcc$new_level_counts
  ),
  true_parent_comparison = list(
    population = sprintf("TRUE_PARENT (%s train / %s test)",
                         format(nrow(wp_tp_train), big.mark=","),
                         format(nrow(wp_tp_test), big.mark=",")),
    AGENCY_LMM_TRUE_PARENT_conditional  = perf_lmm_ag_tp$conditional,
    AGENCY_LMM_TRUE_PARENT_marginal     = perf_lmm_ag_tp$marginal,
    NESTED_LMM_TRUE_PARENT_conditional  = perf_lmm_ne_tp$conditional,
    NESTED_LMM_TRUE_PARENT_marginal     = perf_lmm_ne_tp$marginal,
    delta_nested_vs_agency_conditional  = delta_nested_vs_agency(perf_lmm_ne_tp, perf_lmm_ag_tp, "conditional"),
    delta_nested_vs_agency_marginal     = delta_nested_vs_agency(perf_lmm_ne_tp, perf_lmm_ag_tp, "marginal"),
    agency_new_level_counts = perf_lmm_ag_tp$new_level_counts,
    nested_new_level_counts = perf_lmm_ne_tp$new_level_counts
  ),
  conditional_vs_marginal_summary = list(
    AGENCY_LMM_FULL = list(
      delta_cond_minus_marg_test_r2 = round(
        perf_lmm_ag_full$conditional$test_r2 - perf_lmm_ag_full$marginal$test_r2, 4),
      interpretation = "Positive delta = conditional (EBLUP) predictions outperform fixed-effects-only predictions on test."
    ),
    AGENCY_LMM_PARENT_CC = list(
      delta_cond_minus_marg_test_r2 = round(
        perf_lmm_ag_pcc$conditional$test_r2 - perf_lmm_ag_pcc$marginal$test_r2, 4)
    ),
    NESTED_LMM_PARENT_CC = list(
      delta_cond_minus_marg_test_r2 = round(
        perf_lmm_ne_pcc$conditional$test_r2 - perf_lmm_ne_pcc$marginal$test_r2, 4)
    )
  )
)

write_json_out(lmm_comparison_out, "lmm_model_comparison.json")

# --- lmm_variance_components.json ---
lmm_vc_out <- list(
  description = paste0(
    "Variance components and ICCs for all 10 LMMs. ",
    "Null model ICCs reflect unconditional clustering (before fixed effects). ",
    "Full LMM variance shares reflect residual clustering after OLS_ADDITIVE_FINAL fixed effects. ",
    "Singularity flag isSingular() = TRUE indicates variance component collapsed to zero or boundary."
  ),
  null_model_variance_components = list(
    AGENCY_NULL_FULL        = vc_null_ag_full,
    AGENCY_NULL_PARENT_CC   = vc_null_ag_pcc,
    NESTED_NULL_PARENT_CC   = vc_null_ne_pcc,
    AGENCY_NULL_TRUE_PARENT = vc_null_ag_tp,
    NESTED_NULL_TRUE_PARENT = vc_null_ne_tp
  ),
  full_model_variance_components = list(
    AGENCY_LMM_FULL         = vc_lmm_ag_full,
    AGENCY_LMM_PARENT_CC    = vc_lmm_ag_pcc,
    NESTED_LMM_PARENT_CC    = vc_lmm_ne_pcc,
    AGENCY_LMM_TRUE_PARENT  = vc_lmm_ag_tp,
    NESTED_LMM_TRUE_PARENT  = vc_lmm_ne_tp
  ),
  icc_summary = list(
    null_unconditional = list(
      AGENCY_NULL_FULL_icc_agency         = vc_null_ag_full$icc_agency,
      NESTED_NULL_PARENT_CC_icc_parent    = vc_null_ne_pcc$icc_parent,
      NESTED_NULL_PARENT_CC_icc_agency    = vc_null_ne_pcc$icc_agency,
      NESTED_NULL_PARENT_CC_icc_hierarchy = vc_null_ne_pcc$icc_hierarchy,
      NESTED_NULL_TRUE_PARENT_icc_parent  = vc_null_ne_tp$icc_parent,
      NESTED_NULL_TRUE_PARENT_icc_agency  = vc_null_ne_tp$icc_agency,
      NESTED_NULL_TRUE_PARENT_icc_hierarchy = vc_null_ne_tp$icc_hierarchy
    ),
    full_residual = list(
      AGENCY_LMM_FULL_residual_icc_agency        = vc_lmm_ag_full$icc_agency,
      NESTED_LMM_PARENT_CC_residual_icc_parent   = vc_lmm_ne_pcc$icc_parent,
      NESTED_LMM_PARENT_CC_residual_icc_agency   = vc_lmm_ne_pcc$icc_agency,
      NESTED_LMM_TRUE_PARENT_residual_icc_parent = vc_lmm_ne_tp$icc_parent,
      NESTED_LMM_TRUE_PARENT_residual_icc_agency = vc_lmm_ne_tp$icc_agency
    )
  ),
  singularity_diagnostics = list(
    AGENCY_NULL_FULL        = list(singular = null_agency_full$singular, warnings = null_agency_full$warnings),
    AGENCY_NULL_PARENT_CC   = list(singular = null_agency_pcc$singular,  warnings = null_agency_pcc$warnings),
    NESTED_NULL_PARENT_CC   = list(singular = null_nested_pcc$singular,  warnings = null_nested_pcc$warnings),
    AGENCY_NULL_TRUE_PARENT = list(singular = null_agency_tp$singular,   warnings = null_agency_tp$warnings),
    NESTED_NULL_TRUE_PARENT = list(singular = null_nested_tp$singular,   warnings = null_nested_tp$warnings),
    AGENCY_LMM_FULL         = list(singular = lmm_agency_full$singular,  warnings = lmm_agency_full$warnings),
    AGENCY_LMM_PARENT_CC    = list(singular = lmm_agency_pcc$singular,   warnings = lmm_agency_pcc$warnings),
    NESTED_LMM_PARENT_CC    = list(singular = lmm_nested_pcc$singular,   warnings = lmm_nested_pcc$warnings),
    AGENCY_LMM_TRUE_PARENT  = list(singular = lmm_agency_tp$singular,    warnings = lmm_agency_tp$warnings),
    NESTED_LMM_TRUE_PARENT  = list(singular = lmm_nested_tp$singular,    warnings = lmm_nested_tp$warnings)
  )
)

write_json_out(lmm_vc_out, "lmm_variance_components.json")

# --- lmm_predictions_sample.json ---
lmm_pred_out <- list(
  description = paste0(
    "Prediction sample for frontend/reporting visualization. ",
    length(pred_sample), " rows from the FULL 2013-2014 test set. ",
    "IMPORTANT: for visualization only — all official metrics computed on the full ",
    format(nrow(wp_full_test), big.mark=","), "-row test set. ",
    "Conditional predictions use EBLUP random effects from training; unseen agencies get random effect = 0. ",
    "Marginal predictions use fixed effects only."
  ),
  n_rows      = length(pred_sample),
  sample_type = "stratified by year, up to 2000 rows from FULL test population",
  predictions = pred_sample
)

write_json_out(lmm_pred_out, "lmm_predictions_sample.json")

cat(sprintf("\n=== LMM modeling complete ===\nOutputs in: %s\n", output_dir))
