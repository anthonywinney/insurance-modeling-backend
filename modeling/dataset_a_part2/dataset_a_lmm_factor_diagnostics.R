# =============================================================================
# Dataset A Part 2 — LMM Factor Level Alignment Diagnostics
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_lmm_factor_diagnostics.R
#
# Outputs (modeling/dataset_a_part2/outputs/):
#   lmm_factor_level_diagnostics.json
#
# Purpose:
#   Audit which categorical fixed-effect levels (STATE_ABBR, PROD_ABBR, VENDOR)
#   are present in test but absent from training, per LMM population. Rows with
#   unseen levels are recoded to the reference level by align_to_model() in
#   dataset_a_wp_lmm.R to avoid predict.merMod non-conformable argument errors.
#   This script documents the scope of that fallback — no models are refit.
#
# Reference level derivation:
#   lme4/lm use the first factor level (alphabetical by default) as the
#   reference / baseline. After droplevels() on the training subset, the
#   reference is the first alphabetically-sorted value present in that subset.
#   This script derives the reference level the same way — no model object needed.
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(jsonlite)
})

db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
out_file   <- file.path(output_dir, "lmm_factor_level_diagnostics.json")

SENTINEL_PAI <- 99999L
cat_vars     <- c("STATE_ABBR", "PROD_ABBR", "VENDOR")

cat("=== LMM Factor Level Diagnostics ===\n\n")

# =============================================================================
# Load data and build populations (identical to dataset_a_wp_lmm.R)
# =============================================================================

cat("Loading data...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

wp_base <- raw |>
  filter(
    STAT_PROFILE_DATE_YEAR %in% 2006:2014,
    PROD_ABBR != "COMMPOL",
    WRTN_PREM_AMT > 0
  ) |>
  mutate(
    log_prev_wp   = suppressWarnings(log(PREV_WRTN_PREM_AMT    + 1)),
    log_prev_poly = suppressWarnings(log(PREV_POLY_INFORCE_QTY + 1)),
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  ) |>
  filter(!is.na(log_prev_wp), !is.na(log_prev_poly))

wp_full_train <- wp_base |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_full_test  <- wp_base |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)

wp_pcc_train  <- wp_full_train |> filter(PRIMARY_AGENCY_ID != SENTINEL_PAI)
wp_pcc_test   <- wp_full_test  |> filter(PRIMARY_AGENCY_ID != SENTINEL_PAI)

wp_tp_train   <- wp_full_train |>
  filter(PRIMARY_AGENCY_ID != SENTINEL_PAI, PRIMARY_AGENCY_ID != AGENCY_ID)
wp_tp_test    <- wp_full_test  |>
  filter(PRIMARY_AGENCY_ID != SENTINEL_PAI, PRIMARY_AGENCY_ID != AGENCY_ID)

cat(sprintf("FULL       : %s train / %s test\n",
            format(nrow(wp_full_train), big.mark=","),
            format(nrow(wp_full_test),  big.mark=",")))
cat(sprintf("PARENT_CC  : %s train / %s test\n",
            format(nrow(wp_pcc_train), big.mark=","),
            format(nrow(wp_pcc_test),  big.mark=",")))
cat(sprintf("TRUE_PARENT: %s train / %s test\n\n",
            format(nrow(wp_tp_train), big.mark=","),
            format(nrow(wp_tp_test),  big.mark=",")))

# =============================================================================
# Factor diagnostic function
# =============================================================================

# Returns a list documenting train vs test level coverage for one variable in
# one population. ref_level is the first alphabetically-sorted training value —
# the same convention as R's factor() default and lme4's reference-level choice
# after droplevels().

factor_diag <- function(train_df, test_df, varname, pop_label) {
  train_vals <- sort(unique(as.character(train_df[[varname]])))
  test_vals  <- sort(unique(as.character(test_df[[varname]])))
  unseen     <- setdiff(test_vals, train_vals)
  ref_level  <- train_vals[1]

  recoded_mask <- as.character(test_df[[varname]]) %in% unseen
  n_recoded    <- sum(recoded_mask)
  pct_recoded  <- round(100 * n_recoded / nrow(test_df), 4)

  # Per-unseen-level detail — how many test rows each unseen level affects
  unseen_detail <- lapply(unseen, function(lvl) {
    n_rows <- sum(as.character(test_df[[varname]]) == lvl)
    list(
      level                 = lvl,
      n_test_rows           = n_rows,
      pct_of_test_rows      = round(100 * n_rows / nrow(test_df), 4),
      recoded_to            = ref_level
    )
  })

  cat(sprintf(
    "  %-12s  %-14s  train_levels=%-3d  test_levels=%-3d  unseen=%-2d  recoded_rows=%d  (%.4f%%)\n",
    pop_label, varname,
    length(train_vals), length(test_vals), length(unseen),
    n_recoded, pct_recoded
  ))

  list(
    variable                  = varname,
    n_train_levels            = length(train_vals),
    n_test_levels             = length(test_vals),
    n_unseen_levels           = length(unseen),
    n_test_rows               = nrow(test_df),
    n_test_rows_recoded       = n_recoded,
    pct_test_rows_recoded     = pct_recoded,
    recoding_occurred         = n_recoded > 0L,
    reference_level           = ref_level,
    train_levels              = train_vals,
    test_levels               = test_vals,
    unseen_test_levels        = unseen,
    unseen_level_detail       = unseen_detail
  )
}

# =============================================================================
# Run diagnostics across all populations
# =============================================================================

populations <- list(
  list(label = "FULL",        train = wp_full_train, test = wp_full_test),
  list(label = "PARENT_CC",   train = wp_pcc_train,  test = wp_pcc_test),
  list(label = "TRUE_PARENT", train = wp_tp_train,   test = wp_tp_test)
)

pop_results <- list()

for (pop in populations) {
  cat(sprintf("\n%s population:\n", pop$label))
  var_results <- list()
  for (v in cat_vars) {
    var_results[[v]] <- factor_diag(pop$train, pop$test, v, pop$label)
  }
  pop_results[[pop$label]] <- var_results
}

# =============================================================================
# Population-level summary: did any recoding occur at all?
# =============================================================================

pop_summaries <- lapply(names(pop_results), function(pname) {
  pr   <- pop_results[[pname]]
  any_recoding <- any(sapply(pr, function(d) d$recoding_occurred))
  total_recoded <- sum(sapply(pr, function(d) d$n_test_rows_recoded))
  # deduplicate: a single row might be recoded on multiple variables — use union
  # of row indices is not easily available after summarization, so report per-variable
  list(
    population       = pname,
    any_recoding     = any_recoding,
    vars_with_unseen = names(which(sapply(pr, function(d) d$recoding_occurred)))
  )
})
names(pop_summaries) <- names(pop_results)

# =============================================================================
# Build and write output JSON
# =============================================================================

cat("\n")

out <- list(
  description = paste0(
    "Audit of categorical fixed-effect factor levels in LMM predictions. ",
    "For each population (FULL, PARENT_CC, TRUE_PARENT) and each categorical ",
    "fixed effect (STATE_ABBR, PROD_ABBR, VENDOR), reports which test-set levels ",
    "were absent from training. Rows with unseen levels are recoded to the reference ",
    "level by align_to_model() in dataset_a_wp_lmm.R before calling predict.merMod. ",
    "This prevents the non-conformable X %*% fixef() error that occurs when the ",
    "design matrix has more columns than the fitted coefficient vector."
  ),
  align_to_model_note = paste0(
    "align_to_model() derives the active factor levels from names(fixef(model)). ",
    "Levels absent from fixef names — including both the reference level and any ",
    "level not present in training — are mapped to the reference level. The reference ",
    "level's dummy coefficient is 0 by definition, so recoded rows receive the same ",
    "fixed-effect contribution as if the reference level had been observed. This is the ",
    "standard practice for out-of-sample prediction with factor covariates in lme4."
  ),
  reference_level_derivation = paste0(
    "Reference level = first alphabetically-sorted value present in the training subset, ",
    "after droplevels(). This is R's default: factor() sorts levels alphabetically and ",
    "lm/lmer use the first level as the baseline/reference. No model object is required ",
    "to derive this — it is deterministic from the training data."
  ),
  populations = pop_results,
  summary_by_population = pop_summaries
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write(toJSON(out, auto_unbox = TRUE, pretty = TRUE, na = "null"), out_file)

sz <- file.size(out_file) / 1024
cat(sprintf("  %-40s  %.1f KB\n", basename(out_file), sz))
cat("\n=== Factor level diagnostics complete ===\n")
