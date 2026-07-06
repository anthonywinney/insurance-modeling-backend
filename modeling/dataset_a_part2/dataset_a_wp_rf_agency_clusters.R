# =============================================================================
# Dataset A Part 2 — RF Agency Cluster Sensitivity Test
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_wp_rf_agency_clusters.R
#
# Tests whether adding k-means agency cluster labels as an RF predictor
# improves out-of-sample performance. K-means on 2006-2012 agency profiles
# only. Agency cluster joined to row-level data as AGENCY_CLUSTER predictor.
# Primary comparison: seen-agency test rows (agencies in 2006-2012 training).
# K selected by OOB RMSE; test performance is diagnostic only.
#
# Outputs (modeling/dataset_a_part2/outputs/):
#   rf_agency_cluster_summary.json
#   rf_agency_cluster_model_comparison.json
#   rf_agency_cluster_diagnostics.json
#   rf_agency_cluster_importance.json
#   rf_agency_cluster_predictions_sample.json
#
# Required packages: DBI, RSQLite, dplyr, ranger, jsonlite
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(ranger); library(jsonlite)
})

SEED       <- 42L
K_VALUES   <- c(5L, 10L, 20L, 30L, 50L)
NSTART_KM  <- 25L
ITER_KM    <- 100L
db_path    <- "insurance.db"
scaled_csv <- file.path("modeling", "dataset_a_part2", "outputs",
                        "agency_profile_clustering_scaled.csv")
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Locked hyperparameters (same as RF_1_SAFE_TUNED)
RF_NUM_TREES <- 500L
RF_MTRY      <- 3L
RF_MNS       <- 5L
RF_SF        <- 0.6
RF_REPLACE   <- FALSE
RF_IMP       <- "permutation"
RF_UNORD     <- "order"

RF_PREDICTORS <- c("log_prev_wp", "log_prev_poly",
                   "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR",
                   "MAX_AGE", "MIN_AGE", "STAT_PROFILE_DATE_YEAR",
                   "STATE_ABBR", "PROD_ABBR", "VENDOR")

cat("=== Dataset A Part 2 — RF Agency Cluster Sensitivity Test ===\n\n")

# =============================================================================
# Section 1: Load database and build modeling population
# =============================================================================

cat("Loading data from database...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

cat("Building modeling population (same filters as RF_1_SAFE_TUNED)...\n")
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
    # Factor levels from full 2006-2014 effective population (same as RF_1_SAFE_TUNED)
    STATE_ABBR = factor(STATE_ABBR),
    PROD_ABBR  = factor(PROD_ABBR),
    VENDOR     = factor(VENDOR)
  )

wp_rf <- wp_base |>
  filter(!is.na(log_prev_wp), !is.na(log_prev_poly), !is.na(log_wp))

wp_train_rf <- wp_rf |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_test_rf  <- wp_rf |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)

n_train <- nrow(wp_train_rf)
n_test  <- nrow(wp_test_rf)

cat(sprintf("  Train (2006-2012): %s rows\n", format(n_train, big.mark = ",")))
cat(sprintf("  Test  (2013-2014): %s rows\n\n", format(n_test, big.mark = ",")))

# =============================================================================
# Section 2: Agency coverage (seen vs new)
# =============================================================================

train_agency_ids <- unique(wp_train_rf$AGENCY_ID)
test_agency_ids  <- unique(wp_test_rf$AGENCY_ID)
seen_agency_ids  <- intersect(test_agency_ids, train_agency_ids)
new_agency_ids   <- setdiff(test_agency_ids, train_agency_ids)

n_train_agencies <- length(train_agency_ids)
n_test_agencies  <- length(test_agency_ids)
n_seen_agencies  <- length(seen_agency_ids)
n_new_agencies   <- length(new_agency_ids)

seen_mask    <- wp_test_rf$AGENCY_ID %in% seen_agency_ids
wp_test_seen <- wp_test_rf[seen_mask, ]
n_test_seen  <- nrow(wp_test_seen)
n_test_new   <- n_test - n_test_seen

cat("Agency coverage:\n")
cat(sprintf("  Training agencies  : %d\n", n_train_agencies))
cat(sprintf("  Test agencies      : %d (seen=%d, new=%d)\n",
            n_test_agencies, n_seen_agencies, n_new_agencies))
cat(sprintf("  Seen-agency rows   : %s  (primary evaluation)\n",
            format(n_test_seen, big.mark = ",")))
cat(sprintf("  New-agency rows    : %s  (NEW_AGENCY fallback)\n\n",
            format(n_test_new, big.mark = ",")))

# =============================================================================
# Section 3: K-means on scaled agency profiles
# =============================================================================

cat("Loading scaled agency profiles...\n")
scaled_df  <- read.csv(scaled_csv, stringsAsFactors = FALSE)
scale_cols <- grep("^scaled_", names(scaled_df), value = TRUE)
n_prof     <- nrow(scaled_df)
cat(sprintf("  %d agencies x %d scaled features\n\n", n_prof, length(scale_cols)))

if (n_prof != n_train_agencies) {
  cat(sprintf(
    "  NOTE: profile count (%d) != training agency count (%d)\n\n",
    n_prof, n_train_agencies))
}

cat(sprintf("Running k-means (k = %s, nstart=%d, iter.max=%d, seed=%d)...\n",
            paste(K_VALUES, collapse = ", "), NSTART_KM, ITER_KM, SEED))

kmeans_stats   <- list()
cluster_lookup <- scaled_df[, "AGENCY_ID", drop = FALSE]

for (k in K_VALUES) {
  set.seed(SEED)
  km <- kmeans(scaled_df[, scale_cols], centers = k,
               nstart = NSTART_KM, iter.max = ITER_KM)
  col_name <- sprintf("CLUSTER_k%d", k)
  cluster_lookup[[col_name]] <- sprintf("C%02d", km$cluster)
  kmeans_stats[[as.character(k)]] <- list(
    k             = k,
    totss         = round(km$totss, 4),
    tot_withinss  = round(km$tot.withinss, 4),
    betweenss     = round(km$betweenss, 4),
    explained_var = round(km$betweenss / km$totss, 4),
    n_iter        = km$iter,
    cluster_sizes = list(
      min    = as.integer(min(km$size)),
      median = as.integer(median(km$size)),
      max    = as.integer(max(km$size)),
      sizes  = as.list(as.integer(sort(km$size)))
    )
  )
  cat(sprintf("  k=%2d  withinss=%.2f  betweenss=%.2f  R2=%.4f  iters=%d\n",
              k, km$tot.withinss, km$betweenss, km$betweenss / km$totss, km$iter))
}
cat("\n")

# =============================================================================
# Section 4: Helper functions
# =============================================================================

r2_score <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  a <- actual[ok]; p <- predicted[ok]
  if (length(a) == 0L) return(NA_real_)
  ss_res <- sum((a - p)^2); ss_tot <- sum((a - mean(a))^2)
  if (ss_tot == 0) return(NA_real_)
  round(1 - ss_res / ss_tot, 4)
}
rmse_fn <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) == 0L) return(NA_real_)
  round(sqrt(mean((actual[ok] - predicted[ok])^2)), 4)
}
mae_fn <- function(actual, predicted) {
  ok <- !is.na(actual) & !is.na(predicted)
  if (sum(ok) == 0L) return(NA_real_)
  round(mean(abs(actual[ok] - predicted[ok])), 4)
}
eval_preds <- function(actual, predicted, n_rows) {
  list(n = n_rows,
       r2   = r2_score(actual, predicted),
       rmse = rmse_fn(actual, predicted),
       mae  = mae_fn(actual, predicted))
}
write_json_out <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(toJSON(obj, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
  cat(sprintf("  %-55s %.1f KB\n", filename, file.size(path) / 1024))
}

# =============================================================================
# Section 5: Train RF_BASE (baseline — no agency cluster predictor)
# =============================================================================

cat("Training RF_BASE (baseline, no agency cluster)...\n")

rf_base_train <- wp_train_rf |> select(all_of(c(RF_PREDICTORS, "log_wp")))
rf_base_test  <- wp_test_rf  |> select(all_of(c(RF_PREDICTORS, "log_wp")))
rf_base_seen  <- wp_test_seen |> select(all_of(c(RF_PREDICTORS, "log_wp")))

t0 <- proc.time()
rf_base <- ranger(
  log_wp ~ .,
  data                      = rf_base_train,
  num.trees                 = RF_NUM_TREES,
  mtry                      = RF_MTRY,
  min.node.size             = RF_MNS,
  replace                   = RF_REPLACE,
  sample.fraction           = RF_SF,
  importance                = RF_IMP,
  respect.unordered.factors = RF_UNORD,
  seed                      = SEED
)
rt_base <- round((proc.time() - t0)["elapsed"], 1)

oob_rmse_base <- round(sqrt(rf_base$prediction.error), 4)
oob_r2_base   <- round(rf_base$r.squared, 4)

pred_base_train_v <- predict(rf_base, data = rf_base_train)$predictions
pred_base_full_v  <- predict(rf_base, data = rf_base_test)$predictions
pred_base_seen_v  <- predict(rf_base, data = rf_base_seen)$predictions

perf_base_train <- eval_preds(wp_train_rf$log_wp,  pred_base_train_v, n_train)
perf_base_full  <- eval_preds(wp_test_rf$log_wp,   pred_base_full_v,  n_test)
perf_base_seen  <- eval_preds(wp_test_seen$log_wp, pred_base_seen_v,  n_test_seen)

imp_base_raw   <- sort(rf_base$variable.importance, decreasing = TRUE)
imp_base_total <- sum(abs(imp_base_raw))

cat(sprintf("  Done in %.1fs  OOB RMSE=%.4f  OOB R2=%.4f\n", rt_base, oob_rmse_base, oob_r2_base))
cat(sprintf("  Train R2=%.4f  Seen-agency test R2=%.4f  Full test R2=%.4f\n\n",
            perf_base_train$r2, perf_base_seen$r2, perf_base_full$r2))

# =============================================================================
# Section 6: Train clustered RF for each k
# =============================================================================

k_results      <- list()
pred_seen_vecs <- list()   # store seen-agency predictions for sample generation

for (k in K_VALUES) {
  k_label  <- sprintf("k%d", k)
  col_name <- sprintf("CLUSTER_k%d", k)
  model_id <- sprintf("RF_CLUSTER_k%d", k)

  cat(sprintf("Training %s ...\n", model_id))

  # Factor levels: sorted training cluster labels + NEW_AGENCY for unseen test agencies
  train_cluster_vals  <- cluster_lookup[[col_name]]
  train_levels_sorted <- sort(unique(train_cluster_vals))
  all_levels          <- c(train_levels_sorted, "NEW_AGENCY")

  # RF training data with AGENCY_CLUSTER
  train_cl <- wp_train_rf |>
    left_join(cluster_lookup[, c("AGENCY_ID", col_name)], by = "AGENCY_ID") |>
    mutate(AGENCY_CLUSTER = factor(.data[[col_name]], levels = all_levels)) |>
    select(all_of(c(RF_PREDICTORS, "AGENCY_CLUSTER", "log_wp")))

  # Full test data (new agencies → "NEW_AGENCY")
  test_cl_full <- wp_test_rf |>
    left_join(cluster_lookup[, c("AGENCY_ID", col_name)], by = "AGENCY_ID") |>
    mutate(
      cluster_val    = if_else(is.na(.data[[col_name]]), "NEW_AGENCY", .data[[col_name]]),
      AGENCY_CLUSTER = factor(cluster_val, levels = all_levels)
    ) |>
    select(all_of(c(RF_PREDICTORS, "AGENCY_CLUSTER", "log_wp")))

  # Seen-agency test data (all have real cluster labels)
  test_cl_seen <- wp_test_seen |>
    left_join(cluster_lookup[, c("AGENCY_ID", col_name)], by = "AGENCY_ID") |>
    mutate(AGENCY_CLUSTER = factor(.data[[col_name]], levels = all_levels)) |>
    select(all_of(c(RF_PREDICTORS, "AGENCY_CLUSTER", "log_wp")))

  n_na_train <- sum(is.na(train_cl$AGENCY_CLUSTER))
  n_na_seen  <- sum(is.na(test_cl_seen$AGENCY_CLUSTER))
  if (n_na_train > 0) cat(sprintf("  WARNING: %d NA AGENCY_CLUSTER in training\n", n_na_train))
  if (n_na_seen  > 0) cat(sprintf("  WARNING: %d NA AGENCY_CLUSTER in seen test\n", n_na_seen))

  t0 <- proc.time()
  rf_cl <- ranger(
    log_wp ~ .,
    data                      = train_cl,
    num.trees                 = RF_NUM_TREES,
    mtry                      = RF_MTRY,
    min.node.size             = RF_MNS,
    replace                   = RF_REPLACE,
    sample.fraction           = RF_SF,
    importance                = RF_IMP,
    respect.unordered.factors = RF_UNORD,
    seed                      = SEED
  )
  rt <- round((proc.time() - t0)["elapsed"], 1)

  oob_rmse <- round(sqrt(rf_cl$prediction.error), 4)
  oob_r2   <- round(rf_cl$r.squared, 4)

  pred_train_v <- predict(rf_cl, data = train_cl)$predictions
  pred_full_v  <- predict(rf_cl, data = test_cl_full)$predictions
  pred_seen_v  <- predict(rf_cl, data = test_cl_seen)$predictions

  perf_train <- eval_preds(wp_train_rf$log_wp,  pred_train_v, n_train)
  perf_full  <- eval_preds(wp_test_rf$log_wp,   pred_full_v,  n_test)
  perf_seen  <- eval_preds(wp_test_seen$log_wp, pred_seen_v,  n_test_seen)

  delta_seen_r2  <- round(perf_seen$r2  - perf_base_seen$r2,  4)
  delta_full_r2  <- round(perf_full$r2  - perf_base_full$r2,  4)
  delta_oob_rmse <- round(oob_rmse      - oob_rmse_base,       4)

  imp_raw   <- sort(rf_cl$variable.importance, decreasing = TRUE)
  imp_total <- sum(abs(imp_raw))
  cl_rank   <- which(names(imp_raw) == "AGENCY_CLUSTER")
  cl_imp_pct <- if (length(cl_rank) > 0) {
    round(unname(imp_raw["AGENCY_CLUSTER"]) / imp_total * 100, 2)
  } else NA_real_

  imp_list <- lapply(seq_along(imp_raw), function(i) {
    list(variable     = names(imp_raw)[i],
         importance   = round(imp_raw[[i]], 6),
         rank         = i,
         pct_of_total = round(imp_raw[[i]] / imp_total * 100, 2))
  })

  train_cluster_dist <- as.list(table(train_cl$AGENCY_CLUSTER))
  seen_cluster_dist  <- as.list(table(test_cl_seen$AGENCY_CLUSTER))

  cat(sprintf(
    "  Done %.1fs  OOB RMSE=%.4f (d=%+.4f)  seen R2=%.4f (d=%+.4f)  full R2=%.4f\n",
    rt, oob_rmse, delta_oob_rmse, perf_seen$r2, delta_seen_r2, perf_full$r2))

  pred_seen_vecs[[k_label]] <- pred_seen_v

  k_results[[k_label]] <- list(
    k                      = k,
    model_id               = model_id,
    n_predictors           = length(RF_PREDICTORS) + 1L,
    n_clusters             = k,
    oob_rmse               = oob_rmse,
    oob_r2                 = oob_r2,
    delta_oob_rmse_vs_base = delta_oob_rmse,
    runtime_s              = rt,
    perf_train             = perf_train,
    perf_seen              = perf_seen,
    perf_full              = perf_full,
    delta_seen_r2          = delta_seen_r2,
    delta_full_r2          = delta_full_r2,
    importance             = imp_list,
    agency_cluster_rank    = if (length(cl_rank) > 0) as.integer(cl_rank) else NA_integer_,
    agency_cluster_imp_pct = cl_imp_pct,
    train_cluster_dist     = train_cluster_dist,
    seen_cluster_dist      = seen_cluster_dist
  )

  rm(rf_cl, train_cl, test_cl_full, test_cl_seen, pred_train_v, pred_full_v)
  invisible(gc())
}

cat("\n")

# =============================================================================
# Section 7: OOB-based k selection
# =============================================================================

oob_rmse_by_k <- sapply(K_VALUES, function(k) k_results[[sprintf("k%d", k)]]$oob_rmse)
names(oob_rmse_by_k) <- as.character(K_VALUES)
best_k_idx   <- which.min(oob_rmse_by_k)
best_k       <- K_VALUES[best_k_idx]
best_k_label <- sprintf("k%d", best_k)
best_k_col   <- sprintf("CLUSTER_k%d", best_k)

best_delta_seen <- k_results[[best_k_label]]$delta_seen_r2

cat("OOB-based k selection:\n")
for (i in seq_along(K_VALUES)) {
  cat(sprintf("  k=%2d  OOB RMSE=%.4f  delta=%+.4f  %s\n",
              K_VALUES[i], oob_rmse_by_k[i],
              oob_rmse_by_k[i] - oob_rmse_base,
              if (K_VALUES[i] == best_k) "<-- SELECTED" else ""))
}
cat(sprintf("\n  Best k=%d  (OOB RMSE=%.4f vs baseline=%.4f, d=%+.4f)\n",
            best_k, oob_rmse_by_k[best_k_idx], oob_rmse_base,
            oob_rmse_by_k[best_k_idx] - oob_rmse_base))
cat(sprintf("  Seen-agency test R2 delta for k=%d: %+.4f\n\n", best_k, best_delta_seen))

gain_category <- if (best_delta_seen >= 0.01) {
  "MATERIAL"
} else if (best_delta_seen >= 0.005) {
  "MODEST"
} else {
  "NONE"
}

recommendation <- switch(gain_category,
  MATERIAL = list(
    decision      = "MATERIAL_GAIN_RECOMMEND_CLUSTER",
    best_k        = best_k,
    threshold_used = "delta seen-agency test R2 >= 0.01",
    justification  = sprintf(
      paste0("Best k by OOB RMSE: k=%d (OOB RMSE=%.4f vs baseline=%.4f, d=%+.4f). ",
             "Seen-agency test R2 improves by %+.4f (threshold: >=0.01 = material). ",
             "Agency cluster provides material predictive lift. ",
             "Recommend RF_CLUSTER_k%d as primary clustered specification."),
      best_k, oob_rmse_by_k[best_k_idx], oob_rmse_base,
      oob_rmse_by_k[best_k_idx] - oob_rmse_base, best_delta_seen, best_k
    )
  ),
  MODEST = list(
    decision      = "MODEST_GAIN_RECOMMEND_CLUSTER",
    best_k        = best_k,
    threshold_used = "delta seen-agency test R2 in [0.005, 0.01)",
    justification  = sprintf(
      paste0("Best k by OOB RMSE: k=%d (OOB RMSE=%.4f vs baseline=%.4f, d=%+.4f). ",
             "Seen-agency test R2 improves by %+.4f (threshold: 0.005-0.01 = modest). ",
             "Consider RF_CLUSTER_k%d if cluster interpretability adds value."),
      best_k, oob_rmse_by_k[best_k_idx], oob_rmse_base,
      oob_rmse_by_k[best_k_idx] - oob_rmse_base, best_delta_seen, best_k
    )
  ),
  NONE = list(
    decision      = "NO_MATERIAL_GAIN_FROM_CLUSTERING",
    best_k        = best_k,
    threshold_used = "delta seen-agency test R2 < 0.005",
    justification  = sprintf(
      paste0("Best k by OOB RMSE: k=%d (OOB RMSE=%.4f vs baseline=%.4f, d=%+.4f). ",
             "Seen-agency test R2 improves by only %+.4f (threshold: <0.005 = no material gain). ",
             "Agency cluster adds no material predictive lift. ",
             "Retain RF_BASE (equivalent to RF_1_SAFE_TUNED) as primary RF specification."),
      best_k, oob_rmse_by_k[best_k_idx], oob_rmse_base,
      oob_rmse_by_k[best_k_idx] - oob_rmse_base, best_delta_seen
    )
  )
)

cat(sprintf("  Decision: %s\n\n", recommendation$decision))

# =============================================================================
# Section 8: Predictions sample (seen-agency test rows — visualization only)
# =============================================================================

cat("Building predictions sample...\n")
n_years_test  <- length(unique(wp_test_seen$STAT_PROFILE_DATE_YEAR))
n_per_year    <- ceiling(min(2000L, n_test_seen) / n_years_test)

set.seed(SEED)
samp_df <- wp_test_seen |>
  mutate(.row_idx = row_number()) |>
  group_by(STAT_PROFILE_DATE_YEAR) |>
  slice_sample(n = n_per_year) |>
  ungroup() |>
  slice_head(n = 2000L)

samp_idx        <- samp_df$.row_idx
samp_pred_base  <- pred_base_seen_v[samp_idx]
samp_pred_bestk <- pred_seen_vecs[[best_k_label]][samp_idx]
samp_cluster    <- cluster_lookup[[best_k_col]][
  match(samp_df$AGENCY_ID, cluster_lookup$AGENCY_ID)
]

cat(sprintf("  %d rows sampled (%d per year from %s seen-agency test rows)\n\n",
            nrow(samp_df), n_per_year, format(n_test_seen, big.mark = ",")))

# =============================================================================
# Section 9: Assemble JSON outputs
# =============================================================================

cat("Assembling JSON outputs...\n")

# Lean summary per k (no per-cluster distributions — those go in diagnostics)
clustered_rf_summary <- lapply(K_VALUES, function(k) {
  r <- k_results[[sprintf("k%d", k)]]
  list(k                      = r$k,
       model_id               = r$model_id,
       oob_rmse               = r$oob_rmse,
       oob_r2                 = r$oob_r2,
       delta_oob_rmse_vs_base = r$delta_oob_rmse_vs_base,
       runtime_s              = r$runtime_s,
       perf_train             = r$perf_train,
       perf_seen              = r$perf_seen,
       perf_full              = r$perf_full,
       delta_seen_r2          = r$delta_seen_r2,
       delta_full_r2          = r$delta_full_r2,
       agency_cluster_rank    = r$agency_cluster_rank,
       agency_cluster_imp_pct = r$agency_cluster_imp_pct)
})
names(clustered_rf_summary) <- paste0("k", K_VALUES)

# ------------------------------------------------------------------
# rf_agency_cluster_summary.json
# ------------------------------------------------------------------

summary_out <- list(
  description = paste0(
    "RF agency cluster sensitivity test for Dataset A Part 2. ",
    "K-means (k=5,10,20,30,50) on 2006-2012 agency profiles ",
    "(", length(scale_cols), " z-scored features, ", n_prof, " agencies). ",
    "Agency cluster labels joined to row-level data as AGENCY_CLUSTER predictor. ",
    "Baseline RF (no cluster) vs 5 clustered RFs trained on full 2006-2012 training population. ",
    "Primary evaluation: seen-agency test rows (agencies present in training). ",
    "Secondary: full 2013-2014 test with NEW_AGENCY fallback for unseen agencies. ",
    "K selection criterion: OOB RMSE only (test set not used for model selection)."
  ),
  modeling_population = list(
    target              = "log_wp = log(WRTN_PREM_AMT + 1)",
    train_years         = "2006-2012",
    test_years          = "2013-2014",
    n_train_rows        = n_train,
    n_test_rows         = n_test,
    n_test_seen_rows    = n_test_seen,
    n_test_new_rows     = n_test_new,
    n_train_agencies    = n_train_agencies,
    n_test_agencies     = n_test_agencies,
    n_seen_agencies     = n_seen_agencies,
    n_new_agencies      = n_new_agencies,
    pct_test_rows_seen  = round(100 * n_test_seen / n_test, 2)
  ),
  hyperparameters = list(
    num_trees               = RF_NUM_TREES,
    mtry                    = RF_MTRY,
    min_node_size           = RF_MNS,
    sample_fraction         = RF_SF,
    replace                 = RF_REPLACE,
    importance              = RF_IMP,
    respect_unordered_factors = RF_UNORD,
    seed                    = SEED,
    note = paste0(
      "mtry=3 fixed for all models (baseline p=10, clustered p=11). ",
      "Same hyperparameters as RF_1_SAFE_TUNED from rf_summary.json."
    )
  ),
  clustering_config = list(
    k_values    = K_VALUES,
    nstart      = NSTART_KM,
    iter_max    = ITER_KM,
    seed        = SEED,
    n_features  = length(scale_cols),
    n_agencies  = n_prof,
    source_file = "agency_profile_clustering_scaled.csv",
    note = "Full k-means statistics in rf_agency_cluster_diagnostics.json"
  ),
  RF_BASE = list(
    model_id        = "RF_BASE",
    n_predictors    = length(RF_PREDICTORS),
    oob_rmse        = oob_rmse_base,
    oob_r2          = oob_r2_base,
    runtime_s       = rt_base,
    perf_train      = perf_base_train,
    perf_seen       = perf_base_seen,
    perf_full       = perf_base_full
  ),
  clustered_rf_results = clustered_rf_summary,
  oob_k_selection = list(
    method            = "minimum OOB RMSE across k values",
    baseline_oob_rmse = oob_rmse_base,
    oob_rmse_by_k     = as.list(oob_rmse_by_k),
    best_k            = best_k,
    best_oob_rmse     = unname(oob_rmse_by_k[best_k_idx])
  ),
  recommendation = recommendation
)

# ------------------------------------------------------------------
# rf_agency_cluster_model_comparison.json
# ------------------------------------------------------------------

comparison_models <- c(
  list(list(
    model_id            = "RF_BASE",
    k                   = NA,
    n_predictors        = length(RF_PREDICTORS),
    oob_rmse            = oob_rmse_base,
    oob_r2              = oob_r2_base,
    perf_train          = perf_base_train,
    seen_agency_test    = perf_base_seen,
    full_test           = perf_base_full,
    delta_seen_r2       = 0.0,
    delta_full_r2       = 0.0,
    is_oob_selected     = FALSE,
    is_baseline         = TRUE
  )),
  lapply(K_VALUES, function(k) {
    r <- k_results[[sprintf("k%d", k)]]
    list(
      model_id         = r$model_id,
      k                = k,
      n_predictors     = r$n_predictors,
      oob_rmse         = r$oob_rmse,
      oob_r2           = r$oob_r2,
      perf_train       = r$perf_train,
      seen_agency_test = r$perf_seen,
      full_test        = r$perf_full,
      delta_seen_r2    = r$delta_seen_r2,
      delta_full_r2    = r$delta_full_r2,
      is_oob_selected  = (k == best_k),
      is_baseline      = FALSE
    )
  })
)

comparison_out <- list(
  description = paste0(
    "Model-by-model performance comparison. ",
    "Primary: seen-agency test rows (n=", format(n_test_seen, big.mark = ","), "). ",
    "Secondary: full test (n=", format(n_test, big.mark = ","), ") with NEW_AGENCY fallback. ",
    "delta_seen_r2 / delta_full_r2: relative to RF_BASE. ",
    "OOB metrics from 2006-2012 training (no test data in k selection). ",
    "Gain thresholds: delta seen R2 < 0.005 = none; 0.005-0.01 = modest; > 0.01 = material."
  ),
  n_test_seen   = n_test_seen,
  n_test_full   = n_test,
  selected_k    = best_k,
  gain_category = gain_category,
  models        = comparison_models
)

# ------------------------------------------------------------------
# rf_agency_cluster_diagnostics.json
# ------------------------------------------------------------------

cluster_dist_by_k <- lapply(K_VALUES, function(k) {
  k_label <- sprintf("k%d", k)
  r       <- k_results[[k_label]]
  list(
    k = k,
    training_row_distribution  = r$train_cluster_dist,
    seen_test_row_distribution = r$seen_cluster_dist,
    n_new_agency_test_rows     = n_test_new
  )
})
names(cluster_dist_by_k) <- paste0("k", K_VALUES)

diagnostics_out <- list(
  description = paste0(
    "Clustering diagnostics: k-means fit statistics, agency coverage, ",
    "cluster label distributions in training and seen-agency test rows, ",
    "and OOB RMSE trajectory across k values."
  ),
  agency_coverage = list(
    n_train_agencies      = n_train_agencies,
    n_test_agencies       = n_test_agencies,
    n_seen_agencies       = n_seen_agencies,
    n_new_agencies        = n_new_agencies,
    pct_seen_agencies     = round(100 * n_seen_agencies / n_test_agencies, 2),
    pct_new_agencies      = round(100 * n_new_agencies  / n_test_agencies, 2),
    n_test_seen_rows      = n_test_seen,
    n_test_new_rows       = n_test_new,
    pct_test_rows_seen    = round(100 * n_test_seen / n_test, 2),
    new_agency_fallback   = paste0(
      "NEW_AGENCY factor level — assigned to 2013-2014 test rows from agencies ",
      "absent in 2006-2012 training. Included in full test secondary evaluation; ",
      "excluded from seen-agency primary evaluation."
    )
  ),
  kmeans_fit_statistics = kmeans_stats,
  oob_rmse_trajectory = list(
    baseline_oob_rmse = oob_rmse_base,
    by_k = lapply(K_VALUES, function(k) {
      r <- k_results[[sprintf("k%d", k)]]
      list(k            = k,
           oob_rmse     = r$oob_rmse,
           delta_vs_base = r$delta_oob_rmse_vs_base)
    })
  ),
  cluster_distributions = cluster_dist_by_k
)

# ------------------------------------------------------------------
# rf_agency_cluster_importance.json
# ------------------------------------------------------------------

imp_base_list <- lapply(seq_along(imp_base_raw), function(i) {
  list(variable     = names(imp_base_raw)[i],
       importance   = round(imp_base_raw[[i]], 6),
       rank         = i,
       pct_of_total = round(imp_base_raw[[i]] / imp_base_total * 100, 2))
})

importance_out <- list(
  description = paste0(
    "Permutation feature importance for RF_BASE and all 5 clustered RF models. ",
    "agency_cluster_rank shows where AGENCY_CLUSTER ranks among all predictors; ",
    "agency_cluster_imp_pct shows its share of total permutation importance. ",
    "Rank 1 = most important."
  ),
  RF_BASE = list(
    model_id     = "RF_BASE",
    n_predictors = length(RF_PREDICTORS),
    importance   = imp_base_list
  ),
  clustered_rf_importance = lapply(K_VALUES, function(k) {
    r <- k_results[[sprintf("k%d", k)]]
    list(
      model_id               = r$model_id,
      k                      = k,
      n_predictors           = r$n_predictors,
      agency_cluster_rank    = r$agency_cluster_rank,
      agency_cluster_imp_pct = r$agency_cluster_imp_pct,
      importance             = r$importance
    )
  })
)

# ------------------------------------------------------------------
# rf_agency_cluster_predictions_sample.json
# ------------------------------------------------------------------

pred_sample_out <- list(
  description = paste0(
    "VISUALIZATION ONLY — do not use for model evaluation. ",
    nrow(samp_df), " rows sampled from ", format(n_test_seen, big.mark = ","),
    " seen-agency test rows (2013-2014 agencies present in 2006-2012 training). ",
    "RF_BASE: baseline RF (no cluster). ",
    "RF_CLUSTER_k", best_k, ": OOB-selected clustered RF. ",
    "All official metrics computed on the full seen-agency test set and reported in ",
    "rf_agency_cluster_model_comparison.json."
  ),
  n_rows        = nrow(samp_df),
  baseline_model = "RF_BASE",
  cluster_model  = sprintf("RF_CLUSTER_k%d", best_k),
  selected_k     = best_k,
  predictions    = lapply(seq_len(nrow(samp_df)), function(i) {
    r <- samp_df[i, ]
    list(
      AGENCY_ID              = r$AGENCY_ID,
      AGENCY_CLUSTER         = samp_cluster[i],
      STATE_ABBR             = as.character(r$STATE_ABBR),
      PROD_ABBR              = as.character(r$PROD_ABBR),
      VENDOR                 = as.character(r$VENDOR),
      STAT_PROFILE_DATE_YEAR = r$STAT_PROFILE_DATE_YEAR,
      actual_log_wp          = round(r$log_wp, 6),
      actual_wp              = round(exp(r$log_wp) - 1, 2),
      rf_base_pred_log_wp    = round(samp_pred_base[i],  6),
      rf_base_pred_wp        = round(pmax(exp(samp_pred_base[i])  - 1, 0), 2),
      rf_cluster_pred_log_wp = round(samp_pred_bestk[i], 6),
      rf_cluster_pred_wp     = round(pmax(exp(samp_pred_bestk[i]) - 1, 0), 2),
      rf_base_residual_log    = round(r$log_wp - samp_pred_base[i],  6),
      rf_cluster_residual_log = round(r$log_wp - samp_pred_bestk[i], 6)
    )
  })
)

# =============================================================================
# Section 10: Write outputs
# =============================================================================

cat("\nWriting outputs...\n")
write_json_out(summary_out,       "rf_agency_cluster_summary.json")
write_json_out(comparison_out,    "rf_agency_cluster_model_comparison.json")
write_json_out(diagnostics_out,   "rf_agency_cluster_diagnostics.json")
write_json_out(importance_out,    "rf_agency_cluster_importance.json")
write_json_out(pred_sample_out,   "rf_agency_cluster_predictions_sample.json")

cat(sprintf("\n=== RF agency cluster sensitivity test complete ===\nOutputs: %s\n", output_dir))
