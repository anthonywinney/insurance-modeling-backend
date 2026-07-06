# =============================================================================
# Dataset A Part 2 — Agency Hierarchy Diagnostics
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_agency_hierarchy_diagnostics.R
#
# Output: modeling/dataset_a_part2/outputs/agency_hierarchy_diagnostics.json
#
# Purpose: Investigate AGENCY_ID and PRIMARY_AGENCY_ID before LMM fitting.
# No models are fit in this script.
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI); library(RSQLite); library(dplyr); library(jsonlite)
})

db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

cat("=== Agency Hierarchy Diagnostics ===\n\n")

# =============================================================================
# Load data and build effective modeling population
# =============================================================================

cat("Loading data...\n")
con <- dbConnect(SQLite(), db_path)
raw <- dbReadTable(con, "agency_performance")
dbDisconnect(con)
cat(sprintf("  %s rows x %s columns\n", format(nrow(raw), big.mark = ","), ncol(raw)))

# Verify PRIMARY_AGENCY_ID exists
if (!"PRIMARY_AGENCY_ID" %in% names(raw)) {
  stop("PRIMARY_AGENCY_ID not found in agency_performance table. Columns: ",
       paste(names(raw), collapse = ", "))
}

cat("Building effective modeling population (same filters as OLS/RF)...\n")

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
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  )

n_cand_train <- sum(wp_base$STAT_PROFILE_DATE_YEAR %in% 2006:2012)
n_cand_test  <- sum(wp_base$STAT_PROFILE_DATE_YEAR %in% 2013:2014)

wp_eff <- wp_base |> filter(!is.na(log_prev_wp), !is.na(log_prev_poly))

wp_train <- wp_eff |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2012)
wp_test  <- wp_eff |> filter(STAT_PROFILE_DATE_YEAR %in% 2013:2014)
n_eff_train <- nrow(wp_train)
n_eff_test  <- nrow(wp_test)

cat(sprintf("  Candidate : %s train / %s test\n",
            format(n_cand_train, big.mark = ","), format(n_cand_test, big.mark = ",")))
cat(sprintf("  Effective : %s train / %s test",
            format(n_eff_train, big.mark = ","), format(n_eff_test, big.mark = ",")))

row_count_match_note <- if (n_eff_train == 103377L && n_eff_test == 30981L) {
  cat("  [matches expected 103,377 / 30,981]\n\n")
  "Matches expected 103,377 train / 30,981 test."
} else {
  note <- sprintf("MISMATCH: expected 103,377 train / 30,981 test, got %d / %d.", n_eff_train, n_eff_test)
  cat(sprintf("  [%s]\n\n", note))
  note
}

# =============================================================================
# Helper functions
# =============================================================================

pct <- function(n, d) round(n / d * 100, 2)

quantile_summary <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(min=NA,p25=NA,median=NA,mean=NA,p75=NA,p90=NA,p95=NA,p99=NA,max=NA))
  q <- quantile(x, probs = c(0.25, 0.75, 0.90, 0.95, 0.99))
  list(
    min    = round(min(x), 4),
    p25    = round(q[["25%"]], 4),
    median = round(median(x), 4),
    mean   = round(mean(x), 4),
    p75    = round(q[["75%"]], 4),
    p90    = round(q[["90%"]], 4),
    p95    = round(q[["95%"]], 4),
    p99    = round(q[["99%"]], 4),
    max    = round(max(x), 4)
  )
}

id_coverage <- function(df, col, label) {
  vals <- df[[col]]
  n    <- nrow(df)
  n_null  <- sum(is.na(vals))
  n_blank <- if (is.character(vals)) sum(vals == "", na.rm = TRUE) else 0L
  n_valid <- n - n_null - n_blank
  uniq    <- length(unique(vals[!is.na(vals)]))
  rows_per <- df |> group_by(.data[[col]]) |> summarise(n = n(), .groups = "drop") |> pull(n)
  top20 <- df |>
    group_by(.data[[col]]) |>
    summarise(row_count = n(), .groups = "drop") |>
    arrange(desc(row_count)) |>
    slice_head(n = 20) |>
    mutate(row_pct = pct(row_count, n)) |>
    as.list() |>
    (\(x) lapply(seq_len(length(x[[1]])), function(i) lapply(x, `[[`, i)))()
  list(
    label           = label,
    n_rows          = n,
    n_null          = n_null,
    n_blank         = n_blank,
    n_valid         = n_valid,
    n_distinct      = uniq,
    min_val         = if (n_valid > 0) min(vals, na.rm = TRUE) else NA,
    max_val         = if (n_valid > 0) max(vals, na.rm = TRUE) else NA,
    rows_per_id     = quantile_summary(rows_per),
    top20_by_freq   = top20
  )
}

# =============================================================================
# PART A — Basic ID coverage
# =============================================================================

cat("Part A: Basic ID coverage...\n")

part_a <- list(
  agency_id = list(
    full   = id_coverage(wp_eff,   "AGENCY_ID",         "full effective population"),
    train  = id_coverage(wp_train, "AGENCY_ID",         "train 2006-2012"),
    test   = id_coverage(wp_test,  "AGENCY_ID",         "test 2013-2014")
  ),
  primary_agency_id = list(
    full   = id_coverage(wp_eff,   "PRIMARY_AGENCY_ID", "full effective population"),
    train  = id_coverage(wp_train, "PRIMARY_AGENCY_ID", "train 2006-2012"),
    test   = id_coverage(wp_test,  "PRIMARY_AGENCY_ID", "test 2013-2014")
  )
)

cat(sprintf("  AGENCY_ID      : %d distinct (full pop)\n", part_a$agency_id$full$n_distinct))
cat(sprintf("  PRIMARY_AGENCY_ID: %d distinct (full pop)\n", part_a$primary_agency_id$full$n_distinct))

# =============================================================================
# PART B — Sentinel / placeholder detection in PRIMARY_AGENCY_ID
# =============================================================================

cat("Part B: Sentinel detection...\n")

pai      <- wp_eff$PRIMARY_AGENCY_ID
n_eff    <- nrow(wp_eff)
n_agencies <- length(unique(wp_eff$AGENCY_ID))

# Common numeric sentinel candidates
sentinel_candidates_numeric <- c(0, -1, 99999, 999999, 9999999, 99997, 99998, 99999)
if (is.numeric(pai)) {
  sentinel_hits <- lapply(unique(sentinel_candidates_numeric), function(v) {
    n_rows <- sum(pai == v, na.rm = TRUE)
    if (n_rows == 0) return(NULL)
    rows_train <- sum(wp_train$PRIMARY_AGENCY_ID == v, na.rm = TRUE)
    rows_test  <- sum(wp_test$PRIMARY_AGENCY_ID  == v, na.rm = TRUE)
    agencies   <- length(unique(wp_eff$AGENCY_ID[pai == v & !is.na(pai)]))
    yrs        <- range(wp_eff$STAT_PROFILE_DATE_YEAR[pai == v & !is.na(pai)], na.rm = TRUE)
    list(
      value             = v,
      row_count         = n_rows,
      row_pct           = pct(n_rows, n_eff),
      distinct_agencies = agencies,
      agency_pct        = pct(agencies, n_agencies),
      train_rows        = rows_train,
      test_rows         = rows_test,
      first_year        = yrs[1],
      last_year         = yrs[2],
      hypothesis        = "Numeric sentinel candidate"
    )
  })
  sentinel_hits <- Filter(Negate(is.null), sentinel_hits)
} else {
  # Character field: check blank, "0", "NA", "NONE", etc.
  char_sentinels <- c("0", "-1", "NA", "NONE", "UNKNOWN", "NULL", "", "99999", "999999")
  sentinel_hits <- lapply(char_sentinels, function(v) {
    n_rows <- sum(pai == v, na.rm = TRUE)
    if (n_rows == 0) return(NULL)
    rows_train <- sum(wp_train$PRIMARY_AGENCY_ID == v, na.rm = TRUE)
    rows_test  <- sum(wp_test$PRIMARY_AGENCY_ID  == v, na.rm = TRUE)
    agencies   <- length(unique(wp_eff$AGENCY_ID[pai == v & !is.na(pai)]))
    yrs        <- range(wp_eff$STAT_PROFILE_DATE_YEAR[pai == v & !is.na(pai)], na.rm = TRUE)
    list(
      value             = v,
      row_count         = n_rows,
      row_pct           = pct(n_rows, n_eff),
      distinct_agencies = agencies,
      agency_pct        = pct(agencies, n_agencies),
      train_rows        = rows_train,
      test_rows         = rows_test,
      first_year        = yrs[1],
      last_year         = yrs[2],
      hypothesis        = "Character sentinel candidate"
    )
  })
  sentinel_hits <- Filter(Negate(is.null), sentinel_hits)
}

# Self-parenting: PRIMARY_AGENCY_ID == AGENCY_ID
self_parent_rows <- sum(wp_eff$PRIMARY_AGENCY_ID == wp_eff$AGENCY_ID, na.rm = TRUE)
self_parent_agencies <- wp_eff |>
  group_by(AGENCY_ID) |>
  summarise(all_self = all(PRIMARY_AGENCY_ID == AGENCY_ID, na.rm = TRUE),
            any_self = any(PRIMARY_AGENCY_ID == AGENCY_ID, na.rm = TRUE), .groups = "drop")
n_always_self <- sum(self_parent_agencies$all_self)
n_sometimes_self <- sum(self_parent_agencies$any_self) - n_always_self

# Top parents by distinct agency count — to find catch-all buckets
top_parents_by_agency <- wp_eff |>
  group_by(PRIMARY_AGENCY_ID) |>
  summarise(n_rows = n(), n_agencies = n_distinct(AGENCY_ID), .groups = "drop") |>
  arrange(desc(n_agencies)) |>
  slice_head(n = 20) |>
  mutate(row_pct = pct(n_rows, n_eff), agency_pct = pct(n_agencies, .env$n_agencies))

# Top parents by row count
top_parents_by_row <- wp_eff |>
  group_by(PRIMARY_AGENCY_ID) |>
  summarise(n_rows = n(), n_agencies = n_distinct(AGENCY_ID), .groups = "drop") |>
  arrange(desc(n_rows)) |>
  slice_head(n = 20) |>
  mutate(row_pct = pct(n_rows, n_eff), agency_pct = pct(n_agencies, .env$n_agencies))

# Concentration check: does top-1 parent hold >10% of rows or >10% of agencies?
top1_pai_rows    <- top_parents_by_row$n_rows[1]
top1_pai_agencies <- top_parents_by_agency$n_agencies[1]
top1_pai_id      <- top_parents_by_row$PRIMARY_AGENCY_ID[1]

# Classify suspicious: sentinel hit OR self-parent with large share OR >10% concentration
suspicious_ids <- unique(c(
  sapply(sentinel_hits, `[[`, "value"),
  if (self_parent_rows / n_eff > 0.01) {
    # self-parenting is widespread — collect all self-parent IDs
    wp_eff$PRIMARY_AGENCY_ID[wp_eff$PRIMARY_AGENCY_ID == wp_eff$AGENCY_ID]
  } else character(0),
  # top parents with implausibly many agencies (>5% of all agencies)
  top_parents_by_agency$PRIMARY_AGENCY_ID[top_parents_by_agency$agency_pct > 5]
))

# Build detailed suspicious table
build_suspicious_detail <- function(pid) {
  sub <- wp_eff[wp_eff$PRIMARY_AGENCY_ID == pid & !is.na(wp_eff$PRIMARY_AGENCY_ID), ]
  if (nrow(sub) == 0) return(NULL)
  yrs <- range(sub$STAT_PROFILE_DATE_YEAR, na.rm = TRUE)
  self <- pid == sub$AGENCY_ID[1]   # crude check
  n_self_rows <- sum(wp_eff$AGENCY_ID == pid & wp_eff$PRIMARY_AGENCY_ID == pid, na.rm = TRUE)
  list(
    primary_agency_id = pid,
    row_count         = nrow(sub),
    row_pct           = pct(nrow(sub), n_eff),
    distinct_agencies = n_distinct(sub$AGENCY_ID),
    agency_pct        = pct(n_distinct(sub$AGENCY_ID), n_agencies),
    train_rows        = sum(sub$STAT_PROFILE_DATE_YEAR %in% 2006:2012),
    test_rows         = sum(sub$STAT_PROFILE_DATE_YEAR %in% 2013:2014),
    first_year        = yrs[1],
    last_year         = yrs[2],
    self_parent_rows  = n_self_rows,
    flags             = paste(c(
      if (pid %in% sapply(sentinel_hits, `[[`, "value")) "numeric_sentinel",
      if (n_distinct(sub$AGENCY_ID) / n_agencies > 0.05) "high_agency_concentration",
      if (nrow(sub) / n_eff > 0.05) "high_row_concentration",
      if (n_self_rows > 0) "self_parent_match_exists"
    ), collapse = ", ")
  )
}

suspicious_detail <- lapply(unique(suspicious_ids), function(pid) {
  tryCatch(build_suspicious_detail(pid), error = function(e) NULL)
})
suspicious_detail <- Filter(Negate(is.null), suspicious_detail)
suspicious_detail <- suspicious_detail[order(sapply(suspicious_detail, `[[`, "row_count"), decreasing = TRUE)]

part_b <- list(
  n_effective_rows     = n_eff,
  n_distinct_agencies  = n_agencies,
  self_parenting = list(
    n_rows_where_pai_equals_aid  = self_parent_rows,
    pct_rows                     = pct(self_parent_rows, n_eff),
    n_agencies_always_self_parent = n_always_self,
    n_agencies_sometimes_self_parent = n_sometimes_self,
    pct_agencies_always_self_parent = pct(n_always_self, n_agencies)
  ),
  numeric_sentinel_candidates  = sentinel_hits,
  top_20_parents_by_agency_count = lapply(seq_len(nrow(top_parents_by_agency)), function(i) {
    r <- top_parents_by_agency[i, ]
    list(primary_agency_id = r$PRIMARY_AGENCY_ID, n_agencies = r$n_agencies,
         agency_pct = r$agency_pct, n_rows = r$n_rows, row_pct = r$row_pct)
  }),
  top_20_parents_by_row_count = lapply(seq_len(nrow(top_parents_by_row)), function(i) {
    r <- top_parents_by_row[i, ]
    list(primary_agency_id = r$PRIMARY_AGENCY_ID, n_rows = r$n_rows,
         row_pct = r$row_pct, n_agencies = r$n_agencies, agency_pct = r$agency_pct)
  }),
  top1_parent_id              = top1_pai_id,
  top1_parent_row_count       = top1_pai_rows,
  top1_parent_row_pct         = pct(top1_pai_rows, n_eff),
  top1_parent_agency_count    = top1_pai_agencies,
  top1_parent_agency_pct      = pct(top1_pai_agencies, n_agencies),
  n_suspicious_ids_flagged    = length(suspicious_detail),
  suspicious_parent_ids       = suspicious_detail
)

cat(sprintf("  Self-parent rows: %d (%.1f%%)\n", self_parent_rows, pct(self_parent_rows, n_eff)))
cat(sprintf("  Sentinel hits: %d\n", length(sentinel_hits)))
cat(sprintf("  Suspicious IDs flagged: %d\n", length(suspicious_detail)))

# =============================================================================
# Identify sentinel IDs for sentinel-adjusted analysis (Parts D, E, F)
# =============================================================================

# Sentinel IDs: those with >5% agency concentration, or numeric sentinel hits,
# or self-parent where agency_pct > 5%
sentinel_pai_ids <- unique(c(
  sapply(sentinel_hits, `[[`, "value"),
  top_parents_by_agency$PRIMARY_AGENCY_ID[top_parents_by_agency$agency_pct > 5]
))
cat(sprintf("  Sentinel-adjusted exclusion list: %d parent IDs\n\n",
            length(sentinel_pai_ids)))

wp_eff_sa    <- wp_eff    |> filter(!PRIMARY_AGENCY_ID %in% sentinel_pai_ids | is.na(PRIMARY_AGENCY_ID))
wp_train_sa  <- wp_train  |> filter(!PRIMARY_AGENCY_ID %in% sentinel_pai_ids | is.na(PRIMARY_AGENCY_ID))
wp_test_sa   <- wp_test   |> filter(!PRIMARY_AGENCY_ID %in% sentinel_pai_ids | is.na(PRIMARY_AGENCY_ID))

# =============================================================================
# PART C — Agency to parent mapping stability
# =============================================================================

cat("Part C: Agency-parent mapping stability...\n")

agency_parent_map <- wp_eff |>
  group_by(AGENCY_ID) |>
  summarise(
    n_distinct_parents      = n_distinct(PRIMARY_AGENCY_ID, na.rm = TRUE),
    primary_agency_ids      = list(sort(unique(PRIMARY_AGENCY_ID))),
    years_observed          = list(sort(unique(STAT_PROFILE_DATE_YEAR))),
    n_years                 = n_distinct(STAT_PROFILE_DATE_YEAR),
    .groups = "drop"
  )

n_one_parent    <- sum(agency_parent_map$n_distinct_parents == 1)
n_multi_parent  <- sum(agency_parent_map$n_distinct_parents > 1)
n_total_agencies_c <- nrow(agency_parent_map)

# Check within-year inconsistency (same agency, same year, different parents)
within_year_inconsistent <- wp_eff |>
  group_by(AGENCY_ID, STAT_PROFILE_DATE_YEAR) |>
  summarise(n_parents_this_year = n_distinct(PRIMARY_AGENCY_ID, na.rm = TRUE), .groups = "drop") |>
  filter(n_parents_this_year > 1)

n_agency_year_inconsistent <- n_distinct(within_year_inconsistent$AGENCY_ID)

# Examples of agencies with multiple parents (up to 15)
multi_parent_agencies <- agency_parent_map |>
  filter(n_distinct_parents > 1) |>
  arrange(desc(n_distinct_parents)) |>
  slice_head(n = 15)

classify_change <- function(aid) {
  sub <- wp_eff |>
    filter(AGENCY_ID == aid) |>
    arrange(STAT_PROFILE_DATE_YEAR) |>
    select(STAT_PROFILE_DATE_YEAR, PRIMARY_AGENCY_ID) |>
    distinct()
  pids    <- sub$PRIMARY_AGENCY_ID
  is_sent <- pids %in% sentinel_pai_ids
  if (all(is_sent[-length(is_sent)]) && !is_sent[length(is_sent)]) return("sentinel_to_real")
  if (!is_sent[1] && any(is_sent[-1])) return("real_to_sentinel")
  if (any(is_sent)) return("mixed_sentinel")
  return("real_reassignment")
}

multi_parent_examples <- lapply(seq_len(nrow(multi_parent_agencies)), function(i) {
  aid <- multi_parent_agencies$AGENCY_ID[i]
  sub <- wp_eff |>
    filter(AGENCY_ID == aid) |>
    group_by(STAT_PROFILE_DATE_YEAR, PRIMARY_AGENCY_ID) |>
    summarise(n_rows = n(), .groups = "drop") |>
    arrange(STAT_PROFILE_DATE_YEAR)
  change_type <- tryCatch(classify_change(aid), error = function(e) "unknown")
  list(
    AGENCY_ID          = aid,
    n_distinct_parents = multi_parent_agencies$n_distinct_parents[i],
    change_type        = change_type,
    parent_by_year     = lapply(seq_len(nrow(sub)), function(j) {
      list(year = sub$STAT_PROFILE_DATE_YEAR[j],
           primary_agency_id = sub$PRIMARY_AGENCY_ID[j],
           n_rows = sub$n_rows[j])
    })
  )
})

part_c <- list(
  n_total_agencies           = n_total_agencies_c,
  n_one_parent               = n_one_parent,
  n_multi_parent             = n_multi_parent,
  pct_one_parent             = pct(n_one_parent,   n_total_agencies_c),
  pct_multi_parent           = pct(n_multi_parent, n_total_agencies_c),
  n_agencies_within_year_inconsistent = n_agency_year_inconsistent,
  pct_within_year_inconsistent = pct(n_agency_year_inconsistent, n_total_agencies_c),
  multi_parent_examples      = multi_parent_examples,
  nested_lmm_threat_assessment = if (pct(n_multi_parent, n_total_agencies_c) < 5) {
    "LOW: <5% of agencies have multiple parents. Hierarchy is largely stable."
  } else if (pct(n_multi_parent, n_total_agencies_c) < 20) {
    "MODERATE: 5-20% of agencies have multiple parents. Hierarchy mostly stable but requires sentinel handling."
  } else {
    "HIGH: >20% of agencies have multiple parents. Hierarchy instability may threaten nested LMM."
  }
)

cat(sprintf("  One parent: %d (%.1f%%)  |  Multiple parents: %d (%.1f%%)\n",
            n_one_parent, pct(n_one_parent, n_total_agencies_c),
            n_multi_parent, pct(n_multi_parent, n_total_agencies_c)))
cat(sprintf("  Within-year inconsistent agencies: %d\n\n", n_agency_year_inconsistent))

# =============================================================================
# PART D — Train/test group overlap
# =============================================================================

cat("Part D: Train/test group overlap...\n")

train_agencies <- unique(wp_train$AGENCY_ID)
test_agencies  <- unique(wp_test$AGENCY_ID)
n_train_ag     <- length(train_agencies)
n_test_ag      <- length(test_agencies)
test_ag_in_train <- intersect(test_agencies, train_agencies)
test_ag_new      <- setdiff(test_agencies, train_agencies)
n_test_rows_known_ag <- sum(wp_test$AGENCY_ID %in% train_agencies)
n_test_rows_new_ag   <- sum(!wp_test$AGENCY_ID %in% train_agencies)

# Parent overlap — raw
train_pai_raw <- unique(wp_train$PRIMARY_AGENCY_ID)
test_pai_raw  <- unique(wp_test$PRIMARY_AGENCY_ID)
n_train_pai   <- length(train_pai_raw)
n_test_pai    <- length(test_pai_raw)
test_pai_in_train <- intersect(test_pai_raw, train_pai_raw)
test_pai_new      <- setdiff(test_pai_raw, train_pai_raw)
n_test_rows_known_pai <- sum(wp_test$PRIMARY_AGENCY_ID %in% train_pai_raw, na.rm = TRUE)
n_test_rows_new_pai   <- sum(!wp_test$PRIMARY_AGENCY_ID %in% train_pai_raw, na.rm = TRUE)

# Parent overlap — sentinel-adjusted
train_pai_sa  <- unique(wp_train_sa$PRIMARY_AGENCY_ID)
test_pai_sa   <- unique(wp_test_sa$PRIMARY_AGENCY_ID)
n_train_pai_sa <- length(train_pai_sa)
n_test_pai_sa  <- length(test_pai_sa)
test_pai_sa_in_train <- intersect(test_pai_sa, train_pai_sa)
test_pai_sa_new      <- setdiff(test_pai_sa,  train_pai_sa)
n_test_rows_known_pai_sa <- sum(wp_test_sa$PRIMARY_AGENCY_ID %in% train_pai_sa, na.rm = TRUE)
n_test_rows_new_pai_sa   <- nrow(wp_test_sa) - n_test_rows_known_pai_sa

n_test <- nrow(wp_test)
n_test_sa <- nrow(wp_test_sa)

part_d <- list(
  agency_id_overlap = list(
    n_train_agencies             = n_train_ag,
    n_test_agencies              = n_test_ag,
    n_test_agencies_in_train     = length(test_ag_in_train),
    n_test_agencies_new          = length(test_ag_new),
    pct_test_agencies_in_train   = pct(length(test_ag_in_train), n_test_ag),
    pct_test_rows_known_agency   = pct(n_test_rows_known_ag, n_test),
    pct_test_rows_new_agency     = pct(n_test_rows_new_ag,   n_test),
    lmm_note = "EBLUP available for seen agencies. New test agencies get random effect = 0."
  ),
  primary_agency_id_raw = list(
    n_train_parents              = n_train_pai,
    n_test_parents               = n_test_pai,
    n_test_parents_in_train      = length(test_pai_in_train),
    n_test_parents_new           = length(test_pai_new),
    pct_test_parents_in_train    = pct(length(test_pai_in_train), n_test_pai),
    pct_test_rows_known_parent   = pct(n_test_rows_known_pai, n_test),
    pct_test_rows_new_parent     = pct(n_test_rows_new_pai,   n_test)
  ),
  primary_agency_id_sentinel_adjusted = list(
    sentinel_ids_excluded        = sentinel_pai_ids,
    n_train_parents              = n_train_pai_sa,
    n_test_parents               = n_test_pai_sa,
    n_test_parents_in_train      = length(test_pai_sa_in_train),
    n_test_parents_new           = length(test_pai_sa_new),
    pct_test_parents_in_train    = pct(length(test_pai_sa_in_train), n_test_pai_sa),
    pct_test_rows_known_parent   = pct(n_test_rows_known_pai_sa, n_test_sa),
    pct_test_rows_new_parent     = pct(n_test_rows_new_pai_sa,   n_test_sa)
  )
)

cat(sprintf("  AGENCY_ID test seen in train: %.1f%% of test agencies, %.1f%% of test rows\n",
            pct(length(test_ag_in_train), n_test_ag),
            pct(n_test_rows_known_ag, n_test)))
cat(sprintf("  PRIMARY_AGENCY_ID (raw) test seen in train: %.1f%% of test parents\n",
            pct(length(test_pai_in_train), n_test_pai)))
cat(sprintf("  PRIMARY_AGENCY_ID (SA)  test seen in train: %.1f%% of test parents\n\n",
            pct(length(test_pai_sa_in_train), n_test_pai_sa)))

# =============================================================================
# PART E — Hierarchy shape
# =============================================================================

cat("Part E: Hierarchy shape...\n")

hierarchy_stats <- function(df, label) {
  h <- df |>
    group_by(PRIMARY_AGENCY_ID) |>
    summarise(n_rows = n(), n_agencies = n_distinct(AGENCY_ID), .groups = "drop")
  list(
    label                 = label,
    n_parent_ids          = nrow(h),
    n_agencies            = sum(h$n_agencies),
    agencies_per_parent   = quantile_summary(h$n_agencies),
    rows_per_parent       = quantile_summary(h$n_rows),
    singletons_pct        = pct(sum(h$n_agencies == 1), nrow(h)),
    top_parent_agency_pct = pct(max(h$n_agencies), sum(h$n_agencies))
  )
}

rows_per_agency_summary <- quantile_summary(
  (wp_eff |> group_by(AGENCY_ID) |> summarise(n = n(), .groups = "drop"))$n
)

part_e <- list(
  rows_per_agency           = rows_per_agency_summary,
  hierarchy_raw             = hierarchy_stats(wp_eff, "raw PRIMARY_AGENCY_ID"),
  hierarchy_sentinel_adjusted = hierarchy_stats(wp_eff_sa, "sentinel-adjusted PRIMARY_AGENCY_ID"),
  assessment = {
    h_sa <- hierarchy_stats(wp_eff_sa, "sa")
    singletons <- h_sa$singletons_pct
    top_conc   <- h_sa$top_parent_agency_pct
    if (singletons > 50) "MOSTLY SINGLETONS: most parents have only one child agency — nested structure adds little over agency-only LMM"
    else if (top_conc > 20) "HIGH CONCENTRATION: one parent dominates agency count — hierarchy is unbalanced"
    else "MEANINGFUL HIERARCHY: multiple agencies per parent with reasonable balance"
  }
)

cat(sprintf("  Raw:  %d parents, median agencies/parent = %.0f\n",
            part_e$hierarchy_raw$n_parent_ids,
            part_e$hierarchy_raw$agencies_per_parent$median))
cat(sprintf("  SA:   %d parents, median agencies/parent = %.0f, singletons = %.1f%%\n\n",
            part_e$hierarchy_sentinel_adjusted$n_parent_ids,
            part_e$hierarchy_sentinel_adjusted$agencies_per_parent$median,
            part_e$hierarchy_sentinel_adjusted$singletons_pct))

# =============================================================================
# PART F — Descriptive variance pre-check
# =============================================================================

cat("Part F: Variance pre-check...\n")

# Agency-level
agency_means <- wp_eff |>
  group_by(AGENCY_ID) |>
  summarise(mean_log_wp = mean(log_wp, na.rm = TRUE),
            n_rows = n(), .groups = "drop")

between_agency_var <- var(agency_means$mean_log_wp, na.rm = TRUE)

# Overall within-agency variance: mean of within-agency variances
within_vars <- wp_eff |>
  group_by(AGENCY_ID) |>
  summarise(within_var = var(log_wp, na.rm = TRUE), n = n(), .groups = "drop") |>
  filter(n > 1)
mean_within_agency_var <- mean(within_vars$within_var, na.rm = TRUE)

icc_agency_approx <- between_agency_var / (between_agency_var + mean_within_agency_var)

# Parent-level (raw)
parent_means_raw <- wp_eff |>
  group_by(PRIMARY_AGENCY_ID) |>
  summarise(mean_log_wp = mean(log_wp, na.rm = TRUE), n_rows = n(), .groups = "drop")

between_parent_var_raw <- var(parent_means_raw$mean_log_wp, na.rm = TRUE)

# Parent-level (sentinel-adjusted)
parent_means_sa <- wp_eff_sa |>
  group_by(PRIMARY_AGENCY_ID) |>
  summarise(mean_log_wp = mean(log_wp, na.rm = TRUE), n_rows = n(), .groups = "drop")

between_parent_var_sa <- var(parent_means_sa$mean_log_wp, na.rm = TRUE)

# Overall variance of log_wp
total_var <- var(wp_eff$log_wp, na.rm = TRUE)

part_f <- list(
  total_log_wp_variance           = round(total_var, 4),
  agency_level = list(
    n_agencies                    = nrow(agency_means),
    between_agency_variance       = round(between_agency_var, 4),
    mean_within_agency_variance   = round(mean_within_agency_var, 4),
    approx_icc                    = round(icc_agency_approx, 4),
    agency_mean_log_wp_dist       = quantile_summary(agency_means$mean_log_wp),
    agency_row_count_dist         = quantile_summary(agency_means$n_rows),
    interpretation = sprintf(
      "Approx ICC = %.3f. Between-agency variance = %.4f accounts for %.1f%% of total variance. %s",
      icc_agency_approx, between_agency_var,
      between_agency_var / total_var * 100,
      if (icc_agency_approx > 0.10) "Substantial agency-level clustering — agency random effects strongly motivated."
      else if (icc_agency_approx > 0.05) "Moderate agency-level clustering — agency random effects likely useful."
      else "Weak agency-level clustering — agency random effects may add little."
    )
  ),
  parent_level_raw = list(
    n_parents                     = nrow(parent_means_raw),
    between_parent_variance       = round(between_parent_var_raw, 4),
    parent_mean_log_wp_dist       = quantile_summary(parent_means_raw$mean_log_wp),
    parent_row_count_dist         = quantile_summary(parent_means_raw$n_rows)
  ),
  parent_level_sentinel_adjusted = list(
    n_parents                     = nrow(parent_means_sa),
    between_parent_variance       = round(between_parent_var_sa, 4),
    parent_mean_log_wp_dist       = quantile_summary(parent_means_sa$mean_log_wp),
    parent_row_count_dist         = quantile_summary(parent_means_sa$n_rows)
  )
)

cat(sprintf("  Total log_wp variance: %.4f\n", total_var))
cat(sprintf("  Between-agency variance: %.4f  (approx ICC = %.3f)\n",
            between_agency_var, icc_agency_approx))
cat(sprintf("  Between-parent variance (SA): %.4f\n\n", between_parent_var_sa))

# =============================================================================
# PART G — Recommendations
# =============================================================================

cat("Part G: Building recommendations...\n")

# Heuristics
sentinel_row_pct <- if (length(sentinel_pai_ids) > 0) {
  sum(wp_eff$PRIMARY_AGENCY_ID %in% sentinel_pai_ids, na.rm = TRUE) / n_eff * 100
} else 0.0

agency_overlap_pct  <- pct(length(test_ag_in_train), n_test_ag)
parent_sa_overlap_pct <- pct(length(test_pai_sa_in_train), n_test_pai_sa)
singleton_pct <- part_e$hierarchy_sentinel_adjusted$singletons_pct

agency_rec <- if (icc_agency_approx > 0.05 && agency_overlap_pct > 80) {
  list(decision = "USE_AGENCY_RANDOM_INTERCEPT",
       reasoning = sprintf(
         "Approx ICC = %.3f indicates substantial agency-level clustering. %.1f%% of test agencies seen in train — EBLUPs reliable for most test rows.",
         icc_agency_approx, agency_overlap_pct))
} else if (icc_agency_approx > 0.02) {
  list(decision = "USE_AGENCY_RANDOM_INTERCEPT",
       reasoning = sprintf(
         "Approx ICC = %.3f suggests moderate clustering. Proceed with agency random intercept but expect modest improvement over OLS/RF.",
         icc_agency_approx))
} else {
  list(decision = "INVESTIGATE_FURTHER",
       reasoning = "Weak clustering signal. Verify before committing to LMM.")
}

parent_rec <- if (sentinel_row_pct > 30) {
  list(decision = "USE_SENTINEL_ADJUSTED_PARENT_ID",
       reasoning = sprintf(
         "%.1f%% of rows have sentinel parent IDs. Raw PRIMARY_AGENCY_ID is unsuitable. Use sentinel-adjusted (excluding %d sentinel IDs) for nested LMM.",
         sentinel_row_pct, length(sentinel_pai_ids)))
} else if (sentinel_row_pct > 5) {
  list(decision = "USE_SENTINEL_ADJUSTED_PARENT_ID",
       reasoning = sprintf(
         "%.1f%% of rows have sentinel parent IDs. Exclude %d sentinel IDs before using PRIMARY_AGENCY_ID.",
         sentinel_row_pct, length(sentinel_pai_ids)))
} else if (length(sentinel_pai_ids) == 0) {
  list(decision = "USE_RAW_PARENT_ID",
       reasoning = "No sentinel parent IDs detected. PRIMARY_AGENCY_ID appears clean.")
} else {
  list(decision = "USE_SENTINEL_ADJUSTED_PARENT_ID",
       reasoning = sprintf("%d potential sentinel IDs detected but low row coverage. Minor cleanup sufficient.", length(sentinel_pai_ids)))
}

nested_rec <- if (singleton_pct > 70) {
  list(decision = "AGENCY_ONLY_LMM_FIRST_NESTED_LMM_OPTIONAL",
       reasoning = sprintf(
         "%.1f%% of non-sentinel parents have exactly one child agency. Nested structure is mostly trivial — agency-only LMM likely sufficient. Run nested as optional comparison.",
         singleton_pct))
} else if (pct(n_multi_parent, n_total_agencies_c) > 20 && sentinel_row_pct < 30) {
  list(decision = "PROCEED_NESTED_LMM_AFTER_SENTINEL_HANDLING",
       reasoning = "Meaningful parent hierarchy exists after sentinel removal. Nested LMM warranted.")
} else {
  list(decision = "AGENCY_ONLY_LMM_FIRST_NESTED_LMM_OPTIONAL",
       reasoning = "Proceed with agency-only LMM first. Evaluate whether adding parent level improves fit meaningfully.")
}

part_g <- list(
  agency_id = agency_rec,
  primary_agency_id = parent_rec,
  nested_lmm = nested_rec,
  sentinel_summary = list(
    n_sentinel_parent_ids    = length(sentinel_pai_ids),
    sentinel_ids             = sentinel_pai_ids,
    pct_rows_affected        = round(sentinel_row_pct, 2),
    sentinel_ids_too_common  = (sentinel_row_pct > 10)
  ),
  agency_parent_coherence = list(
    pct_agencies_one_parent  = pct(n_one_parent, n_total_agencies_c),
    hierarchy_stable         = (pct(n_multi_parent, n_total_agencies_c) < 10)
  ),
  train_test_overlap_summary = list(
    agency_test_pct_seen_in_train  = agency_overlap_pct,
    parent_sa_test_pct_seen_in_train = parent_sa_overlap_pct,
    agency_eblups_reliable         = (agency_overlap_pct > 80),
    parent_eblups_reliable         = (parent_sa_overlap_pct > 80)
  )
)

cat(sprintf("  Agency LMM: %s\n", agency_rec$decision))
cat(sprintf("  Parent LMM: %s\n", parent_rec$decision))
cat(sprintf("  Nested LMM: %s\n\n", nested_rec$decision))

# =============================================================================
# Key findings
# =============================================================================

key_findings <- list(
  sprintf("AGENCY_ID: %d distinct agencies in effective population. Approx ICC = %.3f.",
          nrow(agency_means), icc_agency_approx),
  sprintf("PRIMARY_AGENCY_ID: %d distinct raw parent IDs. %d flagged as suspicious/sentinel.",
          part_a$primary_agency_id$full$n_distinct, length(sentinel_pai_ids)),
  sprintf("Self-parenting: %.1f%% of rows have PRIMARY_AGENCY_ID == AGENCY_ID.",
          pct(self_parent_rows, n_eff)),
  sprintf("Agency-parent stability: %.1f%% of agencies have exactly one parent ID.",
          pct(n_one_parent, n_total_agencies_c)),
  sprintf("Train/test overlap: %.1f%% of test agencies seen in train.",
          agency_overlap_pct),
  sprintf("Sentinel-adjusted hierarchy: %d parents, median %.0f agencies/parent, %.1f%% singletons.",
          part_e$hierarchy_sentinel_adjusted$n_parent_ids,
          part_e$hierarchy_sentinel_adjusted$agencies_per_parent$median,
          part_e$hierarchy_sentinel_adjusted$singletons_pct),
  sprintf("Agency LMM recommendation: %s", agency_rec$decision),
  sprintf("Nested LMM recommendation: %s", nested_rec$decision)
)

# =============================================================================
# Assemble and write JSON
# =============================================================================

cat("Writing output...\n")

out <- list(
  description = paste0(
    "Agency hierarchy diagnostics for Dataset A Part 2. ",
    "Investigates AGENCY_ID and PRIMARY_AGENCY_ID as hierarchical grouping variables ",
    "before LMM fitting. Effective modeling population: ",
    format(n_eff_train, big.mark = ","), " train / ", format(n_eff_test, big.mark = ","), " test ",
    "(same as OLS_ADDITIVE_FINAL and RF_1_SAFE_TUNED). No models fit in this script."
  ),
  modeling_population = list(
    filters             = list(
      years             = "2006-2014",
      exclude_prod_abbr = "COMMPOL",
      wp_filter         = "WRTN_PREM_AMT > 0",
      na_exclusion      = "log_prev_wp and log_prev_poly NA rows excluded"
    ),
    n_candidate_train   = n_cand_train,
    n_candidate_test    = n_cand_test,
    n_effective_train   = n_eff_train,
    n_effective_test    = n_eff_test,
    row_count_note      = row_count_match_note,
    train_years         = "2006-2012",
    test_years          = "2013-2014"
  ),
  part_a_id_coverage                   = part_a,
  part_b_sentinel_detection            = part_b,
  part_c_agency_parent_mapping_stability = part_c,
  part_d_train_test_overlap            = part_d,
  part_e_hierarchy_shape               = part_e,
  part_f_descriptive_variance_precheck = part_f,
  part_g_recommendations               = part_g,
  key_findings                         = key_findings
)

path <- file.path(output_dir, "agency_hierarchy_diagnostics.json")
write(toJSON(out, auto_unbox = TRUE, digits = 6, pretty = TRUE, null = "null"), path)
cat(sprintf("  agency_hierarchy_diagnostics.json  %.1f KB\n", file.size(path) / 1024))
cat("\n=== Agency hierarchy diagnostics complete ===\n")
