# =============================================================================
# Dataset A Part 2 — Phase 1: EDA and Data Documentation
# =============================================================================
# Run from repo root:
#   Rscript modeling/dataset_a_part2/dataset_a_modeling.R
#
# Outputs:
#   modeling/dataset_a_part2/outputs/overview.json
#   modeling/dataset_a_part2/outputs/eda_summary.json
#   modeling/dataset_a_part2/outputs/predictor_audit.json
#   modeling/dataset_a_part2/outputs/correlations.json
#
# Required packages: DBI, RSQLite, dplyr, jsonlite
# =============================================================================

.libPaths(c(file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6"), .libPaths()))

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(dplyr)
  library(jsonlite)
})

SENTINELS <- c(99997, 99998, 99999)

# ---------------------------------------------------------------------------
# Paths
# ---------------------------------------------------------------------------

db_path    <- "insurance.db"
output_dir <- file.path("modeling", "dataset_a_part2", "outputs")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(db_path)) {
  stop(paste0(
    "Database not found: ", db_path,
    "\nRun this script from the repo root: ",
    "Rscript modeling/dataset_a_part2/dataset_a_modeling.R"
  ))
}

cat("=== Dataset A Part 2 — Phase 1: EDA ===\n\n")

# ---------------------------------------------------------------------------
# Connect and load
# ---------------------------------------------------------------------------

cat("Connecting to database...\n")
con <- dbConnect(RSQLite::SQLite(), db_path)
on.exit(dbDisconnect(con), add = TRUE)

cat("Loading agency_performance...\n")
raw <- as_tibble(dbReadTable(con, "agency_performance"))
cat(sprintf("  %s rows x %d columns\n\n",
            format(nrow(raw), big.mark = ","), ncol(raw)))

# Derived columns used throughout
raw <- raw |>
  mutate(
    retention_ratio_clean = if_else(
      RETENTION_RATIO %in% SENTINELS | is.na(RETENTION_RATIO),
      NA_real_,
      RETENTION_RATIO
    )
  )
# NOTE: computed_loss_ratio is NOT derived globally. It is a diagnostic construct
# computed locally inside Section 11 (loss_ratio_diagnostics.json) only.
# Business-facing loss ratio reporting uses portfolio_loss_ratio from portfolio_lr_by_year.

# Valid loss ratio population for portfolio-level calculations.
# Excluded when PRD_ERND_PREM_AMT is NA (no denominator), PRD_ERND_PREM_AMT <= 0
# (undefined denominator), or PRD_INCRD_LOSSES_AMT is NA (unknown loss amount).
# Using the same paired rows for both sum(losses) and sum(earned premium) ensures
# portfolio_loss_ratio = sum(losses) / sum(earned premium) over a consistent
# observation set — not an average of row-level ratios.
lr_valid <- raw |>
  filter(
    !is.na(PRD_INCRD_LOSSES_AMT),
    !is.na(PRD_ERND_PREM_AMT),
    PRD_ERND_PREM_AMT > 0
  )

portfolio_lr_by_year <- lr_valid |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(
    total_incurred_losses = round(sum(PRD_INCRD_LOSSES_AMT), 2),
    total_earned_premium  = round(sum(PRD_ERND_PREM_AMT),    2),
    portfolio_loss_ratio  = round(sum(PRD_INCRD_LOSSES_AMT) / sum(PRD_ERND_PREM_AMT), 6),
    n_lr_valid_rows       = n(),
    .groups = "drop"
  )

# =============================================================================
# SECTION 1: Dataset Overview
# =============================================================================

cat("[1/10] Dataset overview...\n")

year_min <- min(raw$STAT_PROFILE_DATE_YEAR, na.rm = TRUE)
year_max <- max(raw$STAT_PROFILE_DATE_YEAR, na.rm = TRUE)

overview <- list(
  dataset           = "Kaggle Agency Performance Dataset (Dataset A)",
  table             = "agency_performance",
  source_file       = "data/dataset_a.csv",
  total_rows        = nrow(raw),
  total_columns     = ncol(raw),
  column_names      = names(raw),
  year_range        = list(min = year_min, max = year_max),
  years_covered     = sort(unique(raw$STAT_PROFILE_DATE_YEAR)),
  unique_agencies         = n_distinct(raw$AGENCY_ID),
  unique_parent_agencies  = n_distinct(raw$PRIMARY_AGENCY_ID),
  unique_states           = n_distinct(raw$STATE_ABBR),
  unique_product_lines    = n_distinct(raw$PROD_LINE),
  unique_product_abbrs    = n_distinct(raw$PROD_ABBR),
  unique_vendors          = n_distinct(raw$VENDOR),
  modeling_scope = list(
    primary_target    = "log(WRTN_PREM_AMT + 1)",
    secondary_targets = c(
      "LOSS_RATIO (after sentinel handling — dataset-provided row-level loss ratio)",
      "RETENTION_RATIO (after sentinel handling)"
    ),
    train_years       = 2006:2012,
    test_years        = 2013:2014,
    excluded_years    = c(2005, 2015),
    excluded_years_reason = paste0(
      "2005 and 2015 are partial reporting years confirmed by data completeness review. ",
      "Both show MONTHS avg < 12 and/or material declines in row count and agency coverage. ",
      "Excluding them ensures train and test sets contain only complete annual periods."
    ),
    excluded_products = "COMMPOL",
    excluded_products_reason = paste0(
      "COMMPOL is a structural reporting artifact with zero premium. ",
      "It does not represent genuine insurance production activity."
    ),
    wp_filter         = "WRTN_PREM_AMT > 0",
    model_condition   = paste0(
      "Forward-looking Written Premium models are conditioned on active ",
      "premium-producing observations (PROD_ABBR != 'COMMPOL', WRTN_PREM_AMT > 0)."
    )
  )
)

# =============================================================================
# SECTION 2: Data Completeness Review
# =============================================================================

cat("[2/10] Data completeness review...\n")

completeness_by_year <- raw |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(
    row_count              = n(),
    unique_agencies        = n_distinct(AGENCY_ID),
    unique_parent_agencies = n_distinct(PRIMARY_AGENCY_ID),
    total_wrtn_prem        = round(sum(WRTN_PREM_AMT,         na.rm = TRUE), 2),
    total_poly_inforce     = round(sum(POLY_INFORCE_QTY,      na.rm = TRUE), 0),
    total_active_producers = round(sum(ACTIVE_PRODUCERS,      na.rm = TRUE), 0),
    unique_prod_abbr       = n_distinct(PROD_ABBR),
    unique_states          = n_distinct(STATE_ABBR),
    avg_months             = round(mean(MONTHS,               na.rm = TRUE), 4),
    avg_retention_ratio    = round(mean(retention_ratio_clean,na.rm = TRUE), 4),
    .groups = "drop"
  ) |>
  left_join(portfolio_lr_by_year, by = "year") |>
  mutate(
    modeling_role = case_when(
      year %in% c(2005, 2015) ~ "excluded_partial_year",
      year %in% 2006:2012     ~ "train",
      year %in% 2013:2014     ~ "test",
      TRUE                    ~ "unknown"
    ),
    is_partial_year = year %in% c(2005, 2015)
  ) |>
  arrange(year)

# =============================================================================
# SECTION 3: Non-Positive Premium Categorization
# =============================================================================

cat("[3/10] Non-positive premium categorization...\n")

is_commpol       <- raw$PROD_ABBR == "COMMPOL"
non_commpol      <- raw[!is_commpol, ]

cat_commpol      <- raw[is_commpol, ]
cat_struct_zero  <- non_commpol[non_commpol$WRTN_PREM_AMT == 0 & non_commpol$POLY_INFORCE_QTY == 0, ]
cat_zero_nonzpol <- non_commpol[non_commpol$WRTN_PREM_AMT == 0 & non_commpol$POLY_INFORCE_QTY > 0, ]
cat_negative     <- non_commpol[non_commpol$WRTN_PREM_AMT < 0, ]
cat_positive     <- non_commpol[non_commpol$WRTN_PREM_AMT > 0, ]

summarise_prem_category <- function(df, label, description) {
  n <- nrow(df)
  list(
    category          = label,
    description       = description,
    row_count         = n,
    pct_of_total      = round(n / nrow(raw) * 100, 2),
    unique_agencies   = n_distinct(df$AGENCY_ID),
    year_distribution = as.list(table(df$STAT_PROFILE_DATE_YEAR)),
    wrtn_prem_stats   = if (n > 0) list(
      min    = round(min(df$WRTN_PREM_AMT,    na.rm = TRUE), 2),
      max    = round(max(df$WRTN_PREM_AMT,    na.rm = TRUE), 2),
      mean   = round(mean(df$WRTN_PREM_AMT,   na.rm = TRUE), 2),
      median = round(median(df$WRTN_PREM_AMT, na.rm = TRUE), 2)
    ) else NULL
  )
}

non_positive_premium_categories <- list(
  summarise_prem_category(
    cat_commpol, "COMMPOL",
    paste0("PROD_ABBR = 'COMMPOL' — structural reporting artifact; ",
           "zero premium by design. Excluded from all WP, LR, and RR modeling.")
  ),
  summarise_prem_category(
    cat_struct_zero, "structural_zero",
    paste0("WRTN_PREM_AMT = 0 and POLY_INFORCE_QTY = 0 (non-COMMPOL) — ",
           "inactive observations with no policies and no premium.")
  ),
  summarise_prem_category(
    cat_zero_nonzpol, "zero_premium_nonzero_policies",
    paste0("WRTN_PREM_AMT = 0 and POLY_INFORCE_QTY > 0 (non-COMMPOL) — ",
           "ambiguous runoff or reporting edge cases.")
  ),
  summarise_prem_category(
    cat_negative, "negative_premium",
    paste0("WRTN_PREM_AMT < 0 (non-COMMPOL) — accounting adjustments: ",
           "refunds, credits, audit returns.")
  ),
  summarise_prem_category(
    cat_positive, "positive_premium",
    paste0("WRTN_PREM_AMT > 0 (non-COMMPOL) — active premium-producing observations. ",
           "This is the Written Premium modeling dataset.")
  )
)

# =============================================================================
# SECTION 4: COMMPOL Analysis
# =============================================================================

cat("[4/10] COMMPOL analysis...\n")

commpol_by_year <- cat_commpol |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(
    row_count       = n(),
    unique_agencies = n_distinct(AGENCY_ID),
    total_wrtn_prem = round(sum(WRTN_PREM_AMT, na.rm = TRUE), 2),
    mean_wrtn_prem  = round(mean(WRTN_PREM_AMT, na.rm = TRUE), 2),
    pct_zero_prem   = round(mean(WRTN_PREM_AMT == 0, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  ) |>
  arrange(year)

commpol_analysis <- list(
  total_rows              = nrow(cat_commpol),
  pct_of_total            = round(nrow(cat_commpol) / nrow(raw) * 100, 2),
  unique_agencies         = n_distinct(cat_commpol$AGENCY_ID),
  pct_rows_zero_premium   = round(mean(cat_commpol$WRTN_PREM_AMT == 0, na.rm = TRUE) * 100, 2),
  wrtn_prem_stats         = list(
    min    = round(min(cat_commpol$WRTN_PREM_AMT,    na.rm = TRUE), 2),
    max    = round(max(cat_commpol$WRTN_PREM_AMT,    na.rm = TRUE), 2),
    mean   = round(mean(cat_commpol$WRTN_PREM_AMT,   na.rm = TRUE), 2),
    median = round(median(cat_commpol$WRTN_PREM_AMT, na.rm = TRUE), 2)
  ),
  states                  = sort(unique(cat_commpol$STATE_ABBR)),
  prod_lines              = sort(unique(cat_commpol$PROD_LINE)),
  by_year                 = commpol_by_year,
  exclusion_rationale     = paste0(
    "COMMPOL rows are excluded from all Written Premium, Loss Ratio, and Retention Ratio modeling. ",
    "These rows represent a structural reporting artifact with zero premium and do not reflect ",
    "genuine insurance production activity."
  )
)

# =============================================================================
# SECTION 5: Agency Hierarchy Analysis
# =============================================================================

cat("[5/10] Agency hierarchy analysis...\n")

agency_map <- raw |>
  distinct(AGENCY_ID, PRIMARY_AGENCY_ID)

multi_parent_check <- agency_map |>
  group_by(AGENCY_ID) |>
  summarise(n_parents = n(), .groups = "drop")

agencies_with_multiple_parents <- sum(multi_parent_check$n_parents > 1)

agencies_per_primary <- agency_map |>
  group_by(PRIMARY_AGENCY_ID) |>
  summarise(agency_count = n(), .groups = "drop")

pa_99999 <- raw |> filter(PRIMARY_AGENCY_ID == 99999)
pa_99999_agencies <- n_distinct(pa_99999$AGENCY_ID)

agency_hierarchy <- list(
  total_unique_agencies         = n_distinct(raw$AGENCY_ID),
  total_unique_parent_agencies  = n_distinct(raw$PRIMARY_AGENCY_ID),
  one_to_one_agency_to_parent   = (agencies_with_multiple_parents == 0),
  agencies_with_multiple_parents = agencies_with_multiple_parents,
  agencies_per_primary = list(
    min    = min(agencies_per_primary$agency_count),
    max    = max(agencies_per_primary$agency_count),
    mean   = round(mean(agencies_per_primary$agency_count), 2),
    median = median(agencies_per_primary$agency_count),
    q25    = unname(quantile(agencies_per_primary$agency_count, 0.25)),
    q75    = unname(quantile(agencies_per_primary$agency_count, 0.75))
  ),
  single_agency_parents  = sum(agencies_per_primary$agency_count == 1),
  multi_agency_parents   = sum(agencies_per_primary$agency_count > 1),
  largest_parent_size    = max(agencies_per_primary$agency_count),
  primary_agency_id_99999 = list(
    description       = "Special category — likely independent agencies without a true parent hierarchy",
    row_count         = nrow(pa_99999),
    unique_agencies   = pa_99999_agencies,
    pct_of_total_rows = round(nrow(pa_99999) / nrow(raw) * 100, 2),
    pct_of_agencies   = round(pa_99999_agencies / n_distinct(raw$AGENCY_ID) * 100, 2),
    total_wrtn_prem   = round(sum(pa_99999$WRTN_PREM_AMT, na.rm = TRUE), 2),
    year_distribution = as.list(table(pa_99999$STAT_PROFILE_DATE_YEAR))
  )
)

# =============================================================================
# SECTION 6: Sentinel Value Analysis
# =============================================================================

cat("[6/10] Sentinel value analysis...\n")

sentinel_focus_cols <- c(
  "RETENTION_RATIO", "LOSS_RATIO", "LOSS_RATIO_3YR", "GROWTH_RATE_3YR",
  "PL_START_YEAR", "PL_END_YEAR",
  "CL_START_YEAR", "CL_END_YEAR",
  "COMMISIONS_START_YEAR", "COMMISIONS_END_YEAR",
  "ACTIVITY_NOTES_START_YEAR", "ACTIVITY_NOTES_END_YEAR",
  "AGENCY_APPOINTMENT_YEAR"
)
sentinel_focus_cols <- sentinel_focus_cols[sentinel_focus_cols %in% names(raw)]

sentinel_by_col <- lapply(sentinel_focus_cols, function(col) {
  v <- raw[[col]]
  list(
    column           = col,
    n_99997          = sum(v == 99997, na.rm = TRUE),
    n_99998          = sum(v == 99998, na.rm = TRUE),
    n_99999          = sum(v == 99999, na.rm = TRUE),
    n_sentinel_total = sum(v %in% SENTINELS, na.rm = TRUE),
    pct_sentinel     = round(sum(v %in% SENTINELS, na.rm = TRUE) / length(v) * 100, 2),
    n_null           = sum(is.na(v)),
    n_valid          = sum(!is.na(v) & !v %in% SENTINELS)
  )
})
names(sentinel_by_col) <- sentinel_focus_cols

numeric_cols_all <- names(raw)[sapply(raw, is.numeric)]
broad_scan_list <- lapply(numeric_cols_all, function(col) {
  v <- raw[[col]]
  n_s <- sum(v %in% SENTINELS, na.rm = TRUE)
  if (n_s > 0) list(
    column     = col,
    n_sentinel = n_s,
    pct        = round(n_s / length(v) * 100, 2)
  ) else NULL
})
broad_sentinel_scan <- Filter(Negate(is.null), broad_scan_list)

# =============================================================================
# SECTION 7: Distribution Summaries
# =============================================================================

cat("[7/10] Distribution summaries...\n")

modeling_df <- raw |>
  filter(PROD_ABBR != "COMMPOL", WRTN_PREM_AMT > 0) |>
  mutate(log_wp = log(WRTN_PREM_AMT + 1))

num_summary <- function(x, label) {
  x <- x[!is.na(x) & !is.infinite(x)]
  if (length(x) == 0) return(list(label = label, n_valid = 0L))
  list(
    label    = label,
    n_valid  = length(x),
    min      = round(min(x),                   4),
    q1       = round(quantile(x, 0.25),        4),
    median   = round(median(x),                4),
    mean     = round(mean(x),                  4),
    q3       = round(quantile(x, 0.75),        4),
    max      = round(max(x),                   4),
    sd       = round(sd(x),                    4),
    pct_zero = round(mean(x == 0) * 100,       2)
  )
}

rr_clean <- modeling_df$retention_ratio_clean

distributions <- list(
  WRTN_PREM_AMT_all      = num_summary(raw$WRTN_PREM_AMT,                "WRTN_PREM_AMT (all rows)"),
  WRTN_PREM_AMT_modeling = num_summary(modeling_df$WRTN_PREM_AMT,        "WRTN_PREM_AMT (modeling dataset)"),
  log_wp                 = num_summary(modeling_df$log_wp,                "log(WRTN_PREM_AMT + 1)"),
  PRD_ERND_PREM_AMT      = num_summary(modeling_df$PRD_ERND_PREM_AMT,    "PRD_ERND_PREM_AMT"),
  PRD_INCRD_LOSSES_AMT   = num_summary(modeling_df$PRD_INCRD_LOSSES_AMT, "PRD_INCRD_LOSSES_AMT"),
  RETENTION_RATIO_clean  = num_summary(rr_clean,                          "RETENTION_RATIO (sentinels excluded)"),
  POLY_INFORCE_QTY       = num_summary(modeling_df$POLY_INFORCE_QTY,     "POLY_INFORCE_QTY"),
  PREV_POLY_INFORCE_QTY  = num_summary(modeling_df$PREV_POLY_INFORCE_QTY,"PREV_POLY_INFORCE_QTY"),
  PREV_WRTN_PREM_AMT     = num_summary(modeling_df$PREV_WRTN_PREM_AMT,   "PREV_WRTN_PREM_AMT"),
  NB_WRTN_PREM_AMT       = num_summary(modeling_df$NB_WRTN_PREM_AMT,     "NB_WRTN_PREM_AMT"),
  ACTIVE_PRODUCERS       = num_summary(modeling_df$ACTIVE_PRODUCERS,      "ACTIVE_PRODUCERS")
)

# =============================================================================
# SECTION 8: Time Trend Analysis
# =============================================================================

cat("[8/10] Time trend analysis...\n")

time_trends <- raw |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(
    total_wrtn_prem        = round(sum(WRTN_PREM_AMT,         na.rm = TRUE), 2),
    avg_retention_ratio    = round(mean(retention_ratio_clean, na.rm = TRUE), 4),
    total_poly_inforce     = round(sum(POLY_INFORCE_QTY,       na.rm = TRUE), 0),
    total_active_producers = round(sum(ACTIVE_PRODUCERS,       na.rm = TRUE), 0),
    row_count              = n(),
    .groups = "drop"
  ) |>
  left_join(portfolio_lr_by_year, by = "year") |>
  arrange(year)

# =============================================================================
# SECTION 9: Predictor Audit
# =============================================================================

cat("[9/10] Predictor audit...\n")

audit_entry <- function(name, type, description, wp, lr, rr, leakage = NULL, recommendation) {
  list(
    name           = name,
    type           = type,
    description    = description,
    wp_classification  = wp,
    lr_classification  = lr,
    rr_classification  = rr,
    leakage_concern    = leakage,
    recommendation     = recommendation
  )
}

audit_variables <- list(
  # --- Identifiers ---
  audit_entry("AGENCY_ID", "identifier",
    "Agency identifier — used as grouping factor in LMM random effects, not a fixed-effect predictor",
    "group_factor", "group_factor", "group_factor",
    recommendation = "Use as random effect grouping factor only: (1 | AGENCY_ID)"),

  audit_entry("PRIMARY_AGENCY_ID", "identifier",
    "Parent agency identifier — used as grouping factor in nested LMM, not a fixed-effect predictor",
    "group_factor", "group_factor", "group_factor",
    recommendation = "Use as random effect grouping factor only: (1 | PRIMARY_AGENCY_ID / AGENCY_ID)"),

  # --- Target variable ---
  audit_entry("WRTN_PREM_AMT", "target",
    "Written premium — the primary modeling target. Modeled as log(WRTN_PREM_AMT + 1). Rows with WP <= 0 or COMMPOL excluded.",
    "target", "leakage_if_used_as_predictor", "safe",
    leakage = "If used as a predictor for LR: WRTN_PREM_AMT is correlated with the LR denominator. Use with caution.",
    recommendation = "Primary target for WP modeling. Not a predictor."),

  # --- Safe categorical predictors ---
  audit_entry("STAT_PROFILE_DATE_YEAR", "numeric",
    "Reporting year (2005-2015) — captures temporal trend and macroeconomic context",
    "safe", "safe", "safe",
    recommendation = "Include in all models"),

  audit_entry("STATE_ABBR", "categorical",
    "State of agency operation — captures geographic and regulatory pricing environment",
    "safe", "safe", "safe",
    recommendation = "Include in all models as categorical factor"),

  audit_entry("PROD_LINE", "categorical",
    "Product line (CL = Commercial Lines, PL = Personal Lines) — major business segment indicator",
    "safe", "safe", "safe",
    recommendation = "Include in all models as categorical factor"),

  audit_entry("PROD_ABBR", "categorical",
    "Product abbreviation — specific product code within product line. Note: exclude COMMPOL rows before modeling.",
    "safe", "safe", "safe",
    recommendation = "Include after COMMPOL exclusion"),

  audit_entry("VENDOR", "categorical",
    "Agency management system vendor — proxy for agency technology and workflow maturity",
    "safe", "safe", "safe",
    recommendation = "Include in all models as categorical factor"),

  audit_entry("VENDOR_IND", "categorical",
    "Vendor indicator flag (Y/N) — binary version of vendor affiliation",
    "safe", "safe", "safe",
    recommendation = "Potentially redundant with VENDOR; include one or check VIF"),

  # --- Safe numeric predictors ---
  audit_entry("ACTIVE_PRODUCERS", "numeric",
    "Number of active producers at the agency — direct measure of sales capacity",
    "safe", "safe", "safe",
    recommendation = "Include in all models"),

  audit_entry("PREV_WRTN_PREM_AMT", "numeric",
    "Prior-year written premium — clean lag; strongest single predictor of current-year premium",
    "safe_lag", "safe", "safe",
    recommendation = "Include in all models; apply log(x + 1) transform for WP modeling"),

  audit_entry("PREV_POLY_INFORCE_QTY", "numeric",
    "Prior-year policies in force — clean lag; preferred policy-count predictor for forward-looking WP model",
    "safe_lag", "safe", "safe",
    recommendation = "Include in all models; replaces contemporaneous POLY_INFORCE_QTY in forward-looking WP specification"),

  audit_entry("MONTHS", "numeric",
    "Number of months in the reporting period — partial-year indicator (8 or 12); needed for period normalization",
    "safe", "safe", "safe",
    recommendation = "Include; 2005 and 2015 exclusion removes most partial-year rows"),

  audit_entry("AGENCY_APPOINTMENT_YEAR", "numeric",
    "Year agency was appointed — proxy for tenure and experience. Check for sentinel values.",
    "safe", "safe", "safe",
    recommendation = "Include directly or derive TENURE = STAT_PROFILE_DATE_YEAR - AGENCY_APPOINTMENT_YEAR in feature engineering phase"),

  audit_entry("MAX_AGE", "numeric",
    "Maximum producer age at agency — proxy for producer experience profile",
    "safe", "safe", "safe",
    recommendation = "Include; check VIF against MIN_AGE"),

  audit_entry("MIN_AGE", "numeric",
    "Minimum producer age at agency — proxy for producer youth or new entrants",
    "safe", "safe", "safe",
    recommendation = "Include; check VIF against MAX_AGE"),

  audit_entry("PL_START_YEAR", "numeric",
    "Personal lines appointment start year. Sentinel 99999 = no PL appointment (CL-only agency).",
    "safe", "safe", "safe",
    recommendation = "Replace 99999 with NA before use"),

  audit_entry("PL_END_YEAR", "numeric",
    "Personal lines appointment end year. Sentinel 99999 = ongoing or not applicable.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  audit_entry("CL_START_YEAR", "numeric",
    "Commercial lines appointment start year. Sentinel 99999 = no CL appointment.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  audit_entry("CL_END_YEAR", "numeric",
    "Commercial lines appointment end year.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  audit_entry("COMMISIONS_START_YEAR", "numeric",
    "Commission arrangement start year.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  audit_entry("COMMISIONS_END_YEAR", "numeric",
    "Commission arrangement end year.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  audit_entry("ACTIVITY_NOTES_START_YEAR", "numeric",
    "Activity notes record start year.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  audit_entry("ACTIVITY_NOTES_END_YEAR", "numeric",
    "Activity notes record end year.",
    "safe", "safe", "safe",
    recommendation = "Replace sentinels with NA before use"),

  # --- Quote / bind activity counts ---
  audit_entry("CL_BOUND_CT_MDS",  "numeric", "Commercial lines bound count (MDS system)",      "safe","safe","safe", recommendation="Include"),
  audit_entry("CL_QUO_CT_MDS",    "numeric", "Commercial lines quote count (MDS system)",       "safe","safe","safe", recommendation="Include"),
  audit_entry("CL_BOUND_CT_SBZ",  "numeric", "Commercial lines bound count (SBZ system)",       "safe","safe","safe", recommendation="Include"),
  audit_entry("CL_QUO_CT_SBZ",    "numeric", "Commercial lines quote count (SBZ system)",       "safe","safe","safe", recommendation="Include"),
  audit_entry("CL_BOUND_CT_eQT",  "numeric", "Commercial lines bound count (eQT system)",       "safe","safe","safe", recommendation="Include"),
  audit_entry("CL_QUO_CT_eQT",    "numeric", "Commercial lines quote count (eQT system)",       "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_BOUND_CT_ELINKS",    "numeric","Personal lines bound count (ELINKS system)",   "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_QUO_CT_ELINKS",     "numeric","Personal lines quote count (ELINKS system)",    "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_BOUND_CT_PLRANK",   "numeric","Personal lines bound count (PLRANK system)",    "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_QUO_CT_PLRANK",     "numeric","Personal lines quote count (PLRANK system)",    "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_BOUND_CT_eQTte",    "numeric","Personal lines bound count (eQTte system)",     "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_QUO_CT_eQTte",     "numeric","Personal lines quote count (eQTte system)",      "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_BOUND_CT_APPLIED",  "numeric","Personal lines bound count (APPLIED system)",   "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_QUO_CT_APPLIED",    "numeric","Personal lines quote count (APPLIED system)",   "safe","safe","safe", recommendation="Include"),
  audit_entry("PL_BOUND_CT_TRANSACTNOW","numeric","Personal lines bound count (TRANSACTNOW system)","safe","safe","safe",recommendation="Include"),
  audit_entry("PL_QUO_CT_TRANSACTNOW", "numeric","Personal lines quote count (TRANSACTNOW system)","safe","safe","safe",recommendation="Include"),

  # --- Excluded: contemporaneous / leakage ---
  audit_entry("POLY_INFORCE_QTY", "numeric",
    "Current-year policies in force — contemporaneous with WRTN_PREM_AMT; jointly determined in the same reporting period",
    "excluded_contemporaneous", "safe", "excluded_contemporaneous",
    leakage = paste0(
      "POLY_INFORCE_QTY and WRTN_PREM_AMT are both outcomes of the same period's ",
      "policy-writing activity. Including POLY_INFORCE_QTY in a forward-looking WP model ",
      "conflates a contemporaneous outcome with a predictor. ",
      "PREV_POLY_INFORCE_QTY (the lagged version) is used instead."
    ),
    recommendation = "Exclude from WP forward-looking model. Use PREV_POLY_INFORCE_QTY."),

  audit_entry("RETENTION_RATIO", "numeric",
    "Retention ratio (raw column) — current-period policy retention outcome; sentinel-contaminated (99997/99998/99999)",
    "excluded_contemporaneous", "safe", "target_or_excluded",
    leakage = paste0(
      "Current-period retention rate is a contemporaneous outcome in the same period as WP. ",
      "Raw column also contains sentinel values requiring exclusion."
    ),
    recommendation = "Exclude from WP forward-looking model. If used for RR modeling: exclude sentinels first."),

  audit_entry("RETENTION_POLY_QTY", "numeric",
    "Retained policy count — current-period retention outcome, contemporaneous with WP",
    "excluded_contemporaneous", "safe", "excluded_contemporaneous",
    leakage = "Current-period policy retention count; jointly determined with WP in the same period.",
    recommendation = "Exclude from WP forward-looking model"),

  audit_entry("NB_WRTN_PREM_AMT", "numeric",
    "New business written premium — a direct sub-component of WRTN_PREM_AMT (WP = new business + renewals)",
    "excluded_leakage", "excluded_leakage", "safe",
    leakage = paste0(
      "NB_WRTN_PREM_AMT is a component of WRTN_PREM_AMT. Including it as a predictor would ",
      "inflate R^2 without adding predictive insight and constitutes near-perfect leakage."
    ),
    recommendation = "Exclude from WP model entirely"),

  audit_entry("PRD_ERND_PREM_AMT", "numeric",
    "Earned premium — contemporaneous current-year earned premium derived from written premium over the policy period",
    "excluded_contemporaneous", "excluded_leakage", "safe",
    leakage = paste0(
      "Contemporaneous with WRTN_PREM_AMT; derived from it over the policy period. ",
      "For LR modeling: PRD_ERND_PREM_AMT is the denominator of the loss ratio target."
    ),
    recommendation = "Exclude from WP and LR forward-looking models"),

  audit_entry("PRD_INCRD_LOSSES_AMT", "numeric",
    "Incurred losses — current-year loss outcome; used for portfolio-level loss ratio reporting and diagnostic investigation only",
    "excluded_contemporaneous", "excluded_leakage", "safe",
    leakage = "Current-year loss outcome. Contemporaneous with any current-year target; excluded from forward-looking models.",
    recommendation = "Exclude from WP and LR forward-looking models"),

  audit_entry("LOSS_RATIO", "numeric",
    "Dataset-provided row-level loss ratio field; sentinel-contaminated (99997/99998/99999)",
    "excluded_leakage", "target_or_excluded", "safe",
    leakage = paste0(
      "Derived from current-year incurred losses and earned premium — contemporaneous with any current-year target. ",
      "Contains sentinel values (99997/99998/99999) requiring handling before use. ",
      "For row-level LR modeling: this field is the preferred candidate target after sentinel cleaning, ",
      "not a recomputed ratio."
    ),
    recommendation = "Exclude from WP model. For LR modeling: preferred candidate target after sentinel handling."),

  audit_entry("LOSS_RATIO_3YR", "numeric",
    "3-year rolling average loss ratio — rolling window includes current-year losses; sentinel-contaminated",
    "excluded_leakage", "excluded_leakage", "safe",
    leakage = "Rolling window includes current-year loss outcomes; leakage for any current-year target.",
    recommendation = "Exclude from WP and LR forward-looking models"),

  audit_entry("GROWTH_RATE_3YR", "numeric",
    "3-year rolling growth rate — rolling window includes current-year written premium",
    "excluded_leakage", "safe", "safe",
    leakage = "Rolling window includes current-year WRTN_PREM_AMT; leakage for WP model.",
    recommendation = "Exclude from WP forward-looking model")
)

# Verify all 49 columns accounted for
audited_names <- sapply(audit_variables, `[[`, "name")
missing_from_audit <- setdiff(names(raw), c(audited_names, "retention_ratio_clean"))
if (length(missing_from_audit) > 0) {
  warning("Columns not covered in predictor audit: ", paste(missing_from_audit, collapse = ", "))
}
extra_in_audit <- setdiff(audited_names, names(raw))
if (length(extra_in_audit) > 0) {
  warning("Audit entries with no matching column: ", paste(extra_in_audit, collapse = ", "))
}

wp_excluded <- audited_names[sapply(audit_variables, function(v) grepl("excluded|leakage", v$wp_classification))]
wp_safe     <- audited_names[sapply(audit_variables, function(v) v$wp_classification %in% c("safe","safe_lag"))]

predictor_audit_out <- list(
  description  = paste0(
    "Predictor audit for Dataset A Part 2. ",
    "Covers Written Premium (WP), Loss Ratio (LR), and Retention Ratio (RR) forward-looking models."
  ),
  audit_date   = as.character(Sys.Date()),
  summary = list(
    total_variables_audited = length(audit_variables),
    wp_safe_count           = length(wp_safe),
    wp_excluded_count       = length(wp_excluded),
    wp_group_factor_count   = sum(sapply(audit_variables, function(v) v$wp_classification == "group_factor")),
    wp_target_count         = sum(sapply(audit_variables, function(v) v$wp_classification == "target")),
    columns_not_in_audit    = if (length(missing_from_audit) > 0) missing_from_audit else "none"
  ),
  modeling_decisions = list(
    specification         = "Specification A (forward-looking): uses only lagged and contemporaneously-safe predictors",
    train_years           = 2006:2012,
    test_years            = 2013:2014,
    excluded_years        = c(2005, 2015),
    excluded_years_reason = "Partial reporting years confirmed by data completeness review",
    excluded_products     = "COMMPOL",
    wp_filter             = "WRTN_PREM_AMT > 0",
    model_condition       = paste0(
      "Forward-looking Written Premium models are conditioned on active premium-producing ",
      "observations (PROD_ABBR != 'COMMPOL', WRTN_PREM_AMT > 0)."
    ),
    poly_inforce_decision = paste0(
      "POLY_INFORCE_QTY is excluded from forward-looking WP models because it is jointly ",
      "determined with WRTN_PREM_AMT in the same reporting period. ",
      "PREV_POLY_INFORCE_QTY (lagged) is used instead. ",
      "No secondary explanatory model is built for this relationship in Dataset A Part 2."
    ),
    forward_looking_wp_exclusions = wp_excluded,
    safe_lagged_predictors        = c("PREV_WRTN_PREM_AMT", "PREV_POLY_INFORCE_QTY")
  ),
  variables = audit_variables
)

# =============================================================================
# SECTION 10: Correlation Analysis
# =============================================================================

cat("[10/10] Correlation analysis...\n")

# Use modeling dataset, years 2006-2014 (includes both train and test)
corr_base <- raw |>
  filter(
    PROD_ABBR != "COMMPOL",
    WRTN_PREM_AMT > 0,
    STAT_PROFILE_DATE_YEAR %in% 2006:2014
  ) |>
  mutate(
    log_wp        = log(WRTN_PREM_AMT + 1),
    log_prev_wp   = log(PREV_WRTN_PREM_AMT + 1),
    log_prev_poly = log(PREV_POLY_INFORCE_QTY + 1)
  )

# Replace sentinels with NA across all numeric columns
replace_sentinels <- function(x) { if (is.numeric(x)) { x[x %in% SENTINELS] <- NA }; x }
corr_clean <- corr_base |> mutate(across(where(is.numeric), replace_sentinels))

corr_cols <- c(
  "log_wp", "log_prev_wp", "log_prev_poly",
  "ACTIVE_PRODUCERS", "MONTHS",
  "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
  "PL_START_YEAR", "PL_END_YEAR",
  "CL_START_YEAR", "CL_END_YEAR",
  "COMMISIONS_START_YEAR", "COMMISIONS_END_YEAR",
  "ACTIVITY_NOTES_START_YEAR", "ACTIVITY_NOTES_END_YEAR",
  "CL_BOUND_CT_MDS",  "CL_QUO_CT_MDS",
  "CL_BOUND_CT_SBZ",  "CL_QUO_CT_SBZ",
  "CL_BOUND_CT_eQT",  "CL_QUO_CT_eQT",
  "PL_BOUND_CT_ELINKS",  "PL_QUO_CT_ELINKS",
  "PL_BOUND_CT_PLRANK",  "PL_QUO_CT_PLRANK",
  "PL_BOUND_CT_eQTte",   "PL_QUO_CT_eQTte",
  "PL_BOUND_CT_APPLIED", "PL_QUO_CT_APPLIED",
  "PL_BOUND_CT_TRANSACTNOW", "PL_QUO_CT_TRANSACTNOW",
  # Reference-only (excluded from forward-looking WP model but included for correlation context)
  "POLY_INFORCE_QTY", "NB_WRTN_PREM_AMT", "PRD_ERND_PREM_AMT"
)
corr_cols <- unique(corr_cols[corr_cols %in% names(corr_clean)])

mat <- cor(corr_clean[, corr_cols], use = "pairwise.complete.obs")
mat[is.nan(mat)] <- NA
mat_rounded <- round(mat, 4)

# Correlations with log_wp
if ("log_wp" %in% rownames(mat_rounded)) {
  wp_cor_vec  <- mat_rounded["log_wp", ]
  wp_cor_vec  <- wp_cor_vec[names(wp_cor_vec) != "log_wp"]
  wp_cor_df   <- data.frame(
    variable = names(wp_cor_vec),
    r        = as.numeric(wp_cor_vec),
    stringsAsFactors = FALSE
  )
  wp_cor_df <- wp_cor_df[!is.na(wp_cor_df$r), ]
  wp_cor_df <- wp_cor_df[order(wp_cor_df$r, decreasing = TRUE), ]
  top_pos <- head(wp_cor_df, 10)
  top_neg <- tail(wp_cor_df, 10)
  top_neg <- top_neg[order(top_neg$r), ]
} else {
  top_pos <- data.frame(variable = character(), r = numeric())
  top_neg <- data.frame(variable = character(), r = numeric())
}

# Top pairwise pairs by |r|
vars_c  <- rownames(mat_rounded)
pairs_list <- list()
for (i in seq_along(vars_c)) {
  for (j in seq_along(vars_c)) {
    if (j > i) {
      r_val <- mat_rounded[i, j]
      if (!is.na(r_val)) {
        pairs_list[[length(pairs_list) + 1]] <- list(
          var1 = vars_c[i], var2 = vars_c[j], r = r_val
        )
      }
    }
  }
}
pairs_list_sorted <- pairs_list[order(sapply(pairs_list, function(p) abs(p$r)), decreasing = TRUE)]

# Matrix as list of rows for JSON
corr_matrix_rows <- lapply(vars_c, function(v) {
  c(list(variable = v), as.list(mat_rounded[v, ]))
})

correlations_out <- list(
  description = paste0(
    "Pearson correlation matrix computed on the modeling dataset ",
    "(non-COMMPOL, WRTN_PREM_AMT > 0, years 2006-2014). ",
    "Sentinels (99997/99998/99999) replaced with NA. ",
    "Pairwise complete observations used."
  ),
  n_rows_used  = nrow(corr_clean),
  variables    = corr_cols,
  reference_only_note = paste0(
    "POLY_INFORCE_QTY, NB_WRTN_PREM_AMT, PRD_ERND_PREM_AMT are included in the ",
    "correlation matrix for reference only. They are excluded from the forward-looking WP model."
  ),
  correlation_matrix             = corr_matrix_rows,
  top_correlations_with_log_wp   = list(
    positive = top_pos,
    negative = top_neg
  ),
  top_20_pairwise_by_abs_r       = head(pairs_list_sorted, 20)
)

# =============================================================================
# Assemble eda_summary.json
# =============================================================================

cat("\nAssembling eda_summary.json...\n")

modeling_decisions_section <- list(
  train_years   = 2006:2012,
  test_years    = 2013:2014,
  excluded_years = c(2005, 2015),
  excluded_years_reason = paste0(
    "2005 and 2015 are partial reporting years. ",
    "Both show reduced MONTHS averages and/or material declines in row count and agency coverage. ",
    "Excluding them ensures train (2006-2012) and test (2013-2014) sets contain only complete annual periods."
  ),
  excluded_products        = "COMMPOL",
  excluded_products_reason = paste0(
    "COMMPOL is a structural reporting artifact with zero premium. ",
    "It does not represent genuine insurance production activity and would introduce spurious observations."
  ),
  wp_filter        = "WRTN_PREM_AMT > 0",
  wp_filter_reason = paste0(
    "The Written Premium model is conditioned on active premium-producing observations. ",
    "Rows with WRTN_PREM_AMT <= 0 represent inactive agencies, runoff, or accounting adjustments."
  ),
  forward_looking_wp_exclusions = list(
    list(variable="POLY_INFORCE_QTY",       reason="Contemporaneous with WP — jointly determined in the same reporting period; use PREV_POLY_INFORCE_QTY instead"),
    list(variable="RETENTION_RATIO",        reason="Contemporaneous current-period retention outcome; also sentinel-contaminated"),
    list(variable="RETENTION_POLY_QTY",     reason="Contemporaneous current-period retention count"),
    list(variable="NB_WRTN_PREM_AMT",       reason="Direct sub-component of WRTN_PREM_AMT — near-perfect leakage"),
    list(variable="PRD_ERND_PREM_AMT",      reason="Contemporaneous earned premium derived from written premium"),
    list(variable="PRD_INCRD_LOSSES_AMT",   reason="Current-year loss outcome"),
    list(variable="LOSS_RATIO",             reason="Derived from excluded variables; sentinel-contaminated"),
    list(variable="LOSS_RATIO_3YR",         reason="Rolling window includes current-year losses"),
    list(variable="GROWTH_RATE_3YR",        reason="Rolling window includes current-year written premium")
  ),
  safe_lagged_predictors = list(
    list(variable="PREV_WRTN_PREM_AMT",     reason="Prior-year premium — clean lag, not contemporaneous"),
    list(variable="PREV_POLY_INFORCE_QTY",  reason="Prior-year policy count — clean lag; preferred over POLY_INFORCE_QTY")
  )
)

eda_summary <- list(
  data_completeness                 = completeness_by_year,
  non_positive_premium_categories   = non_positive_premium_categories,
  commpol_analysis                  = commpol_analysis,
  agency_hierarchy                  = agency_hierarchy,
  sentinel_values = list(
    focused_columns = sentinel_by_col,
    broad_scan_hits = broad_sentinel_scan
  ),
  distributions                     = distributions,
  time_trends                       = time_trends,
  modeling_decisions                = modeling_decisions_section
)

# =============================================================================
# SECTION 11: Loss Ratio Diagnostics
# =============================================================================

cat("[11] Loss ratio diagnostics...\n")

# Compute computed_loss_ratio locally — diagnostic construct only, not on `raw`.
# Rows included: PRD_ERND_PREM_AMT > 0 and both components non-null.
lr_all <- raw |>
  filter(
    !is.na(PRD_ERND_PREM_AMT), PRD_ERND_PREM_AMT > 0,
    !is.na(PRD_INCRD_LOSSES_AMT)
  ) |>
  mutate(computed_loss_ratio = PRD_INCRD_LOSSES_AMT / PRD_ERND_PREM_AMT)
total_with_lr <- nrow(lr_all)

# --- Extreme value counts ---------------------------------------------------
extreme_counts <- list(
  n_rows_total           = nrow(raw),
  n_with_computed_lr     = total_with_lr,
  abs_gt_1               = sum(abs(lr_all$computed_loss_ratio) > 1),
  abs_gt_2               = sum(abs(lr_all$computed_loss_ratio) > 2),
  abs_gt_10              = sum(abs(lr_all$computed_loss_ratio) > 10),
  abs_gt_100             = sum(abs(lr_all$computed_loss_ratio) > 100),
  abs_gt_1000            = sum(abs(lr_all$computed_loss_ratio) > 1000),
  positive_gt_1          = sum(lr_all$computed_loss_ratio > 1),
  positive_gt_2          = sum(lr_all$computed_loss_ratio > 2),
  positive_gt_10         = sum(lr_all$computed_loss_ratio > 10),
  positive_gt_100        = sum(lr_all$computed_loss_ratio > 100),
  positive_gt_1000       = sum(lr_all$computed_loss_ratio > 1000),
  negative_any           = sum(lr_all$computed_loss_ratio < 0),
  negative_lt_neg1       = sum(lr_all$computed_loss_ratio < -1),
  negative_lt_neg10      = sum(lr_all$computed_loss_ratio < -10),
  negative_lt_neg100     = sum(lr_all$computed_loss_ratio < -100),
  negative_lt_neg1000    = sum(lr_all$computed_loss_ratio < -1000)
)

# --- Columns for top-N tables -----------------------------------------------
diag_cols <- intersect(
  c("STAT_PROFILE_DATE_YEAR", "PROD_ABBR", "PROD_LINE", "STATE_ABBR",
    "PRD_INCRD_LOSSES_AMT", "PRD_ERND_PREM_AMT", "computed_loss_ratio",
    "LOSS_RATIO", "WRTN_PREM_AMT", "POLY_INFORCE_QTY"),
  names(lr_all)
)

top_positive_lr <- head(lr_all[order(-lr_all$computed_loss_ratio), diag_cols], 100)
top_negative_lr <- head(lr_all[order( lr_all$computed_loss_ratio), diag_cols], 100)

# --- Breakdown for |LR| > 10 ------------------------------------------------
extreme_subset <- lr_all[abs(lr_all$computed_loss_ratio) > 10, ]

breakdown_col <- function(df, col) {
  df |>
    group_by(.data[[col]]) |>
    summarise(
      count              = n(),
      pct_of_extreme     = round(n() / nrow(extreme_subset) * 100, 2),
      median_computed_lr = round(median(computed_loss_ratio, na.rm = TRUE), 4),
      median_ernd_prem   = round(median(PRD_ERND_PREM_AMT,  na.rm = TRUE), 2),
      median_incrd_losses = round(median(PRD_INCRD_LOSSES_AMT, na.rm = TRUE), 2),
      n_negative_losses  = sum(PRD_INCRD_LOSSES_AMT < 0, na.rm = TRUE),
      n_tiny_denom_lt100 = sum(PRD_ERND_PREM_AMT < 100, na.rm = TRUE),
      .groups = "drop"
    ) |>
    arrange(desc(count))
}

breakdown_extreme <- list(
  by_prod_abbr = breakdown_col(extreme_subset, "PROD_ABBR"),
  by_prod_line  = breakdown_col(extreme_subset, "PROD_LINE"),
  by_state      = breakdown_col(extreme_subset, "STATE_ABBR"),
  by_year       = breakdown_col(extreme_subset, "STAT_PROFILE_DATE_YEAR")
)

# --- Small denominator analysis ---------------------------------------------
denom_thresholds <- c(0.01, 1, 10, 100, 1000, 10000, 100000)
denom_analysis <- lapply(denom_thresholds, function(t) {
  list(
    ernd_prem_below = t,
    n_in_extreme    = sum(extreme_subset$PRD_ERND_PREM_AMT < t, na.rm = TRUE),
    pct_of_extreme  = round(
      sum(extreme_subset$PRD_ERND_PREM_AMT < t, na.rm = TRUE) / nrow(extreme_subset) * 100, 2)
  )
})

# Compare earned premium distribution: extreme vs normal
ernd_normal  <- lr_all$PRD_ERND_PREM_AMT[abs(lr_all$computed_loss_ratio) <= 2]
ernd_extreme <- extreme_subset$PRD_ERND_PREM_AMT

ernd_prem_comparison <- list(
  normal_lr_abs_le_2 = list(
    n      = length(ernd_normal),
    min    = round(min(ernd_normal,                    na.rm = TRUE), 2),
    q1     = round(quantile(ernd_normal, 0.25,         na.rm = TRUE), 2),
    median = round(median(ernd_normal,                 na.rm = TRUE), 2),
    mean   = round(mean(ernd_normal,                   na.rm = TRUE), 2),
    q3     = round(quantile(ernd_normal, 0.75,         na.rm = TRUE), 2),
    max    = round(max(ernd_normal,                    na.rm = TRUE), 2)
  ),
  extreme_lr_abs_gt_10 = list(
    n      = length(ernd_extreme),
    min    = round(min(ernd_extreme,                   na.rm = TRUE), 2),
    q1     = round(quantile(ernd_extreme, 0.25,        na.rm = TRUE), 2),
    median = round(median(ernd_extreme,                na.rm = TRUE), 2),
    mean   = round(mean(ernd_extreme,                  na.rm = TRUE), 2),
    q3     = round(quantile(ernd_extreme, 0.75,        na.rm = TRUE), 2),
    max    = round(max(ernd_extreme,                   na.rm = TRUE), 2)
  )
)

# --- Negative incurred losses analysis --------------------------------------
neg_loss_rows <- lr_all[lr_all$PRD_INCRD_LOSSES_AMT < 0, ]

neg_loss_by_prod <- if (nrow(neg_loss_rows) > 0) {
  neg_loss_rows |>
    group_by(PROD_ABBR, PROD_LINE) |>
    summarise(
      count          = n(),
      total_losses   = round(sum(PRD_INCRD_LOSSES_AMT), 2),
      median_losses  = round(median(PRD_INCRD_LOSSES_AMT), 2),
      min_losses     = round(min(PRD_INCRD_LOSSES_AMT), 2),
      .groups = "drop"
    ) |>
    arrange(desc(count))
} else {
  data.frame()
}

neg_loss_by_year <- if (nrow(neg_loss_rows) > 0) {
  neg_loss_rows |>
    group_by(year = STAT_PROFILE_DATE_YEAR) |>
    summarise(count = n(), total_losses = round(sum(PRD_INCRD_LOSSES_AMT), 2), .groups = "drop") |>
    arrange(year)
} else {
  data.frame()
}

negative_losses_analysis <- list(
  n_rows_negative_losses       = nrow(neg_loss_rows),
  pct_of_all_rows              = round(nrow(neg_loss_rows) / nrow(raw) * 100, 2),
  pct_of_rows_with_computed_lr = round(nrow(neg_loss_rows) / total_with_lr * 100, 2),
  n_negative_computed_lr       = sum(lr_all$computed_loss_ratio < 0, na.rm = TRUE),
  median_negative_loss_amt     = if (nrow(neg_loss_rows) > 0) round(median(neg_loss_rows$PRD_INCRD_LOSSES_AMT), 2) else NULL,
  min_negative_loss_amt        = if (nrow(neg_loss_rows) > 0) round(min(neg_loss_rows$PRD_INCRD_LOSSES_AMT), 2) else NULL,
  by_product                   = neg_loss_by_prod,
  by_year                      = neg_loss_by_year
)

# --- Raw LOSS_RATIO column vs computed comparison ---------------------------
raw_lr     <- raw$LOSS_RATIO
raw_lr_val <- raw_lr[!is.na(raw_lr) & !raw_lr %in% SENTINELS]

# Where raw column is sentinel but computed LR is extreme
# Use lr_all (which has computed_loss_ratio as a local column) for both comparisons.
# lr_all only contains rows where PRD_ERND_PREM_AMT > 0 and both components are non-null,
# which is the correct base: we can only compare to the raw column where a ratio exists.
sentinel_masked_extreme <- lr_all[
  !is.na(lr_all$LOSS_RATIO) &
    lr_all$LOSS_RATIO %in% SENTINELS &
    abs(lr_all$computed_loss_ratio) > 10,
]

# Where raw LOSS_RATIO is non-sentinel and computed ratio is available
both_present <- lr_all[
  !is.na(lr_all$LOSS_RATIO) &
    !lr_all$LOSS_RATIO %in% SENTINELS,
]
r_raw_vs_computed <- if (nrow(both_present) > 100) {
  round(cor(both_present$LOSS_RATIO, both_present$computed_loss_ratio,
            use = "complete.obs"), 4)
} else NA_real_

raw_vs_computed <- list(
  raw_lr_column = list(
    n_total          = length(raw_lr),
    n_null           = sum(is.na(raw_lr)),
    n_sentinel_99999 = sum(raw_lr == 99999, na.rm = TRUE),
    n_sentinel_99998 = sum(raw_lr == 99998, na.rm = TRUE),
    n_sentinel_99997 = sum(raw_lr == 99997, na.rm = TRUE),
    n_sentinel_total = sum(raw_lr %in% SENTINELS, na.rm = TRUE),
    pct_sentinel     = round(sum(raw_lr %in% SENTINELS, na.rm = TRUE) / length(raw_lr) * 100, 2),
    n_valid          = length(raw_lr_val),
    valid_stats = list(
      min    = round(min(raw_lr_val),                    4),
      q1     = round(quantile(raw_lr_val, 0.25),         4),
      median = round(median(raw_lr_val),                 4),
      mean   = round(mean(raw_lr_val),                   4),
      q3     = round(quantile(raw_lr_val, 0.75),         4),
      max    = round(max(raw_lr_val),                    4)
    )
  ),
  computed_lr = list(
    n_with_value  = total_with_lr,
    n_abs_gt_10   = sum(abs(lr_all$computed_loss_ratio) > 10),
    stats = list(
      min    = round(min(lr_all$computed_loss_ratio),                4),
      q1     = round(quantile(lr_all$computed_loss_ratio, 0.25),     4),
      median = round(median(lr_all$computed_loss_ratio),             4),
      mean   = round(mean(lr_all$computed_loss_ratio),               4),
      q3     = round(quantile(lr_all$computed_loss_ratio, 0.75),     4),
      max    = round(max(lr_all$computed_loss_ratio),                4)
    )
  ),
  n_sentinel_masked_and_computed_extreme = nrow(sentinel_masked_extreme),
  n_both_nonsentinel_present             = nrow(both_present),
  correlation_raw_vs_computed            = r_raw_vs_computed,
  interpretation = paste0(
    "The raw LOSS_RATIO column uses sentinel values (99997/99998/99999) to mask rows where the ",
    "loss ratio is undefined, infinite, or otherwise flagged. ",
    "computed_loss_ratio = PRD_INCRD_LOSSES_AMT / PRD_ERND_PREM_AMT exposes those values directly. ",
    "Rows where the raw column is sentinel and the computed ratio is extreme (|LR|>10) reveal ",
    "cases the dataset originally suppressed."
  )
)

# --- Yearly LR statistics showing distortion --------------------------------
# LR stats from lr_all (has computed_loss_ratio locally); negative-loss count from raw (all rows).
neg_losses_by_year_all <- raw |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(n_negative_losses = sum(PRD_INCRD_LOSSES_AMT < 0, na.rm = TRUE), .groups = "drop")

yearly_lr_stats <- lr_all |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(
    n_with_lr          = n(),
    n_abs_gt_10        = sum(abs(computed_loss_ratio) > 10),
    n_abs_gt_100       = sum(abs(computed_loss_ratio) > 100),
    n_negative_lr      = sum(computed_loss_ratio < 0),
    mean_lr_unfiltered = round(mean(computed_loss_ratio), 4),
    mean_lr_abs_le_2   = round(mean(computed_loss_ratio[abs(computed_loss_ratio) <= 2]), 4),
    median_lr          = round(median(computed_loss_ratio), 4),
    .groups = "drop"
  ) |>
  left_join(neg_losses_by_year_all, by = "year") |>
  arrange(year)

# --- Assemble output --------------------------------------------------------
lr_diagnostics_out <- list(
  description = paste0(
    "Diagnostic investigation of computed_loss_ratio = PRD_INCRD_LOSSES_AMT / PRD_ERND_PREM_AMT. ",
    "Triggered by implausibly large positive and negative yearly averages in eda_summary.json. ",
    "Analysis covers all rows where PRD_ERND_PREM_AMT > 0, including COMMPOL and zero-WP rows, ",
    "to capture the full scope of the instability before any modeling filters are applied."
  ),
  extreme_value_counts             = extreme_counts,
  yearly_lr_statistics             = yearly_lr_stats,
  top_100_most_positive_lr         = top_positive_lr,
  top_100_most_negative_lr         = top_negative_lr,
  breakdown_of_extreme_abs_gt_10   = breakdown_extreme,
  small_denominator_analysis = list(
    description = paste0(
      "Among rows where |computed_loss_ratio| > 10: ",
      "distribution of PRD_ERND_PREM_AMT vs normal rows (|LR| <= 2)."
    ),
    ernd_prem_among_extreme_rows  = ernd_prem_comparison$extreme_lr_abs_gt_10,
    ernd_prem_among_normal_rows   = ernd_prem_comparison$normal_lr_abs_le_2,
    extreme_rows_pct_below_threshold = denom_analysis
  ),
  negative_losses_analysis         = negative_losses_analysis,
  raw_vs_computed_comparison       = raw_vs_computed
)

# =============================================================================
# SECTION 12: Lag Feature Investigation
# =============================================================================

cat("[12] Lag feature investigation...\n")

# --- Dataset grain check ----------------------------------------------------
# Verify (AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR, year) is unique before self-join.
grain_dupes <- raw |>
  count(AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR, STAT_PROFILE_DATE_YEAR) |>
  filter(n > 1)
grain_is_unique <- nrow(grain_dupes) == 0

# --- Prior-year lookup (all raw years including 2005) -----------------------
# 2005 is excluded from modeling but its rows serve as valid lag sources for 2006 train rows.
# Sentinel values in RETENTION_RATIO and LOSS_RATIO are copied faithfully — not filtered here.
# Construction and cleaning are kept as separate steps.
prior_year_lookup <- raw |>
  select(
    AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR,
    prior_year    = STAT_PROFILE_DATE_YEAR,
    prev_rr_raw   = RETENTION_RATIO,
    prev_lr_raw   = LOSS_RATIO
  ) |>
  mutate(
    current_year    = prior_year + 1L,
    prior_row_found = TRUE
  )

# --- Base modeling dataset for investigation (years 2006-2014) --------------
lag_base <- raw |>
  filter(PROD_ABBR != "COMMPOL", WRTN_PREM_AMT > 0,
         STAT_PROFILE_DATE_YEAR %in% 2006:2014) |>
  mutate(log_wp = log(WRTN_PREM_AMT + 1))

n_lag_total <- nrow(lag_base)
n_lag_train <- sum(lag_base$STAT_PROFILE_DATE_YEAR %in% 2006:2012)
n_lag_test  <- sum(lag_base$STAT_PROFILE_DATE_YEAR %in% 2013:2014)

# --- Self-join for Features 1 & 2 ------------------------------------------
# Construction philosophy: copy prior-year field values faithfully, including sentinel values.
# Sentinel values are excluded only when computing usable coverage and distribution statistics.
lag_df <- lag_base |>
  left_join(
    prior_year_lookup,
    by = c("AGENCY_ID", "PROD_ABBR", "PROD_LINE", "STATE_ABBR",
           "STAT_PROFILE_DATE_YEAR" = "current_year")
  ) |>
  mutate(
    prior_year_row_found = !is.na(prior_row_found),
    # Feature 1 — usable after sentinel exclusion (for analysis only; construction = prev_rr_raw)
    prev_rr_usable = if_else(
      prior_year_row_found & !is.na(prev_rr_raw) & !prev_rr_raw %in% SENTINELS,
      prev_rr_raw,
      NA_real_
    ),
    # Feature 2 — usable after sentinel exclusion (for analysis only; construction = prev_lr_raw)
    prev_lr_usable = if_else(
      prior_year_row_found & !is.na(prev_lr_raw) & !prev_lr_raw %in% SENTINELS,
      prev_lr_raw,
      NA_real_
    ),
    # Feature 3: AVG_PREMIUM_PER_POLICY_LAST_YEAR (no join — uses existing PREV_ columns)
    avg_prem_per_policy_ly = if_else(
      !is.na(PREV_POLY_INFORCE_QTY) & PREV_POLY_INFORCE_QTY > 0 & !is.na(PREV_WRTN_PREM_AMT),
      PREV_WRTN_PREM_AMT / PREV_POLY_INFORCE_QTY,
      NA_real_
    )
  )

n_join_miss <- sum(!lag_df$prior_year_row_found)

# --- Helpers ----------------------------------------------------------------
# Reports coverage statistics on a usable (sentinel-cleaned) column.
usable_coverage_stats <- function(usable_col, year_col) {
  total <- length(usable_col)
  avail <- sum(!is.na(usable_col))
  tr    <- year_col %in% 2006:2012
  te    <- year_col %in% 2013:2014
  list(
    n_total        = total,
    n_usable       = avail,
    pct_usable     = round(avail / total * 100, 2),
    n_not_usable   = total - avail,
    pct_not_usable = round((total - avail) / total * 100, 2),
    train = list(
      n_total  = sum(tr),
      n_usable = sum(!is.na(usable_col[tr])),
      pct      = round(sum(!is.na(usable_col[tr])) / sum(tr) * 100, 2)
    ),
    test = list(
      n_total  = sum(te),
      n_usable = sum(!is.na(usable_col[te])),
      pct      = round(sum(!is.na(usable_col[te])) / sum(te) * 100, 2)
    )
  )
}

safe_cor <- function(x, y) {
  r <- tryCatch(cor(x, y, use = "pairwise.complete.obs"), error = function(e) NA_real_)
  if (is.null(r) || is.na(r) || is.nan(r)) NA_real_ else round(r, 4)
}

# Reference correlations (existing safe predictors)
cor_ref_prev_wp   <- safe_cor(lag_df$log_wp, log(lag_df$PREV_WRTN_PREM_AMT + 1))
cor_ref_prev_poly <- safe_cor(lag_df$log_wp, log(lag_df$PREV_POLY_INFORCE_QTY + 1))

# ============ FEATURE 1: PREV_RETENTION_RATIO ================================

# Construction: prior-year RETENTION_RATIO copied faithfully (prev_rr_raw includes sentinels).
# NA in constructed value means: no prior-year row found, OR RETENTION_RATIO was null in prior row.
f1_n_joined      <- sum(lag_df$prior_year_row_found)
f1_null_in_join  <- sum(lag_df$prior_year_row_found & is.na(lag_df$prev_rr_raw))
f1_sentinel_99997 <- sum(lag_df$prior_year_row_found & !is.na(lag_df$prev_rr_raw) & lag_df$prev_rr_raw == 99997)
f1_sentinel_99998 <- sum(lag_df$prior_year_row_found & !is.na(lag_df$prev_rr_raw) & lag_df$prev_rr_raw == 99998)
f1_sentinel_99999 <- sum(lag_df$prior_year_row_found & !is.na(lag_df$prev_rr_raw) & lag_df$prev_rr_raw == 99999)
f1_usable_cov    <- usable_coverage_stats(lag_df$prev_rr_usable, lag_df$STAT_PROFILE_DATE_YEAR)
f1_vals          <- lag_df$prev_rr_usable[!is.na(lag_df$prev_rr_usable)]
f1_cor           <- safe_cor(lag_df$log_wp, lag_df$prev_rr_usable)

feature_1_prev_rr <- list(
  name         = "PREV_RETENTION_RATIO",
  construction = paste0(
    "Self-join on (AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR) matching year Y to year Y-1. ",
    "Copies the prior-year RETENTION_RATIO field faithfully, including sentinel values ",
    "(99997/99998/99999). 2005 rows are used as lag source for 2006 training observations. ",
    "Sentinel values are retained during construction and excluded only for usable-coverage analysis."
  ),
  construction_coverage = list(
    n_total           = n_lag_total,
    n_prior_row_found = f1_n_joined,
    n_no_prior_row    = n_join_miss,
    pct_prior_row_found = round(f1_n_joined  / n_lag_total * 100, 2),
    pct_no_prior_row    = round(n_join_miss   / n_lag_total * 100, 2)
  ),
  sentinel_diagnostics = list(
    n_sentinel_99997  = f1_sentinel_99997,
    n_sentinel_99998  = f1_sentinel_99998,
    n_sentinel_99999  = f1_sentinel_99999,
    n_sentinel_any    = f1_sentinel_99997 + f1_sentinel_99998 + f1_sentinel_99999,
    pct_sentinel_any  = round((f1_sentinel_99997 + f1_sentinel_99998 + f1_sentinel_99999) / n_lag_total * 100, 2)
  ),
  null_diagnostics = list(
    n_null_in_joined_row = f1_null_in_join,
    pct_null_in_joined   = round(f1_null_in_join / n_lag_total * 100, 2)
  ),
  usable_coverage = f1_usable_cov,
  distribution_usable     = num_summary(f1_vals, "PREV_RETENTION_RATIO (sentinel-excluded)"),
  correlation_with_log_wp = f1_cor,
  conceptual_note = paste0(
    "PREV_RETENTION_RATIO uses the dataset-provided prior-year RETENTION_RATIO field. ",
    "It is not leakage: it uses only the prior period's retention outcome. ",
    "Lagged retention may capture client relationship quality not encoded in volume metrics alone — ",
    "an agency can grow premium without strong retention (new business offsetting runoff) or retain ",
    "clients while writing less premium (coverage downgrades). ",
    "Its incremental value over PREV_WRTN_PREM_AMT depends on the partial correlation structure, ",
    "to be assessed via VIF in the modeling phase. ",
    "Sentinel values in RETENTION_RATIO are retained during construction but excluded when evaluating usable coverage. ",
    "Coverage gaps are driven primarily by new agency-product-state combinations with no prior-year record, ",
    "compounded by the sentinel rate in the underlying RETENTION_RATIO field."
  )
)

# ============ FEATURE 2: PREV_LOSS_RATIO =====================================

# Construction: prior-year LOSS_RATIO field copied faithfully (prev_lr_raw includes sentinels).
# NA in constructed value means: no prior-year row found, OR LOSS_RATIO was null in prior row.
f2_n_joined       <- sum(lag_df$prior_year_row_found)
f2_null_in_join   <- sum(lag_df$prior_year_row_found & is.na(lag_df$prev_lr_raw))
f2_sentinel_99997 <- sum(lag_df$prior_year_row_found & !is.na(lag_df$prev_lr_raw) & lag_df$prev_lr_raw == 99997)
f2_sentinel_99998 <- sum(lag_df$prior_year_row_found & !is.na(lag_df$prev_lr_raw) & lag_df$prev_lr_raw == 99998)
f2_sentinel_99999 <- sum(lag_df$prior_year_row_found & !is.na(lag_df$prev_lr_raw) & lag_df$prev_lr_raw == 99999)
f2_usable_cov     <- usable_coverage_stats(lag_df$prev_lr_usable, lag_df$STAT_PROFILE_DATE_YEAR)
f2_vals           <- lag_df$prev_lr_usable[!is.na(lag_df$prev_lr_usable)]
f2_cor            <- safe_cor(lag_df$log_wp, lag_df$prev_lr_usable)

feature_2_prev_lr <- list(
  name         = "PREV_LOSS_RATIO",
  construction = paste0(
    "Self-join on (AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR) matching year Y to year Y-1. ",
    "Copies the prior-year LOSS_RATIO field faithfully, including sentinel values ",
    "(99997/99998/99999). 2005 rows are used as lag source for 2006 training observations. ",
    "Sentinel values are retained during construction and excluded only for usable-coverage analysis. ",
    "This is NOT leakage: it uses only the prior period's loss ratio, not the current period's."
  ),
  construction_coverage = list(
    n_total           = n_lag_total,
    n_prior_row_found = f2_n_joined,
    n_no_prior_row    = n_join_miss,
    pct_prior_row_found = round(f2_n_joined / n_lag_total * 100, 2),
    pct_no_prior_row    = round(n_join_miss  / n_lag_total * 100, 2)
  ),
  sentinel_diagnostics = list(
    n_sentinel_99997  = f2_sentinel_99997,
    n_sentinel_99998  = f2_sentinel_99998,
    n_sentinel_99999  = f2_sentinel_99999,
    n_sentinel_any    = f2_sentinel_99997 + f2_sentinel_99998 + f2_sentinel_99999,
    pct_sentinel_any  = round((f2_sentinel_99997 + f2_sentinel_99998 + f2_sentinel_99999) / n_lag_total * 100, 2)
  ),
  null_diagnostics = list(
    n_null_in_joined_row = f2_null_in_join,
    pct_null_in_joined   = round(f2_null_in_join / n_lag_total * 100, 2)
  ),
  usable_coverage         = f2_usable_cov,
  distribution_usable     = num_summary(f2_vals, "PREV_LOSS_RATIO (sentinel-excluded)"),
  correlation_with_log_wp = f2_cor,
  conceptual_note = paste0(
    "PREV_LOSS_RATIO uses the dataset-provided prior-year LOSS_RATIO field — the curated row-level ",
    "loss ratio metric — rather than a recomputed ratio. ",
    "It is not leakage: it uses only the prior period's loss experience. ",
    "It may capture prior-period risk quality or underwriting experience of the agency/product/state book. ",
    "Agencies with low prior-year loss ratios may reflect better underwriting discipline or risk selection, ",
    "potentially signaling sustained or growing premium. ",
    "Sentinel values (99997/99998/99999) are retained during construction but excluded when evaluating ",
    "usable coverage and distribution statistics. Usable coverage should be the primary feasibility criterion: ",
    "if too many rows carry sentinel values, the feature cannot be used without imputation or indicator encoding. ",
    "The feature should advance to predictor-set review only if usable coverage and missingness are acceptable. ",
    "Its usefulness should not be judged solely by marginal correlation — the partial relationship with log_wp ",
    "conditional on PREV_WRTN_PREM_AMT is the relevant quantity, to be assessed in the modeling phase. ",
    "Features 1 and 2 share the same join key, so their join-miss patterns are identical."
  )
)

# ============ FEATURE 3: AVG_PREMIUM_PER_POLICY_LAST_YEAR ====================

f3_null_denom  <- sum(is.na(lag_df$PREV_POLY_INFORCE_QTY))
f3_zero_denom  <- sum(!is.na(lag_df$PREV_POLY_INFORCE_QTY) & lag_df$PREV_POLY_INFORCE_QTY == 0)
f3_null_num    <- sum(!is.na(lag_df$PREV_POLY_INFORCE_QTY) & lag_df$PREV_POLY_INFORCE_QTY > 0 &
                        is.na(lag_df$PREV_WRTN_PREM_AMT))
f3_usable_cov  <- usable_coverage_stats(lag_df$avg_prem_per_policy_ly, lag_df$STAT_PROFILE_DATE_YEAR)
f3_vals        <- lag_df$avg_prem_per_policy_ly[!is.na(lag_df$avg_prem_per_policy_ly)]
f3_cor_raw     <- safe_cor(lag_df$log_wp, lag_df$avg_prem_per_policy_ly)
f3_cor_log     <- safe_cor(lag_df$log_wp, log(lag_df$avg_prem_per_policy_ly + 1))

cor_f3_vs_prev_wp   <- safe_cor(lag_df$avg_prem_per_policy_ly, lag_df$PREV_WRTN_PREM_AMT)
cor_f3_vs_prev_poly <- safe_cor(lag_df$avg_prem_per_policy_ly, lag_df$PREV_POLY_INFORCE_QTY)

feature_3_avg_prem <- list(
  name         = "AVG_PREMIUM_PER_POLICY_LAST_YEAR",
  construction = paste0(
    "Direct computation: PREV_WRTN_PREM_AMT / PREV_POLY_INFORCE_QTY. ",
    "No self-join required — both columns are already present in the dataset as existing lag features. ",
    "Set to NA where PREV_POLY_INFORCE_QTY is zero or NA to avoid undefined ratios."
  ),
  coverage = f3_usable_cov,
  na_decomposition = list(
    prev_poly_null       = f3_null_denom,
    prev_poly_zero       = f3_zero_denom,
    prev_wrtn_null_given_valid_denom = f3_null_num,
    pct_zero_denom       = round(f3_zero_denom / n_lag_total * 100, 2),
    note = "No join required — coverage driven by missingness in existing PREV_ columns only."
  ),
  distribution               = num_summary(f3_vals, "AVG_PREMIUM_PER_POLICY_LAST_YEAR"),
  correlation_with_log_wp            = f3_cor_raw,
  correlation_with_log_wp_log_scaled = f3_cor_log,
  collinearity_with_existing_predictors = list(
    cor_with_PREV_WRTN_PREM_AMT    = cor_f3_vs_prev_wp,
    cor_with_PREV_POLY_INFORCE_QTY = cor_f3_vs_prev_poly,
    collinearity_note = paste0(
      "By construction: PREV_WRTN_PREM_AMT = PREV_POLY_INFORCE_QTY * AVG_PREM_PER_POLICY_LY. ",
      "If both PREV_WRTN_PREM_AMT and PREV_POLY_INFORCE_QTY are included as model predictors ",
      "(or their log transforms), this feature is perfectly collinear and must be excluded. ",
      "Its value is as a parsimonious substitute or when only one PREV_ predictor is used. ",
      "VIF analysis in the modeling phase will confirm this definitively."
    )
  ),
  conceptual_note = paste0(
    "This feature decomposes prior-year premium into book size (PREV_POLY_INFORCE_QTY) and ",
    "pricing intensity (premium per policy). Two agencies with identical PREV_WRTN_PREM_AMT ",
    "but different policy counts have different unit economics: one prices high with few policies, ",
    "the other prices low with many. Whether pricing intensity adds predictive signal beyond ",
    "the two PREV_ components depends on the model specification. If both PREV_ columns ",
    "are already included (as planned), this feature adds no new information and should be excluded."
  )
)

# --- Assemble output --------------------------------------------------------
lag_diagnostics_out <- list(
  description = paste0(
    "Lag feature feasibility investigation for Dataset A Part 2. ",
    "Evaluates three candidate features before OLS predictor selection: ",
    "PREV_RETENTION_RATIO, PREV_LOSS_RATIO, AVG_PREMIUM_PER_POLICY_LAST_YEAR. ",
    "Base population: non-COMMPOL, WRTN_PREM_AMT > 0, years 2006-2014 (train + test). ",
    "No features added to the modeling dataset at this stage. ",
    "Construction philosophy: sentinel values are copied faithfully during the self-join. ",
    "Usable coverage is reported separately after sentinel exclusion."
  ),
  dataset_grain = list(
    join_key          = c("AGENCY_ID", "PROD_ABBR", "PROD_LINE", "STATE_ABBR", "STAT_PROFILE_DATE_YEAR"),
    is_unique_on_key  = grain_is_unique,
    n_duplicate_keys  = nrow(grain_dupes),
    note = paste0(
      "2005 data (partial year, excluded from modeling) is retained in the prior-year lookup. ",
      "This allows 2006 training rows to obtain lag features from 2005 observations."
    )
  ),
  investigation_base = list(
    n_total = n_lag_total,
    n_train = n_lag_train,
    n_test  = n_lag_test,
    n_join_miss_shared = n_join_miss,
    pct_join_miss      = round(n_join_miss / n_lag_total * 100, 2),
    note = paste0(
      "Join miss rate applies to Features 1 and 2 (identical self-join key). ",
      "Feature 3 requires no join and has independent coverage characteristics."
    )
  ),
  reference_correlations = list(
    description        = "Correlations of existing confirmed-safe predictors with log_wp for context",
    log_prev_wp_cor    = cor_ref_prev_wp,
    log_prev_poly_cor  = cor_ref_prev_poly
  ),
  feature_1_prev_retention_ratio = feature_1_prev_rr,
  feature_2_prev_loss_ratio      = feature_2_prev_lr,
  feature_3_avg_prem_per_policy_ly = feature_3_avg_prem
)

# =============================================================================
# SECTION 13: LOSS_RATIO Zero Investigation
# =============================================================================

cat("[13] LOSS_RATIO zero investigation...\n")

# Working population: non-null, non-sentinel LOSS_RATIO rows (all raw years).
lr_ns <- raw |>
  filter(!is.na(LOSS_RATIO), !LOSS_RATIO %in% SENTINELS)

n_raw_total   <- nrow(raw)
n_lr_null     <- sum(is.na(raw$LOSS_RATIO))
n_lr_sentinel <- sum(!is.na(raw$LOSS_RATIO) & raw$LOSS_RATIO %in% SENTINELS)
n_ns_total    <- nrow(lr_ns)
n_ns_zero     <- sum(lr_ns$LOSS_RATIO == 0)
n_ns_pos      <- sum(lr_ns$LOSS_RATIO > 0)
n_ns_neg      <- sum(lr_ns$LOSS_RATIO < 0)

# --- Classification of zero-LR rows by PRD_INCRD_LOSSES_AMT state ----------
lr_zero    <- lr_ns |> filter(LOSS_RATIO == 0)
lr_nonzero <- lr_ns |> filter(LOSS_RATIO != 0)

zero_type_counts <- lr_zero |>
  mutate(zero_type = case_when(
    is.na(PRD_INCRD_LOSSES_AMT)     ~ "incrd_losses_null",
    PRD_INCRD_LOSSES_AMT == 0       ~ "incrd_losses_zero",
    PRD_INCRD_LOSSES_AMT > 0        ~ "incrd_losses_positive_anomaly",
    PRD_INCRD_LOSSES_AMT < 0        ~ "incrd_losses_negative",
    TRUE                            ~ "other"
  )) |>
  count(zero_type) |>
  mutate(pct = round(n / sum(n) * 100, 2)) |>
  arrange(desc(n))

# Consistency check: zero LR but positive incurred losses — data anomaly.
n_inconsistent <- sum(
  !is.na(lr_zero$PRD_INCRD_LOSSES_AMT) & lr_zero$PRD_INCRD_LOSSES_AMT > 0
)

# Clean zero: LR = 0 AND incurred losses = 0 AND earned premium > 0
n_clean_zero <- sum(
  !is.na(lr_zero$PRD_INCRD_LOSSES_AMT) & lr_zero$PRD_INCRD_LOSSES_AMT == 0 &
  !is.na(lr_zero$PRD_ERND_PREM_AMT)   & lr_zero$PRD_ERND_PREM_AMT > 0
)

# --- Population context -----------------------------------------------------
population_context <- list(
  n_raw_total         = n_raw_total,
  n_lr_null           = n_lr_null,
  pct_lr_null         = round(n_lr_null / n_raw_total * 100, 2),
  n_lr_sentinel       = n_lr_sentinel,
  pct_lr_sentinel     = round(n_lr_sentinel / n_raw_total * 100, 2),
  n_usable            = n_ns_total,
  pct_usable          = round(n_ns_total / n_raw_total * 100, 2),
  n_usable_zero       = n_ns_zero,
  n_usable_positive   = n_ns_pos,
  n_usable_negative   = n_ns_neg,
  pct_usable_zero     = round(n_ns_zero / n_ns_total * 100, 2),
  pct_usable_positive = round(n_ns_pos  / n_ns_total * 100, 2),
  pct_usable_negative = round(n_ns_neg  / n_ns_total * 100, 2)
)

# --- Zero classification ----------------------------------------------------
zero_classification <- list(
  n_zero_total              = n_ns_zero,
  n_incrd_losses_null       = sum(is.na(lr_zero$PRD_INCRD_LOSSES_AMT)),
  n_incrd_losses_zero       = sum(!is.na(lr_zero$PRD_INCRD_LOSSES_AMT) & lr_zero$PRD_INCRD_LOSSES_AMT == 0),
  n_incrd_losses_positive   = n_inconsistent,
  n_incrd_losses_negative   = sum(!is.na(lr_zero$PRD_INCRD_LOSSES_AMT) & lr_zero$PRD_INCRD_LOSSES_AMT < 0),
  pct_incrd_losses_null     = round(sum(is.na(lr_zero$PRD_INCRD_LOSSES_AMT)) / n_ns_zero * 100, 2),
  pct_incrd_losses_zero     = round(sum(!is.na(lr_zero$PRD_INCRD_LOSSES_AMT) & lr_zero$PRD_INCRD_LOSSES_AMT == 0) / n_ns_zero * 100, 2),
  pct_incrd_losses_positive = round(n_inconsistent / n_ns_zero * 100, 2),
  pct_incrd_losses_negative = round(sum(!is.na(lr_zero$PRD_INCRD_LOSSES_AMT) & lr_zero$PRD_INCRD_LOSSES_AMT < 0) / n_ns_zero * 100, 2),
  n_clean_zero_lr_incrd0_ernd_pos = n_clean_zero,
  pct_clean_zero              = round(n_clean_zero / n_ns_zero * 100, 2),
  note = paste0(
    "A 'clean zero' is a row where LOSS_RATIO = 0, PRD_INCRD_LOSSES_AMT = 0, and PRD_ERND_PREM_AMT > 0. ",
    "A 'positive anomaly' is LOSS_RATIO = 0 but PRD_INCRD_LOSSES_AMT > 0 — a data consistency issue. ",
    "A 'negative incrd_losses' row has negative incurred losses (recoveries/adjustments), ",
    "which can produce LOSS_RATIO = 0 only if PRD_INCRD_LOSSES_AMT = 0 or the field is rounded."
  )
)

# --- Earned premium state among zero-LR rows --------------------------------
ernd_prem_among_zeros <- list(
  n_ernd_null     = sum(is.na(lr_zero$PRD_ERND_PREM_AMT)),
  n_ernd_zero     = sum(!is.na(lr_zero$PRD_ERND_PREM_AMT) & lr_zero$PRD_ERND_PREM_AMT == 0),
  n_ernd_positive = sum(!is.na(lr_zero$PRD_ERND_PREM_AMT) & lr_zero$PRD_ERND_PREM_AMT > 0),
  n_ernd_negative = sum(!is.na(lr_zero$PRD_ERND_PREM_AMT) & lr_zero$PRD_ERND_PREM_AMT < 0),
  pct_ernd_null     = round(sum(is.na(lr_zero$PRD_ERND_PREM_AMT)) / n_ns_zero * 100, 2),
  pct_ernd_zero     = round(sum(!is.na(lr_zero$PRD_ERND_PREM_AMT) & lr_zero$PRD_ERND_PREM_AMT == 0) / n_ns_zero * 100, 2),
  pct_ernd_positive = round(sum(!is.na(lr_zero$PRD_ERND_PREM_AMT) & lr_zero$PRD_ERND_PREM_AMT > 0) / n_ns_zero * 100, 2)
)

# --- POLY_INFORCE_QTY comparison: zero-LR vs non-zero-LR -------------------
pif_summary <- function(df, label) {
  vals <- df$POLY_INFORCE_QTY
  pos  <- vals[!is.na(vals) & vals > 0]
  list(
    label       = label,
    n           = nrow(df),
    n_null      = sum(is.na(vals)),
    n_zero      = sum(!is.na(vals) & vals == 0),
    n_positive  = sum(!is.na(vals) & vals > 0),
    pct_null    = round(sum(is.na(vals)) / nrow(df) * 100, 2),
    pct_zero    = round(sum(!is.na(vals) & vals == 0) / nrow(df) * 100, 2),
    median_pos  = if (length(pos) > 0) round(median(pos), 0) else NA_real_,
    mean_pos    = if (length(pos) > 0) round(mean(pos), 1) else NA_real_,
    p25_pos     = if (length(pos) > 0) round(quantile(pos, 0.25), 0) else NA_real_,
    p75_pos     = if (length(pos) > 0) round(quantile(pos, 0.75), 0) else NA_real_
  )
}

poly_inforce_comparison <- list(
  zero_lr_rows    = pif_summary(lr_zero,    "LOSS_RATIO = 0"),
  nonzero_lr_rows = pif_summary(lr_nonzero, "LOSS_RATIO != 0")
)

# --- COMMPOL vs non-COMMPOL -------------------------------------------------
commpol_split <- lr_ns |>
  mutate(is_commpol = PROD_ABBR == "COMMPOL") |>
  group_by(is_commpol) |>
  summarise(
    n_total  = n(),
    n_zero   = sum(LOSS_RATIO == 0),
    pct_zero = round(sum(LOSS_RATIO == 0) / n() * 100, 2),
    .groups = "drop"
  ) |>
  mutate(group = if_else(is_commpol, "COMMPOL", "non_COMMPOL")) |>
  select(group, n_total, n_zero, pct_zero)

# Non-COMMPOL only: zero rate after excluding the structural reporting artifact.
lr_ns_nc     <- lr_ns |> filter(PROD_ABBR != "COMMPOL")
n_nc_total   <- nrow(lr_ns_nc)
n_nc_zero    <- sum(lr_ns_nc$LOSS_RATIO == 0)

# --- By year ----------------------------------------------------------------
by_year <- lr_ns |>
  group_by(year = STAT_PROFILE_DATE_YEAR) |>
  summarise(
    n_total  = n(),
    n_zero   = sum(LOSS_RATIO == 0),
    n_pos    = sum(LOSS_RATIO > 0),
    n_neg    = sum(LOSS_RATIO < 0),
    pct_zero = round(sum(LOSS_RATIO == 0) / n() * 100, 2),
    .groups  = "drop"
  ) |>
  arrange(year)

# --- By PROD_LINE -----------------------------------------------------------
by_prod_line <- lr_ns |>
  group_by(PROD_LINE) |>
  summarise(
    n_total  = n(),
    n_zero   = sum(LOSS_RATIO == 0),
    pct_zero = round(sum(LOSS_RATIO == 0) / n() * 100, 2),
    .groups  = "drop"
  ) |>
  arrange(desc(pct_zero))

# --- By PROD_ABBR -----------------------------------------------------------
by_prod_abbr <- lr_ns |>
  group_by(PROD_ABBR) |>
  summarise(
    n_total  = n(),
    n_zero   = sum(LOSS_RATIO == 0),
    pct_zero = round(sum(LOSS_RATIO == 0) / n() * 100, 2),
    .groups  = "drop"
  ) |>
  arrange(desc(pct_zero))

# --- By STATE_ABBR ----------------------------------------------------------
by_state <- lr_ns |>
  group_by(STATE_ABBR) |>
  summarise(
    n_total  = n(),
    n_zero   = sum(LOSS_RATIO == 0),
    pct_zero = round(sum(LOSS_RATIO == 0) / n() * 100, 2),
    .groups  = "drop"
  ) |>
  arrange(desc(pct_zero))

# --- Assemble ---------------------------------------------------------------
lr_zero_analysis_out <- list(
  description = paste0(
    "Investigation of LOSS_RATIO = 0 in the raw dataset. ",
    "Working population: non-null, non-sentinel LOSS_RATIO rows across all years (2005-2015). ",
    "Motivated by observing ~65% zero rate in usable PREV_LOSS_RATIO values in lag_feature_diagnostics.json."
  ),
  population_context       = population_context,
  zero_classification      = zero_classification,
  ernd_prem_among_zero_lr  = ernd_prem_among_zeros,
  poly_inforce_comparison  = poly_inforce_comparison,
  commpol_split            = commpol_split,
  non_commpol_zero_rate = list(
    n_total  = n_nc_total,
    n_zero   = n_nc_zero,
    pct_zero = round(n_nc_zero / n_nc_total * 100, 2),
    note     = "Zero rate after excluding COMMPOL rows from usable population."
  ),
  by_year      = by_year,
  by_prod_line = by_prod_line,
  by_prod_abbr = by_prod_abbr,
  by_state     = by_state
)

# =============================================================================
# SECTION 14: WP Modeling Dataset Construction & Pre-Specification Diagnostics
# =============================================================================

cat("[14] WP modeling dataset construction and diagnostics...\n")

# ---------------------------------------------------------------------------
# Part A — Construct wp_modeling_data
# Base: lag_df (non-COMMPOL, WP>0, 2006:2014, with self-joined lag features)
# Add: split, log transforms, cleaned lag feature column names.
# ---------------------------------------------------------------------------

wp_modeling_data <- lag_df |>
  mutate(
    split         = if_else(STAT_PROFILE_DATE_YEAR %in% 2006:2012, "train", "test"),
    log_prev_wp   = suppressWarnings(log(PREV_WRTN_PREM_AMT   + 1)),
    log_prev_poly = suppressWarnings(log(PREV_POLY_INFORCE_QTY + 1)),
    # Standardized names for candidate lag features
    PREV_RETENTION_RATIO      = prev_rr_usable,
    PREV_LOSS_RATIO           = prev_lr_usable,
    AVG_PREMIUM_PER_POLICY_LY = avg_prem_per_policy_ly
  ) |>
  mutate(
    log_prev_wp   = if_else(is.nan(log_prev_wp),   NA_real_, log_prev_wp),
    log_prev_poly = if_else(is.nan(log_prev_poly), NA_real_, log_prev_poly)
  )

n_wp_total <- nrow(wp_modeling_data)
n_wp_train <- sum(wp_modeling_data$split == "train")
n_wp_test  <- sum(wp_modeling_data$split == "test")
wp_train   <- wp_modeling_data |> filter(split == "train")
wp_test    <- wp_modeling_data |> filter(split == "test")

# ---------------------------------------------------------------------------
# Population waterfall
# ---------------------------------------------------------------------------

n_after_year    <- nrow(raw |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2014))
n_after_commpol <- nrow(raw |> filter(STAT_PROFILE_DATE_YEAR %in% 2006:2014, PROD_ABBR != "COMMPOL"))

pop_waterfall <- list(
  n_raw                    = nrow(raw),
  n_after_year_filter      = n_after_year,
  n_removed_by_year_filter = nrow(raw) - n_after_year,
  n_after_commpol_exclusion = n_after_commpol,
  n_removed_commpol        = n_after_year - n_after_commpol,
  n_after_wp_filter        = n_wp_total,
  n_removed_wp_le0         = n_after_commpol - n_wp_total,
  n_final_train            = n_wp_train,
  n_final_test             = n_wp_test,
  note = paste0(
    "Year filter: 2006-2014 (2005 and 2015 excluded as partial reporting years). ",
    "COMMPOL (PROD_ABBR = 'COMMPOL') excluded as structural reporting artifact with zero premium by design. ",
    "WP > 0 conditions on active premium-producing observations."
  )
)

# ---------------------------------------------------------------------------
# Part B — Candidate predictors
# ---------------------------------------------------------------------------

candidate_predictors <- list(
  core = list(
    list(name = "STAT_PROFILE_DATE_YEAR", type = "integer",     role = "year_index",         leakage = "none"),
    list(name = "STATE_ABBR",             type = "categorical",  role = "geography",           leakage = "none"),
    list(name = "PROD_LINE",              type = "categorical",  role = "product_line",        leakage = "none"),
    list(name = "PROD_ABBR",             type = "categorical",  role = "product",             leakage = "none"),
    list(name = "VENDOR",                 type = "categorical",  role = "technology_vendor",   leakage = "none"),
    list(name = "ACTIVE_PRODUCERS",       type = "numeric",      role = "agency_size_proxy",   leakage = "none"),
    list(name = "AGENCY_APPOINTMENT_YEAR",type = "integer",      role = "agency_tenure_proxy", leakage = "none"),
    list(name = "MAX_AGE",                type = "numeric",      role = "producer_age_profile",leakage = "none"),
    list(name = "MIN_AGE",                type = "numeric",      role = "producer_age_profile",leakage = "none"),
    list(name = "PREV_WRTN_PREM_AMT",    type = "numeric",      role = "lagged_volume",       leakage = "none",
         modeling_transform = "log(PREV_WRTN_PREM_AMT + 1)"),
    list(name = "PREV_POLY_INFORCE_QTY", type = "numeric",      role = "lagged_volume",       leakage = "none",
         modeling_transform = "log(PREV_POLY_INFORCE_QTY + 1)")
  ),
  lag_features_investigated = list(
    list(name = "PREV_RETENTION_RATIO",      type = "numeric", role = "lagged_behavior",
         source = "self_join_prior_year_RETENTION_RATIO_sentinel_cleaned",
         leakage = "none"),
    list(name = "PREV_LOSS_RATIO",           type = "numeric", role = "lagged_risk",
         source = "self_join_prior_year_LOSS_RATIO_sentinel_cleaned",
         leakage = "none"),
    list(name = "AVG_PREMIUM_PER_POLICY_LY", type = "numeric", role = "pricing_intensity",
         source = "derived_PREV_WRTN_PREM_AMT_div_PREV_POLY_INFORCE_QTY",
         leakage = "none",
         collinearity_warning = "Perfectly collinear with PREV_WRTN_PREM_AMT + PREV_POLY_INFORCE_QTY if both are in the model")
  )
)

# ---------------------------------------------------------------------------
# Coverage summaries
# ---------------------------------------------------------------------------

by_year_cov <- wp_modeling_data |>
  group_by(year = STAT_PROFILE_DATE_YEAR, split) |>
  summarise(n = n(), .groups = "drop") |>
  arrange(year)

by_prod_line_cov <- wp_modeling_data |>
  group_by(PROD_LINE) |>
  summarise(
    n_total = n(),
    n_train = sum(split == "train"),
    n_test  = sum(split == "test"),
    .groups = "drop"
  )

by_prod_abbr_cov <- wp_modeling_data |>
  group_by(PROD_ABBR) |>
  summarise(
    n_total = n(),
    n_train = sum(split == "train"),
    n_test  = sum(split == "test"),
    .groups = "drop"
  ) |>
  arrange(desc(n_total))

by_state_cov <- wp_modeling_data |>
  group_by(STATE_ABBR) |>
  summarise(
    n_total = n(),
    n_train = sum(split == "train"),
    n_test  = sum(split == "test"),
    .groups = "drop"
  ) |>
  arrange(desc(n_total))

# ---------------------------------------------------------------------------
# Agency diagnostics (descriptive only — agencies not used as predictors)
# ---------------------------------------------------------------------------

agencies_train <- unique(wp_train$AGENCY_ID)
agencies_test  <- unique(wp_test$AGENCY_ID)
agencies_both  <- intersect(agencies_train, agencies_test)

agency_diagnostics <- list(
  n_unique_train    = length(agencies_train),
  n_unique_test     = length(agencies_test),
  n_in_both         = length(agencies_both),
  n_train_only      = length(setdiff(agencies_train, agencies_test)),
  n_test_only       = length(setdiff(agencies_test,  agencies_train)),
  pct_test_in_train = round(length(agencies_both) / length(agencies_test) * 100, 2),
  note = paste0(
    "Agencies are not used as predictors. Reported for structural awareness only. ",
    "Agency overlap between train and test sets informs how well the model is expected to generalize ",
    "to agencies it has not seen, vs. agencies seen in training with different year profiles."
  )
)

# ---------------------------------------------------------------------------
# Missingness review
# ---------------------------------------------------------------------------

all_candidate_cols <- c(
  "STAT_PROFILE_DATE_YEAR", "STATE_ABBR", "PROD_LINE", "PROD_ABBR",
  "VENDOR", "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
  "PREV_WRTN_PREM_AMT", "PREV_POLY_INFORCE_QTY",
  "PREV_RETENTION_RATIO", "PREV_LOSS_RATIO", "AVG_PREMIUM_PER_POLICY_LY"
)

miss_rows <- lapply(all_candidate_cols, function(col) {
  n_miss_tr  <- sum(is.na(wp_train[[col]]))
  n_miss_te  <- sum(is.na(wp_test[[col]]))
  pct_miss   <- round(sum(is.na(wp_modeling_data[[col]])) / n_wp_total * 100, 2)
  list(
    predictor         = col,
    n_missing         = sum(is.na(wp_modeling_data[[col]])),
    pct_missing       = pct_miss,
    train_pct_missing = round(n_miss_tr / n_wp_train * 100, 2),
    test_pct_missing  = round(n_miss_te / n_wp_test  * 100, 2)
  )
})

extract_flag <- function(rows, threshold) {
  flagged <- Filter(function(x) x$pct_missing > threshold, rows)
  lapply(flagged, function(x) list(predictor = x$predictor, pct_missing = x$pct_missing))
}

missingness_review <- list(
  predictor_missingness      = miss_rows,
  predictors_gt5pct_missing  = extract_flag(miss_rows, 5),
  predictors_gt20pct_missing = extract_flag(miss_rows, 20),
  predictors_gt50pct_missing = extract_flag(miss_rows, 50)
)

# ---------------------------------------------------------------------------
# Complete-case impact
# ---------------------------------------------------------------------------

core_cols <- c(
  "STAT_PROFILE_DATE_YEAR", "STATE_ABBR", "PROD_LINE", "PROD_ABBR",
  "VENDOR", "ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE",
  "PREV_WRTN_PREM_AMT", "PREV_POLY_INFORCE_QTY"
)

cc_stats <- function(cols) {
  n_cc_all  <- sum(complete.cases(wp_modeling_data[, cols, drop = FALSE]))
  n_cc_tr   <- sum(complete.cases(wp_train[,          cols, drop = FALSE]))
  n_cc_te   <- sum(complete.cases(wp_test[,           cols, drop = FALSE]))
  list(
    predictors_included = cols,
    n_complete          = n_cc_all,
    n_dropped           = n_wp_total - n_cc_all,
    pct_complete        = round(n_cc_all / n_wp_total * 100, 2),
    pct_dropped         = round((n_wp_total - n_cc_all) / n_wp_total * 100, 2),
    train = list(
      n_complete   = n_cc_tr,
      n_dropped    = n_wp_train - n_cc_tr,
      pct_complete = round(n_cc_tr / n_wp_train * 100, 2)
    ),
    test = list(
      n_complete   = n_cc_te,
      n_dropped    = n_wp_test - n_cc_te,
      pct_complete = round(n_cc_te / n_wp_test * 100, 2)
    )
  )
}

complete_case_impact <- list(
  core_only                        = cc_stats(core_cols),
  core_plus_prev_retention_ratio   = cc_stats(c(core_cols, "PREV_RETENTION_RATIO")),
  core_plus_prev_loss_ratio        = cc_stats(c(core_cols, "PREV_LOSS_RATIO")),
  core_plus_avg_prem_per_policy_ly = cc_stats(c(core_cols, "AVG_PREMIUM_PER_POLICY_LY")),
  all_candidates                   = cc_stats(all_candidate_cols)
)

# ---------------------------------------------------------------------------
# Part D — WP vs log(WP) distributional analysis
# ---------------------------------------------------------------------------

skewness_moment <- function(x) {
  x <- x[!is.na(x) & is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  s <- sd(x)
  if (s == 0) return(NA_real_)
  round(mean((x - m)^3) / s^3, 4)
}

extended_dist <- function(x, label) {
  x <- x[!is.na(x) & is.finite(x)]
  list(
    label   = label,
    n_valid = length(x),
    min     = round(min(x),                4),
    p1      = round(quantile(x, 0.01),     4),
    p5      = round(quantile(x, 0.05),     4),
    p25     = round(quantile(x, 0.25),     4),
    p50     = round(median(x),             4),
    mean    = round(mean(x),               4),
    p75     = round(quantile(x, 0.75),     4),
    p95     = round(quantile(x, 0.95),     4),
    p99     = round(quantile(x, 0.99),     4),
    max     = round(max(x),                4),
    sd      = round(sd(x),                 4)
  )
}

skew_raw <- skewness_moment(wp_modeling_data$WRTN_PREM_AMT)
skew_log <- skewness_moment(wp_modeling_data$log_wp)

wp_raw_p99  <- round(quantile(wp_modeling_data$WRTN_PREM_AMT, 0.99), 2)
wp_raw_p50  <- round(median(wp_modeling_data$WRTN_PREM_AMT), 2)

wp_log_transform_analysis <- list(
  wrtn_prem_raw = extended_dist(wp_modeling_data$WRTN_PREM_AMT, "WRTN_PREM_AMT"),
  log_wp        = extended_dist(wp_modeling_data$log_wp,        "log(WRTN_PREM_AMT + 1)"),
  skewness_raw  = skew_raw,
  skewness_log  = skew_log,
  p99_to_p50_ratio_raw = round(wp_raw_p99 / wp_raw_p50, 1),
  commentary = paste0(
    "WRTN_PREM_AMT is highly right-skewed (skewness = ", skew_raw, "). ",
    "The p99/p50 ratio of ", round(wp_raw_p99 / wp_raw_p50, 1), " illustrates the extreme right tail: ",
    "the 99th-percentile row writes roughly ", round(wp_raw_p99 / wp_raw_p50, 0), "x more premium than the median row. ",
    "A small fraction of agency-product-state-year cells dominate the variance in raw dollars. ",
    "log(WRTN_PREM_AMT + 1) reduces skewness to ", skew_log, ", compressing the right tail and producing ",
    "a distribution that is far more consistent with OLS assumptions (near-symmetric, stabilized variance). ",
    "Modeling raw WRTN_PREM_AMT directly would violate OLS homoskedasticity: residual variance would grow ",
    "with fitted values across the premium range. ",
    "The log transform is strongly justified as the primary specification. ",
    "Raw-dollar modeling is not recommended but could be explored as a sensitivity check after ",
    "the log-scale model is established, or as a comparison using a robust or quantile regression."
  )
)

# ---------------------------------------------------------------------------
# Assemble output
# ---------------------------------------------------------------------------

modeling_dataset_diagnostics_out <- list(
  description = paste0(
    "WP modeling dataset construction and pre-specification diagnostics. ",
    "Population: non-COMMPOL, WRTN_PREM_AMT > 0, years 2006-2014. ",
    "Train: 2006-2012. Test: 2013-2014. ",
    "No models are fitted in this section."
  ),
  population_waterfall      = pop_waterfall,
  candidate_predictors      = candidate_predictors,
  coverage_summaries        = list(
    by_year      = by_year_cov,
    by_prod_line = by_prod_line_cov,
    by_prod_abbr = by_prod_abbr_cov,
    by_state     = by_state_cov
  ),
  agency_diagnostics        = agency_diagnostics,
  missingness_review        = missingness_review,
  complete_case_impact      = complete_case_impact,
  wp_log_transform_analysis = wp_log_transform_analysis
)

# =============================================================================
# SECTION 15: Quote/Bind Variable Review
# =============================================================================

cat("[15] Quote/bind variable review...\n")

QB_CL  <- c("CL_BOUND_CT_MDS",    "CL_QUO_CT_MDS",
            "CL_BOUND_CT_SBZ",    "CL_QUO_CT_SBZ",
            "CL_BOUND_CT_eQT",    "CL_QUO_CT_eQT")
QB_PL  <- c("PL_BOUND_CT_ELINKS",      "PL_QUO_CT_ELINKS",
            "PL_BOUND_CT_PLRANK",      "PL_QUO_CT_PLRANK",
            "PL_BOUND_CT_eQTte",       "PL_QUO_CT_eQTte",
            "PL_BOUND_CT_APPLIED",     "PL_QUO_CT_APPLIED",
            "PL_BOUND_CT_TRANSACTNOW", "PL_QUO_CT_TRANSACTNOW")
QB_ALL <- c(QB_CL, QB_PL)

vars_present <- QB_ALL[QB_ALL %in% names(raw)]
vars_absent  <- QB_ALL[!QB_ALL %in% names(raw)]
cat(sprintf("  Variables present: %d  absent: %d\n", length(vars_present), length(vars_absent)))

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

safe_cor_qb <- function(x, y) {
  r <- tryCatch(cor(x, y, use = "pairwise.complete.obs"), error = function(e) NA_real_)
  if (length(r) == 0 || is.na(r)) return(NA_real_)
  round(r, 4)
}

dist_summary_qb <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(n = 0L, note = "all_missing"))
  list(
    n        = length(x),
    min      = round(min(x), 2),
    p01      = as.numeric(round(quantile(x, 0.01, names = FALSE), 2)),
    p25      = as.numeric(round(quantile(x, 0.25, names = FALSE), 2)),
    median   = round(median(x), 2),
    mean     = round(mean(x), 2),
    p75      = as.numeric(round(quantile(x, 0.75, names = FALSE), 2)),
    p99      = as.numeric(round(quantile(x, 0.99, names = FALSE), 2)),
    max      = round(max(x), 2),
    n_zero   = sum(x == 0),
    pct_zero = round(mean(x == 0) * 100, 2)
  )
}

# ---------------------------------------------------------------------------
# Part A: Temporal classification
# ---------------------------------------------------------------------------

cat("  [15A] Temporal classification...\n")

part_a_qb <- setNames(lapply(vars_present, function(v) {

  x <- raw[[v]]
  n <- length(x)

  basic <- list(
    n_total      = n,
    n_null       = sum(is.na(x)),
    n_zero       = sum(x == 0, na.rm = TRUE),
    n_positive   = sum(x > 0,  na.rm = TRUE),
    pct_null     = round(sum(is.na(x)) / n * 100, 2),
    pct_zero     = round(sum(x == 0, na.rm = TRUE) / n * 100, 2),
    pct_positive = round(sum(x > 0, na.rm = TRUE) / n * 100, 2)
  )

  # De-duplicate to one row per (AGENCY_ID, PROD_LINE, STATE_ABBR, year).
  # QB variables are agency-level activity counts that repeat across all PROD_ABBR
  # rows sharing the same agency-line-state-year. Taking the first non-NA value
  # per group gives a clean series for temporal analysis.
  dedup <- raw |>
    select(AGENCY_ID, PROD_LINE, STATE_ABBR,
           yr  = STAT_PROFILE_DATE_YEAR,
           val = all_of(v)) |>
    filter(!is.na(val)) |>
    group_by(AGENCY_ID, PROD_LINE, STATE_ABBR, yr) |>
    summarise(val = val[1L], .groups = "drop")

  # Among groups with 4+ observations, classify the temporal pattern.
  series_stats <- dedup |>
    group_by(AGENCY_ID, PROD_LINE, STATE_ABBR) |>
    arrange(yr, .by_group = TRUE) |>
    filter(n() >= 4) |>
    summarise(
      n_years           = n(),
      all_same          = n_distinct(val) == 1L,
      all_nondecreasing = all(diff(val) >= 0),
      has_decrease      = any(diff(val) < 0),
      has_increase      = any(diff(val) > 0),
      .groups           = "drop"
    )

  n_series <- nrow(series_stats)

  if (n_series == 0) {
    tc    <- "INSUFFICIENT_DATA"
    tstat <- list(n_multi_year_series = 0L, temporal_classification = "INSUFFICIENT_DATA")
  } else {
    pct_const  <- round(mean(series_stats$all_same)          * 100, 1)
    pct_nondec <- round(mean(series_stats$all_nondecreasing)  * 100, 1)
    pct_dec    <- round(mean(series_stats$has_decrease)       * 100, 1)
    pct_inc    <- round(mean(series_stats$has_increase)       * 100, 1)

    tc <- if (pct_const  > 70)                        "CONSTANT"    else
          if (pct_nondec > 70 && pct_const < 30)      "CUMULATIVE"  else
          "ANNUAL_RESET"

    tstat <- list(
      n_multi_year_series        = n_series,
      pct_constant               = pct_const,
      pct_monotone_nondecreasing = pct_nondec,
      pct_has_any_decrease       = pct_dec,
      pct_has_any_increase       = pct_inc,
      temporal_classification    = tc
    )
  }

  # Example series: 3 most-observed (agency, PROD_LINE, STATE_ABBR) combos
  top3 <- dedup |>
    group_by(AGENCY_ID, PROD_LINE, STATE_ABBR) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(-n) |>
    head(3)

  examples <- lapply(seq_len(nrow(top3)), function(i) {
    ag <- top3[i, ]
    s  <- dedup |>
      filter(AGENCY_ID  == ag$AGENCY_ID,
             PROD_LINE  == ag$PROD_LINE,
             STATE_ABBR == ag$STATE_ABBR) |>
      arrange(yr)
    list(
      AGENCY_ID  = ag$AGENCY_ID,
      PROD_LINE  = ag$PROD_LINE,
      STATE_ABBR = ag$STATE_ABBR,
      n_years    = nrow(s),
      by_year    = setNames(as.list(s$val), as.character(s$yr))
    )
  })

  list(
    basic_stats             = basic,
    temporal_stats          = tstat,
    temporal_classification = tc,
    example_series          = examples
  )

}), vars_present)

for (v in vars_absent) part_a_qb[[v]] <- list(note = "Variable not found in dataset")

# ---------------------------------------------------------------------------
# Part B: Leakage assessment
# ---------------------------------------------------------------------------

cat("  [15B] Leakage assessment...\n")

part_b_qb <- setNames(lapply(QB_ALL, function(v) {
  if (!v %in% vars_present) {
    return(list(classification = "ABSENT", reasoning = "Variable not found in dataset"))
  }
  tc <- part_a_qb[[v]]$temporal_classification
  if (tc == "ANNUAL_RESET") {
    list(
      temporal_basis = tc,
      classification = "CONTEMPORANEOUS",
      reasoning = paste0(
        "Values fluctuate year-to-year, consistent with current-year platform activity counts. ",
        "Quote/bind counts for year Y accumulate throughout year Y and are fully known only after ",
        "the reporting period closes. Using the contemporaneous value to predict year Y written ",
        "premium introduces leakage. The prior-year (PREV_) version is safe to use."
      )
    )
  } else if (tc == "CUMULATIVE") {
    list(
      temporal_basis = tc,
      classification = "LIKELY_LEAKAGE",
      reasoning = paste0(
        "Values accumulate monotonically — consistent with cumulative counts since inception. ",
        "The cumulative total as of year Y includes year-Y activity, making it contemporaneous ",
        "with the target. Year-over-year deltas could serve as a lagged proxy but require ",
        "additional construction."
      )
    )
  } else if (tc == "CONSTANT") {
    list(
      temporal_basis = tc,
      classification = "SAFE_FORWARD_LOOKING",
      reasoning = paste0(
        "Values do not change across years — this behaves as a static agency attribute. ",
        "No year-Y activity is encoded, so it is known before year Y begins. ",
        "However, constant predictors provide no year-over-year variation and will have ",
        "negligible predictive value for changes in written premium."
      )
    )
  } else {
    list(
      temporal_basis = tc,
      classification = "AMBIGUOUS",
      reasoning      = "Temporal meaning could not be determined. Manual review recommended."
    )
  }
}), QB_ALL)

# ---------------------------------------------------------------------------
# Part C: Lagged version coverage
# ---------------------------------------------------------------------------

cat("  [15C] Lagged version coverage...\n")

# One prior-year lookup for all QB variables at once.
# Rename QB columns to PREV_* before joining so the result is clean.
qb_py_lookup <- raw |>
  select(AGENCY_ID, PROD_ABBR, PROD_LINE, STATE_ABBR,
         prior_year = STAT_PROFILE_DATE_YEAR,
         all_of(vars_present)) |>
  rename_with(.fn = ~ paste0("PREV_", .), .cols = all_of(vars_present)) |>
  mutate(current_year = prior_year + 1L) |>
  select(-prior_year)

wp_mod_qb <- wp_modeling_data |>
  left_join(
    qb_py_lookup,
    by = c("AGENCY_ID", "PROD_ABBR", "PROD_LINE", "STATE_ABBR",
           "STAT_PROFILE_DATE_YEAR" = "current_year")
  )

part_c_qb <- setNames(lapply(vars_present, function(v) {
  pv <- paste0("PREV_", v)
  x  <- wp_mod_qb[[pv]]
  yr <- wp_mod_qb$STAT_PROFILE_DATE_YEAR
  tr <- yr %in% 2006:2012
  te <- yr %in% 2013:2014
  n  <- length(x)

  list(
    prev_variable_name = pv,
    coverage = list(
      n_total       = n,
      n_available   = sum(!is.na(x)),
      n_missing     = sum(is.na(x)),
      pct_available = round(sum(!is.na(x)) / n * 100, 2),
      pct_missing   = round(sum(is.na(x)) / n * 100, 2),
      train = list(n = sum(tr), n_available = sum(!is.na(x[tr])),
                   pct = round(sum(!is.na(x[tr])) / sum(tr) * 100, 2)),
      test  = list(n = sum(te), n_available = sum(!is.na(x[te])),
                   pct = round(sum(!is.na(x[te])) / sum(te) * 100, 2))
    ),
    distribution = dist_summary_qb(x)
  )
}), vars_present)

for (v in vars_absent) part_c_qb[[v]] <- list(note = "Source variable not found in dataset")

# ---------------------------------------------------------------------------
# Part D: Correlations
# ---------------------------------------------------------------------------

cat("  [15D] Correlations...\n")

part_d_qb <- setNames(lapply(vars_present, function(v) {
  pv          <- paste0("PREV_", v)
  x_cont      <- wp_modeling_data[[v]]
  x_lag       <- wp_mod_qb[[pv]]
  log_wp_col  <- wp_modeling_data$log_wp
  log_ppw_col <- wp_modeling_data$log_prev_wp
  log_ppy_col <- wp_modeling_data$log_prev_poly

  list(
    contemporaneous = list(
      n_pairs_with_log_wp    = sum(!is.na(x_cont) & !is.na(log_wp_col)),
      cor_with_log_wp        = safe_cor_qb(x_cont, log_wp_col),
      cor_with_log_prev_wp   = safe_cor_qb(x_cont, log_ppw_col),
      cor_with_log_prev_poly = safe_cor_qb(x_cont, log_ppy_col)
    ),
    lagged = list(
      n_pairs_with_log_wp    = sum(!is.na(x_lag) & !is.na(log_wp_col)),
      cor_with_log_wp        = safe_cor_qb(x_lag, log_wp_col),
      cor_with_log_prev_wp   = safe_cor_qb(x_lag, log_ppw_col),
      cor_with_log_prev_poly = safe_cor_qb(x_lag, log_ppy_col)
    )
  )
}), vars_present)

for (v in vars_absent) part_d_qb[[v]] <- list(note = "Source variable not found in dataset")

# ---------------------------------------------------------------------------
# Part E: Recommendations
# ---------------------------------------------------------------------------

cat("  [15E] Recommendations...\n")

part_e_qb <- setNames(lapply(QB_ALL, function(v) {
  if (!v %in% vars_present) {
    return(list(recommendation = "ABSENT", justification = "Variable not found in dataset"))
  }
  tc       <- part_a_qb[[v]]$temporal_classification
  lk       <- part_b_qb[[v]]$classification
  lag_cov  <- part_c_qb[[v]]$coverage$pct_available
  cor_lag  <- part_d_qb[[v]]$lagged$cor_with_log_wp
  cor_ppw  <- part_d_qb[[v]]$contemporaneous$cor_with_log_prev_wp

  if (tc == "CONSTANT") {
    list(
      recommendation = "EXCLUDE",
      justification  = "No year-over-year variation. Does not carry predictive information."
    )
  } else if (lk %in% c("CONTEMPORANEOUS", "LIKELY_LEAKAGE")) {
    lag_promising <- !is.na(lag_cov) && lag_cov >= 80 &&
                     !is.na(cor_lag) && abs(cor_lag) >= 0.10
    if (lag_promising) {
      list(
        recommendation = "CREATE_LAGGED_VERSION",
        justification  = sprintf(
          paste0("Contemporaneous variable is leakage (%s). Lagged version has %.1f%% coverage ",
                 "and correlates with log_wp at r=%.3f. Correlation with log_prev_wp is r=%.3f. ",
                 "Worth testing in OLS 5+."),
          lk, lag_cov,
          ifelse(is.na(cor_lag), 0, cor_lag),
          ifelse(is.na(cor_ppw), 0, cor_ppw)
        )
      )
    } else {
      list(
        recommendation = "EXCLUDE",
        justification  = sprintf(
          paste0("Contemporaneous variable is leakage (%s). Lagged version has %.1f%% coverage ",
                 "and correlates with log_wp at r=%.3f — below the 0.10 threshold for inclusion."),
          lk,
          ifelse(is.na(lag_cov), 0, lag_cov),
          ifelse(is.na(cor_lag), 0, cor_lag)
        )
      )
    }
  } else {
    list(
      recommendation = "INVESTIGATE_FURTHER",
      justification  = paste0("Temporal classification: ", tc, ". Manual review recommended.")
    )
  }
}), QB_ALL)

recs_vec <- sapply(part_e_qb, function(x) x$recommendation)
part_e_summary_qb <- list(
  n_create_lagged    = sum(recs_vec == "CREATE_LAGGED_VERSION"),
  n_exclude          = sum(recs_vec == "EXCLUDE"),
  n_investigate      = sum(recs_vec == "INVESTIGATE_FURTHER"),
  n_absent           = sum(recs_vec == "ABSENT"),
  create_lagged_vars = as.list(names(recs_vec[recs_vec == "CREATE_LAGGED_VERSION"])),
  exclude_vars       = as.list(names(recs_vec[recs_vec == "EXCLUDE"])),
  investigate_vars   = as.list(names(recs_vec[recs_vec == "INVESTIGATE_FURTHER"])),
  overall_assessment = paste0(
    if (sum(recs_vec == "CREATE_LAGGED_VERSION") > 0)
      sprintf("%d variable(s) meet the threshold for lagged version construction and OLS testing. ",
              sum(recs_vec == "CREATE_LAGGED_VERSION"))
    else
      "No quote/bind variables meet the threshold for lagged version construction. ",
    "Contemporaneous versions introduce leakage and must not be used directly in predictive models."
  )
)

# ---------------------------------------------------------------------------
# Assemble output
# ---------------------------------------------------------------------------

quote_bind_review_out <- list(
  description = paste0(
    "Investigation of quote/bind activity variables ",
    "(CL platforms: MDS, SBZ, eQT; PL platforms: ELINKS, PLRANK, eQTte, APPLIED, TRANSACTNOW). ",
    "Parts: (A) temporal meaning determined from multi-year agency tracking, ",
    "(B) leakage classification for WP modeling, ",
    "(C) lagged version coverage on the modeling population (non-COMMPOL, WP>0, 2006-2014), ",
    "(D) predictive screening via correlations with log_wp / log_prev_wp / log_prev_poly, ",
    "(E) recommendations. No models are fitted."
  ),
  variables_present      = as.list(vars_present),
  variables_absent       = as.list(vars_absent),
  part_a_temporal        = part_a_qb,
  part_b_leakage         = part_b_qb,
  part_c_lag_coverage    = part_c_qb,
  part_d_correlations    = part_d_qb,
  part_e_recommendations = part_e_qb,
  part_e_summary         = part_e_summary_qb
)

# =============================================================================
# SECTION 16: Operational-History Variable Review
# =============================================================================

cat("[16] Operational-history variable review...\n")

OH_VARS    <- c("PL_START_YEAR",             "PL_END_YEAR",
                "CL_START_YEAR",             "CL_END_YEAR",
                "COMMISIONS_START_YEAR",     "COMMISIONS_END_YEAR",
                "ACTIVITY_NOTES_START_YEAR", "ACTIVITY_NOTES_END_YEAR")
oh_present <- OH_VARS[OH_VARS %in% names(raw)]
oh_absent  <- OH_VARS[!OH_VARS %in% names(raw)]
cat(sprintf("  Variables present: %d  absent: %d\n", length(oh_present), length(oh_absent)))

# ---------------------------------------------------------------------------
# Part A: Per-variable summary
# ---------------------------------------------------------------------------

cat("  [16A] Variable summaries...\n")

part_a_oh <- setNames(lapply(oh_present, function(v) {
  x  <- raw[[v]]
  n  <- length(x)
  nn <- sum(is.na(x))
  ns <- sum(!is.na(x) & x %in% SENTINELS)
  nu <- sum(!is.na(x) & !x %in% SENTINELS)
  us <- x[!is.na(x) & !x %in% SENTINELS]
  list(
    n_total          = n,
    n_null           = nn,
    n_sentinel       = ns,
    n_usable         = nu,
    pct_null         = round(nn / n * 100, 2),
    pct_sentinel     = round(ns / n * 100, 2),
    pct_usable       = round(nu / n * 100, 2),
    sentinel_breakdown = list(
      n_99997 = sum(!is.na(x) & x == 99997),
      n_99998 = sum(!is.na(x) & x == 99998),
      n_99999 = sum(!is.na(x) & x == 99999)
    ),
    usable_range = if (length(us) > 0) list(
      min             = min(us),
      max             = max(us),
      n_distinct      = length(unique(us)),
      distinct_values = as.list(sort(unique(us)))
    ) else list(note = "no usable values")
  )
}), oh_present)

for (v in oh_absent) part_a_oh[[v]] <- list(note = "Variable not found in dataset")

# ---------------------------------------------------------------------------
# Part B: Temporal patterns across agencies
# ---------------------------------------------------------------------------

cat("  [16B] Temporal patterns...\n")

# Check whether the variable is consistent within (AGENCY_ID, year) across all PROD_ABBR rows.
oh_within_consistency <- setNames(lapply(oh_present, function(v) {
  raw |>
    select(AGENCY_ID, yr = STAT_PROFILE_DATE_YEAR, val = all_of(v)) |>
    filter(!is.na(val)) |>
    group_by(AGENCY_ID, yr) |>
    summarise(n_distinct_vals = n_distinct(val), .groups = "drop") |>
    summarise(
      n_agency_years      = n(),
      n_inconsistent      = sum(n_distinct_vals > 1),
      pct_consistent      = round(mean(n_distinct_vals == 1) * 100, 1)
    ) |>
    as.list()
}), oh_present)

# De-duplicate to (AGENCY_ID, year) and classify temporal pattern across years.
oh_temporal <- setNames(lapply(oh_present, function(v) {
  dedup <- raw |>
    select(AGENCY_ID, yr = STAT_PROFILE_DATE_YEAR, val = all_of(v)) |>
    filter(!is.na(val)) |>
    group_by(AGENCY_ID, yr) |>
    summarise(val = val[1L], .groups = "drop")

  series <- dedup |>
    group_by(AGENCY_ID) |>
    arrange(yr, .by_group = TRUE) |>
    filter(n() >= 4) |>
    summarise(
      n_years            = n(),
      all_same           = n_distinct(val) == 1L,
      all_sentinel       = all(val %in% SENTINELS),
      has_sentinel       = any(val %in% SENTINELS),
      has_real           = any(!val %in% SENTINELS),
      sentinel_to_real   = any(diff(as.integer(!val %in% SENTINELS)) == 1),
      real_to_sentinel   = any(diff(as.integer(!val %in% SENTINELS)) == -1),
      .groups            = "drop"
    )

  n_ser <- nrow(series)
  if (n_ser == 0) {
    return(list(n_multi_year_series = 0L,
                temporal_classification = "INSUFFICIENT_DATA"))
  }

  tc <- if (round(mean(series$all_same) * 100, 1) >= 95) "CONSTANT" else "VARIABLE"

  list(
    n_multi_year_series            = n_ser,
    pct_constant                   = round(mean(series$all_same)                          * 100, 1),
    pct_always_sentinel            = round(mean(series$all_sentinel)                       * 100, 1),
    pct_always_real                = round(mean(!series$has_sentinel & series$has_real)    * 100, 1),
    pct_mixed_sentinel_real        = round(mean(series$has_sentinel & series$has_real)     * 100, 1),
    n_agencies_sentinel_to_real    = sum(series$sentinel_to_real),
    n_agencies_real_to_sentinel    = sum(series$real_to_sentinel),
    temporal_classification        = tc
  )
}), oh_present)

# Example agency time series (3 most-observed agencies per variable)
oh_examples <- setNames(lapply(oh_present, function(v) {
  dedup <- raw |>
    select(AGENCY_ID, yr = STAT_PROFILE_DATE_YEAR, val = all_of(v)) |>
    filter(!is.na(val)) |>
    group_by(AGENCY_ID, yr) |>
    summarise(val = val[1L], .groups = "drop")

  top3 <- dedup |>
    group_by(AGENCY_ID) |>
    summarise(n = n(), .groups = "drop") |>
    arrange(-n) |>
    head(3)

  lapply(seq_len(nrow(top3)), function(i) {
    ag <- top3[i, ]
    s  <- dedup |> filter(AGENCY_ID == ag$AGENCY_ID) |> arrange(yr)
    list(
      AGENCY_ID = ag$AGENCY_ID,
      n_years   = nrow(s),
      by_year   = setNames(as.list(s$val), as.character(s$yr))
    )
  })
}), oh_present)

# ---------------------------------------------------------------------------
# Part C: END_YEAR sentinel interpretation and transition analysis
# ---------------------------------------------------------------------------

cat("  [16C] END_YEAR sentinel interpretation...\n")

end_yr_vars <- oh_present[grepl("END_YEAR$", oh_present)]

part_c_oh <- setNames(lapply(end_yr_vars, function(v) {
  x  <- raw[[v]]
  n  <- length(x)
  nu <- sum(!is.na(x) & !x %in% SENTINELS)
  ns <- sum(!is.na(x) & x %in% SENTINELS)
  us <- x[!is.na(x) & !x %in% SENTINELS]

  dedup <- raw |>
    select(AGENCY_ID, yr = STAT_PROFILE_DATE_YEAR, val = all_of(v)) |>
    filter(!is.na(val)) |>
    group_by(AGENCY_ID, yr) |>
    summarise(val = val[1L], .groups = "drop")

  transitions <- dedup |>
    mutate(is_real = !val %in% SENTINELS) |>
    group_by(AGENCY_ID) |>
    arrange(yr, .by_group = TRUE) |>
    filter(n() >= 2) |>
    summarise(
      has_sentinel     = any(!is_real),
      has_real         = any(is_real),
      sentinel_to_real = any(diff(as.integer(is_real)) == 1),
      real_to_sentinel = any(diff(as.integer(is_real)) == -1),
      .groups          = "drop"
    )

  n_trans <- sum(transitions$sentinel_to_real)

  list(
    n_sentinel      = ns,
    n_real_end_year = nu,
    pct_sentinel    = round(ns / n * 100, 2),
    pct_real        = round(nu / n * 100, 2),
    usable_end_years = if (length(us) > 0) list(
      min             = min(us),
      max             = max(us),
      n_distinct      = length(unique(us)),
      distinct_values = as.list(sort(unique(us)))
    ) else list(note = "all sentinel or null"),
    transition_analysis = list(
      n_agencies_sentinel_to_real = sum(transitions$sentinel_to_real),
      n_agencies_real_to_sentinel = sum(transitions$real_to_sentinel),
      n_agencies_always_sentinel  = sum(!transitions$has_real),
      n_agencies_always_real      = sum(!transitions$has_sentinel),
      n_agencies_mixed            = sum(transitions$has_sentinel & transitions$has_real)
    ),
    sentinel_interpretation = if (n_trans > 0)
      sprintf(paste0(
        "Sentinel END_YEAR confirmed to represent 'still active / relationship not yet ended'. ",
        "%d agency(ies) transition from sentinel (active) to a real end year (terminated) ",
        "within the dataset window. Real end years represent recorded termination events."),
        n_trans)
    else
      sprintf(paste0(
        "No agency transitions from sentinel to real end year within this dataset. ",
        "Sentinel rate is %.1f%%. Sentinel most likely encodes 'relationship still active'. ",
        "The %d rows with real end years represent agencies that terminated before or during ",
        "the dataset window."),
        round(ns / n * 100, 1), nu)
  )
}), end_yr_vars)

# ---------------------------------------------------------------------------
# Part D: Tenure feature construction and screening
# ---------------------------------------------------------------------------

cat("  [16D] Tenure feature analysis...\n")

TENURE_DEFS <- list(
  PL_TENURE             = "PL_START_YEAR",
  CL_TENURE             = "CL_START_YEAR",
  COMMISSIONS_TENURE    = "COMMISIONS_START_YEAR",
  ACTIVITY_NOTES_TENURE = "ACTIVITY_NOTES_START_YEAR"
)

part_d_oh <- setNames(lapply(names(TENURE_DEFS), function(tn) {
  sv <- TENURE_DEFS[[tn]]
  if (!sv %in% oh_present) {
    return(list(start_variable = sv, note = "start variable not found in dataset"))
  }

  sc  <- wp_modeling_data[[sv]]
  yr  <- wp_modeling_data$STAT_PROFILE_DATE_YEAR
  tr  <- yr %in% 2006:2012
  te  <- yr %in% 2013:2014
  n   <- length(sc)

  tenure <- if_else(!is.na(sc) & !sc %in% SENTINELS,
                    as.numeric(yr - sc), NA_real_)

  list(
    tenure_definition = paste0(tn, " = STAT_PROFILE_DATE_YEAR - ", sv),
    start_variable    = sv,
    n_negative_tenure = sum(!is.na(tenure) & tenure < 0),
    coverage = list(
      n_total       = n,
      n_available   = sum(!is.na(tenure)),
      n_missing     = sum(is.na(tenure)),
      pct_available = round(sum(!is.na(tenure)) / n * 100, 2),
      pct_missing   = round(sum(is.na(tenure)) / n * 100, 2),
      train = list(n = sum(tr), n_available = sum(!is.na(tenure[tr])),
                   pct = round(sum(!is.na(tenure[tr])) / sum(tr) * 100, 2)),
      test  = list(n = sum(te), n_available = sum(!is.na(tenure[te])),
                   pct = round(sum(!is.na(tenure[te])) / sum(te) * 100, 2))
    ),
    distribution = dist_summary_qb(tenure),
    correlations = list(
      cor_with_log_wp                  = safe_cor_qb(tenure, wp_modeling_data$log_wp),
      cor_with_log_prev_wp             = safe_cor_qb(tenure, wp_modeling_data$log_prev_wp),
      cor_with_log_prev_poly           = safe_cor_qb(tenure, wp_modeling_data$log_prev_poly),
      cor_with_agency_appointment_year = safe_cor_qb(tenure, wp_modeling_data$AGENCY_APPOINTMENT_YEAR)
    )
  )
}), names(TENURE_DEFS))

# ---------------------------------------------------------------------------
# Part E: Comparison with AGENCY_APPOINTMENT_YEAR
# ---------------------------------------------------------------------------

cat("  [16E] Comparison with AGENCY_APPOINTMENT_YEAR...\n")

log_wp_v  <- wp_modeling_data$log_wp
log_ppw_v <- wp_modeling_data$log_prev_wp
log_ppy_v <- wp_modeling_data$log_prev_poly
aay_v     <- wp_modeling_data$AGENCY_APPOINTMENT_YEAR

aay_baseline <- list(
  variable = "AGENCY_APPOINTMENT_YEAR",
  note     = "Already included in OLS 2+. Fixed year; does not change across reporting years.",
  cor_with_log_wp        = safe_cor_qb(aay_v, log_wp_v),
  cor_with_log_prev_wp   = safe_cor_qb(aay_v, log_ppw_v),
  cor_with_log_prev_poly = safe_cor_qb(aay_v, log_ppy_v)
)

start_yr_vars <- oh_present[grepl("START_YEAR$", oh_present)]

start_yr_cors <- setNames(lapply(start_yr_vars, function(v) {
  xu <- if_else(!is.na(wp_modeling_data[[v]]) & !wp_modeling_data[[v]] %in% SENTINELS,
                as.numeric(wp_modeling_data[[v]]), NA_real_)
  list(
    n_usable_pairs_with_aay          = sum(!is.na(xu) & !is.na(aay_v)),
    cor_with_agency_appointment_year = safe_cor_qb(xu, aay_v),
    cor_with_log_wp                  = safe_cor_qb(xu, log_wp_v),
    cor_with_log_prev_wp             = safe_cor_qb(xu, log_ppw_v),
    cor_with_log_prev_poly           = safe_cor_qb(xu, log_ppy_v)
  )
}), start_yr_vars)

part_e_oh <- list(
  agency_appointment_year_baseline = aay_baseline,
  start_year_correlations          = start_yr_cors,
  business_interpretation = paste0(
    "AGENCY_APPOINTMENT_YEAR (AAY) is the year the agency was formally appointed — already in OLS 2+. ",
    "START_YEAR variables record when the agency began a specific activity (writing PL/CL, ",
    "receiving commissions, appearing in activity notes). A START_YEAR may lag AAY if an agency ",
    "was appointed before activating a specific line. Tenure = STAT_PROFILE_DATE_YEAR - START_YEAR ",
    "is a time-varying measure that grows each year, unlike the fixed AAY value. ",
    "High r(TENURE, AAY) indicates redundancy. Moderate r(TENURE, AAY) with independent ",
    "r(TENURE, log_wp) suggests non-redundant incremental signal worth testing."
  )
)

# ---------------------------------------------------------------------------
# Part F: Per-family recommendations and overall assessment
# ---------------------------------------------------------------------------

cat("  [16F] Recommendations...\n")

families_oh <- list(
  PL = list(
    start  = "PL_START_YEAR",
    end    = "PL_END_YEAR",
    tenure = "PL_TENURE"
  ),
  CL = list(
    start  = "CL_START_YEAR",
    end    = "CL_END_YEAR",
    tenure = "CL_TENURE"
  ),
  COMMISSIONS = list(
    start  = "COMMISIONS_START_YEAR",
    end    = "COMMISIONS_END_YEAR",
    tenure = "COMMISSIONS_TENURE"
  ),
  ACTIVITY_NOTES = list(
    start  = "ACTIVITY_NOTES_START_YEAR",
    end    = "ACTIVITY_NOTES_END_YEAR",
    tenure = "ACTIVITY_NOTES_TENURE"
  )
)

part_f_oh <- setNames(lapply(names(families_oh), function(fn) {
  fm  <- families_oh[[fn]]
  sv  <- fm$start
  tn  <- fm$tenure

  sv_pct  <- if (sv %in% oh_present) part_a_oh[[sv]]$pct_usable else NA_real_
  td      <- if (tn %in% names(part_d_oh)) part_d_oh[[tn]] else NULL
  ten_cov <- if (!is.null(td) && "coverage" %in% names(td))     td$coverage$pct_available                           else NA_real_
  ten_wp  <- if (!is.null(td) && "correlations" %in% names(td)) td$correlations$cor_with_log_wp                     else NA_real_
  ten_aay <- if (!is.null(td) && "correlations" %in% names(td)) td$correlations$cor_with_agency_appointment_year    else NA_real_
  sv_aay  <- if (sv %in% names(start_yr_cors)) start_yr_cors[[sv]]$cor_with_agency_appointment_year                 else NA_real_

  if (is.na(sv_pct) || sv_pct < 30) {
    rec  <- "EXCLUDE"
    just <- sprintf("START_YEAR usable coverage is %.1f%% — too low for modeling.",
                    ifelse(is.na(sv_pct), 0, sv_pct))
  } else if (!is.na(ten_aay) && abs(ten_aay) > 0.85) {
    rec  <- "EXCLUDE"
    just <- sprintf(paste0("Tenure correlates with AGENCY_APPOINTMENT_YEAR at r=%.3f. ",
                           "Since AAY is already in OLS 2+, this tenure feature is largely redundant."),
                    ten_aay)
  } else if (!is.na(ten_wp) && abs(ten_wp) >= 0.10 &&
             !is.na(ten_cov) && ten_cov >= 70) {
    rec  <- "CREATE_TENURE_FEATURE"
    just <- sprintf(
      paste0("Tenure has %.1f%% coverage and correlates with log_wp at r=%.3f. ",
             "Correlation with AGENCY_APPOINTMENT_YEAR is r=%.3f — non-redundant. ",
             "Worth testing in a future OLS candidate model."),
      ten_cov,
      ifelse(is.na(ten_wp),  0, ten_wp),
      ifelse(is.na(ten_aay), 0, ten_aay)
    )
  } else {
    rec  <- "EXCLUDE"
    just <- sprintf(
      paste0("Tenure coverage is %.1f%% and correlates with log_wp at r=%.3f — ",
             "below threshold for inclusion."),
      ifelse(is.na(ten_cov), 0, ten_cov),
      ifelse(is.na(ten_wp),  0, ten_wp)
    )
  }

  list(
    start_variable  = sv,
    end_variable    = fm$end,
    tenure_variable = tn,
    recommendation  = rec,
    justification   = just
  )
}), names(families_oh))

recs_oh_vec   <- sapply(part_f_oh, function(x) x$recommendation)
advance_fams  <- names(recs_oh_vec[recs_oh_vec == "CREATE_TENURE_FEATURE"])

overall_oh <- list(
  n_families_create_tenure  = length(advance_fams),
  n_families_exclude        = sum(recs_oh_vec == "EXCLUDE"),
  families_to_create_tenure = as.list(advance_fams),
  advance_to_ols_testing    = length(advance_fams) > 0,
  overall_recommendation    = if (length(advance_fams) > 0)
    sprintf(
      paste0("%d operational-history family/families warrant tenure feature construction: %s. ",
             "Tenure features should be added to the modeling dataset and tested in a future OLS candidate model. ",
             "Use START_YEAR variables only as inputs to tenure construction — not as direct predictors. ",
             "END_YEAR variables encode relationship termination status; they are not predictors of WP level."),
      length(advance_fams), paste(advance_fams, collapse = ", ")
    )
  else
    paste0(
      "No operational-history families clear the threshold for tenure feature construction. ",
      "This variable family does not advance to model-testing stage. ",
      "Primary reasons: sentinel-driven coverage gaps, or tenure is largely redundant with ",
      "AGENCY_APPOINTMENT_YEAR (already in the OLS specification), or correlation with log_wp ",
      "is below the 0.10 threshold."
    )
)

# ---------------------------------------------------------------------------
# Assemble output
# ---------------------------------------------------------------------------

operational_history_review_out <- list(
  description = paste0(
    "Investigation of operational-history variables: PL/CL/COMMISIONS/ACTIVITY_NOTES START_YEAR and END_YEAR. ",
    "Parts: (A) per-variable summary with sentinel breakdown, ",
    "(B) temporal patterns and within-agency consistency, ",
    "(C) END_YEAR sentinel interpretation and transition analysis, ",
    "(D) tenure feature construction and predictive screening, ",
    "(E) comparison with AGENCY_APPOINTMENT_YEAR, ",
    "(F) per-family recommendations and overall advance decision. ",
    "No models are fitted."
  ),
  variables_present              = as.list(oh_present),
  variables_absent               = as.list(oh_absent),
  part_a_variable_summaries      = part_a_oh,
  part_b_temporal                = list(
    within_agency_year_consistency = oh_within_consistency,
    across_year_patterns           = oh_temporal,
    example_series                 = oh_examples
  ),
  part_c_end_year_interpretation = part_c_oh,
  part_d_tenure_features         = part_d_oh,
  part_e_comparison_with_aay     = part_e_oh,
  part_f_recommendations         = list(
    per_family           = part_f_oh,
    summary              = overall_oh
  )
)

# =============================================================================
# Write JSON outputs
# =============================================================================

cat("\nWriting JSON outputs...\n")

write_json_file <- function(obj, filename) {
  path <- file.path(output_dir, filename)
  write(
    toJSON(obj, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 6),
    path
  )
  size_kb <- round(file.size(path) / 1024, 1)
  cat(sprintf("  %-35s  %s KB\n", filename, size_kb))
}

write_json_file(overview,             "overview.json")
write_json_file(eda_summary,          "eda_summary.json")
write_json_file(predictor_audit_out,  "predictor_audit.json")
write_json_file(correlations_out,     "correlations.json")
write_json_file(lr_diagnostics_out,   "loss_ratio_diagnostics.json")
write_json_file(lag_diagnostics_out,    "lag_feature_diagnostics.json")
write_json_file(lr_zero_analysis_out,              "loss_ratio_zero_analysis.json")
write_json_file(modeling_dataset_diagnostics_out,  "modeling_dataset_diagnostics.json")
write_json_file(quote_bind_review_out,             "quote_bind_variable_review.json")
write_json_file(operational_history_review_out,    "operational_history_review.json")

cat(sprintf("\n=== Phase 1 complete ===\nOutputs in: %s\n", output_dir))
