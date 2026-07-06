.libPaths(c("C:/Users/Anthony/R/win-library/4.6", .libPaths()))

library(jsonlite)
library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)

# ── Reusable report theme ──────────────────────────────────────────────────────
theme_report <- function() {
  theme_minimal(base_size = 11) +
    theme(
      plot.background  = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(color = "#D0D0D0", linewidth = 0.4),
      panel.grid.minor = element_line(color = "#EBEBEB", linewidth = 0.25),
      panel.border     = element_blank(),
      axis.text        = element_text(color = "black", size = 10),
      axis.title       = element_text(color = "black", size = 12),
      plot.title       = element_text(size = 14, hjust = 0.5, face = "bold"),
      plot.subtitle    = element_text(size = 11, hjust = 0.5, color = "#444444"),
      legend.position  = "bottom",
      plot.margin      = margin(16, 20, 16, 20)
    )
}

# ── Paths ──────────────────────────────────────────────────────────────────────
out_dir <- "modeling/dataset_a_part2/outputs"

# ── Read correlations.json ─────────────────────────────────────────────────────
corr_raw <- fromJSON(file.path(out_dir, "correlations.json"), simplifyVector = FALSE)

log_wp_row <- Filter(
  function(x) !is.null(x$variable) && x$variable == "log_wp",
  corr_raw$correlation_matrix
)[[1]]

exclude_vars <- c("log_wp", "POLY_INFORCE_QTY", "NB_WRTN_PREM_AMT", "PRD_ERND_PREM_AMT")

corr_df <- data.frame(
  variable                = names(log_wp_row),
  correlation_with_log_wp = unlist(lapply(
    log_wp_row,
    function(v) if (is.null(v)) NA_real_ else as.numeric(v)
  )),
  stringsAsFactors = FALSE
) |>
  filter(!variable %in% c(exclude_vars, "variable")) |>
  mutate(abs_correlation_with_log_wp = abs(correlation_with_log_wp))

# ── Read lag_feature_diagnostics.json ─────────────────────────────────────────
lag_raw  <- fromJSON(file.path(out_dir, "lag_feature_diagnostics.json"), simplifyVector = FALSE)
safe_num <- function(x) if (is.null(x)) NA_real_ else as.numeric(x)

lag_df <- data.frame(
  variable = c(
    "PREV_RETENTION_RATIO",
    "PREV_LOSS_RATIO",
    "AVG_PREMIUM_PER_POLICY_LAST_YEAR",
    "log_avg_prem_per_policy_ly"
  ),
  correlation_with_log_wp = c(
    safe_num(lag_raw$feature_1_prev_retention_ratio$correlation_with_log_wp),
    safe_num(lag_raw$feature_2_prev_loss_ratio$correlation_with_log_wp),
    safe_num(lag_raw$feature_3_avg_prem_per_policy_ly$correlation_with_log_wp),
    safe_num(lag_raw$feature_3_avg_prem_per_policy_ly$correlation_with_log_wp_log_scaled)
  ),
  stringsAsFactors = FALSE
) |>
  mutate(abs_correlation_with_log_wp = abs(correlation_with_log_wp))

# ── Combine ────────────────────────────────────────────────────────────────────
lagged_vars <- c(
  "log_prev_wp", "log_prev_poly",
  "PREV_RETENTION_RATIO", "PREV_LOSS_RATIO",
  "AVG_PREMIUM_PER_POLICY_LAST_YEAR", "log_avg_prem_per_policy_ly"
)
agency_vars <- c("ACTIVE_PRODUCERS", "AGENCY_APPOINTMENT_YEAR", "MAX_AGE", "MIN_AGE")

df <- bind_rows(corr_df, lag_df) |>
  mutate(variable_group = case_when(
    variable %in% lagged_vars ~ "Lagged / engineered",
    variable %in% agency_vars ~ "Agency characteristic",
    (str_starts(variable, "PL_") | str_starts(variable, "CL_") |
       str_starts(variable, "COMMISIONS_") | str_starts(variable, "ACTIVITY_NOTES_")) &
      (str_ends(variable, "START_YEAR") | str_ends(variable, "END_YEAR")) ~ "Operational history",
    str_detect(variable, "BOUND_CT|QUO_CT") ~ "Quote/bind platform",
    variable == "MONTHS" ~ "Reporting-period diagnostic",
    TRUE ~ "Other"
  ))

# ── Save CSV ───────────────────────────────────────────────────────────────────
write.csv(df, file.path(out_dir, "abs_corr_log_wp_candidates.csv"), row.names = FALSE)
message("CSV written: abs_corr_log_wp_candidates.csv")

# ── Plot: top 15 (landscape, report-ready) ────────────────────────────────────
top15_df <- df |>
  filter(!is.na(abs_correlation_with_log_wp)) |>
  slice_max(abs_correlation_with_log_wp, n = 15) |>
  mutate(variable = fct_reorder(variable, abs_correlation_with_log_wp))

p15 <- ggplot(top15_df, aes(x = variable, y = abs_correlation_with_log_wp)) +
  geom_col(fill = "#1565C0", width = 0.65) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.04))) +
  labs(
    title    = "Absolute Correlation with Log Written Premium",
    subtitle = "Top 15 non-leakage candidate predictors",
    x        = NULL,
    y        = "Absolute Pearson correlation with log written premium"
  ) +
  theme_report()

ggsave(
  filename = file.path(out_dir, "fig_abs_corr_log_wp_candidates_top15.png"),
  plot     = p15,
  width    = 10,
  height   = 6,
  dpi      = 150,
  bg       = "white"
)
message("PNG written: fig_abs_corr_log_wp_candidates_top15.png")
