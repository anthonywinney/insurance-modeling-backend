.libPaths(c("C:/Users/Anthony/R/win-library/4.6", .libPaths()))
library(ggplot2)
library(dplyr)

out_dir <- "modeling/dataset_a_part2/outputs"

# ── Column layout (data units) ─────────────────────────────────────────────────
# Edges: 0 | 2.5 | 4.0 | 5.5 | 7.0 | 8.5 | 10.0 | 11.5
# Cols:  [  Model  ] [Orig R²][Orig RMSE][Refit R²][Refit RMSE][Δ R²][Δ RMSE]
col_x <- c(1.25, 3.25, 4.75, 6.25, 7.75, 9.25, 10.75)
col_w <- c(2.50, 1.50, 1.50, 1.50, 1.50, 1.50,  1.50)

BLUE_DARK  <- "#1565C0"
BLUE_MED   <- "#1A5EA8"
ROW_LIGHT  <- "#FFFFFF"
ROW_STRIPE <- "#EEF2F7"
TEXT_COL   <- "#1A1A1A"

# ── Source data ────────────────────────────────────────────────────────────────
models <- c(
  "OLS_ADDITIVE_FINAL",
  "RF_1_SAFE_TUNED †",   # dagger = OOB
  "AGENCY_LMM_FULL *"          # asterisk = conditional
)
orig_r2    <- c(0.781,  0.8989, 0.804)
orig_rmse  <- c(1.0238, 0.6955, 0.9685)
refit_r2   <- c(0.7809, 0.9000, 0.8033)
refit_rmse <- c(1.0267, 0.6935, 0.9727)
delta_r2   <- refit_r2   - orig_r2
delta_rmse <- refit_rmse - orig_rmse
n <- length(models)

# ── Cell builder ───────────────────────────────────────────────────────────────
cell <- function(x, y, w, label, bg, fg, face) {
  data.frame(x=x, y=y, w=w, label=label, bg=bg, fg=fg, face=face,
             stringsAsFactors=FALSE)
}

# Row y=0: group header — blank corner + three spanning tiles
grp <- bind_rows(
  cell(1.25,  0, 2.50, "",                          BLUE_DARK, "white", "bold"),
  cell(4.00,  0, 3.00, "Original (2006–2012)", BLUE_DARK, "white", "bold"),
  cell(7.00,  0, 3.00, "Refit (2006–2014)",    BLUE_DARK, "white", "bold"),
  cell(10.00, 0, 3.00, "Δ = Refit metric - Original metric", BLUE_DARK, "white", "bold")
)

# Row y=-1: sub-headers
sub <- cell(col_x, -1, col_w,
            c("Model", "Train R²", "Train RMSE", "Train R²", "Train RMSE", "Train R²", "Train RMSE"),
            BLUE_MED, "white", "bold")

# Data rows y=-2, -3, -4
dat <- bind_rows(lapply(seq_len(n), function(i) {
  cell(col_x, -(i + 1), col_w,
       c(models[i],
         sprintf("%.4f", orig_r2[i]),
         sprintf("%.4f", orig_rmse[i]),
         sprintf("%.4f", refit_r2[i]),
         sprintf("%.4f", refit_rmse[i]),
         sprintf("%+.4f", delta_r2[i]),
         sprintf("%+.4f", delta_rmse[i])),
       if (i %% 2 == 1) ROW_LIGHT else ROW_STRIPE,
       TEXT_COL, "plain")
}))

cells <- bind_rows(grp, sub, dat)

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(cells, aes(x = x, y = y)) +
  geom_tile(aes(width = w, height = 1, fill = bg),
            color = "#CCCCCC", linewidth = 0.25) +
  # vertical dividers between groups
  annotate("segment",
           x = 5.5, xend = 5.5, y = -(n + 1) - 0.5, yend = 0.5,
           color = "#777777", linewidth = 0.65) +
  annotate("segment",
           x = 8.5, xend = 8.5, y = -(n + 1) - 0.5, yend = 0.5,
           color = "#777777", linewidth = 0.65) +
  geom_text(aes(label = label, color = fg, fontface = face),
            size = 3.5, hjust = 0.5, vjust = 0.5) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, 11.5), ylim = c(-(n + 1) - 0.5, 0.5)) +
  labs(caption = paste0(
    "No holdout available after all-data refit.\n",
    "* Conditional predictions.\n",
    "† OOB metrics."
  )) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption    = element_text(size = 10, hjust = 0, color = "#555555",
                                   margin = margin(t = 8)),
    plot.margin     = margin(16, 20, 12, 20)
  )

ggsave(
  filename = file.path(out_dir, "fig_refit_benchmark_comparison.png"),
  plot     = p,
  width    = 11,
  height   = 3.8,
  dpi      = 150,
  bg       = "white"
)
message("PNG written: fig_refit_benchmark_comparison.png")
