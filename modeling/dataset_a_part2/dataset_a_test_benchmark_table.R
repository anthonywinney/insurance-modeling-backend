.libPaths(c("C:/Users/Anthony/R/win-library/4.6", .libPaths()))
library(ggplot2)
library(dplyr)

out_dir <- "modeling/dataset_a_part2/outputs"

# ── Column layout (data units) ─────────────────────────────────────────────────
# Edges: 0 | 4.0 | 7.0 | 10.0
# Cols:  [     Model     ] [ Test R² ] [ Test RMSE ]
col_x <- c(2.00, 5.50, 8.50)
col_w <- c(4.00, 3.00, 3.00)

BLUE_DARK  <- "#1565C0"
ROW_LIGHT  <- "#FFFFFF"
ROW_STRIPE <- "#EEF2F7"
TEXT_COL   <- "#1A1A1A"

# ── Source data (locked 2013-2014 holdout metrics) ─────────────────────────────
models   <- c(
  "OLS_ADDITIVE_FINAL",
  "RF_1_SAFE_TUNED",
  "AGENCY_LMM_FULL *"
)
test_r2   <- c(0.7552, 0.8839, 0.7607)
test_rmse <- c(1.0937, 0.7532, 1.0814)
n <- length(models)

# ── Cell builder ───────────────────────────────────────────────────────────────
cell <- function(x, y, w, label, bg, fg, face) {
  data.frame(x=x, y=y, w=w, label=label, bg=bg, fg=fg, face=face,
             stringsAsFactors=FALSE)
}

# Row y=0: header
hdr <- cell(col_x, 0, col_w,
            c("Model", "Test R²", "Test RMSE"),
            BLUE_DARK, "white", "bold")

# Data rows y=-1, -2, -3
dat <- bind_rows(lapply(seq_len(n), function(i) {
  cell(col_x, -i, col_w,
       c(models[i],
         sprintf("%.4f", test_r2[i]),
         sprintf("%.4f", test_rmse[i])),
       if (i %% 2 == 1) ROW_LIGHT else ROW_STRIPE,
       TEXT_COL, "plain")
}))

cells <- bind_rows(hdr, dat)

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(cells, aes(x = x, y = y)) +
  geom_tile(aes(width = w, height = 1, fill = bg),
            color = "#CCCCCC", linewidth = 0.25) +
  geom_text(aes(label = label, color = fg, fontface = face),
            size = 3.5, hjust = 0.5, vjust = 0.5) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0, 10), ylim = c(-n - 0.5, 0.5)) +
  labs(caption = "* Conditional predictions.") +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.caption    = element_text(size = 10, hjust = 0, color = "#555555",
                                   margin = margin(t = 8)),
    plot.margin     = margin(16, 20, 12, 20)
  )

ggsave(
  filename = file.path(out_dir, "fig_test_benchmark_comparison.png"),
  plot     = p,
  width    = 8,
  height   = 2.6,
  dpi      = 150,
  bg       = "white"
)
message("PNG written: fig_test_benchmark_comparison.png")
