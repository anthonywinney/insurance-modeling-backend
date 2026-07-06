.libPaths(c("C:/Users/Anthony/R/win-library/4.6", .libPaths()))
library(ggplot2)
library(dplyr)

out_dir <- "modeling/dataset_a_part2/outputs"

# ── Data ───────────────────────────────────────────────────────────────────────
raw <- data.frame(
  Parameter = c("num.trees", "mtry", "min.node.size", "sample.fraction"),
  Values    = c("500", "3, 5, 7, 10", "5, 20, 50, 100", "0.6, 0.8, 1.0"),
  stringsAsFactors = FALSE
)

write.csv(raw, file.path(out_dir, "rf_tuning_grid.csv"), row.names = FALSE)
message("CSV written: rf_tuning_grid.csv")

# ── Build cell data ────────────────────────────────────────────────────────────
header <- data.frame(
  row       = 0L,
  col       = 1:2,
  label     = c("Parameter", "Values"),
  is_header = TRUE,
  stringsAsFactors = FALSE
)

body <- data.frame(
  row       = rep(1:4, times = 2),
  col       = rep(1:2, each  = 4),
  label     = c(raw$Parameter, raw$Values),
  is_header = FALSE,
  stringsAsFactors = FALSE
)

cells <- bind_rows(header, body) |>
  mutate(
    bg        = case_when(
      is_header     ~ "#1565C0",
      row %% 2 == 1 ~ "#FFFFFF",
      TRUE          ~ "#EEF2F7"
    ),
    fg        = if_else(is_header, "white", "#1A1A1A"),
    font_face = if_else(is_header, "bold", "plain")
  )

# ── Plot ───────────────────────────────────────────────────────────────────────
p <- ggplot(cells, aes(x = col, y = -row)) +
  geom_tile(aes(fill = bg), color = "#CCCCCC", linewidth = 0.3,
            width = 1, height = 1) +
  geom_text(aes(label = label, color = fg, fontface = font_face),
            size = 4, hjust = 0.5, vjust = 0.5) +
  scale_fill_identity() +
  scale_color_identity() +
  coord_cartesian(xlim = c(0.5, 2.5), ylim = c(-4.5, 0.5)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(20, 60, 20, 60)
  )

ggsave(
  filename = file.path(out_dir, "fig_rf_tuning_grid.png"),
  plot     = p,
  width    = 5,
  height   = 3,
  dpi      = 150,
  bg       = "white"
)
message("PNG written: fig_rf_tuning_grid.png")
