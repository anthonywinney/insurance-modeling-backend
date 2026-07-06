.libPaths(c("C:/Users/Anthony/R/win-library/4.6", .libPaths()))
library(ggplot2)
library(dplyr)

out_dir <- "modeling/dataset_a_part2/outputs"

# ── Data ───────────────────────────────────────────────────────────────────────
raw <- data.frame(
  Model     = c("OLS 0", "OLS 1", "OLS 2", "OLS 3"),
  Test_R2   = c(0.6035, 0.6976, 0.7003, 0.7552),
  Test_RMSE = c(1.3919, 1.2156, 1.2102, 1.0937)
)

write.csv(raw, file.path(out_dir, "ols_progression.csv"), row.names = FALSE)
message("CSV written: ols_progression.csv")

# ── Build cell data ────────────────────────────────────────────────────────────
header <- data.frame(
  row       = 0L,
  col       = 1:3,
  label     = c("Model", "Test R²", "Test RMSE"),
  is_header = TRUE,
  stringsAsFactors = FALSE
)

body <- data.frame(
  row       = rep(1:4, times = 3),
  col       = rep(1:3, each  = 4),
  label     = c(raw$Model,
                sprintf("%.4f", raw$Test_R2),
                sprintf("%.4f", raw$Test_RMSE)),
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
  coord_cartesian(xlim = c(0.5, 3.5), ylim = c(-4.5, 0.5)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin     = margin(20, 60, 20, 60)
  )

ggsave(
  filename = file.path(out_dir, "fig_ols_progression_table.png"),
  plot     = p,
  width    = 6,
  height   = 3.2,
  dpi      = 150,
  bg       = "white"
)
message("PNG written: fig_ols_progression_table.png")
