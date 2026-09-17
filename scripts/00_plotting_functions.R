# ---- Plotting Helpers

theme_bs <- function(base_size = 14) {
  sysfonts::font_add_google("News Cycle", "news")
  showtext::showtext_auto()

  paper <- "white"
  half <- base_size / 2

  ggplot2::theme_bw(base_size = base_size, base_family = "news") +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.4), hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = ggplot2::rel(1.1), hjust = 0.5),
      strip.text = ggplot2::element_text(size = ggplot2::rel(1.15), hjust = 0.5),
      strip.background = ggplot2::element_rect(fill = NA, colour = NA),
      axis.title = ggplot2::element_text(size = ggplot2::rel(1.05)),
      axis.text = ggplot2::element_text(size = ggplot2::rel(0.9)),

      axis.ticks = ggplot2::element_line(colour = "#AAAAAA", linewidth = 0.3),
      axis.line = ggplot2::element_line(colour = "#6d6d6e"),

      axis.text.x.bottom = ggplot2::element_text(margin = ggplot2::margin(t = half)),
      axis.text.x.top = ggplot2::element_text(margin = ggplot2::margin(b = half)),
      axis.text.y.left = ggplot2::element_text(margin = ggplot2::margin(r = half)),
      axis.text.y.right = ggplot2::element_text(margin = ggplot2::margin(l = half)),

      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = "#E0E0E0"),
      panel.grid.minor = ggplot2::element_blank(),

      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      plot.background = ggplot2::element_rect(fill = paper, colour = NA),

      legend.background = ggplot2::element_blank(),
      legend.key = ggplot2::element_blank(),
      legend.title = ggplot2::element_text(face = "plain"),
      legend.position = "top",
      legend.justification = 1,

      panel.spacing.x = ggplot2::unit(1.25, "lines"),
      panel.spacing.y = ggplot2::unit(1.25, "lines")
    )
}


# Color palettes

model_palette <- setNames(
  MetBrewer::met.brewer("Hiroshige", 8),
  c("Mean", "LOCF", "Trend", "RI", "AR", "VAR", "mlAR", "mlVAR")
)
