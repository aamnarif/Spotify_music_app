# ============================================================
# R/theme.R – App colour themes and ggplot2 base theme
# ============================================================

make_theme <- function(dark = TRUE) {
  if (dark) list(
    plot_bg   = "#1A1A1A", grid = "#2A2A2A",
    text      = "#FFFFFF", text2 = "#AAAAAA", axis_text = "#AAAAAA",
    surface2  = "#242424"
  ) else list(
    plot_bg   = "#FFFFFF", grid = "#EEEEEE",
    text      = "#0F0F0F", text2 = "#555555", axis_text = "#555555",
    surface2  = "#F0F0F0"
  )
}

gg_theme <- function(t,
                     axis_x_angle = 0,
                     axis_x_hjust = 0.5,
                     axis_x_size  = 9,
                     axis_y_size  = 9,
                     no_grid      = FALSE,
                     legend_key_h = NULL,
                     hide_axes    = FALSE,
                     left_margin  = 16) {

  ax_text  <- if (hide_axes) element_blank() else element_text(colour = t$axis_text, size = 9)
  ax_textx <- if (hide_axes) element_blank() else element_text(colour = t$axis_text,
                                                               size  = axis_x_size,
                                                               angle = axis_x_angle,
                                                               hjust = axis_x_hjust)
  ax_texty <- if (hide_axes) element_blank() else element_text(colour = t$axis_text, size = axis_y_size)
  ax_title <- if (hide_axes) element_blank() else element_text(colour = t$text2, face = "bold", size = 10)
  ax_ticks <- if (hide_axes) element_blank() else element_line()

  base <- theme_minimal(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = t$plot_bg, colour = NA),
      panel.background  = element_rect(fill = t$plot_bg, colour = NA),
      panel.grid.major  = if (no_grid) element_blank()
                          else element_line(colour = t$grid, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(colour = t$text,  face = "bold", size = 14),
      plot.subtitle     = element_text(colour = t$text2, size = 10),
      axis.title        = ax_title,
      axis.text         = ax_text,
      axis.text.x       = ax_textx,
      axis.text.y       = ax_texty,
      axis.ticks        = ax_ticks,
      legend.background = element_rect(fill = t$plot_bg, colour = NA),
      legend.text       = element_text(colour = t$text2, size = 9),
      legend.title      = element_text(colour = t$text,  face = "bold", size = 9),
      plot.margin       = margin(16, 16, 16, left_margin),
      strip.text        = element_text(colour = t$text, face = "bold")
    )

  if (!is.null(legend_key_h))
    base <- base + theme(legend.key.height = unit(legend_key_h, "cm"))

  base
}