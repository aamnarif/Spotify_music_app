# ============================================================
# R/theme.R – Colour Palette & Shared ggplot2 Theme
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================

# ── Colour Palette (colourblind-friendly) ─────────────────────
SPOTIFY_GREEN <- "#1DB954"
ACCENT1       <- "#2196F3"
ACCENT2       <- "#FF9800"
ACCENT3       <- "#E91E63"
PANEL_BG      <- "#F8F9FA"

# ── Shared ggplot2 Theme ──────────────────────────────────────
spotify_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15, colour = "#212121"),
      plot.subtitle    = element_text(size = 11, colour = "#616161"),
      axis.title       = element_text(face = "bold", size = 11),
      axis.text        = element_text(size = 10),
      legend.title     = element_text(face = "bold", size = 10),
      legend.text      = element_text(size = 9),
      panel.grid.major = element_line(colour = "#E0E0E0"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = PANEL_BG, colour = NA),
      panel.background = element_rect(fill = "white",  colour = NA),
      plot.margin      = margin(15, 15, 15, 15)
    )
}