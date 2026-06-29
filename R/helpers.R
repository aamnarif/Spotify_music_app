# ============================================================
# R/helpers.R – Reusable UI helper functions
# ============================================================

# KPI card widget
kpi <- function(value, label, accent) {
  tags$div(
    class = "kpi-card",
    style = paste0("--accent:", accent, ";"),
    tags$div(class = "kpi-value", value),
    tags$div(class = "kpi-label", label)
  )
}