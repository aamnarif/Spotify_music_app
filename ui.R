# ============================================================
# ui.R – Main UI definition
# ============================================================

ui <- dashboardPage(
  skin = "black",

  # ── Header ─────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      class = "logo-text",
      tags$span(class = "logo-red", "♫ "), "Solace"
    ),
    titleWidth = 240,
    tags$li(class = "dropdown",
      tags$div(
        style = "display:flex; align-items:center; height:50px; padding-right:16px;",
        actionButton("toggle_mode", "☀ Light Mode", class = "toggle-btn")
      )
    )
  ),

  # ── Sidebar ─────────────────────────────────────────────
  dashboardSidebar(
    width = 210,
    sidebarMenu(id = "tabs",
      menuItem("🏠  Home",           tabName = "overview"),
      menuItem("🎛  Audio Explorer", tabName = "audio"),
      menuItem("⭐  Popularity",     tabName = "popularity"),
      menuItem("🔍  What's Missing", tabName = "missing"),
      menuItem("📊  Correlations",   tabName = "heatmap"),
      menuItem("💡  Conclusions",    tabName = "conclusions")
    ),
    tags$hr(),
    tags$div(style = "padding:0 14px;",
      tags$p(style = "color:#666;font-size:10px;font-weight:600;text-transform:uppercase;letter-spacing:1px;",
             "Dataset"),
      tags$p(style = "color:#AAA;font-size:11px;line-height:1.6;",
             "700 tracks · 114 genres · 20 variables")
    )
  ),

  # ── Body ────────────────────────────────────────────────
  dashboardBody(
    tags$head(
      tags$style(id = "dyn_css", HTML(css_dark)),
      tags$script(HTML(theme_js))
    ),
    tabItems(
      overview_tab_ui,
      audio_tab_ui,
      popularity_tab_ui,
      missing_tab_ui,
      heatmap_tab_ui,
      conclusions_tab_ui
    )
  )
)