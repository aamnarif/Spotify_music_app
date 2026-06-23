# ============================================================
# R/ui.R – Full Dashboard UI
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================

ui <- dashboardPage(
  skin = "green",
  
  # ── Header ───────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(
        src   = "https://encrypted-tbn0.gstatic.com/images?q=tbn:ANd9GcTPwEjYHuyf95ihfsIXAdc59S_3EvEKWb539w&s",
        height = "28px",
        style  = "margin-right:8px; vertical-align:middle;"
      ),
      tags$span("",
                style = "vertical-align:middle; font-size:16px; font-weight:bold;")
    ),
    titleWidth = 260
  ),
  
  # ── Sidebar ──────────────────────────────────────────────
  dashboardSidebar(
    width = 260,
    collapsed = FALSE,
    sidebarMenu(
      id = "sidebar_menu",
      menuItem("Overview",       tabName = "overview",    icon = icon("music")),
      menuItem("Audio Features", tabName = "audio",       icon = icon("sliders-h")),
      menuItem("Popularity",     tabName = "popularity",  icon = icon("star")),
      menuItem("Conclusions",    tabName = "conclusions", icon = icon("lightbulb"))
    ),
    tags$hr(),
    tags$p(tags$b("About"),
           style = "color:#aaa; font-size:11px; padding:0 15px;"),
    tags$p(
      "Exploring 700 Spotify tracks across 114 genres using audio features and popularity data.",
      style = "color:#bbb; font-size:10px; padding:0 15px; line-height:1.4;"
    )
  ),
  
  # ── Body ─────────────────────────────────────────────────
  dashboardBody(
    
    tags$head(tags$style(HTML("

      /* ── General layout ── */
      .content-wrapper, .right-side {
        background-color: #F4F6F9;
      }

      /* ── Box styling ── */
      .box {
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.08);
      }
      .box-header     { border-radius: 8px 8px 0 0; }
      .info-box       { border-radius: 8px; }
      .small-box      { border-radius: 8px; }

      /* ── Intro text ── */
      h4.intro-title  { color: #1DB954; font-weight: bold; margin-bottom: 6px; }
      .intro-text     { color: #424242; line-height: 1.7; font-size: 13px; }

      /* ── Header: keep logo + title visible always ── */
      .main-header .logo {
        width: 260px !important;
        background-color: #1DB954 !important;
        font-size: 15px !important;
        padding: 0 10px !important;
        display: flex !important;
        align-items: center !important;
      }
      .main-header .navbar {
        margin-left: 260px !important;
      }

      /* ── Sidebar full width ── */
      .main-sidebar {
        width: 260px !important;
        transition: width 0.3s ease !important;
      }
      .content-wrapper,
      .main-footer {
        margin-left: 260px !important;
        transition: margin-left 0.3s ease !important;
      }

      /* ── Sidebar menu items ── */
      .sidebar-menu > li > a {
        display: flex !important;
        align-items: center !important;
        padding: 13px 15px !important;
        font-size: 14px !important;
      }
      .sidebar-menu > li > a > .fa,
      .sidebar-menu > li > a > .fas,
      .sidebar-menu > li > a > .far {
        font-size: 16px !important;
        min-width: 20px !important;
        margin-right: 10px !important;
      }

      /* ── COLLAPSED state ── */
      .sidebar-collapse .main-sidebar {
        width: 60px !important;
      }
      .sidebar-collapse .content-wrapper,
      .sidebar-collapse .main-footer {
        margin-left: 60px !important;
      }
      /* Hide text labels when collapsed */
      .sidebar-collapse .sidebar-menu > li > a > span {
        display: none !important;
      }
      /* Center icons when collapsed */
      .sidebar-collapse .sidebar-menu > li > a {
        justify-content: center !important;
        padding: 13px 0px !important;
      }
      .sidebar-collapse .sidebar-menu > li > a > .fa,
      .sidebar-collapse .sidebar-menu > li > a > .fas {
        margin-right: 0 !important;
        font-size: 18px !important;
      }
      /* Hide About section when collapsed */
      .sidebar-collapse .main-sidebar hr,
      .sidebar-collapse .main-sidebar p {
        display: none !important;
      }
      /* Keep header logo area same size */
      .sidebar-collapse .main-header .logo {
        width: 60px !important;
      }
      .sidebar-collapse .main-header .navbar {
        margin-left: 60px !important;
      }
      /* Show full title always in header */
      .main-header .logo .sidebar-toggle-icon { display:none; }

      /* ── Active menu item highlight ── */
      .sidebar-menu > li.active > a {
        background-color: #1DB954 !important;
        color: #fff !important;
        border-left: 4px solid #fff !important;
      }
      .sidebar-menu > li > a:hover {
        background-color: #17a845 !important;
        color: #fff !important;
      }

    "))),
    
    tabItems(
      overview_ui(),
      audio_ui(),
      popularity_ui(),
      conclusions_ui()
    )
  )
)