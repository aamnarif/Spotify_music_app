# ============================================================
# ui.R – Main UI definition
# ============================================================

ui <- dashboardPage(
  skin = "black",

  # ── Header ─────────────────────────────────────────────
  dashboardHeader(
  title = tags$div(
    class = "logo-container",

    tags$img(
      src = "spotify-logo.svg",
      class = "app-logo"
    ),

    tags$div(
      class = "logo-text-stack",
      tags$span(class = "logo-title", "Solace"),
      tags$span(class = "logo-sub", "Music Analytics")
    )
  ),

    titleWidth = 280,

    # Search Bar
    tags$li(
      class = "dropdown header-search-li",
      tags$div(
        class = "header-search",
        tags$span(class = "header-search-icon", "🔎"),
        tags$input(
          type = "text",
          class = "header-search-input",
          placeholder = "Search tracks, genres, or styles..."
        )
      )
    ),

    # Generate Music Button
    tags$li(
      class = "dropdown",
      actionButton(
        "generate_music",
        label = HTML("Generate Music"),
        class = "generate-btn"
      )
    ),

    # Right Side Actions
    tags$li(
      class = "dropdown header-actions-li",
      tags$div(
        class = "header-actions",

        actionButton(
          "toggle_mode",
          label = "☾",
          class = "btn-icon-round"
        ),

        actionButton(
          "notifications",
          label = "🔔",
          class = "btn-icon-round"
        ),

        tags$div(
          class = "header-profile",

          tags$div(
            class = "header-profile-avatar",
            "G3"
          ),

          tags$div(
            class = "header-profile-text",

            tags$span(
              class = "header-profile-name",
              "Group 3"
            ),

            tags$span(
              class = "header-profile-role",
              "DSA8045 Applied Anlytics"
            )
          )
        )
      )
    )
  ),  # ← IMPORTANT COMMA HERE

  # ── Sidebar ─────────────────────────────────────────────
  dashboardSidebar(
    width = 230,

    sidebarMenu(
      id = "tabs",

      tags$div(
        class = "sidebar-section-label",
        "STUDIO"
      ),

      menuItem(
        "🏠 Home",
        tabName = "overview"
      ),

      menuItem(
        "🎛 Audio Explorer",
        tabName = "audio"
      ),

      menuItem(
        "⭐ Popularity",
        tabName = "popularity"
      ),

      menuItem(
        "🔍 What's Missing",
        tabName = "missing"
      ),

      menuItem(
        "📊 Correlations",
        tabName = "heatmap"
      ),

      tags$div(
        class = "sidebar-section-label",
        "GENERAL"
      ),

      menuItem(
        "💡 Conclusions",
        tabName = "conclusions"
      )
    ),

    tags$div(
      class = "sidebar-promo",

      tags$div(
        class = "sidebar-promo-title",
        "Dataset"
      ),

      tags$div(
        class = "sidebar-promo-sub",
        "700 tracks · 114 genres · 20 variables"
      )
    )
  ),

  # ── Body ────────────────────────────────────────────────
  dashboardBody(

    tags$head(

      tags$link(
        id = "theme-css",
        rel = "stylesheet",
        type = "text/css",
        href = "dark.css"
      ),

      tags$script(src = "theme.js")
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