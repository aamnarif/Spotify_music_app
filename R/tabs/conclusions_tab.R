# ============================================================
# R/tabs/conclusions_tab.R – Conclusions tab UI
# ============================================================

conclusions_tab_ui <- tabItem(tabName = "conclusions",
  fluidRow(column(12,
    tags$div(class = "section-header", "Key Findings & Conclusions"),
    tags$div(class = "section-sub", "What the data tells us about music on Spotify")
  )),
  fluidRow(
    column(6,
      tags$div(class = "insight-card",
        tags$h5("🎵 Genre Distribution"),
        tags$p("700 tracks span 114 genres. Dancehall, country and Turkish music are most represented. 647 of 700 tracks (92.4%) are non-explicit, suggesting the dataset reflects mainstream, radio-friendly content.")
      ),
      tags$div(class = "insight-card",
        tags$h5("⚡ Audio Features"),
        tags$p("Danceability (mean 0.58) and energy (mean 0.64) dominate. A strong negative correlation exists between acousticness and energy — acoustic tracks are consistently less energetic.")
      ),
      tags$div(class = "insight-card",
        tags$h5("🔗 Correlations"),
        tags$p("Energy and loudness are strongly positively correlated. Instrumentalness negatively correlates with popularity — vocal tracks consistently outperform instrumental ones.")
      )
    ),
    column(6,
      tags$div(class = "insight-card",
        tags$h5("⭐ Popularity Drivers"),
        tags$p("Alt-rock, grunge and k-pop lead on average popularity. No single feature predicts popularity alone. Top track: 'Left and Right' feat. Jung Kook (BTS) at score 92.")
      ),
      tags$div(class = "insight-card",
        tags$h5("🔍 What Spotify Hides"),
        tags$p("Users cannot see audio DNA profiles per genre, mood distributions, or why tracks are recommended. This dashboard fills that gap by exposing the raw feature data behind the algorithm.")
      ),
      tags$div(class = "insight-card",
        tags$h5("⚠ Limitations"),
        tags$p("700 tracks is a small sample. Popularity scores are dynamic snapshots. Genre labels may not fully reflect listener perception.")
      )
    )
  ),
  fluidRow(
    box(width = 12, solidHeader = FALSE,
      tags$p(style = "color:#888;font-size:11px;margin:0;",
        tags$b("Dataset:"), " Spotify Music — Group 3, DSA8045 | 700 tracks · 20 variables · 114 genres",
        tags$br(),
        tags$b("Tools:"), " R · Shiny · ggplot2 · dplyr · shinydashboard"
      )
    )
  )
)