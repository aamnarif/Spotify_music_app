# ============================================================
# R/tabs/overview_tab.R – Home / Overview tab UI
# ============================================================

overview_tab_ui <- tabItem(tabName = "overview",
  fluidRow(column(12,
    tags$div(class = "section-header", "Spotify Music Dashboard"),
    tags$div(class = "section-sub",
             "Exploring 700 tracks across 114 genres — the data behind the music")
  )),
  fluidRow(
    column(3, uiOutput("kpi_tracks")),
    column(3, uiOutput("kpi_genres")),
    column(3, uiOutput("kpi_popularity")),
    column(3, uiOutput("kpi_danceability"))
  ),
  fluidRow(
    box(width = 3, title = "🎚 Controls", solidHeader = TRUE, status = "danger",
      sliderInput("overview_n", "Top N Genres:", min = 5, max = 30, value = 15, step = 1),
      checkboxInput("overview_explicit", "Include Explicit Tracks", value = TRUE),
      tags$hr(),
      downloadButton("dl_overview", "⬇ Download Data", class = "btn-dl")
    ),
    box(width = 9, title = "🎵 Track Count by Genre", solidHeader = TRUE, status = "danger",
      plotOutput("plot_genre_bar", height = "360px"))
  ),
  fluidRow(
    box(width = 4, title = "🔞 Explicit vs Clean", solidHeader = TRUE, status = "warning",
      plotOutput("plot_explicit_pie", height = "280px")),
    box(width = 8, title = "🥁 Tempo Distribution", solidHeader = TRUE, status = "warning",
      plotOutput("plot_tempo_hist", height = "280px"))
  )
)