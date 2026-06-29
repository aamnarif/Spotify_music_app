# ============================================================
# R/tabs/popularity_tab.R – Popularity Analysis tab UI
# ============================================================

popularity_tab_ui <- tabItem(tabName = "popularity",
  fluidRow(column(12,
    tags$div(class = "section-header", "Popularity Analysis"),
    tags$div(class = "section-sub", "What makes a track blow up on Spotify?")
  )),
  fluidRow(
    box(width = 3, title = "🎚 Controls", solidHeader = TRUE, status = "danger",
      sliderInput("pop_genre_n", "Top N Genres:", min = 5, max = 25, value = 15),
      checkboxGroupInput("pop_explicit_filter", "Include:",
                         choices = c("Explicit", "Clean"), selected = c("Explicit", "Clean")),
      tags$hr(),
      downloadButton("dl_popularity", "⬇ Download Data", class = "btn-dl")
    ),
    box(width = 9, title = "🏆 Average Popularity by Genre", solidHeader = TRUE, status = "danger",
      plotOutput("plot_pop_genre", height = "380px"))
  ),
  fluidRow(
    box(width = 3, title = "🎚 Feature Controls", solidHeader = TRUE, status = "primary",
      selectInput("pop_feature", "Feature vs Popularity:", choices = audio_features, selected = "danceability"),
      sliderInput("pop_top_n", "Top N Tracks in Table:", min = 5, max = 50, value = 10, step = 5)
    ),
    box(width = 5, title = "📈 Feature vs Popularity", solidHeader = TRUE, status = "primary",
      plotOutput("plot_pop_scatter", height = "320px")),
    box(width = 4, title = "🎤 Top Tracks", solidHeader = TRUE, status = "primary",
      div(style = "overflow-x:auto;max-height:340px;overflow-y:auto;",
          tableOutput("table_top_tracks")))
  )
)