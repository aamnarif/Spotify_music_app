# ============================================================
# R/tabs/audio_tab.R – Audio Feature Explorer tab UI
# ============================================================

audio_tab_ui <- tabItem(tabName = "audio",
  fluidRow(column(12,
    tags$div(class = "section-header", "Audio Feature Explorer"),
    tags$div(class = "section-sub",
             "Discover how Spotify measures the sound and feel of every track")
  )),
  fluidRow(
    box(width = 3, title = "🎚 Scatter Controls", solidHeader = TRUE, status = "warning",
      selectInput("audio_feature_x", "X-Axis:", choices = audio_features, selected = "danceability"),
      selectInput("audio_feature_y", "Y-Axis:", choices = audio_features, selected = "energy"),
      selectInput("audio_genre_filter", "Genre Filter:",
                  choices = c("All Genres", sort(valid_genres)), selected = "All Genres"),
      tags$hr(),
      downloadButton("dl_audio", "⬇ Download Data", class = "btn-dl")
    ),
    box(width = 9, title = "🔵 Feature Scatter Plot", solidHeader = TRUE, status = "warning",
      plotOutput("plot_scatter", height = "350px"))
  ),
  fluidRow(
    box(width = 3, title = "🎚 Boxplot Controls", solidHeader = TRUE, status = "primary",
      selectInput("boxplot_feature", "Feature:", choices = audio_features, selected = "valence"),
      sliderInput("boxplot_n_genres", "No. of Genres:", min = 5, max = 20, value = 10)
    ),
    box(width = 9, title = "📦 Feature Distribution by Genre", solidHeader = TRUE, status = "primary",
      plotOutput("plot_boxplot", height = "400px"))
  )
)