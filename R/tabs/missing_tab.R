# ============================================================
# R/tabs/missing_tab.R – What's Missing tab UI
# ============================================================

missing_tab_ui <- tabItem(tabName = "missing",
  fluidRow(column(12,
    tags$div(class = "section-header", "What Spotify Doesn't Show You"),
    tags$div(class = "section-sub",
             "These insights exist in your music — but Spotify and YouTube Music hide them.")
  )),
  fluidRow(
    box(width = 3, title = "🎚 Genre Comparison", solidHeader = TRUE, status = "danger",
      selectInput("missing_genre1", "Genre A:", choices = sort(valid_genres), selected = "pop"),
      selectInput("missing_genre2", "Genre B:", choices = sort(valid_genres), selected = "hip-hop")
    ),
    box(width = 9, title = "🎭 Audio DNA — Genre vs Genre", solidHeader = TRUE, status = "danger",
      plotOutput("plot_mood_compare", height = "280px"))
  ),
  fluidRow(
    box(width = 2, title = "🎚 Audio Profile", solidHeader = TRUE, status = "warning",
      selectInput("radar_genre", "Select Genre:", choices = sort(valid_genres), selected = "pop")
    ),
    box(width = 5, title = "🕸 Audio Profile by Genre", solidHeader = TRUE, status = "warning",
      plotOutput("plot_radar", height = "300px")),
    box(width = 2, title = "🎚 Speechiness", solidHeader = TRUE, status = "primary",
      sliderInput("speech_n", "No. of Genres:", min = 5, max = 20, value = 12)
    ),
    box(width = 3, title = "🗣 Speechiness by Genre", solidHeader = TRUE, status = "primary",
      plotOutput("plot_speechiness", height = "300px"))
  ),
  fluidRow(
    column(3, tags$div(class = "insight-card",
      tags$h5("🎭 Mood by Genre"),
      tags$p("Spotify never shows the average emotional tone of a genre. Valence varies wildly — some genres are consistently upbeat, others dark.")
    )),
    column(3, tags$div(class = "insight-card",
      tags$h5("⚡ Energy vs Popularity Gap"),
      tags$p("High energy doesn't always mean high popularity. Many high-energy genres rank low — a gap the algorithm ignores.")
    )),
    column(3, tags$div(class = "insight-card",
      tags$h5("🎸 The Acoustic Paradox"),
      tags$p("Acoustic tracks score low on energy but often high on valence. Neither platform surfaces this relationship.")
    )),
    column(3, tags$div(class = "insight-card",
      tags$h5("🗣 Speechiness & Identity"),
      tags$p("Hip-hop and spoken word have uniquely high speechiness — invisible to users but it defines the genre's sonic identity.")
    ))
  )
)