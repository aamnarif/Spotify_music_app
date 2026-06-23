# ============================================================
# R/mod_conclusions.R – Tab 4: Conclusions
# UI only (static written content — no server logic needed)
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================


# ── TAB 4 UI ─────────────────────────────────────────────────
conclusions_ui <- function() {
  tabItem(tabName = "conclusions",
          
          fluidRow(
            box(width = 12, status = "success", solidHeader = TRUE,
                title = "Key Findings & Conclusions",
                
                # Finding 1
                tags$h4("1. Genre & Track Distribution", style = "color:#1DB954;"),
                tags$p(
                  "The dataset spans 114 genres with considerable variation in track count per genre.
           Dancehall, country, and Turkish music have the highest representation. The majority
           of tracks (647 out of 700) are non-explicit, suggesting the dataset skews towards
           mainstream, radio-friendly content."
                ),
                
                tags$hr(),
                
                # Finding 2
                tags$h4("2. Audio Feature Insights", style = "color:#1DB954;"),
                tags$p(
                  "Danceability (mean = 0.58) and energy (mean = 0.64) are the highest-scoring features
           on average, confirming that the dataset is dominated by upbeat, rhythmically driven
           music. Acousticness and instrumentalness are lower on average, consistent with the
           genre mix (primarily pop-adjacent styles). A positive correlation exists between energy
           and danceability, and a negative correlation is observed between acousticness and
           energy — acoustic tracks tend to be less energetic."
                ),
                
                tags$hr(),
                
                # Finding 3
                tags$h4("3. Popularity Drivers", style = "color:#1DB954;"),
                tags$p(
                  "Genres such as alt-rock, grunge, and k-pop achieve the highest average popularity
           scores, while niche or regional genres score lower. No single audio feature strongly
           predicts popularity in isolation — popularity appears to be multi-factorial. The most
           popular track in the dataset is 'Left and Right' (feat. Jung Kook of BTS) with a
           score of 92, consistent with k-pop's dominance in streaming metrics."
                ),
                
                tags$hr(),
                
                # Recommendations
                tags$h4("4. Recommendations", style = "color:#1DB954;"),
                tags$ul(
                  tags$li("Artists seeking to maximise Spotify popularity should target high-danceability,
                   high-energy production styles consistent with current streaming trends."),
                  tags$li("Genre labels alone are insufficient predictors of popularity — audio feature
                   profiles within genres vary considerably."),
                  tags$li("Explicit content does not appear to negatively impact popularity scores
                   in this dataset.")
                ),
                
                tags$hr(),
                
                # Limitations
                tags$h4("5. Dashboard Limitations", style = "color:#1DB954;"),
                tags$p(
                  "The dataset contains 700 tracks — a small sample relative to Spotify's full catalogue.
           Genre labels are assigned at the track level and may not reflect listener perception.
           Popularity scores are dynamic and reflect a snapshot in time. Future work could
           incorporate time-series data or streaming counts for deeper analysis."
                ),
                
                tags$hr(),
                
                # Footer info
                tags$p(
                  tags$b("Dataset:"), " Spotify Music (Group 3, DSA8045) — 700 tracks, 20 variables, 114 genres.",
                  style = "color:#888; font-size:11px;"
                ),
                tags$p(
                  tags$b("Tools:"), " R, Shiny, ggplot2, dplyr, shinydashboard.",
                  style = "color:#888; font-size:11px;"
                )
            )
          )
  )
}