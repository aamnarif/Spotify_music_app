# ============================================================
# R/mod_audio.R – Tab 2: Audio Features
# UI function + Server function
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================


# ── TAB 2 UI ─────────────────────────────────────────────────
audio_ui <- function() {
  tabItem(tabName = "audio",
          
          # Introduction panel
          fluidRow(
            box(width = 12, status = "warning", solidHeader = FALSE,
                tags$h4("Audio Features Explorer", class = "intro-title"),
                tags$p(class = "intro-text",
                       "Spotify calculates audio features for every track — from how suitable a track is for
           dancing (danceability) to its musical positivity (valence). Use the controls below to
           compare features across genres and discover relationships between them."
                )
            )
          ),
          
          # Controls + Scatter Plot
          fluidRow(
            box(width = 3, status = "warning", title = "Controls", solidHeader = TRUE,
                
                # Scatter controls
                selectInput("audio_feature_x", "X-Axis Feature:",
                            choices = audio_features, selected = "danceability"),
                selectInput("audio_feature_y", "Y-Axis Feature:",
                            choices = audio_features, selected = "energy"),
                selectInput("audio_genre_filter", "Filter by Genre (Scatter):",
                            choices = c("All Genres", sort(valid_genres)),
                            selected = "All Genres"),
                tags$hr(),
                
                # Boxplot controls
                selectInput("boxplot_feature", "Boxplot Feature:",
                            choices = audio_features, selected = "valence"),
                sliderInput("boxplot_n_genres", "Number of Genres in Boxplot:",
                            min = 5, max = 20, value = 10, step = 1),
                tags$hr(),
                tags$p(
                  tags$b("Tip:"), " Select the same feature for X and Y to see its distribution.",
                  style = "font-size:11px; color:#666;"
                )
            ),
            box(width = 9, status = "warning",
                title = "Audio Feature Scatter Plot", solidHeader = TRUE,
                plotOutput("plot_scatter", height = "380px"))
          ),
          
          # Boxplot
          fluidRow(
            box(width = 12, status = "primary",
                title = "Feature Distribution Across Genres (Boxplot)", solidHeader = TRUE,
                plotOutput("plot_boxplot", height = "400px"))
          )
  )
}


# ── TAB 2 SERVER ─────────────────────────────────────────────
audio_server <- function(input, output) {
  
  # Reactive: filter by genre for scatter plot
  scatter_data <- reactive({
    df <- music_valid
    if (input$audio_genre_filter != "All Genres") {
      df <- df[df$track_genre == input$audio_genre_filter, ]
    }
    df
  })
  
  # PLOT: Scatter plot
  output$plot_scatter <- renderPlot({
    df    <- scatter_data()
    x_var <- input$audio_feature_x
    y_var <- input$audio_feature_y
    x_lab <- names(audio_features)[audio_features == x_var]
    y_lab <- names(audio_features)[audio_features == y_var]
    
    ggplot(df, aes_string(x = x_var, y = y_var, colour = "track_genre")) +
      geom_point(alpha = 0.65, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = "#212121", linetype = "dashed",
                  linewidth = 0.9, alpha = 0.15) +
      scale_colour_viridis_d(option = "turbo", guide = "none") +
      labs(
        title    = paste(x_lab, "vs", y_lab),
        subtitle = paste("Genre:", input$audio_genre_filter, "| n =", nrow(df), "tracks"),
        x = x_lab, y = y_lab
      ) +
      spotify_theme()
  })
  
  # PLOT: Boxplot across genres
  output$plot_boxplot <- renderPlot({
    feat     <- input$boxplot_feature
    feat_lab <- names(audio_features)[audio_features == feat]
    n        <- input$boxplot_n_genres
    
    # Select top-N genres by median of chosen feature
    top_g <- music_valid %>%
      group_by(track_genre) %>%
      summarise(med = median(.data[[feat]], na.rm = TRUE)) %>%
      slice_max(order_by = med, n = n) %>%
      pull(track_genre)
    
    df <- music_valid[music_valid$track_genre %in% top_g, ]
    
    ggplot(df, aes_string(x = paste0("reorder(track_genre, -", feat, ")"),
                          y = feat, fill = "track_genre")) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.8,
                   outlier.colour = "#757575", show.legend = FALSE,
                   alpha = 0.8, colour = "#424242") +
      scale_fill_viridis_d(option = "plasma") +
      coord_flip() +
      labs(
        title    = paste("Distribution of", feat_lab, "Across Top", n, "Genres"),
        subtitle = "Genres ordered by median score (highest first)",
        x = "Genre", y = feat_lab
      ) +
      spotify_theme()
  })
}