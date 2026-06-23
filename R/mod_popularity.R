# ============================================================
# R/mod_popularity.R – Tab 3: Popularity
# UI function + Server function
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================


# ── TAB 3 UI ─────────────────────────────────────────────────
popularity_ui <- function() {
  tabItem(tabName = "popularity",
          
          # Introduction panel
          fluidRow(
            box(width = 12, status = "danger", solidHeader = FALSE,
                tags$h4("Popularity Analysis", class = "intro-title"),
                tags$p(class = "intro-text",
                       "Spotify's popularity score (0–100) reflects how frequently a track has been played
           recently. Explore which genres top the charts, whether audio features predict
           popularity, and browse the most popular tracks in the dataset."
                )
            )
          ),
          
          # Controls + Popularity by Genre bar chart
          fluidRow(
            box(width = 3, status = "danger", title = "Controls", solidHeader = TRUE,
                
                sliderInput("pop_top_n", "Top N Tracks in Table:",
                            min = 5, max = 50, value = 10, step = 5),
                selectInput("pop_feature", "Audio Feature vs Popularity:",
                            choices = audio_features, selected = "danceability"),
                checkboxGroupInput("pop_explicit_filter", "Include:",
                                   choices  = c("Explicit", "Clean"),
                                   selected = c("Explicit", "Clean")),
                tags$hr(),
                sliderInput("pop_genre_n", "Top N Genres (Bar Chart):",
                            min = 5, max = 25, value = 15, step = 1),
                tags$hr(),
                tags$p(
                  tags$b("Tip:"), " Use the checkboxes to compare popularity between explicit and clean tracks.",
                  style = "font-size:11px; color:#666;"
                )
            ),
            box(width = 9, status = "danger",
                title = "Average Popularity by Genre", solidHeader = TRUE,
                plotOutput("plot_pop_genre", height = "380px"))
          ),
          
          # Feature vs Popularity scatter + Top Tracks table
          fluidRow(
            box(width = 6, status = "primary",
                title = "Audio Feature vs Popularity", solidHeader = TRUE,
                plotOutput("plot_pop_scatter", height = "320px")),
            box(width = 6, status = "primary",
                title = "Top Tracks", solidHeader = TRUE,
                div(style = "overflow-x:auto;",
                    tableOutput("table_top_tracks")))
          )
  )
}


# ── TAB 3 SERVER ─────────────────────────────────────────────
popularity_server <- function(input, output) {
  
  # Reactive: filter by explicit/clean checkboxes
  popularity_data <- reactive({
    df       <- music
    selected <- c()
    if ("Explicit" %in% input$pop_explicit_filter) selected <- c(selected, TRUE)
    if ("Clean"    %in% input$pop_explicit_filter) selected <- c(selected, FALSE)
    df[df$explicit %in% selected, ]
  })
  
  # PLOT: Average popularity by genre (bar chart)
  output$plot_pop_genre <- renderPlot({
    df <- popularity_data()
    top_g <- df %>%
      group_by(track_genre) %>%
      summarise(avg_pop = mean(popularity, na.rm = TRUE), n = n()) %>%
      filter(n >= 3) %>%
      slice_max(order_by = avg_pop, n = input$pop_genre_n)
    
    ggplot(top_g, aes(x = reorder(track_genre, avg_pop), y = avg_pop, fill = avg_pop)) +
      geom_col(show.legend = FALSE, width = 0.75) +
      geom_text(aes(label = round(avg_pop, 1)),
                hjust = -0.15, size = 3.2, colour = "#424242") +
      scale_fill_gradient(low = "#FFB74D", high = ACCENT3) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 80),
                         expand = expansion(mult = c(0, 0.05))) +
      labs(
        title    = paste("Top", input$pop_genre_n, "Genres by Average Popularity"),
        subtitle = "Genres with fewer than 3 tracks excluded",
        x = "Genre", y = "Average Popularity Score (0–100)"
      ) +
      spotify_theme()
  })
  
  # PLOT: Audio feature vs popularity scatter
  output$plot_pop_scatter <- renderPlot({
    df   <- popularity_data()
    feat <- input$pop_feature
    lab  <- names(audio_features)[audio_features == feat]
    
    ggplot(df, aes_string(x = feat, y = "popularity", colour = "explicit_label")) +
      geom_point(alpha = 0.55, size = 2) +
      geom_smooth(method = "lm", se = TRUE,
                  colour = "#212121", linetype = "dashed",
                  linewidth = 0.9, alpha = 0.15) +
      scale_colour_manual(values = c("Clean" = SPOTIFY_GREEN, "Explicit" = ACCENT3)) +
      labs(
        title    = paste(lab, "vs Popularity"),
        subtitle = paste("n =", nrow(df), "tracks"),
        x = lab, y = "Popularity (0–100)", colour = NULL
      ) +
      spotify_theme()
  })
  
  # TABLE: Top tracks
  output$table_top_tracks <- renderTable({
    popularity_data() %>%
      arrange(desc(popularity)) %>%
      slice_head(n = input$pop_top_n) %>%
      mutate(
        Rank       = row_number(),
        Track      = track_name,
        Artist     = artists,
        Genre      = track_genre,
        Popularity = popularity,
        Explicit   = explicit_label,
        Duration   = paste0(duration_min, " min")
      ) %>%
      select(Rank, Track, Artist, Genre, Popularity, Explicit, Duration)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
}