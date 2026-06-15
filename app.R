# ============================================================
# DSA8045 – Applied Analytics | Assignment 1
# Group 3 – Spotify Music Dashboard
# ============================================================


library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# ── Load & pre-process data ──────────────────────────────────
music <- read.csv("Group3_music.csv", stringsAsFactors = FALSE)

# Derived columns
music$duration_min  <- round(music$duration_ms / 60000, 2)
music$mode_label    <- ifelse(music$mode == 1, "Major", "Minor")
music$explicit_label <- ifelse(music$explicit, "Explicit", "Clean")

# Key names (standard pitch-class notation)
key_labels <- c("C","C#/Db","D","D#/Eb","E","F","F#/Gb","G","G#/Ab","A","A#/Bb","B")
music$key_name <- key_labels[music$key + 1]

# Genres with at least 5 tracks (for readable boxplots)
genre_counts <- table(music$track_genre)
valid_genres  <- names(genre_counts[genre_counts >= 5])
music_valid   <- music[music$track_genre %in% valid_genres, ]

# Colour palette – colourblind-friendly (ColorBrewer)
SPOTIFY_GREEN <- "#1DB954"
ACCENT1       <- "#2196F3"
ACCENT2       <- "#FF9800"
ACCENT3       <- "#E91E63"
PANEL_BG      <- "#F8F9FA"

# Audio feature choices used across tabs
audio_features <- c(
  "Danceability"     = "danceability",
  "Energy"           = "energy",
  "Valence (Mood)"   = "valence",
  "Acousticness"     = "acousticness",
  "Speechiness"      = "speechiness",
  "Instrumentalness" = "instrumentalness",
  "Liveness"         = "liveness"
)

# ── Shared ggplot2 theme ─────────────────────────────────────
spotify_theme <- function() {
  theme_minimal(base_size = 13) +
    theme(
      plot.title       = element_text(face = "bold", size = 15, colour = "#212121"),
      plot.subtitle    = element_text(size = 11, colour = "#616161"),
      axis.title       = element_text(face = "bold", size = 11),
      axis.text        = element_text(size = 10),
      legend.title     = element_text(face = "bold", size = 10),
      legend.text      = element_text(size = 9),
      panel.grid.major = element_line(colour = "#E0E0E0"),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = PANEL_BG, colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      plot.margin      = margin(15, 15, 15, 15)
    )
}

# ── Value box helper ─────────────────────────────────────────
make_vbox <- function(value, subtitle, icon_name, colour) {
  valueBox(value = value, subtitle = subtitle,
           icon = icon(icon_name), color = colour)
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "green",

  # ── Header ──────────────────────────────────────────────
  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/19/Spotify_logo_without_text.svg/168px-Spotify_logo_without_text.svg.png",
               height = "28px", style = "margin-right:8px;"),
      "Spotify Music Dashboard"
    ),
    titleWidth = 300
  ),

  # ── Sidebar ─────────────────────────────────────────────
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("Overview",          tabName = "overview",    icon = icon("music")),
      menuItem("Audio Features",    tabName = "audio",       icon = icon("sliders-h")),
      menuItem("Popularity",        tabName = "popularity",  icon = icon("star")),
      menuItem("Conclusions",       tabName = "conclusions", icon = icon("lightbulb"))
    ),
    tags$hr(),
    tags$p(tags$b("About"),
           style = "color:#aaa; font-size:11px; padding:0 15px;"),
    tags$p("Exploring 700 Spotify tracks across 114 genres using audio features and popularity data.",
           style = "color:#bbb; font-size:10px; padding:0 15px; line-height:1.4;")
  ),

  # ── Body ────────────────────────────────────────────────
  dashboardBody(

    # Custom CSS
    tags$head(tags$style(HTML("
      .content-wrapper, .right-side { background-color: #F4F6F9; }
      .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
      .box-header { border-radius: 8px 8px 0 0; }
      .info-box { border-radius: 8px; }
      .small-box { border-radius: 8px; }
      h4.intro-title { color: #1DB954; font-weight: bold; margin-bottom: 6px; }
      .intro-text { color: #424242; line-height: 1.7; font-size: 13px; }
    "))),

    tabItems(

      # ══════════════════════════════════════════════════
      # TAB 1 – OVERVIEW
      # ══════════════════════════════════════════════════
      tabItem(tabName = "overview",

        # Introduction panel
        fluidRow(
          box(width = 12, status = "success", solidHeader = FALSE,
            tags$h4("Welcome to the Spotify Music Dashboard", class = "intro-title"),
            tags$p(class = "intro-text",
              "This interactive dashboard explores a dataset of ", tags$b("700 Spotify tracks"),
              " spanning ", tags$b("114 genres"), ". Use the tabs above to investigate audio
               characteristics, popularity trends, and genre distributions. All visualisations
               respond dynamically to your selections."
            )
          )
        ),

        # KPI value boxes
        fluidRow(
          valueBox(700,       "Total Tracks",       icon = icon("music"),       color = "green"),
          valueBox(114,       "Unique Genres",      icon = icon("list"),        color = "blue"),
          valueBox(
            paste0(round(mean(music$popularity), 1)),
            "Avg Popularity Score",               icon = icon("star"),        color = "orange"
          ),
          valueBox(
            paste0(round(mean(music$danceability), 2)),
            "Avg Danceability",                   icon = icon("headphones"),  color = "purple"
          )
        ),

        # Controls row
        fluidRow(
          box(width = 3, status = "success", title = "Controls", solidHeader = TRUE,
            sliderInput("overview_n", "Top N Genres to Show:",
                        min = 5, max = 30, value = 15, step = 1),
            checkboxInput("overview_explicit", "Include Explicit Tracks", value = TRUE),
            tags$hr(),
            tags$p(tags$b("Tip:"), " Adjust the slider to change how many genres appear in the bar chart.",
                   style = "font-size:11px; color:#666;")
          ),
          box(width = 9, status = "success", title = "Track Count by Genre",
              solidHeader = TRUE,
              plotOutput("plot_genre_bar", height = "350px"))
        ),

        # Explicit breakdown + tempo distribution
        fluidRow(
          box(width = 5, status = "info", title = "Explicit vs Clean Tracks",
              solidHeader = TRUE,
              plotOutput("plot_explicit_pie", height = "300px")),
          box(width = 7, status = "info", title = "Tempo Distribution by Explicit Content",
              solidHeader = TRUE,
              plotOutput("plot_tempo_hist", height = "300px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 2 – AUDIO FEATURES
      # ══════════════════════════════════════════════════
      tabItem(tabName = "audio",

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

        # Controls
        fluidRow(
          box(width = 3, status = "warning", title = "Controls", solidHeader = TRUE,
            selectInput("audio_feature_x", "X-Axis Feature:",
                        choices = audio_features, selected = "danceability"),
            selectInput("audio_feature_y", "Y-Axis Feature:",
                        choices = audio_features, selected = "energy"),
            selectInput("audio_genre_filter", "Filter by Genre (Scatter):",
                        choices = c("All Genres", sort(valid_genres)),
                        selected = "All Genres"),
            tags$hr(),
            selectInput("boxplot_feature", "Boxplot Feature:",
                        choices = audio_features, selected = "valence"),
            sliderInput("boxplot_n_genres", "Number of Genres in Boxplot:",
                        min = 5, max = 20, value = 10, step = 1),
            tags$hr(),
            tags$p(tags$b("Tip:"), " Select the same feature for X and Y to see its distribution.",
                   style = "font-size:11px; color:#666;")
          ),
          box(width = 9, status = "warning",
              title = "Audio Feature Scatter Plot", solidHeader = TRUE,
              plotOutput("plot_scatter", height = "380px"))
        ),

        fluidRow(
          box(width = 12, status = "primary",
              title = "Feature Distribution Across Genres (Boxplot)", solidHeader = TRUE,
              plotOutput("plot_boxplot", height = "400px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 3 – POPULARITY
      # ══════════════════════════════════════════════════
      tabItem(tabName = "popularity",

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

        # Controls
        fluidRow(
          box(width = 3, status = "danger", title = "Controls", solidHeader = TRUE,
            sliderInput("pop_top_n", "Top N Tracks in Table:",
                        min = 5, max = 50, value = 10, step = 5),
            selectInput("pop_feature", "Audio Feature vs Popularity:",
                        choices = audio_features, selected = "danceability"),
            checkboxGroupInput("pop_explicit_filter", "Include:",
                               choices = c("Explicit", "Clean"),
                               selected = c("Explicit", "Clean")),
            tags$hr(),
            sliderInput("pop_genre_n", "Top N Genres (Bar Chart):",
                        min = 5, max = 25, value = 15, step = 1),
            tags$hr(),
            tags$p(tags$b("Tip:"), " Use the checkboxes to compare popularity between explicit and clean tracks.",
                   style = "font-size:11px; color:#666;")
          ),
          box(width = 9, status = "danger",
              title = "Average Popularity by Genre", solidHeader = TRUE,
              plotOutput("plot_pop_genre", height = "380px"))
        ),

        fluidRow(
          box(width = 6, status = "primary",
              title = "Audio Feature vs Popularity", solidHeader = TRUE,
              plotOutput("plot_pop_scatter", height = "320px")),
          box(width = 6, status = "primary",
              title = "Top Tracks", solidHeader = TRUE,
              div(style = "overflow-x:auto;",
                  tableOutput("table_top_tracks")))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 4 – CONCLUSIONS
      # ══════════════════════════════════════════════════
      tabItem(tabName = "conclusions",

        fluidRow(
          box(width = 12, status = "success", solidHeader = TRUE,
              title = "Key Findings & Conclusions",

            tags$h4("1. Genre & Track Distribution", style = "color:#1DB954;"),
            tags$p("The dataset spans 114 genres with considerable variation in track count per genre.
                    Dancehall, country, and Turkish music have the highest representation. The
                    majority of tracks (647 out of 700) are non-explicit, suggesting the dataset
                    skews towards mainstream, radio-friendly content."),

            tags$hr(),
            tags$h4("2. Audio Feature Insights", style = "color:#1DB954;"),
            tags$p("Danceability (mean = 0.58) and energy (mean = 0.64) are the highest-scoring
                    features on average, confirming that the dataset is dominated by upbeat,
                    rhythmically driven music. Acousticness and instrumentalness are lower on
                    average, consistent with the genre mix (primarily pop-adjacent styles).
                    A positive correlation exists between energy and danceability, and a negative
                    correlation is observed between acousticness and energy — acoustic tracks
                    tend to be less energetic."),

            tags$hr(),
            tags$h4("3. Popularity Drivers", style = "color:#1DB954;"),
            tags$p("Genres such as alt-rock, grunge, and k-pop achieve the highest average
                    popularity scores, while niche or regional genres score lower. No single audio
                    feature strongly predicts popularity in isolation — popularity appears to be
                    multi-factorial. The most popular track in the dataset is 'Left and Right'
                    (feat. Jung Kook of BTS) with a score of 92, consistent with k-pop's dominance
                    in streaming metrics."),

            tags$hr(),
            tags$h4("4. Recommendations", style = "color:#1DB954;"),
            tags$ul(
              tags$li("Artists seeking to maximise Spotify popularity should target high-danceability, high-energy production styles consistent with current streaming trends."),
              tags$li("Genre labels alone are insufficient predictors of popularity — audio feature profiles within genres vary considerably."),
              tags$li("Explicit content does not appear to negatively impact popularity scores in this dataset.")
            ),

            tags$hr(),
            tags$h4("5. Dashboard Limitations", style = "color:#1DB954;"),
            tags$p("The dataset contains 700 tracks — a small sample relative to Spotify's full
                    catalogue. Genre labels are assigned at the track level and may not reflect
                    listener perception. Popularity scores are dynamic and reflect a snapshot in
                    time. Future work could incorporate time-series data or streaming counts for
                    deeper analysis."),

            tags$hr(),
            tags$p(tags$b("Dataset:"), " Spotify Music (Group 3, DSA8045) — 700 tracks, 20 variables, 114 genres.",
                   style = "color:#888; font-size:11px;"),
            tags$p(tags$b("Tools:"), " R, Shiny, ggplot2, dplyr, shinydashboard.",
                   style = "color:#888; font-size:11px;")
          )
        )
      )
    )
  )
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════
server <- function(input, output, session) {

  # ── Reactive: filtered data for explicit checkbox ──────
  overview_data <- reactive({
    df <- music
    if (!input$overview_explicit) df <- df[!df$explicit, ]
    df
  })

  # ── Reactive: filtered data for popularity tab ─────────
  popularity_data <- reactive({
    df <- music
    selected <- c()
    if ("Explicit" %in% input$pop_explicit_filter) selected <- c(selected, TRUE)
    if ("Clean"    %in% input$pop_explicit_filter) selected <- c(selected, FALSE)
    df[df$explicit %in% selected, ]
  })

  # ── Reactive: scatter data for audio tab ───────────────
  scatter_data <- reactive({
    df <- music_valid
    if (input$audio_genre_filter != "All Genres") {
      df <- df[df$track_genre == input$audio_genre_filter, ]
    }
    df
  })

  # ── PLOT: Genre bar chart (Tab 1) ──────────────────────
  output$plot_genre_bar <- renderPlot({
    df <- overview_data()
    top_genres <- df %>%
      count(track_genre, sort = TRUE) %>%
      slice_head(n = input$overview_n)

    ggplot(top_genres, aes(x = reorder(track_genre, n), y = n, fill = n)) +
      geom_col(show.legend = FALSE, width = 0.75) +
      scale_fill_gradient(low = "#81C784", high = SPOTIFY_GREEN) +
      coord_flip() +
      labs(title   = paste("Top", input$overview_n, "Genres by Track Count"),
           subtitle = if (!input$overview_explicit) "Explicit tracks excluded" else "All tracks included",
           x = "Genre", y = "Number of Tracks") +
      spotify_theme()
  })

  # ── PLOT: Explicit pie chart (Tab 1) ───────────────────
  output$plot_explicit_pie <- renderPlot({
    df <- music %>%
      count(explicit_label) %>%
      mutate(pct = round(n / sum(n) * 100, 1),
             label = paste0(explicit_label, "\n", pct, "%"))

    ggplot(df, aes(x = "", y = n, fill = explicit_label)) +
      geom_col(width = 1, colour = "white", linewidth = 1.2) +
      coord_polar("y") +
      scale_fill_manual(values = c("Clean" = SPOTIFY_GREEN, "Explicit" = ACCENT3)) +
      geom_text(aes(label = label), position = position_stack(vjust = 0.5),
                size = 4.5, fontface = "bold", colour = "white") +
      labs(title = "Explicit vs Clean Tracks", fill = NULL) +
      theme_void(base_size = 13) +
      theme(plot.title    = element_text(face = "bold", size = 14, hjust = 0.5),
            legend.position = "none",
            plot.background = element_rect(fill = PANEL_BG, colour = NA))
  })

  # ── PLOT: Tempo histogram by explicit (Tab 1) ──────────
  output$plot_tempo_hist <- renderPlot({
    ggplot(music, aes(x = tempo, fill = explicit_label)) +
      geom_histogram(binwidth = 10, colour = "white", linewidth = 0.4, alpha = 0.85,
                     position = "identity") +
      scale_fill_manual(values = c("Clean" = SPOTIFY_GREEN, "Explicit" = ACCENT3)) +
      labs(title    = "Tempo Distribution (BPM)",
           subtitle = "Comparing explicit and clean tracks",
           x = "Tempo (BPM)", y = "Number of Tracks", fill = NULL) +
      spotify_theme()
  })

  # ── PLOT: Scatter (Tab 2) ──────────────────────────────
  output$plot_scatter <- renderPlot({
    df    <- scatter_data()
    x_var <- input$audio_feature_x
    y_var <- input$audio_feature_y
    x_lab <- names(audio_features)[audio_features == x_var]
    y_lab <- names(audio_features)[audio_features == y_var]

    ggplot(df, aes_string(x = x_var, y = y_var, colour = "track_genre")) +
      geom_point(alpha = 0.65, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, colour = "#212121",
                  linetype = "dashed", linewidth = 0.9, alpha = 0.15) +
      scale_colour_viridis_d(option = "turbo", guide = "none") +
      labs(title    = paste(x_lab, "vs", y_lab),
           subtitle = paste("Genre:", input$audio_genre_filter, "| n =", nrow(df), "tracks"),
           x = x_lab, y = y_lab) +
      spotify_theme()
  })

  # ── PLOT: Boxplot (Tab 2) ──────────────────────────────
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

    ggplot(df, aes_string(x = "reorder(track_genre, -get(feat))", y = feat, fill = "track_genre")) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.8,
                   outlier.colour = "#757575", show.legend = FALSE,
                   alpha = 0.8, colour = "#424242") +
      scale_fill_viridis_d(option = "plasma") +
      coord_flip() +
      labs(title    = paste("Distribution of", feat_lab, "Across Top", n, "Genres"),
           subtitle = "Genres ordered by median score (highest first)",
           x = "Genre", y = feat_lab) +
      spotify_theme()
  })

  # ── PLOT: Popularity by genre bar (Tab 3) ─────────────
  output$plot_pop_genre <- renderPlot({
    df <- popularity_data()
    top_g <- df %>%
      group_by(track_genre) %>%
      summarise(avg_pop = mean(popularity, na.rm = TRUE), n = n()) %>%
      filter(n >= 3) %>%
      slice_max(order_by = avg_pop, n = input$pop_genre_n)

    ggplot(top_g, aes(x = reorder(track_genre, avg_pop), y = avg_pop, fill = avg_pop)) +
      geom_col(show.legend = FALSE, width = 0.75) +
      geom_text(aes(label = round(avg_pop, 1)), hjust = -0.15, size = 3.2, colour = "#424242") +
      scale_fill_gradient(low = "#FFB74D", high = ACCENT3) +
      coord_flip() +
      scale_y_continuous(limits = c(0, 80), expand = expansion(mult = c(0, 0.05))) +
      labs(title    = paste("Top", input$pop_genre_n, "Genres by Average Popularity"),
           subtitle = "Genres with fewer than 3 tracks excluded",
           x = "Genre", y = "Average Popularity Score (0–100)") +
      spotify_theme()
  })

  # ── PLOT: Feature vs Popularity scatter (Tab 3) ────────
  output$plot_pop_scatter <- renderPlot({
    df    <- popularity_data()
    feat  <- input$pop_feature
    lab   <- names(audio_features)[audio_features == feat]

    ggplot(df, aes_string(x = feat, y = "popularity", colour = "explicit_label")) +
      geom_point(alpha = 0.55, size = 2) +
      geom_smooth(method = "lm", se = TRUE, colour = "#212121",
                  linetype = "dashed", linewidth = 0.9, alpha = 0.15) +
      scale_colour_manual(values = c("Clean" = SPOTIFY_GREEN, "Explicit" = ACCENT3)) +
      labs(title    = paste(lab, "vs Popularity"),
           subtitle = paste("n =", nrow(df), "tracks"),
           x = lab, y = "Popularity (0–100)", colour = NULL) +
      spotify_theme()
  })

  # ── TABLE: Top tracks (Tab 3) ──────────────────────────
  output$table_top_tracks <- renderTable({
    popularity_data() %>%
      arrange(desc(popularity)) %>%
      slice_head(n = input$pop_top_n) %>%
      mutate(
        Rank        = row_number(),
        Track       = track_name,
        Artist      = artists,
        Genre       = track_genre,
        Popularity  = popularity,
        Explicit    = explicit_label,
        Duration    = paste0(duration_min, " min")
      ) %>%
      select(Rank, Track, Artist, Genre, Popularity, Explicit, Duration)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")
}

# ── Launch ───────────────────────────────────────────────────
shinyApp(ui = ui, server = server)
