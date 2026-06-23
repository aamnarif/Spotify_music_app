# ============================================================
# R/mod_overview.R – Tab 1: Overview
# UI function + Server function
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================


# ── TAB 1 UI ─────────────────────────────────────────────────
overview_ui <- function() {
  tabItem(tabName = "overview",
          
          # Introduction panel
          fluidRow(
            box(width = 12, status = "success", solidHeader = FALSE,
                tags$h4("Welcome to the Spotify Music Dashboard", class = "intro-title"),
                tags$p(class = "intro-text",
                       "This interactive dashboard explores a dataset of ", tags$b("700 Spotify tracks"),
                       " spanning ", tags$b("114 genres"), ". Use the tabs to investigate audio
           characteristics, popularity trends, and genre distributions.
           All visualisations respond dynamically to your selections."
                )
            )
          ),
          
          # KPI Value Boxes
          fluidRow(
            valueBox(700,   "Total Tracks",      icon = icon("music"),      color = "green"),
            valueBox(114,   "Unique Genres",     icon = icon("list"),       color = "blue"),
            valueBox(
              paste0(round(mean(music$popularity), 1)),
              "Avg Popularity Score",            icon = icon("star"),       color = "orange"
            ),
            valueBox(
              paste0(round(mean(music$danceability), 2)),
              "Avg Danceability",                icon = icon("headphones"), color = "purple"
            )
          ),
          
          # Controls + Genre Bar Chart
          fluidRow(
            box(width = 3, status = "success", title = "Controls", solidHeader = TRUE,
                sliderInput("overview_n", "Top N Genres to Show:",
                            min = 5, max = 30, value = 15, step = 1),
                checkboxInput("overview_explicit", "Include Explicit Tracks", value = TRUE),
                tags$hr(),
                tags$p(
                  tags$b("Tip:"), " Adjust the slider to change how many genres appear in the bar chart.",
                  style = "font-size:11px; color:#666;"
                )
            ),
            box(width = 9, status = "success",
                title = "Track Count by Genre", solidHeader = TRUE,
                plotOutput("plot_genre_bar", height = "350px"))
          ),
          
          # Explicit Pie + Tempo Histogram
          fluidRow(
            box(width = 5, status = "info",
                title = "Explicit vs Clean Tracks", solidHeader = TRUE,
                plotOutput("plot_explicit_pie", height = "300px")),
            box(width = 7, status = "info",
                title = "Tempo Distribution by Explicit Content", solidHeader = TRUE,
                plotOutput("plot_tempo_hist", height = "300px"))
          )
  )
}


# ── TAB 1 SERVER ─────────────────────────────────────────────
overview_server <- function(input, output) {
  
  # Reactive: filter explicit if checkbox is unchecked
  overview_data <- reactive({
    df <- music
    if (!input$overview_explicit) df <- df[!df$explicit, ]
    df
  })
  
  # PLOT: Genre bar chart
  output$plot_genre_bar <- renderPlot({
    df <- overview_data()
    top_genres <- df %>%
      count(track_genre, sort = TRUE) %>%
      slice_head(n = input$overview_n)
    
    ggplot(top_genres, aes(x = reorder(track_genre, n), y = n, fill = n)) +
      geom_col(show.legend = FALSE, width = 0.75) +
      scale_fill_gradient(low = "#81C784", high = SPOTIFY_GREEN) +
      coord_flip() +
      labs(
        title    = paste("Top", input$overview_n, "Genres by Track Count"),
        subtitle = if (!input$overview_explicit) "Explicit tracks excluded" else "All tracks included",
        x = "Genre", y = "Number of Tracks"
      ) +
      spotify_theme()
  })
  
  # PLOT: Explicit vs Clean pie chart
  output$plot_explicit_pie <- renderPlot({
    df <- music %>%
      count(explicit_label) %>%
      mutate(
        pct   = round(n / sum(n) * 100, 1),
        label = paste0(explicit_label, "\n", pct, "%")
      )
    
    ggplot(df, aes(x = "", y = n, fill = explicit_label)) +
      geom_col(width = 1, colour = "white", linewidth = 1.2) +
      coord_polar("y") +
      scale_fill_manual(values = c("Clean" = SPOTIFY_GREEN, "Explicit" = ACCENT3)) +
      geom_text(aes(label = label),
                position = position_stack(vjust = 0.5),
                size = 4.5, fontface = "bold", colour = "white") +
      labs(title = "Explicit vs Clean Tracks", fill = NULL) +
      theme_void(base_size = 13) +
      theme(
        plot.title      = element_text(face = "bold", size = 14, hjust = 0.5),
        legend.position = "none",
        plot.background = element_rect(fill = PANEL_BG, colour = NA)
      )
  })
  
  # PLOT: Tempo histogram by explicit content
  output$plot_tempo_hist <- renderPlot({
    ggplot(music, aes(x = tempo, fill = explicit_label)) +
      geom_histogram(binwidth = 10, colour = "white",
                     linewidth = 0.4, alpha = 0.85, position = "identity") +
      scale_fill_manual(values = c("Clean" = SPOTIFY_GREEN, "Explicit" = ACCENT3)) +
      labs(
        title    = "Tempo Distribution (BPM)",
        subtitle = "Comparing explicit and clean tracks",
        x = "Tempo (BPM)", y = "Number of Tracks", fill = NULL
      ) +
      spotify_theme()
  })
}