# ============================================================
# server.R – Server logic, reactive data, and plot outputs
# ============================================================

server <- function(input, output, session) {

  # ── Dark / light mode toggle ──────────────────────────
  dark_mode <- reactiveVal(TRUE)

  observeEvent(input$toggle_mode, {
    new_dark <- !dark_mode()
    dark_mode(new_dark)
    updateActionButton(session, "toggle_mode",
                       label = if (new_dark) "☀ Light Mode" else "🌙 Dark Mode")
    session$sendCustomMessage("switch_theme", new_dark)
  })

  # ── Reactive theme ────────────────────────────────────
  theme <- reactive({ make_theme(dark_mode()) })

  # ── Reactive datasets ─────────────────────────────────
  overview_data <- reactive({
    df <- music
    if (!input$overview_explicit) df <- df[!df$explicit, ]
    df
  })

  popularity_data <- reactive({
    df  <- music
    sel <- c()
    if ("Explicit" %in% input$pop_explicit_filter) sel <- c(sel, TRUE)
    if ("Clean"    %in% input$pop_explicit_filter) sel <- c(sel, FALSE)
    df[df$explicit %in% sel, ]
  })

  scatter_data <- reactive({
    df <- music_valid
    if (input$audio_genre_filter != "All Genres")
      df <- df[df$track_genre == input$audio_genre_filter, ]
    df
  })

  heatmap_data <- reactive({
    df    <- music_valid
    if (input$heatmap_genre != "All Genres")
      df  <- df[df$track_genre == input$heatmap_genre, ]
    feats <- input$heatmap_features
    if (isTRUE(input$heatmap_popularity)) feats <- c(feats, "popularity")
    df[, feats, drop = FALSE]
  })

  # ── Downloads ─────────────────────────────────────────
  output$dl_overview <- downloadHandler(
    filename = function() paste0("spotify_overview_",   Sys.Date(), ".csv"),
    content  = function(f) write.csv(overview_data(),   f, row.names = FALSE)
  )
  output$dl_audio <- downloadHandler(
    filename = function() paste0("spotify_audio_",      Sys.Date(), ".csv"),
    content  = function(f) write.csv(scatter_data(),    f, row.names = FALSE)
  )
  output$dl_popularity <- downloadHandler(
    filename = function() paste0("spotify_popularity_", Sys.Date(), ".csv"),
    content  = function(f) write.csv(popularity_data(), f, row.names = FALSE)
  )
  output$dl_heatmap <- downloadHandler(
    filename = function() paste0("spotify_heatmap_",    Sys.Date(), ".csv"),
    content  = function(f) write.csv(heatmap_data(),    f, row.names = FALSE)
  )

  # ── KPI cards ─────────────────────────────────────────
  output$kpi_tracks <- renderUI({
    kpi(as.character(nrow(overview_data())), "Total Tracks", "#FF0000")
  })
  output$kpi_genres <- renderUI({
    kpi(as.character(length(unique(overview_data()$track_genre))), "Unique Genres", "#1DB954")
  })
  output$kpi_popularity <- renderUI({
    kpi(as.character(round(mean(overview_data()$popularity, na.rm = TRUE), 1)),
        "Avg Popularity", "#FF9800")
  })
  output$kpi_danceability <- renderUI({
    kpi(as.character(round(mean(overview_data()$danceability, na.rm = TRUE), 2)),
        "Avg Danceability", "#2196F3")
  })

  # ── PLOT: Genre bar ───────────────────────────────────
  output$plot_genre_bar <- renderPlot({
    t  <- theme()
    df <- overview_data() %>% count(track_genre, sort = TRUE) %>% slice_head(n = input$overview_n)
    ggplot(df, aes(x = reorder(track_genre, n), y = n, fill = n)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      scale_fill_gradient(low = "#FF6B6B", high = "#FF0000") +
      coord_flip() +
      labs(title    = paste("Top", input$overview_n, "Genres by Track Count"),
           subtitle = ifelse(!input$overview_explicit, "Explicit excluded", "All tracks"),
           x = "Genre", y = "Tracks") +
      gg_theme(t)
  }, bg = "transparent")

  # ── PLOT: Pie chart ───────────────────────────────────
  output$plot_explicit_pie <- renderPlot({
    t  <- theme()
    df <- music %>%
      count(explicit_label) %>%
      mutate(pct = round(n / sum(n) * 100, 1),
             lbl = paste0(explicit_label, "\n", pct, "%"))
    ggplot(df, aes(x = 2, y = n, fill = explicit_label)) +
      geom_col(width = 1, colour = t$plot_bg, linewidth = 2) +
      coord_polar(theta = "y", start = 0) +
      xlim(0.5, 2.5) +
      scale_fill_manual(values = c("Clean" = "#1DB954", "Explicit" = "#FF0000"), guide = "none") +
      geom_text(aes(label = lbl), position = position_stack(vjust = 0.5),
                size = 4, fontface = "bold", colour = "white") +
      labs(title = "Explicit vs Clean", x = NULL, y = NULL) +
      gg_theme(t, no_grid = TRUE, hide_axes = TRUE)
  }, bg = "transparent")

  # ── PLOT: Tempo histogram ─────────────────────────────
  output$plot_tempo_hist <- renderPlot({
    t <- theme()
    ggplot(music, aes(x = tempo, fill = explicit_label)) +
      geom_histogram(binwidth = 10, colour = t$plot_bg, linewidth = 0.3,
                     alpha = 0.9, position = "identity") +
      scale_fill_manual(values = c("Clean" = "#1DB954", "Explicit" = "#FF0000")) +
      labs(title    = "Tempo Distribution (BPM)",
           subtitle = "Explicit vs Clean",
           x = "Tempo (BPM)", y = "Tracks", fill = NULL) +
      gg_theme(t)
  }, bg = "transparent")

  # ── PLOT: Scatter ─────────────────────────────────────
  output$plot_scatter <- renderPlot({
    t     <- theme()
    df    <- scatter_data()
    x_var <- input$audio_feature_x
    y_var <- input$audio_feature_y
    x_lab <- names(audio_features)[audio_features == x_var]
    y_lab <- names(audio_features)[audio_features == y_var]
    ggplot(df, aes_string(x = x_var, y = y_var, colour = "track_genre")) +
      geom_point(alpha = 0.65, size = 2.2) +
      geom_smooth(method = "lm", se = TRUE, colour = "#FF0000",
                  linetype = "dashed", linewidth = 1, alpha = 0.12) +
      scale_colour_viridis_d(option = "turbo", guide = "none") +
      labs(title    = paste(x_lab, "vs", y_lab),
           subtitle = paste("Genre:", input$audio_genre_filter, "| n =", nrow(df)),
           x = x_lab, y = y_lab) +
      gg_theme(t)
  }, bg = "transparent")

  # ── PLOT: Boxplot ─────────────────────────────────────
  output$plot_boxplot <- renderPlot({
    t        <- theme()
    feat     <- input$boxplot_feature
    feat_lab <- names(audio_features)[audio_features == feat]
    top_g <- music_valid %>%
      group_by(track_genre) %>%
      summarise(med = median(.data[[feat]], na.rm = TRUE)) %>%
      slice_max(order_by = med, n = input$boxplot_n_genres) %>%
      pull(track_genre)
    df <- music_valid[music_valid$track_genre %in% top_g, ]
    ggplot(df, aes_string(x = "reorder(track_genre,-get(feat))", y = feat, fill = "track_genre")) +
      geom_boxplot(outlier.shape = 21, outlier.size = 1.5, show.legend = FALSE,
                   alpha = 0.85, colour = t$text2, linewidth = 0.4) +
      scale_fill_viridis_d(option = "plasma") +
      coord_flip() +
      labs(title    = paste("Distribution of", feat_lab),
           subtitle = "Top genres by median",
           x = "Genre", y = feat_lab) +
      gg_theme(t)
  }, bg = "transparent")

  # ── PLOT: Popularity by genre ─────────────────────────
  output$plot_pop_genre <- renderPlot({
    t  <- theme()
    df <- popularity_data() %>%
      group_by(track_genre) %>%
      summarise(avg_pop = mean(popularity, na.rm = TRUE), n = n()) %>%
      filter(n >= 3) %>%
      slice_max(order_by = avg_pop, n = input$pop_genre_n)
    ggplot(df, aes(x = reorder(track_genre, avg_pop), y = avg_pop, fill = avg_pop)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      geom_text(aes(label = round(avg_pop, 1)), hjust = -0.1, size = 3, colour = t$text2) +
      scale_fill_gradient(low = "#FF6B6B", high = "#FF0000") +
      coord_flip() +
      scale_y_continuous(limits = c(0, 82), expand = expansion(mult = c(0, 0.05))) +
      labs(title    = paste("Top", input$pop_genre_n, "Genres by Avg Popularity"),
           subtitle = "Min 3 tracks per genre",
           x = "Genre", y = "Avg Popularity (0–100)") +
      gg_theme(t)
  }, bg = "transparent")

  # ── PLOT: Feature vs Popularity ───────────────────────
  output$plot_pop_scatter <- renderPlot({
    t    <- theme()
    df   <- popularity_data()
    feat <- input$pop_feature
    lab  <- names(audio_features)[audio_features == feat]
    ggplot(df, aes_string(x = feat, y = "popularity", colour = "explicit_label")) +
      geom_point(alpha = 0.5, size = 1.8) +
      geom_smooth(method = "lm", se = TRUE, colour = "#FF9800",
                  linetype = "dashed", linewidth = 1, alpha = 0.12) +
      scale_colour_manual(values = c("Clean" = "#1DB954", "Explicit" = "#FF0000")) +
      labs(title    = paste(lab, "vs Popularity"),
           subtitle = paste("n =", nrow(df), "tracks"),
           x = lab, y = "Popularity (0–100)", colour = NULL) +
      gg_theme(t)
  }, bg = "transparent")

  # ── TABLE: Top tracks ─────────────────────────────────
  output$table_top_tracks <- renderTable({
    popularity_data() %>%
      arrange(desc(popularity)) %>%
      slice_head(n = input$pop_top_n) %>%
      mutate(Rank     = row_number(),
             Track    = track_name,
             Artist   = artists,
             Genre    = track_genre,
             Score    = popularity,
             Explicit = explicit_label) %>%
      select(Rank, Track, Artist, Genre, Score, Explicit)
  }, striped = TRUE, bordered = TRUE, hover = TRUE, width = "100%")

  # ── PLOT: Genre comparison (Audio DNA) ────────────────
  output$plot_mood_compare <- renderPlot({
    t   <- theme()
    g1  <- input$missing_genre1
    g2  <- input$missing_genre2
    df  <- music_valid[music_valid$track_genre %in% c(g1, g2), ]
    feats <- names(audio_features)
    vals  <- unname(audio_features)
    long  <- do.call(rbind, lapply(seq_along(vals), function(i)
      data.frame(Feature = feats[i], Value = df[[vals[i]]], Genre = df$track_genre)
    ))
    avg <- long %>%
      group_by(Feature, Genre) %>%
      summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")
    ggplot(avg, aes(x = Feature, y = Mean, fill = Genre)) +
      geom_col(position = "dodge", width = 0.65, alpha = 0.9) +
      scale_fill_manual(values = setNames(c("#FF0000", "#1DB954"), c(g1, g2))) +
      scale_y_continuous(limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
      labs(title    = paste("Audio DNA:", g1, "vs", g2),
           subtitle = "Average score per feature (0–1)",
           x = NULL, y = "Score", fill = "Genre") +
      gg_theme(t, axis_x_angle = 25, axis_x_hjust = 1, axis_x_size = 9)
  }, bg = "transparent")

  # ── PLOT: Audio profile bar chart ─────────────────────
  output$plot_radar <- renderPlot({
    t     <- theme()
    genre <- input$radar_genre
    df    <- music_valid[music_valid$track_genre == genre, ]
    vals  <- unname(audio_features)
    feats <- names(audio_features)
    means <- sapply(vals, function(v) mean(df[[v]], na.rm = TRUE))
    radar_df <- data.frame(Feature = factor(feats, levels = feats), Value = means)
    ggplot(radar_df, aes(x = Feature, y = Value, fill = Value)) +
      geom_col(width = 0.7, show.legend = FALSE, alpha = 0.9) +
      geom_text(aes(label = round(Value, 2)), vjust = -0.4, size = 3, colour = t$text2) +
      scale_fill_gradient(low = "#FF6B6B", high = "#FF0000") +
      scale_y_continuous(limits = c(0, 1.15), expand = expansion(mult = c(0, 0))) +
      labs(title    = paste("Audio Profile:", genre),
           subtitle = "Mean feature scores (0–1)",
           x = NULL, y = "Score") +
      gg_theme(t, axis_x_angle = 25, axis_x_hjust = 1, axis_x_size = 8)
  }, bg = "transparent")

  # ── PLOT: Speechiness ─────────────────────────────────
  output$plot_speechiness <- renderPlot({
    t  <- theme()
    df <- music_valid %>%
      group_by(track_genre) %>%
      summarise(mean_speech = mean(speechiness, na.rm = TRUE)) %>%
      slice_max(order_by = mean_speech, n = input$speech_n)
    ggplot(df, aes(x = reorder(track_genre, mean_speech), y = mean_speech, fill = mean_speech)) +
      geom_col(width = 0.72, show.legend = FALSE) +
      scale_fill_gradient(low = "#FF9800", high = "#E91E63") +
      coord_flip() +
      labs(title    = "Speechiness by Genre",
           subtitle = "Higher = more spoken words",
           x = NULL, y = "Avg Speechiness (0–1)") +
      gg_theme(t)
  }, bg = "transparent")

  # ── PLOT: Correlation heatmap ─────────────────────────
  output$plot_heatmap <- renderPlot({
    t  <- theme()
    df <- heatmap_data()
    validate(
      need(ncol(df) >= 2, "Please select at least 2 features."),
      need(nrow(df) >= 5, "Not enough tracks for this genre.")
    )
    cor_mat    <- cor(df, use = "pairwise.complete.obs")
    feat_names <- colnames(cor_mat)
    cor_long   <- data.frame(
      Var1  = rep(feat_names, each  = length(feat_names)),
      Var2  = rep(feat_names, times = length(feat_names)),
      value = as.vector(cor_mat)
    )
    display <- c(
      danceability     = "Danceability",
      energy           = "Energy",
      valence          = "Valence",
      acousticness     = "Acousticness",
      speechiness      = "Speechiness",
      instrumentalness = "Instrumentalness",
      liveness         = "Liveness",
      popularity       = "Popularity"
    )
    cor_long$Var1 <- ifelse(cor_long$Var1 %in% names(display), display[cor_long$Var1], cor_long$Var1)
    cor_long$Var2 <- ifelse(cor_long$Var2 %in% names(display), display[cor_long$Var2], cor_long$Var2)
    ggplot(cor_long, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile(colour = t$plot_bg, linewidth = 1) +
      geom_text(aes(label = sprintf("%.2f", value)), size = 3.5, fontface = "bold",
                colour = ifelse(abs(cor_long$value) > 0.4, "white", t$text2)) +
      scale_fill_gradient2(low = "#1565C0", mid = t$surface2, high = "#C62828",
                           midpoint = 0, limits = c(-1, 1), name = "r") +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_discrete(expand = c(0, 0)) +
      labs(title    = "Audio Feature Correlation Matrix",
           subtitle = paste("Genre:", input$heatmap_genre, "| n =", nrow(df), "tracks"),
           x = NULL, y = NULL) +
      gg_theme(t, axis_x_angle = 35, axis_x_hjust = 1, axis_x_size = 10,
               axis_y_size = 10, no_grid = TRUE, legend_key_h = 1.4)
  }, bg = "transparent")
}