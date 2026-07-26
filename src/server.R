server <- function(input, output, session) {
  dark_mode <- reactiveVal(TRUE)
  active_page <- reactiveVal("dashboard")

  observeEvent(input$active_page, {
    active_page(input$active_page)
  })

  observeEvent(input$toggle_mode, {
    dark_mode(!dark_mode())
    session$sendCustomMessage("switch_theme", dark_mode())
  })

  #Page router
  output$page_content <- renderUI({
    switch(active_page(),
      "dashboard" = dashboard_page_ui(),
      "category"  = category_page_ui(dark_mode()),
      "popular"   = popular_page_ui(dark_mode()),
      "playlist"  = playlist_page_ui(dark_mode()),
      "statistics"= statistics_page_ui(dark_mode())
    )
  })

  #Dashboard page
  source("src/tabs/dashboard.R", local = TRUE)


  #Category page
genre_stats <- music %>%
  group_by(track_genre) %>%
  summarise(
    avg_pop    = mean(popularity,      na.rm = TRUE),
    avg_dance  = mean(danceability,    na.rm = TRUE),
    avg_energy = mean(energy,          na.rm = TRUE),
    avg_val    = mean(valence,         na.rm = TRUE),
    avg_acou   = mean(acousticness,    na.rm = TRUE),

    # Missing Audio DNA features
    avg_speech = mean(speechiness,     na.rm = TRUE),
    avg_instr  = mean(instrumentalness, na.rm = TRUE),
    avg_live   = mean(liveness,        na.rm = TRUE),

    n = dplyr::n(),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  mutate(
    mood = case_when(
      avg_energy > 0.7 & avg_val > 0.5 ~ "High Energy",
      avg_val > 0.6 & avg_energy < 0.6 ~ "Happy",
      avg_energy < 0.4 | avg_acou > 0.5 ~ "Chill",
      TRUE ~ "Dark"
    )
  )

  mood_colors <- c("High Energy"="#E11D2A","Happy"="#FBBF24","Chill"="#2BB5A0","Dark"="#7C8CF8")

  #Category page
  source("src/tabs/category.R", local = TRUE)




  #Statistics page
  stat_feats      <- c("danceability","energy","valence","acousticness",
                       "speechiness","instrumentalness","liveness")
  stat_feat_names <- c("Danceability","Energy","Valence","Acousticness",
                       "Speechiness","Instrumentalness","Liveness")
  stat_all_feats  <- c(stat_feats, "tempo", "popularity")

  stat_corr <- music %>%
    select(all_of(stat_all_feats)) %>%
    cor(use="complete.obs") %>%
    round(3)

  key_names_full <- c("C","C#/Db","D","D#/Eb","E","F",
                      "F#/Gb","G","G#/Ab","A","A#/Bb","B")

  stat_genre_table <- music %>%
    group_by(track_genre) %>%
    summarise(
      tracks    = dplyr::n(),
      avg_pop   = round(mean(popularity,     na.rm=TRUE), 1),
      avg_dance = round(mean(danceability,   na.rm=TRUE), 2),
      avg_energy= round(mean(energy,         na.rm=TRUE), 2),
      avg_bpm   = round(mean(tempo,          na.rm=TRUE), 1),
      avg_val   = round(mean(valence,        na.rm=TRUE), 2),
      .groups   = "drop"
    ) %>% filter(tracks >= 5) %>%
    arrange(desc(avg_pop))

  stat_dataset_facts <- list(
    list("Longest Track",   music$track_name[which.max(music$duration_ms)],
         fmt_dur(max(music$duration_ms))),
    list("Shortest Track",  music$track_name[which.min(music$duration_ms)],
         fmt_dur(min(music$duration_ms))),
    list("Highest BPM",     music$track_name[which.max(music$tempo)],
         paste0(round(max(music$tempo)), " BPM")),
    list("Lowest BPM",      music$track_name[which.min(music$tempo)],
         paste0(round(min(music$tempo)), " BPM")),
    list("Most Popular",    music$track_name[which.max(music$popularity)],
         paste0(max(music$popularity), " pts")),
    list("Rarest Genre",    names(sort(table(music$track_genre)))[1],
         paste0(min(table(music$track_genre)), " track")),
    list("Most Common Key", key_names_full[as.integer(names(sort(table(music$key), decreasing=TRUE))[1]) + 1],
         paste0(max(table(music$key)), " tracks")),
    list("Explicit Tracks", paste0(sum(music$explicit), " explicit"),
         paste0(round(sum(music$explicit)/nrow(music)*100,1), "% of dataset"))
  )

  #Statistics page UI
  source("src/tabs/statistics.R", local = TRUE)

  #Statistics page server outputs

  stat_theme <- function(dark) {
    theme_minimal(base_size=10) +
      theme(
        plot.background  = element_rect(fill=if(dark)"#1A1A1A" else "#FFFFFF", colour=NA),
        panel.background = element_rect(fill=if(dark)"#1A1A1A" else "#FFFFFF", colour=NA),
        panel.grid.major = element_line(colour=if(dark)"#2A2A2A" else "#EEEEEE", linewidth=0.4),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=8),
        axis.title = element_text(colour=if(dark)"#CCCCCC" else "#555", size=9),
        plot.margin= margin(6,6,6,6),
        legend.text= element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=8),
        legend.title=element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=8)
      )
  }

  #Correlation heatmap
  output$stat_corr_plot <- renderPlot({
    dark <- dark_mode()
    focus <- if (is.null(input$stat_corr_focus)) "all" else input$stat_corr_focus
    mat <- stat_corr
    if (focus != "all") {
      # Bar chart showing correlations with the selected feature
      corr_vals <- mat[focus, ]
      corr_vals <- corr_vals[names(corr_vals) != focus]
      df <- data.frame(Feature=names(corr_vals), r=as.numeric(corr_vals)) %>%
        arrange(r)
      df$Feature <- factor(df$Feature, levels=df$Feature)
      ggplot(df, aes(x=Feature, y=r, fill=r)) +
        geom_col(width=0.65, show.legend=FALSE) +
        geom_hline(yintercept=0, colour=if(dark)"#555" else "#ccc", linewidth=0.5) +
        geom_text(aes(label=sprintf("%.2f",r)),
                  hjust=ifelse(df$r >= 0, -0.15, 1.15), size=3,
                  colour=if(dark)"#CCCCCC" else "#555") +
        scale_fill_gradient2(low="#E11D2A", mid=if(dark)"#333" else "#ddd",
                             high="#2BB5A0", midpoint=0) +
        scale_y_continuous(limits=c(min(df$r)-0.15, max(df$r)+0.15)) +
        coord_flip() +
        labs(x=NULL, y="Correlation (r)") +
        stat_theme(dark)
    } else {
      pairs <- expand.grid(x=rownames(mat), y=rownames(mat))
      pairs$val <- as.vector(mat)
      pairs$x <- factor(pairs$x, levels=rownames(mat))
      pairs$y <- factor(pairs$y, levels=rev(rownames(mat)))
      ggplot(pairs, aes(x=x, y=y, fill=val)) +
        geom_tile(colour=if(dark)"#1A1A1A" else "#FFFFFF", linewidth=0.5) +
        geom_text(aes(label=sprintf("%.2f",val)),
                  colour=if(dark)"#FFFFFF" else "#15161A", size=2.5, alpha=0.8) +
        scale_fill_gradient2(low="#7C8CF8", mid=if(dark)"#1A1A1A" else "#FFFFFF",
                             high="#2BB5A0", midpoint=0, limits=c(-1,1), name="r") +
        scale_x_discrete(guide=guide_axis(angle=35)) +
        labs(x=NULL, y=NULL) +
        stat_theme(dark) +
        theme(panel.grid=element_blank())
    }
  }, bg="transparent")

  #Distribution Explorer
  output$stat_dist_plot <- renderPlot({
    dark <- dark_mode()
    feat <- input$stat_dist_feat
    fname <- stat_feat_names[stat_feats == feat]
    ggplot(music, aes_string(x=feat)) +
      geom_histogram(bins=30, fill=ACCENT_GRAD1, alpha=0.85, colour=NA) +
      geom_vline(xintercept=mean(music[[feat]], na.rm=TRUE),
                 colour="#E11D2A", linewidth=1, linetype="dashed") +
      labs(x=fname, y="Track Count") +
      stat_theme(dark)
  }, bg="transparent")

  #Outlier Finder
  output$stat_outliers <- renderUI({
    feat <- input$stat_outlier_feat
    fname <- stat_feat_names[stat_feats == feat]
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    tc <- if (dark_mode()) "#FFFFFF" else "#15161A"
    top5 <- music %>% arrange(desc(.data[[feat]])) %>% slice_head(n=5) %>%
      mutate(main_artist=sapply(artists,clean_artist))
    bot5 <- music %>% arrange(.data[[feat]]) %>% slice_head(n=5) %>%
      mutate(main_artist=sapply(artists,clean_artist))
    make_rows <- function(df, col, label) {
      tagList(
        tags$div(style=paste0("font-size:10px;font-weight:700;color:",col,
                              ";text-transform:uppercase;margin:10px 0 6px 0;"), label),
        lapply(seq_len(nrow(df)), function(i) {
          r <- df[i,]
          tags$div(style="display:flex;align-items:center;gap:10px;padding:6px 0;border-bottom:1px solid var(--border);",
            tags$div(style=paste0("width:34px;height:34px;border-radius:8px;flex-shrink:0;background:",
                                  grad_for(i),";display:flex;align-items:center;justify-content:center;",
                                  "color:#fff;font-size:9px;font-weight:800;"), initials(r$track_genre)),
            tags$div(style="flex:1;min-width:0;",
              tags$div(style=paste0("font-size:11.5px;font-weight:700;color:",tc,";white-space:nowrap;",
                                   "overflow:hidden;text-overflow:ellipsis;"), r$track_name),
              tags$div(style=paste0("font-size:10px;color:",sc,";"), r$main_artist)
            ),
            tags$div(style=paste0("font-size:12px;font-weight:800;color:",col,";"),
                     sprintf("%.3f", r[[feat]]))
          )
        })
      )
    }
    tagList(
      make_rows(top5, "#2BB5A0", paste("Highest", fname)),
      make_rows(bot5, "#E11D2A", paste("Lowest", fname))
    )
  })

  #Genre Statistics Table
  output$stat_genre_table <- renderUI({
    sort_col <- if (is.null(input$stat_table_sort)) "avg_pop" else input$stat_table_sort
    df <- stat_genre_table %>% arrange(desc(.data[[sort_col]]))
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    tc <- if (dark_mode()) "#FFFFFF" else "#15161A"
    header <- tags$div(
      style=paste0("display:grid;grid-template-columns:1.8fr repeat(5,1fr);gap:6px;",
                   "padding:6px 8px;font-size:9.5px;font-weight:700;color:",sc,";",
                   "text-transform:uppercase;border-bottom:2px solid var(--border);"),
      tags$div("Genre"), tags$div("Tracks"), tags$div("Avg Pop"),
      tags$div("Dance"), tags$div("Energy"), tags$div("BPM")
    )
    rows <- tagList(lapply(seq_len(nrow(df)), function(i) {
      r <- df[i,]
      tags$div(
        style=paste0("display:grid;grid-template-columns:1.8fr repeat(5,1fr);gap:6px;",
                     "padding:7px 8px;border-bottom:1px solid var(--border);",
                     "align-items:center;"),
        tags$div(style=paste0("font-size:12px;font-weight:700;color:",tc,";"), r$track_genre),
        tags$div(style=paste0("font-size:11px;color:",sc,";"), r$tracks),
        tags$div(style="font-size:11px;font-weight:800;color:#2BB5A0;", r$avg_pop),
        tags$div(style=paste0("font-size:11px;color:",sc,";"), r$avg_dance),
        tags$div(style=paste0("font-size:11px;color:",sc,";"), r$avg_energy),
        tags$div(style=paste0("font-size:11px;color:",sc,";"), r$avg_bpm)
      )
    }))
    tagList(header, rows)
  })

  #Key & Mode plot
  output$stat_key_plot <- renderPlot({
    dark <- dark_mode()
    df <- music %>% count(key) %>%
      mutate(key_name=key_names_full[key+1])
    ggplot(df, aes(x=reorder(key_name,key), y=n, fill=n)) +
      geom_col(width=0.7, show.legend=FALSE) +
      scale_fill_gradient(low="#7C8CF8", high="#2BB5A0") +
      labs(x="Key", y="Tracks") +
      stat_theme(dark)
  }, bg="transparent")

  #Explicit vs Clean
  output$stat_explicit_plot <- renderPlot({
    dark <- dark_mode()
    filter_val <- if (is.null(input$stat_explicit_filter)) "both" else input$stat_explicit_filter
    df <- music %>%
      mutate(Type=ifelse(explicit,"Explicit","Clean")) %>%
      { if (filter_val == "clean") filter(., Type=="Clean")
        else if (filter_val == "explicit") filter(., Type=="Explicit")
        else . } %>%
      select(Type, all_of(stat_feats)) %>%
      tidyr::pivot_longer(-Type, names_to="Feature", values_to="Value") %>%
      mutate(Feature=stat_feat_names[match(Feature, stat_feats)]) %>%
      group_by(Type, Feature) %>%
      summarise(Mean=mean(Value,na.rm=TRUE), .groups="drop")

    if (filter_val == "both") {
      p <- ggplot(df, aes(x=Feature, y=Mean, fill=Type)) +
        geom_col(position="dodge", width=0.65) +
        scale_fill_manual(values=c("Explicit"="#E11D2A","Clean"="#2BB5A0")) +
        theme(legend.position="top")
    } else {
      col <- if (filter_val == "explicit") "#E11D2A" else "#2BB5A0"
      p <- ggplot(df, aes(x=Feature, y=Mean)) +
        geom_col(width=0.65, fill=col, alpha=0.9) +
        geom_text(aes(label=sprintf("%.2f",Mean)), vjust=-0.4, size=3,
                  colour=if(dark)"#CCCCCC" else "#555") +
        theme(legend.position="none")
    }
    p +
      scale_x_discrete(guide=guide_axis(angle=30)) +
      scale_y_continuous(expand=expansion(mult=c(0,0.1))) +
      labs(x=NULL, y="Avg Score", fill=NULL) +
      stat_theme(dark)
  }, bg="transparent")

  #Tempo Analysis
  output$stat_tempo_plot <- renderPlot({
    dark <- dark_mode()
    genre_sel <- if (is.null(input$stat_tempo_genre)) "all" else input$stat_tempo_genre
    df <- if (genre_sel == "all") music else music[music$track_genre == genre_sel, ]
    mean_bpm <- mean(df$tempo, na.rm=TRUE)
    subtitle <- if (genre_sel == "all") {
      paste0("All 700 tracks · Mean: ", round(mean_bpm,1), " BPM")
    } else {
      paste0(nrow(df), " tracks in '", genre_sel, "' · Mean: ", round(mean_bpm,1), " BPM")
    }
    ggplot(df, aes(x=tempo)) +
      geom_histogram(bins=25, fill="#FBBF24", alpha=0.85, colour=NA) +
      geom_vline(xintercept=mean_bpm, colour="#E11D2A",
                 linewidth=1, linetype="dashed") +
      annotate("text", x=mean_bpm+2, y=Inf, label=paste0(round(mean_bpm)," BPM"),
               hjust=0, vjust=1.5, size=3,
               colour="#E11D2A", fontface="bold") +
      labs(x="BPM", y="Tracks", subtitle=subtitle) +
      stat_theme(dark) +
      theme(plot.subtitle=element_text(
        colour=if(dark)"#9A9A9A" else "#8A8DA3", size=9))
  }, bg="transparent")

  output$stat_fastest_genre <- renderUI({
    tc <- if (dark_mode()) "#FFFFFF" else "#15161A"
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    df <- music %>% group_by(track_genre) %>%
      summarise(avg_bpm=mean(tempo,na.rm=TRUE), .groups="drop") %>%
      arrange(desc(avg_bpm)) %>% slice(1)
    tagList(
      tags$div(style=paste0("font-size:13px;font-weight:800;color:",tc,";"), df$track_genre),
      tags$div(style=paste0("font-size:11px;color:#E11D2A;font-weight:700;"),
               paste0(round(df$avg_bpm,1)," avg BPM"))
    )
  })

  output$stat_slowest_genre <- renderUI({
    tc <- if (dark_mode()) "#FFFFFF" else "#15161A"
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    df <- music %>% group_by(track_genre) %>%
      summarise(avg_bpm=mean(tempo,na.rm=TRUE), .groups="drop") %>%
      arrange(avg_bpm) %>% slice(1)
    tagList(
      tags$div(style=paste0("font-size:13px;font-weight:800;color:",tc,";"), df$track_genre),
      tags$div(style=paste0("font-size:11px;color:#7C8CF8;font-weight:700;"),
               paste0(round(df$avg_bpm,1)," avg BPM"))
    )
  })

  #Playlist page
  #Audio Journey, tracks sorted by tempo
  journey_tracks <- music %>%
    mutate(main_artist = sapply(artists, clean_artist),
           dur = fmt_dur(duration_ms),
           bpm = round(tempo),
           energy_pct = round(energy * 100),
           era = case_when(
             acousticness >= 0.75 ~ "Classic",
             acousticness >= 0.5  ~ "Indie",
             acousticness >= 0.25 ~ "Modern",
             TRUE                  ~ "Electronic"
           )) %>%
    arrange(tempo)

  #Genre compatibility, avg features per genre for heatmap
  compat_genres <- music %>%
    group_by(track_genre) %>%
    summarise(
      dance = mean(danceability, na.rm=TRUE),
      energy= mean(energy,       na.rm=TRUE),
      val   = mean(valence,      na.rm=TRUE),
      acou  = mean(acousticness, na.rm=TRUE),
      n     = dplyr::n(), .groups="drop"
    ) %>% filter(n >= 5) %>%
    arrange(desc(dance + energy + val))

  #Track list for Track vs Track selector
  track_choices <- setNames(music$ID, paste0(music$track_name, " — ", sapply(music$artists, clean_artist)))

  #Playlist page UI
  source("src/tabs/playlist.R", local = TRUE)

  #Playlist page server outputs

  #Playlist Creator server logic
  user_playlist <- reactiveVal(data.frame(
    ID=integer(), track_name=character(), artists=character(),
    track_genre=character(), popularity=integer(),
    duration_ms=numeric(), stringsAsFactors=FALSE
  ))

  output$pl_search_results_creator <- renderUI({
    q <- input$pl_creator_search
    if (is.null(q) || nchar(trimws(q)) < 1) return(NULL)
    qe <- q
    for (ch in c("\\",".","+","*","?","[","]","^","$","(",")","{","}","|")) {
      qe <- gsub(ch, paste0("\\",ch), qe, fixed=TRUE)
    }
    matches <- music %>%
      mutate(main_artist = sapply(artists, clean_artist)) %>%
      filter(grepl(paste0("^",qe), track_name, ignore.case=TRUE)) %>%
      arrange(desc(popularity)) %>% slice_head(n=6)
    if (nrow(matches) == 0)
      return(tags$div(style="font-size:11.5px;opacity:0.5;padding:6px 0;", "No tracks found."))
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    tags$div(style="border-radius:10px;border:1px solid var(--border);overflow:hidden;margin-bottom:8px;",
      tagList(lapply(seq_len(nrow(matches)), function(i) {
        r <- matches[i,]
        already <- r$ID %in% user_playlist()$ID
        tags$div(
          style=paste0("display:flex;align-items:center;gap:10px;padding:8px 12px;",
                       "cursor:pointer;border-bottom:1px solid var(--border);",
                       if(already) "opacity:0.4;" else ""),
          onclick=if(!already) sprintf(
            "Shiny.setInputValue('pl_add_track', %d, {priority:'event'});", r$ID) else NULL,
          tags$div(style=paste0("width:30px;height:30px;border-radius:7px;flex-shrink:0;background:",
                                grad_for(i),";display:flex;align-items:center;justify-content:center;",
                                "color:#fff;font-size:9px;font-weight:800;"),
                   initials(r$track_genre)),
          tags$div(style="flex:1;min-width:0;",
            tags$div(style="font-size:12px;font-weight:700;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                     r$track_name),
            tags$div(style=paste0("font-size:10px;color:",sc,";"), clean_artist(r$artists))
          ),
          if (already) tags$div(style="font-size:9px;color:#2BB5A0;font-weight:700;", "Added")
          else tags$div(style="font-size:18px;color:#2BB5A0;font-weight:300;line-height:1;", "+")
        )
      }))
    )
  })

  observeEvent(input$pl_add_track, {
    id <- input$pl_add_track
    if (!id %in% user_playlist()$ID) {
      r <- music[music$ID == id, ][1,]
      user_playlist(rbind(user_playlist(), data.frame(
        ID=r$ID, track_name=r$track_name, artists=r$artists,
        track_genre=r$track_genre, popularity=r$popularity,
        duration_ms=r$duration_ms, stringsAsFactors=FALSE
      )))
    }
  })

  observeEvent(input$pl_remove_track, {
    id <- input$pl_remove_track
    user_playlist(user_playlist()[user_playlist()$ID != id, ])
  })

  observeEvent(input$pl_clear_click, {
    user_playlist(data.frame(
      ID=integer(), track_name=character(), artists=character(),
      track_genre=character(), popularity=integer(),
      duration_ms=numeric(), stringsAsFactors=FALSE
    ))
  })

  output$pl_creator_list <- renderUI({
    pl <- user_playlist()
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    if (nrow(pl) == 0)
      return(tags$div(style="font-size:12px;opacity:0.4;padding:10px 0;text-align:center;",
                      "Your playlist is empty — search and add tracks above"))
    tagList(lapply(seq_len(nrow(pl)), function(i) {
      r <- pl[i,]
      tags$div(style="display:flex;align-items:center;gap:10px;padding:7px 4px;border-bottom:1px solid var(--border);",
        tags$div(style="font-size:11px;opacity:0.4;width:16px;", i),
        tags$div(style=paste0("width:30px;height:30px;border-radius:7px;flex-shrink:0;background:",
                              grad_for(i),";display:flex;align-items:center;justify-content:center;",
                              "color:#fff;font-size:9px;font-weight:800;"),
                 initials(r$track_genre)),
        tags$div(style="flex:1;min-width:0;",
          tags$div(style="font-size:12px;font-weight:700;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                   r$track_name),
          tags$div(style=paste0("font-size:10px;color:",sc,";"), clean_artist(r$artists))
        ),
        tags$div(style=paste0("font-size:10px;color:",sc,";width:34px;text-align:right;"),
                 fmt_dur(r$duration_ms)),
        tags$span(style="cursor:pointer;color:#E11D2A;font-size:14px;font-weight:700;padding:0 4px;",
                  onclick=sprintf("Shiny.setInputValue('pl_remove_track',%d,{priority:'event'});", r$ID),
                  "×")
      )
    }))
  })

  output$pl_creator_stats <- renderUI({
    pl <- user_playlist()
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    if (nrow(pl) == 0)
      return(tags$div(style=paste0("font-size:12px;color:",sc,";"), "Add tracks to see stats"))
    total_min <- round(sum(pl$duration_ms) / 60000, 1)
    avg_pop   <- round(mean(pl$popularity), 1)
    top_genre <- names(sort(table(pl$track_genre), decreasing=TRUE))[1]
    tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px;",
      tags$div(style="background:#2BB5A022;border-radius:10px;padding:10px 14px;",
        tags$div(style="font-size:18px;font-weight:800;color:#2BB5A0;", nrow(pl)),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Tracks")),
      tags$div(style="background:#FFB19922;border-radius:10px;padding:10px 14px;",
        tags$div(style="font-size:18px;font-weight:800;color:#FFB199;", paste0(total_min,"m")),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Duration")),
      tags$div(style="background:#7C8CF822;border-radius:10px;padding:10px 14px;",
        tags$div(style="font-size:18px;font-weight:800;color:#7C8CF8;", avg_pop),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Avg Pop")),
      tags$div(style="background:#FBBF2422;border-radius:10px;padding:10px 14px;",
        tags$div(style=paste0("font-size:13px;font-weight:800;color:#FBBF24;",
                              "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"),
                 toupper(top_genre)),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Top Genre"))
    )
  })

  output$pl_download_csv <- downloadHandler(
    filename = function() paste0("my_solace_playlist_", Sys.Date(), ".csv"),
    content  = function(file) {
      pl <- user_playlist()
      if (nrow(pl) == 0) {
        write.csv(data.frame(message="Playlist is empty"), file, row.names=FALSE)
      } else {
        out <- pl %>% mutate(main_artist=sapply(artists,clean_artist),
                             duration=fmt_dur(duration_ms)) %>%
          select(track_name, main_artist, track_genre, popularity, duration)
        write.csv(out, file, row.names=FALSE)
      }
    }
  )

  #Audio Journey
  output$pl_journey <- renderUI({
    df <- journey_tracks
    ef <- input$journey_energy
    if (!is.null(ef) && ef != "All") {
      df <- switch(ef,
        "Low"    = df[df$energy < 0.4, ],
        "Medium" = df[df$energy >= 0.4 & df$energy < 0.7, ],
        "High"   = df[df$energy >= 0.7, ]
      )
    }
    if (nrow(df) == 0) return(tags$p(style="opacity:0.5;", "No tracks match."))
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    tagList(lapply(seq_len(nrow(df)), function(i) {
      r <- df[i,]
      en_col <- if (r$energy >= 0.7) "#E11D2A" else if (r$energy >= 0.4) "#FBBF24" else "#7C8CF8"
      tags$div(style="display:flex;align-items:center;gap:10px;padding:8px 4px;border-bottom:1px solid var(--border);",
        tags$div(style=paste0("width:38px;height:38px;border-radius:9px;flex-shrink:0;background:",
                              grad_for(i),";display:flex;align-items:center;justify-content:center;",
                              "color:#fff;font-size:10px;font-weight:800;"), initials(r$track_genre)),
        tags$div(style="flex:1;min-width:0;",
          tags$div(style="font-size:12px;font-weight:700;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                   r$track_name),
          tags$div(style=paste0("font-size:10px;color:",sc,";"), r$main_artist)
        ),
        tags$div(style="text-align:right;flex-shrink:0;",
          tags$div(style=paste0("font-size:12px;font-weight:800;color:",en_col,";"),
                   paste0(r$bpm," BPM")),
          tags$div(style=paste0("font-size:9px;color:",sc,";"), r$dur)
        )
      )
    }))
  })

  #Playlist Builder
  output$pl_builder_stats <- renderUI({
    tc <- if (dark_mode()) "#FFFFFF" else "#15161A"
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    df <- pl_builder_data()
    total_min <- round(sum(df$duration_ms) / 60000, 1)
    tags$div(style="display:flex;gap:16px;margin-bottom:12px;",
      tags$div(style=paste0("background:#2BB5A022;border-radius:10px;padding:10px 14px;"),
        tags$div(style=paste0("font-size:16px;font-weight:800;color:#2BB5A0;"), nrow(df)),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Tracks")
      ),
      tags$div(style=paste0("background:#FFB19922;border-radius:10px;padding:10px 14px;"),
        tags$div(style=paste0("font-size:16px;font-weight:800;color:#FFB199;"),
                 paste0(total_min," min")),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Duration")
      ),
      tags$div(style=paste0("background:#7C8CF822;border-radius:10px;padding:10px 14px;"),
        tags$div(style=paste0("font-size:16px;font-weight:800;color:#7C8CF8;"),
                 round(mean(df$popularity), 1)),
        tags$div(style=paste0("font-size:9px;color:",sc,";text-transform:uppercase;"), "Avg Pop")
      )
    )
  })

  pl_builder_data <- reactive({
    mood_f <- input$pl_mood
    target_min <- as.numeric(input$pl_duration)
    df <- music %>%
      mutate(main_artist = sapply(artists, clean_artist),
             dur = fmt_dur(duration_ms),
             mood = case_when(
               energy > 0.7 & valence > 0.5 ~ "High Energy",
               valence > 0.6 & energy < 0.6 ~ "Happy",
               energy < 0.4 | acousticness > 0.5 ~ "Chill",
               TRUE ~ "Dark"
             )) %>%
      filter(mood == mood_f) %>%
      arrange(desc(popularity))
    # Greedily pick tracks up to target duration
    selected <- c()
    total_ms <- 0
    target_ms <- target_min * 60000
    for (i in seq_len(nrow(df))) {
      if (total_ms + df$duration_ms[i] <= target_ms) {
        selected <- c(selected, i)
        total_ms <- total_ms + df$duration_ms[i]
      }
      if (total_ms >= target_ms * 0.9) break
    }
    if (length(selected) == 0) selected <- 1:min(5, nrow(df))
    df[selected, ]
  })

  output$pl_builder_tracks <- renderUI({
    df <- pl_builder_data()
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    if (nrow(df) == 0) return(tags$p(style="opacity:0.5;", "No tracks for this mood."))
    tagList(lapply(seq_len(nrow(df)), function(i) {
      r <- df[i,]
      tags$div(style="display:flex;align-items:center;gap:10px;padding:8px 4px;border-bottom:1px solid var(--border);",
        tags$div(style=paste0("font-size:11px;font-weight:700;opacity:0.4;width:18px;"), i),
        tags$div(style=paste0("width:36px;height:36px;border-radius:9px;flex-shrink:0;background:",
                              grad_for(i+2),";display:flex;align-items:center;justify-content:center;",
                              "color:#fff;font-size:10px;font-weight:800;"), initials(r$track_genre)),
        tags$div(style="flex:1;min-width:0;",
          tags$div(style="font-size:12px;font-weight:700;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                   r$track_name),
          tags$div(style=paste0("font-size:10px;color:",sc,";"), paste0(r$main_artist," · ",r$dur))
        ),
        tags$div(style="font-size:11px;font-weight:700;opacity:0.6;", r$popularity)
      )
    }))
  })

  #Compatibility Matrix
  output$pl_compat_plot <- renderPlot({
    dark <- dark_mode()
    feat <- input$pl_compat_feat
    top_genres <- compat_genres %>% slice_head(n=12)
    mat_data <- top_genres %>% select(track_genre, !!sym(feat)) %>%
      rename(val=!!sym(feat))
    n <- nrow(mat_data)
    pairs <- expand.grid(g1=mat_data$track_genre, g2=mat_data$track_genre)
    v1 <- mat_data$val[match(pairs$g1, mat_data$track_genre)]
    v2 <- mat_data$val[match(pairs$g2, mat_data$track_genre)]
    pairs$similarity <- 1 - abs(v1 - v2)
    pairs$g1 <- factor(pairs$g1, levels=mat_data$track_genre)
    pairs$g2 <- factor(pairs$g2, levels=mat_data$track_genre)

    ggplot(pairs, aes(x=g1, y=g2, fill=similarity)) +
      geom_tile(colour=if(dark)"#1A1A1A" else "#FFFFFF", linewidth=0.5) +
      scale_fill_gradient(low=if(dark)"#1A1A1A" else "#F4F5F7",
                          high=ACCENT_GRAD1, name="Match") +
      scale_x_discrete(guide=guide_axis(angle=35)) +
      labs(x=NULL, y=NULL) +
      theme_minimal(base_size=9) +
      theme(
        plot.background  = element_rect(fill=if(dark)"#1A1A1A" else "#FFFFFF", colour=NA),
        panel.background = element_rect(fill=if(dark)"#1A1A1A" else "#FFFFFF", colour=NA),
        panel.grid       = element_blank(),
        axis.text        = element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=8),
        legend.text      = element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=8),
        legend.title     = element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=8),
        plot.margin      = margin(4,4,4,4)
      )
  }, bg="transparent")

  #Track vs Track
  output$pl_versus <- renderUI({
    tc <- if (dark_mode()) "#FFFFFF" else "#15161A"
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    id_a <- as.integer(input$pl_track_a)
    id_b <- as.integer(input$pl_track_b)
    ra <- music[music$ID == id_a, ][1,]
    rb <- music[music$ID == id_b, ][1,]
    feats <- c("danceability","energy","valence","acousticness","speechiness","liveness")
    feat_labels <- c("Dance","Energy","Valence","Acoustic","Speech","Liveness")
    tagList(
      tags$div(style="display:grid;grid-template-columns:1fr auto 1fr;gap:8px;margin-bottom:16px;align-items:center;",
        tags$div(style="text-align:left;",
          tags$div(style=paste0("font-size:13px;font-weight:800;color:",tc,";"), ra$track_name),
          tags$div(style=paste0("font-size:10px;color:",sc,";"), clean_artist(ra$artists)),
          tags$div(style="margin-top:6px;display:flex;gap:6px;",
            tags$span(style=paste0("font-size:10px;font-weight:700;padding:2px 8px;border-radius:20px;",
                                   "background:#2BB5A022;color:#2BB5A0;"), ra$track_genre),
            tags$span(style=paste0("font-size:10px;font-weight:700;padding:2px 8px;border-radius:20px;",
                                   "background:#7C8CF822;color:#7C8CF8;"), paste0(ra$popularity," pts"))
          )
        ),
        tags$div(style=paste0("font-size:13px;font-weight:800;color:",sc,";text-align:center;padding:0 8px;"), "VS"),
        tags$div(style="text-align:right;",
          tags$div(style=paste0("font-size:13px;font-weight:800;color:",tc,";"), rb$track_name),
          tags$div(style=paste0("font-size:10px;color:",sc,";"), clean_artist(rb$artists)),
          tags$div(style="margin-top:6px;display:flex;gap:6px;justify-content:flex-end;",
            tags$span(style=paste0("font-size:10px;font-weight:700;padding:2px 8px;border-radius:20px;",
                                   "background:#2BB5A022;color:#2BB5A0;"), rb$track_genre),
            tags$span(style=paste0("font-size:10px;font-weight:700;padding:2px 8px;border-radius:20px;",
                                   "background:#7C8CF822;color:#7C8CF8;"), paste0(rb$popularity," pts"))
          )
        )
      ),
      tagList(lapply(seq_along(feats), function(i) {
        va <- as.numeric(ra[[feats[i]]])
        vb <- as.numeric(rb[[feats[i]]])
        winner_a <- va >= vb
        tags$div(style="display:flex;align-items:center;gap:8px;margin-bottom:8px;",
          tags$div(style=paste0("width:38%;text-align:right;"),
            tags$div(style=paste0("display:inline-block;height:7px;border-radius:4px;",
                                  "width:",round(va*100),"%;background:",
                                  if(winner_a) ACCENT_GRAD1 else "#444",";"))
          ),
          tags$div(style=paste0("width:14%;text-align:center;font-size:9.5px;font-weight:700;",
                                "color:",sc,";"), feat_labels[i]),
          tags$div(style="width:38%;",
            tags$div(style=paste0("display:inline-block;height:7px;border-radius:4px;",
                                  "width:",round(vb*100),"%;background:",
                                  if(!winner_a) "#E11D2A" else "#444",";"))
          )
        )
      }))
    )
  })

  #Popular page
  pop_top50 <- music %>%
    arrange(desc(popularity)) %>%
    slice_head(n = 50) %>%
    mutate(main_artist = sapply(artists, clean_artist),
           dur = fmt_dur(duration_ms))

  pop_top_artists <- music %>%
    mutate(main_artist = sapply(artists, clean_artist)) %>%
    group_by(main_artist) %>%
    summarise(top_pop = max(popularity), n = dplyr::n(),
              top_track = track_name[which.max(popularity)],
              top_genre = track_genre[which.max(popularity)],
              .groups = "drop") %>%
    arrange(desc(top_pop)) %>%
    slice_head(n = 10)

  feat_cols_pop <- c("danceability","energy","valence",
                     "acousticness","speechiness","instrumentalness","liveness")
  feat_names_pop <- c("Danceability","Energy","Valence","Acousticness",
                      "Speechiness","Instrumentalness","Liveness")
  pop20_means <- music %>%
    arrange(desc(popularity)) %>%
    slice_head(n = 20) %>%
    summarise(across(all_of(feat_cols_pop), mean, na.rm=TRUE))

  #Popular page UI
  source("src/tabs/popular.R", local = TRUE)

  #Popular page server outputs
  output$pop_leaderboard <- renderUI({
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    tagList(lapply(seq_len(nrow(pop_top50)), function(i) {
      r <- pop_top50[i,]
      rank_col <- if (i == 1) "#FBBF24" else if (i <= 3) "#9A9A9A" else "var(--border)"
      tags$div(
        style=paste0("display:flex;align-items:center;gap:12px;padding:9px 4px;",
                     "border-bottom:1px solid var(--border);"),
        tags$div(style=paste0("width:26px;height:26px;border-radius:8px;flex-shrink:0;",
                              "display:flex;align-items:center;justify-content:center;",
                              "font-size:11px;font-weight:800;background:",rank_col,"22;color:",rank_col,";"),
                 i),
        tags$div(style=paste0("width:36px;height:36px;border-radius:8px;flex-shrink:0;",
                              "background:",grad_for(i),";display:flex;align-items:center;",
                              "justify-content:center;color:#fff;font-size:10px;font-weight:800;"),
                 initials(r$track_genre)),
        tags$div(style="flex:1;min-width:0;",
          tags$div(style="font-size:12px;font-weight:700;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                   r$track_name),
          tags$div(style=paste0("font-size:10px;color:",sc,";"), r$main_artist)
        ),
        tags$div(style="display:flex;align-items:center;gap:6px;flex-shrink:0;",
          tags$div(style=paste0("width:60px;height:4px;border-radius:2px;background:var(--border);",
                                "position:relative;"),
            tags$div(style=paste0("position:absolute;left:0;top:0;height:100%;border-radius:2px;",
                                  "width:",r$popularity,"%;background:",grad_for(i),";"))
          ),
          tags$div(style="font-size:11px;font-weight:800;width:24px;text-align:right;", r$popularity)
        )
      )
    }))
  })

  output$pop_artists <- renderUI({
    sc <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    tagList(lapply(seq_len(nrow(pop_top_artists)), function(i) {
      r <- pop_top_artists[i,]
      tags$div(
        style=paste0("display:flex;align-items:center;gap:14px;padding:12px 4px;",
                     "border-bottom:1px solid var(--border);"),
        tags$div(style=paste0("width:46px;height:46px;border-radius:50%;flex-shrink:0;",
                              "background:",grad_for(i+3),";display:flex;align-items:center;",
                              "justify-content:center;color:#fff;font-size:14px;font-weight:800;"),
                 initials(r$main_artist)),
        tags$div(style="flex:1;min-width:0;",
          tags$div(style="font-size:13px;font-weight:800;", r$main_artist),
          tags$div(style=paste0("font-size:10.5px;color:",sc,";white-space:nowrap;",
                                "overflow:hidden;text-overflow:ellipsis;"), r$top_track)
        ),
        tags$div(style="text-align:right;flex-shrink:0;",
          tags$div(style=paste0("font-size:14px;font-weight:800;color:",ACCENT_GRAD1,";"), r$top_pop),
          tags$div(style=paste0("font-size:9.5px;color:",sc,";text-transform:uppercase;",
                                "letter-spacing:0.04em;"), paste(r$n, "tracks"))
        )
      )
    }))
  })

  gg_pop <- function(dark) {
    theme_minimal(base_size=11) +
      theme(
        plot.background  = element_rect(fill=if(dark)"#1A1A1A" else "#FFFFFF", colour=NA),
        panel.background = element_rect(fill=if(dark)"#1A1A1A" else "#FFFFFF", colour=NA),
        panel.grid.major = element_line(colour=if(dark)"#2A2A2A" else "#EEEEEE", linewidth=0.4),
        panel.grid.minor = element_blank(),
        axis.text  = element_text(colour=if(dark)"#9A9A9A" else "#8A8DA3", size=9),
        axis.title = element_text(colour=if(dark)"#CCCCCC" else "#555", size=10, face="bold"),
        plot.margin= margin(8,8,8,8)
      )
  }

  output$pop_hit_dna <- renderPlot({
    dark <- dark_mode()
    df <- data.frame(
      Feature = feat_names_pop,
      Value   = as.numeric(pop20_means[1,])
    )
    ggplot(df, aes(x=reorder(Feature, Value), y=Value, fill=Value)) +
      geom_col(width=0.65, show.legend=FALSE) +
      geom_text(aes(label=sprintf("%.2f",Value)), hjust=-0.15, size=3.2,
                colour=if(dark)"#CCCCCC" else "#555") +
      scale_fill_gradient(low="#FFB199", high="#E11D2A") +
      scale_y_continuous(limits=c(0,1.1), expand=expansion(mult=c(0,0.05))) +
      coord_flip() +
      labs(x=NULL, y="Avg Score (0-1)") +
      gg_pop(dark)
  }, bg="transparent")

  output$pop_scatter <- renderPlot({
    dark <- dark_mode()
    feat <- input$pop_scatter_feat
    feat_lab <- feat_names_pop[feat_cols_pop == feat]
    ggplot(music, aes_string(x=feat, y="popularity")) +
      geom_point(alpha=0.45, size=1.8,
                 colour=ACCENT_GRAD1) +
      geom_smooth(method="lm", se=TRUE, colour="#E11D2A",
                  linetype="dashed", linewidth=0.9, alpha=0.12) +
      labs(x=feat_lab, y="Popularity Score") +
      gg_pop(dark)
  }, bg="transparent")

  #Category reactive inputs
  observeEvent(input$cat_mood, {}, ignoreNULL=FALSE)

  output$cat_subtitle <- renderUI({
    df <- genre_stats
    q <- input$cat_search
    mood_f <- input$cat_mood
    if (!is.null(q) && nchar(trimws(q)) >= 1)
      df <- df[grepl(paste0("^",q), df$track_genre, ignore.case=TRUE), ]
    if (!is.null(mood_f) && mood_f != "All")
      df <- df[df$mood == mood_f, ]
    n_shown <- nrow(df)
    mood_label <- if (!is.null(mood_f) && mood_f != "All") paste0(" · ",mood_f," mood") else ""
    search_label <- if (!is.null(q) && nchar(trimws(q)) >= 1) paste0(" matching \"",q,"\"") else ""
    tags$p(style="font-size:13px;opacity:0.55;margin:0 0 16px 0;",
           paste0(n_shown, " genre", if(n_shown!=1)"s" else "",
                  " · sorted by popularity", mood_label, search_label))
  })

  output$cat_genre_grid <- renderUI({
    df <- genre_stats
    # Filter by search
    q <- input$cat_search
    if (!is.null(q) && nchar(trimws(q)) >= 1)
      df <- df[grepl(paste0("^",q), df$track_genre, ignore.case=TRUE), ]
    # Filter by mood
    mood_f <- input$cat_mood
    if (!is.null(mood_f) && mood_f != "All")
      df <- df[df$mood == mood_f, ]
    # Sort by popularity by default
    df <- df[order(df[["avg_pop"]], decreasing=TRUE), ]

    if (nrow(df) == 0)
      return(tags$p(style="opacity:0.5;font-size:13px;", "No genres match your filters."))

    tags$div(
      style="display:grid;grid-template-columns:repeat(3,1fr);gap:14px;height:65vh;overflow-y:auto;padding-right:4px;",
      tagList(lapply(seq_len(nrow(df)), function(i) {
        g <- df[i,]
        mc <- mood_colors[g$mood]
        tags$div(
          style=paste0("border-radius:14px;padding:16px;border:1px solid;",
                       "border-color:var(--border);background:var(--card);",
                       "cursor:pointer;transition:transform 0.15s;"),
          onclick=sprintf("Shiny.setInputValue('cat_genre_click','%s',{priority:'event'});", g$track_genre),
          onmouseover="this.style.transform='translateY(-2px)'",
          onmouseout="this.style.transform='translateY(0)'",
          # Genre name + mood badge
          tags$div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:10px;",
            tags$div(style="font-size:13px;font-weight:800;", toupper(g$track_genre)),
            tags$span(style=paste0("font-size:9px;font-weight:700;padding:3px 8px;border-radius:20px;",
                                   "background:",mc,"22;color:",mc,";"), g$mood)
          ),
          # Stats row
          tags$div(style="display:flex;gap:14px;margin-bottom:10px;",
            tags$div(
              tags$div(style="font-size:11px;font-weight:800;", round(g$avg_pop,1)),
              tags$div(style="font-size:9px;opacity:0.5;text-transform:uppercase;", "Popularity")
            ),
            tags$div(
              tags$div(style="font-size:11px;font-weight:800;", g$n),
              tags$div(style="font-size:9px;opacity:0.5;text-transform:uppercase;", "Tracks")
            )
          ),
          # Mini audio bar
          tags$div(style="height:4px;border-radius:2px;background:#33333333;position:relative;",
            tags$div(style=paste0("position:absolute;left:0;top:0;height:100%;border-radius:2px;",
                                  "width:",round(g$avg_pop),"%;background:",
                                  grad_for(i),";"))
          )
        )
      }))
    )
  })

  output$cat_detail_panel <- renderUI({
    g <- input$cat_genre_click
    if (is.null(g) || g == "") {
      return(tags$div(
        style="border-radius:16px;padding:24px;text-align:center;opacity:0.4;font-size:13px;",
        "← Click any genre to see details"
      ))
    }
    grow   <- genre_stats[genre_stats$track_genre == g, ]
    tracks <- get_genre_top(g, n=6)
    # top_artist <- music %>% filter(track_genre==g) %>%
    #   mutate(main_artist=sapply(artists,clean_artist)) %>%
    #   count(main_artist,sort=TRUE) %>% slice(1) %>% pull(main_artist)
    # ============================================================
    # Top Artist data for selected genre
    # ============================================================
    top_artist_data <- music %>%
      filter(
        track_genre == g,
        !is.na(artists),
        artists != ""
      ) %>%
      mutate(
        main_artist = sapply(artists, clean_artist)
      ) %>%
      filter(
        !is.na(main_artist),
        main_artist != ""
      ) %>%
      group_by(main_artist) %>%
      summarise(
        track_count = dplyr::n(),
        avg_popularity = mean(popularity, na.rm = TRUE),
        top_popularity = max(popularity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(
        desc(track_count),
        desc(avg_popularity)
      ) %>%
      slice_head(n = 1)

    top_artist <- if (nrow(top_artist_data) > 0) {
      top_artist_data$main_artist[[1]]
    } else {
      "Unknown Artist"
    }

    top_artist_tracks <- if (nrow(top_artist_data) > 0) {
      top_artist_data$track_count[[1]]
    } else {
      0
    }

    top_artist_avg_pop <- if (nrow(top_artist_data) > 0) {
      round(top_artist_data$avg_popularity[[1]], 1)
    } else {
      0
    }
    mc <- mood_colors[grow$mood]

    tags$div(
      style=paste0("border-radius:16px;padding:20px;border:1px solid var(--border);",
                   "background:var(--card);height:65vh;overflow-y:auto;"),
      # Header
      tags$div(style=paste0("background:",grad_for(which(genre_stats$track_genre==g)[1]),
                             ";border-radius:12px;padding:18px;margin-bottom:16px;color:#fff;"),
        tags$div(style="font-size:20px;font-weight:800;", toupper(g)),
        tags$div(style="display:flex;gap:10px;margin-top:8px;flex-wrap:wrap;",
          tags$span(style=paste0("font-size:10px;font-weight:700;padding:3px 8px;border-radius:20px;",
                                  "background:rgba(255,255,255,0.2);"), paste(grow$n,"tracks")),
          tags$span(style=paste0("font-size:10px;font-weight:700;padding:3px 8px;border-radius:20px;",
                                  "background:",mc,"44;color:#fff;"), grow$mood),
          tags$span(style=paste0("font-size:10px;font-weight:700;padding:3px 8px;border-radius:20px;",
                                  "background:rgba(255,255,255,0.2);"), paste0("⭐ ",round(grow$avg_pop,1)))
        )
      ),
      # ============================================================
      # Top Artist
      # ============================================================
      tags$div(
        style = paste0(
          "font-size:10px;",
          "font-weight:700;",
          "opacity:0.5;",
          "text-transform:uppercase;",
          "margin-bottom:6px;"
        ),
        "Top Artist"
      ),

      tags$div(
        style = paste0(
          "font-size:13px;",
          "font-weight:700;",
          "margin-bottom:8px;"
        ),
        top_artist
      ),

      tags$div(
        style = paste0(
          "display:flex;",
          "align-items:center;",
          "gap:8px;",
          "flex-wrap:wrap;",
          "margin-bottom:14px;"
        ),

        # Track count badge
        tags$span(
          style = paste0(
            "font-size:9.5px;",
            "font-weight:700;",
            "padding:3px 8px;",
            "border-radius:20px;",
            "background:#2BB5A022;",
            "color:#2BB5A0;"
          ),
          paste0(
            top_artist_tracks,
            if (top_artist_tracks == 1) " track" else " tracks"
          )
        ),

        # Average popularity badge
        tags$span(
          style = paste0(
            "font-size:9.5px;",
            "font-weight:700;",
            "padding:3px 8px;",
            "border-radius:20px;",
            "background:#FBBF2422;",
            "color:#FBBF24;"
          ),
          paste0(
            "⭐ ",
            top_artist_avg_pop,
            " avg"
          )
        )
      ),

      # ============================================================
      # Audio DNA bars
      # ============================================================

      tags$div(
        style = "font-size:10px;font-weight:700;opacity:0.5;text-transform:uppercase;margin-bottom:10px;",
        "Audio DNA"
      ),

      tagList(
        lapply(seq_along(c(
          "Danceability",
          "Energy",
          "Valence (Mood)",
          "Acousticness",
          "Speechiness",
          "Instrumentalness",
          "Liveness"
        )), function(i) {

          audio_names <- c(
            "Danceability",
            "Energy",
            "Valence (Mood)",
            "Acousticness",
            "Speechiness",
            "Instrumentalness",
            "Liveness"
          )

          audio_cols <- c(
            "avg_dance",
            "avg_energy",
            "avg_val",
            "avg_acou",
            "avg_speech",
            "avg_instr",
            "avg_live"
          )

          fn  <- audio_names[i]
          col <- audio_cols[i]

          val <- as.numeric(grow[[col]][1])

          if (is.na(val) || !is.finite(val)) {
            val <- 0
          }

          val <- max(0, min(1, val))

          tags$div(
            style = "display:flex;align-items:center;gap:8px;margin-bottom:7px;",

            tags$div(
              style = "font-size:10.5px;width:110px;opacity:0.7;",
              fn
            ),

            tags$div(
              style = "flex:1;height:6px;border-radius:3px;background:#33333333;position:relative;",

              tags$div(
                style = paste0(
                  "position:absolute;",
                  "left:0;",
                  "top:0;",
                  "height:100%;",
                  "border-radius:3px;",
                  "width:", round(val * 100), "%;",
                  "background:", grad_for(i), ";"
                )
              )
            ),

            tags$div(
              style = "font-size:10px;font-weight:700;opacity:0.6;width:30px;text-align:right;",
              sprintf("%.2f", val)
            )
          )
        })
      ),

      tags$hr(style="opacity:0.15;margin:14px 0;"),
      
      #Top tracks
      tags$div(style="font-size:10px;font-weight:700;opacity:0.5;text-transform:uppercase;margin-bottom:10px;",
               "Top Tracks"),
      tagList(lapply(seq_len(nrow(tracks)), function(i) {
        r <- tracks[i,]
        tags$div(style="display:flex;align-items:center;gap:10px;padding:6px 0;border-bottom:1px solid var(--border);",
          tags$div(style="font-size:11px;opacity:0.4;width:14px;", i),
          tags$div(style=paste0("width:30px;height:30px;border-radius:7px;flex-shrink:0;",
                                "background:",grad_for(i+3),";display:flex;align-items:center;",
                                "justify-content:center;color:#fff;font-size:10px;font-weight:800;"),
                   initials(r$track_genre)),
          tags$div(style="flex:1;min-width:0;",
            tags$div(style="font-size:11.5px;font-weight:700;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;",
                     r$track_name),
            tags$div(style="font-size:10px;opacity:0.5;", paste0(r$main_artist," · ",r$dur))
          ),
          tags$div(style="font-size:10.5px;font-weight:700;opacity:0.6;", r$popularity)
        )
      }))
    )
  })

  # Live search (filters by track name or artist, prefix match)
  output$search_results <- renderUI({
    q <- input$search_query
    if (is.null(q) || nchar(trimws(q)) < 1) return(NULL)

    #Escape regex metacharacters one literal substitution at a time (fixed=TRUE, no regex parsing)
    special_chars <- c("\\", ".", "^", "$", "|", "?", "*", "+", "(", ")", "[", "]", "{", "}")
    q_escaped <- q
    for (ch in special_chars) {
      q_escaped <- gsub(ch, paste0("\\", ch), q_escaped, fixed = TRUE)
    }

    matches <- music %>%
      mutate(main_artist = sapply(artists, clean_artist)) %>%
      filter(grepl(paste0("^", q_escaped), track_name, ignore.case = TRUE) |
             grepl(paste0("^", q_escaped), main_artist, ignore.case = TRUE)) %>%
      arrange(desc(popularity)) %>%
      slice_head(n = 8)

    if (nrow(matches) == 0) {
      return(tags$div(class="search-results", tags$div(class="search-no-results", "No matches found.")))
    }

    tags$div(class="search-results",
      tagList(lapply(seq_len(nrow(matches)), function(i) {
        r <- matches[i,]
        tags$div(class="search-result-row",
                 onclick = sprintf("Shiny.setInputValue('search_result_click', %d, {priority:'event'})", r$ID),
          tags$div(class="search-result-cover", style=paste0("background:",grad_for(i),";"),
                   initials(r$track_genre)),
          tags$div(
            tags$div(class="search-result-name", r$track_name),
            tags$div(class="search-result-sub", paste0(r$main_artist," · ",r$track_genre))
          )
        )
      }))
    )
  })

  #Reactive Top Trending panel
  output$trend_cover_A <- renderUI({
    tags$div(class="trend-cover", style=paste0("background:",grad_for(1),";"), toupper(input$genre_select_A))
  })

  render_trend_list <- function(genre) {
    df <- get_genre_top(genre, n = 6)
    if (nrow(df) == 0) return(tags$p(style="opacity:0.5;font-size:12px;","No tracks for this genre."))
    tags$div(class="trend-list",
      tagList(lapply(seq_len(nrow(df)), function(i) {
        r <- df[i,]
        tags$div(class="trend-row", id=paste0("trend_row_", r$ID),
                 onclick = sprintf(
                   "var el=this; document.querySelectorAll('.trend-row').forEach(function(x){x.classList.remove('active');}); el.classList.add('active'); setTimeout(function(){ Shiny.setInputValue('trend_row_click', %d, {priority:'event'}); }, 1000);",
                   r$ID),
          tags$div(class="trend-rank", i),
          tags$div(class="trend-info",
            tags$div(class="trend-name", r$track_name),
            tags$div(class="trend-sub", paste0(r$main_artist," · ",r$dur))
          )
        )
      }))
    )
  }

  output$trend_list_A <- renderUI({ render_trend_list(input$genre_select_A) })

  #Genre Comparison DNA chart
  output$compare_subtitle <- renderText({
    paste0("DNA: ", input$compare_genre1, " vs ", input$compare_genre2, ", score per feature (0–1)")
  })

  output$plot_genre_compare <- renderPlot({
    g1 <- input$compare_genre1
    g2 <- input$compare_genre2
    df <- music %>% filter(track_genre %in% c(g1, g2))
    feats <- names(audio_features_map)
    vals  <- unname(audio_features_map)
    long <- do.call(rbind, lapply(seq_along(vals), function(i)
      data.frame(Feature = feats[i], Value = df[[vals[i]]], Genre = df$track_genre)
    ))
    avg <- long %>% group_by(Feature, Genre) %>%
      summarise(Mean = mean(Value, na.rm = TRUE), .groups = "drop")

    bg_col   <- if (dark_mode()) "#1A1A1A" else "#FFFFFF"
    text_col <- if (dark_mode()) "#FFFFFF" else "#15161A"
    sub_col  <- if (dark_mode()) "#9A9A9A" else "#8A8DA3"
    grid_col <- if (dark_mode()) "#2A2A2A" else "#EEEEEE"

    ggplot(avg, aes(x = Feature, y = Mean, fill = Genre)) +
      geom_col(position = "dodge", width = 0.65, alpha = 0.95) +
      scale_fill_manual(values = setNames(c("#2BB5A0", "#7C8CF8"), c(g1, g2))) +
      scale_y_continuous(limits = c(0, 1.05), expand = expansion(mult = c(0, 0.02))) +
      labs(x = NULL, y = "Score", fill = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        plot.background   = element_rect(fill = bg_col, colour = NA),
        panel.background  = element_rect(fill = bg_col, colour = NA),
        panel.grid.major.y = element_line(colour = grid_col, linewidth = 0.4),
        panel.grid.major.x = element_blank(),
        panel.grid.minor  = element_blank(),
        axis.text.x       = element_text(colour = text_col, angle = 25, hjust = 1, size = 9),
        axis.text.y       = element_text(colour = sub_col, size = 9),
        legend.position   = "top",
        legend.justification = "left",
        legend.text       = element_text(colour = text_col, size = 10),
        plot.margin       = margin(4, 8, 4, 4)
      )
  }, bg = "transparent")

  build_modal_rows <- function(df, grad_start) {
    tagList(lapply(seq_len(nrow(df)), function(i) {
      r <- df[i,]
      tags$div(class="modal-row",
        tags$div(class="modal-rank", i),
        tags$div(class="modal-cover", style=paste0("background:",grad_for(grad_start+i),";"),
                 initials(r$track_genre)),
        tags$div(class="modal-info",
          tags$div(class="modal-name", r$track_name),
          tags$div(class="modal-sub", paste0(r$main_artist," · ",r$dur))
        ),
        tags$div(class="modal-pop", paste0(r$popularity," pts"))
      )
    }))
  }

  #"View all" modal
  observeEvent(input$view_all_trending, {
    showModal(modalDialog(
      title = "Top Trending — Browse by Genre",
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      selectInput("modal_genre_select", "Choose a genre:",
                  choices = all_genres_sorted, selected = input$genre_select_A),
      uiOutput("modal_genre_results")
    ))
  })

  output$modal_genre_results <- renderUI({
    req(input$modal_genre_select)
    df <- get_genre_top(input$modal_genre_select, n = 20)
    tagList(
      tags$h4(toupper(input$modal_genre_select), style="margin-top:0;"),
      build_modal_rows(df, 0)
    )
  })

  observeEvent(input$view_all_reco, {
    showModal(modalDialog(
      title = "Recommended for you — Full List",
      size = "l", easyClose = TRUE, footer = modalButton("Close"),
      build_modal_rows(recommended_full, 20)
    ))
  })

  #Track Detail Modal
  show_track_detail <- function(track_id) {
    r <- music[music$ID == track_id, ]
    if (nrow(r) == 0) return(NULL)
    r <- r[1, ]
    r_artist <- clean_artist(r$artists)

    pop_pct  <- round(ecdf(music$popularity)(r$popularity) * 100)
    feat_names <- names(audio_features_map)
    feat_cols  <- unname(audio_features_map)

    showModal(modalDialog(
      title = NULL, size = "l", easyClose = TRUE, footer = modalButton("Close"),

      tags$div(class="td-header",
        tags$div(class="td-cover", style=paste0("background:",grad_for(round(r$ID) %% 10 + 1),";"),
                 initials(r$track_genre)),
        tags$div(
          tags$div(class="td-title", r$track_name),
          tags$div(class="td-artist", paste0(r_artist, " · ", r$album_name)),
          tags$div(class="td-meta-row",
            tags$span(class="td-chip", style="background:#2BB5A020;color:#1F8A70;", toupper(r$track_genre)),
            tags$span(class="td-chip", style="background:#7C8CF820;color:#5B6BD6;",
                      paste0(key_names[r$key + 1], " ", ifelse(r$mode == 1, "Major", "Minor"))),
            if (r$explicit) tags$span(class="td-chip", style="background:#E11D2A20;color:#E11D2A;", "EXPLICIT")
          )
        )
      ),

      tags$div(class="td-stats-grid",
        tags$div(class="td-stat",
          tags$div(class="td-stat-value", r$popularity),
          tags$div(class="td-stat-label", "Popularity")),
        tags$div(class="td-stat",
          tags$div(class="td-stat-value", paste0("Top ", 100 - pop_pct, "%")),
          tags$div(class="td-stat-label", "In Dataset")),
        tags$div(class="td-stat",
          tags$div(class="td-stat-value", fmt_dur(r$duration_ms)),
          tags$div(class="td-stat-label", "Duration")),
        tags$div(class="td-stat",
          tags$div(class="td-stat-value", round(r$tempo)),
          tags$div(class="td-stat-label", "Tempo (BPM)"))
      ),

      tags$div(class="td-section-title", "Audio Features"),
      tagList(lapply(seq_along(feat_cols), function(i) {
        val <- r[[feat_cols[i]]]
        tags$div(class="td-feature-row",
          tags$div(class="td-feature-label", feat_names[i]),
          tags$div(class="td-feature-bar-wrap",
            tags$div(class="td-feature-bar-fill",
                     style=paste0("width:", round(val*100), "%; background:", grad_for(i), ";"))
          ),
          tags$div(class="td-feature-value", sprintf("%.2f", val))
        )
      }))
    ))
  }

  observeEvent(input$search_result_click, {
    show_track_detail(input$search_result_click)
  })

  observeEvent(input$trend_row_click, {
    show_track_detail(input$trend_row_click)
  })
}
