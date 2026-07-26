# ============================================================
# R/tabs/statistics.R
# Exact statistics_page_ui code extracted from the original working app.R
# ============================================================

statistics_page_ui <- function(dark) {
    tc  <- if (dark) "#FFFFFF" else "#15161A"
    sc  <- if (dark) "#9A9A9A" else "#8A8DA3"
    bg  <- if (dark) "#1E1E1E" else "#F4F5F7"
    bdr <- if (dark) "#2A2A2A" else "#ECECEF"
    card <- paste0("border-radius:16px;padding:20px;background:var(--card);",
                   "border:1px solid var(--border);min-width:0;")
    th <- function(t) tags$div(style=paste0("font-size:15px;font-weight:800;color:",tc,";margin-bottom:4px;"), t)
    ts <- function(t) tags$div(style=paste0("font-size:11px;color:",sc,";margin-bottom:14px;"), t)

    tagList(
      tags$div(style=paste0("font-size:22px;font-weight:800;color:",tc,";margin-bottom:4px;"), "Statistics"),
      tags$div(style=paste0("font-size:13px;color:",sc,";margin-bottom:22px;"),
               "Deep statistical analysis of your 700-track Spotify dataset"),

      #Dataset Facts
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:20px;align-items:stretch;",

        #Dataset Summary Cards
        tags$div(style=card,
          th("Dataset Report Card"),
          ts("Extremes and fun facts from across the full dataset"),
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px;overflow-y:auto;",
            lapply(seq_along(stat_dataset_facts), function(i) {
              f <- stat_dataset_facts[[i]]
              col <- grad_colors[(i-1) %% length(grad_colors) + 1]
              tags$div(style=paste0("border-radius:10px;padding:12px 14px;background:",col,"18;",
                                   "border:1px solid ",col,"33;min-width:0;"),
                tags$div(style=paste0("font-size:9.5px;font-weight:700;color:",col,";",
                                     "text-transform:uppercase;letter-spacing:0.04em;margin-bottom:4px;"),
                         f[[1]]),
                tags$div(style=paste0("font-size:12px;font-weight:800;color:",tc,";",
                                     "white-space:nowrap;overflow:hidden;text-overflow:ellipsis;"),
                         f[[2]]),
                tags$div(style=paste0("font-size:10.5px;color:",sc,";margin-top:2px;"), f[[3]])
              )
            })
          )
        ),

        #Key & Mode Analysis
        tags$div(style=paste0(card,"display:flex;flex-direction:column;"),
          th("Key & Mode Analysis"),
          ts("Musical key distribution and major vs minor popularity"),
          plotOutput("stat_key_plot", height="240px"),
          tags$div(style="display:flex;gap:12px;margin-top:auto;padding-top:12px;",
            tags$div(style=paste0("flex:1;border-radius:10px;padding:12px;background:#2BB5A022;",
                                  "border:1px solid #2BB5A044;text-align:center;"),
              tags$div(style="font-size:18px;font-weight:800;color:#2BB5A0;",
                       round(mean(music$popularity[music$mode==1], na.rm=TRUE), 1)),
              tags$div(style=paste0("font-size:10px;color:",sc,";text-transform:uppercase;"),
                       "Major Avg Pop"),
              tags$div(style=paste0("font-size:10px;color:",sc,";"),
                       paste0(sum(music$mode==1)," tracks"))
            ),
            tags$div(style=paste0("flex:1;border-radius:10px;padding:12px;background:#7C8CF822;",
                                  "border:1px solid #7C8CF844;text-align:center;"),
              tags$div(style="font-size:18px;font-weight:800;color:#7C8CF8;",
                       round(mean(music$popularity[music$mode==0], na.rm=TRUE), 1)),
              tags$div(style=paste0("font-size:10px;color:",sc,";text-transform:uppercase;"),
                       "Minor Avg Pop"),
              tags$div(style=paste0("font-size:10px;color:",sc,";"),
                       paste0(sum(music$mode==0)," tracks"))
            )
          )
        )
      ),

      #Correlation Heatmap & Distribution Explorer
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:20px;",

        #Correlation Heatmap
        tags$div(style=card,
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:4px;",
            th("Correlation Heatmap"),
            tags$div(class="genre-select-wrap", style="width:160px;",
              selectInput("stat_corr_focus", NULL,
                choices=c("All Features"="all", setNames(stat_all_feats,
                  c("Danceability","Energy","Valence","Acousticness",
                    "Speechiness","Instrumentalness","Liveness","Tempo","Popularity"))),
                selected="all"))
          ),
          ts("Correlation between audio features and popularity (r value)"),
          plotOutput("stat_corr_plot", height="300px")
        ),

        #Distribution Explorer
        tags$div(style=card,
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:4px;",
            th("Distribution Explorer"),
            tags$div(class="genre-select-wrap", style="width:160px;",
              selectInput("stat_dist_feat", NULL,
                          choices=setNames(stat_feats, stat_feat_names),
                          selected="danceability"))
          ),
          ts("How tracks are distributed across each audio feature"),
          plotOutput("stat_dist_plot", height="280px")
        )
      ),

      #Outlier Finder + Tempo Analysis
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:20px;align-items:stretch;",

        #Outlier Finder
        tags$div(style=paste0(card,"display:flex;flex-direction:column;"),
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:4px;",
            th("Outlier Finder"),
            tags$div(class="genre-select-wrap", style="width:160px;",
              selectInput("stat_outlier_feat", NULL,
                          choices=setNames(stat_feats, stat_feat_names),
                          selected="instrumentalness"))
          ),
          ts("Most extreme tracks for any audio feature"),
          tags$div(style="flex:1;overflow-y:auto;",
            uiOutput("stat_outliers")
          )
        ),

        #Tempo Analysis
        tags$div(style=paste0(card,"display:flex;flex-direction:column;"),
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:4px;",
            th("Tempo Analysis"),
            tags$div(class="genre-select-wrap", style="width:160px;",
              selectInput("stat_tempo_genre", NULL,
                choices=c("All Genres"="all", setNames(sort(unique(music$track_genre)),
                                                       sort(unique(music$track_genre)))),
                selected="all"))
          ),
          ts("BPM distribution: select a genre to filter, red line = mean"),
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-bottom:12px;",
            tags$div(style=paste0("border-radius:12px;padding:12px;background:#E11D2A18;",
                                  "border:1px solid #E11D2A33;"),
              tags$div(style=paste0("font-size:9.5px;font-weight:700;color:#E11D2A;",
                                   "text-transform:uppercase;margin-bottom:4px;"), "Fastest Genre"),
              uiOutput("stat_fastest_genre")
            ),
            tags$div(style=paste0("border-radius:12px;padding:12px;background:#7C8CF818;",
                                  "border:1px solid #7C8CF833;"),
              tags$div(style=paste0("font-size:9.5px;font-weight:700;color:#7C8CF8;",
                                   "text-transform:uppercase;margin-bottom:4px;"), "Slowest Genre"),
              uiOutput("stat_slowest_genre")
            )
          ),
          tags$div(style="flex:1;min-height:280px;",
            plotOutput("stat_tempo_plot", height="100%", width="100%")
          )
        )
      ),

      #Explicit & Genre Stats Table
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;align-items:stretch;",

        #Explicit vs Clean 
        tags$div(style=paste0(card,"display:flex;flex-direction:column;"),
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:4px;",
            th("Explicit vs Clean"),
            tags$div(class="genre-select-wrap", style="width:150px;",
              selectInput("stat_explicit_filter", NULL,
                choices=c("Both"="both","Clean Only"="clean","Explicit Only"="explicit"),
                selected="both"))
          ),
          ts("Explicit and clean tracks differ across audio features"),
          plotOutput("stat_explicit_plot", height="340px")
        ),

        #Genre Statistics Table
        tags$div(style=card,
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:4px;",
            th("Genre Statistics Table"),
            tags$div(class="genre-select-wrap", style="width:160px;",
              selectInput("stat_table_sort", NULL,
                choices=c("Avg Popularity"="avg_pop","Tracks"="tracks",
                          "Avg BPM"="avg_bpm","Avg Dance"="avg_dance",
                          "Avg Energy"="avg_energy"),
                selected="avg_pop"))
          ),
          ts("All genres with full statistical breakdown"),
          tags$div(style="height:340px;overflow-y:auto;",
            uiOutput("stat_genre_table")
          )
        )
      )
    )
  }
