# ============================================================
# R/tabs/popular.R
# Exact popular_page_ui code extracted from the original working app.R
# ============================================================

popular_page_ui <- function(dark) {
    text_col  <- if (dark) "#FFFFFF" else "#15161A"
    sub_col   <- if (dark) "#9A9A9A" else "#8A8DA3"
    input_bg  <- if (dark) "#1E1E1E" else "#F4F5F7"
    border    <- if (dark) "#2A2A2A" else "#ECECEF"

    tagList(
      #Page title
      tags$div(style="margin-bottom:22px;",
        tags$div(style=paste0("font-size:22px;font-weight:800;color:",text_col,";"), "Popular Music"),
        tags$div(style=paste0("font-size:13px;color:",sub_col,";margin-top:4px;"),
                 "Exploring what makes tracks rise to the top of the charts")
      ),

      #Leaderboard & Artist Spotlight
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:20px;",

        #Leaderboard
        tags$div(style=paste0("border-radius:16px;padding:20px;background:var(--card);",
                              "border:1px solid var(--border);min-width:0;"),

          tags$div(style="display:flex;align-items:center;justify-content:space-between;margin-bottom:16px;",
            tags$div(style=paste0("font-size:15px;font-weight:800;color:",text_col,";"), "Top 50 Leaderboard"),
            tags$div(style=paste0("font-size:11px;color:",sub_col,";"), "By popularity score")
          ),
          tags$div(style="height:62vh;overflow-y:auto;",
            uiOutput("pop_leaderboard")
          )
        ),

        #Artist Spotlight
        tags$div(style=paste0("border-radius:16px;padding:20px;background:var(--card);",
                              "border:1px solid var(--border);min-width:0;"),
          tags$div(style=paste0("font-size:15px;font-weight:800;color:",text_col,";margin-bottom:16px;"),
                   "Artist Spotlight"),
          tags$div(style="height:62vh;overflow-y:auto;",
            uiOutput("pop_artists")
          )
        )
      ),

      #Bottom layout
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;",

        #avg features of top 20
        tags$div(style=paste0("border-radius:16px;padding:20px;background:var(--card);",
                              "border:1px solid var(--border);min-width:0;"),
          tags$div(style=paste0("font-size:15px;font-weight:800;color:",text_col,";margin-bottom:4px;"),
                   "Popular Tracks"),
          tags$div(style=paste0("font-size:11px;color:",sub_col,";margin-bottom:16px;"),
                   "Average audio features of the Top 20 most popular tracks"),
          plotOutput("pop_hit_dna", height="280px")
        ),

        #popularity vs selected feature
        tags$div(style=paste0("border-radius:16px;padding:20px;background:var(--card);",
                              "border:1px solid var(--border);min-width:0;"),
          tags$div(style="display:flex;align-items:flex-start;justify-content:space-between;margin-bottom:16px;",
            tags$div(style=paste0("font-size:15px;font-weight:800;color:",text_col,";line-height:34px;"),
                     "Popularity Explorer"),
            tags$div(class="genre-select-wrap", style="width:170px;",
              selectInput("pop_scatter_feat", NULL,
                          choices=setNames(feat_cols_pop, feat_names_pop),
                          selected="danceability"))
          ),
          plotOutput("pop_scatter", height="280px")
        )
      )
    )
  }
