# ============================================================
# R/tabs/playlist.R
# Exact playlist_page_ui code extracted from the original working app.R
# ============================================================

playlist_page_ui <- function(dark) {
    tc  <- if (dark) "#FFFFFF" else "#15161A"
    sc  <- if (dark) "#9A9A9A" else "#8A8DA3"
    bg  <- if (dark) "#1E1E1E" else "#F4F5F7"
    bdr <- if (dark) "#2A2A2A" else "#ECECEF"
    search_svg <- HTML('<svg width="14" height="14" viewBox="0 0 24 24" fill="none"><path d="M10.77 18.3C9.28 18.3 7.82 17.86 6.59 17.03C5.35 16.2 4.38 15.03 3.81 13.65C3.24 12.28 3.09 10.76 3.38 9.3C3.68 7.84 4.39 6.5 5.45 5.45C6.5 4.39 7.84 3.68 9.3 3.38C10.76 3.09 12.28 3.24 13.65 3.81C15.03 4.38 16.2 5.35 17.03 6.59C17.86 7.82 18.3 9.28 18.3 10.77C18.3 12.76 17.51 14.67 16.09 16.09C14.67 17.51 12.76 18.3 10.77 18.3ZM20 20.75C19.81 20.75 19.62 20.68 19.47 20.53L15.34 16.4C15.07 16.12 15.07 15.68 15.34 15.4C15.61 15.12 16.05 15.12 16.33 15.4L20.47 19.53C20.74 19.81 20.74 20.25 20.47 20.53C20.32 20.68 20.13 20.75 20 20.75Z" fill="currentColor"/></svg>')

    card_style <- paste0("border-radius:16px;padding:20px;background:var(--card);",
                         "border:1px solid var(--border);min-width:0;")
    sec_title  <- function(t) tags$div(style=paste0("font-size:15px;font-weight:800;color:",tc,
                                                     ";margin-bottom:6px;"), t)
    sec_sub    <- function(t) tags$div(style=paste0("font-size:11px;color:",sc,
                                                     ";margin-bottom:16px;"), t)

    tagList(
      tags$div(style=paste0("font-size:22px;font-weight:800;color:",tc,";margin-bottom:4px;"),
               "Playlist"),
      tags$div(style=paste0("font-size:13px;color:",sc,";margin-bottom:22px;"),
               "Build, explore and compare playlists from your dataset"),

      #Playlist Creator
      tags$div(style=card_style,
        tags$div(style="display:grid;grid-template-columns:1fr 340px;gap:20px;",

          #Search + tracklist
          tags$div(
            sec_title("Your Playlist"),
            sec_sub("Search and add tracks to build your own playlist."),
            #Search box
            tags$div(style=paste0("display:flex;align-items:center;gap:8px;padding:9px 14px;",
                                  "border-radius:10px;background:",bg,";border:1px solid ",bdr,";",
                                  "margin-bottom:12px;"),
              search_svg,
              tags$input(id="pl_creator_search", type="text",
                         placeholder="Search tracks to add...",
                         oninput="Shiny.setInputValue(\'pl_creator_search\', this.value)",
                         style=paste0("border:none;outline:none;background:transparent;",
                                      "font-size:12.5px;color:",tc,";flex:1;width:100%;"))
            ),
            tags$div(style="height:120px;overflow-y:auto;border-bottom:1px solid var(--border);margin-bottom:8px;",
              uiOutput("pl_search_results_creator")
            ),
            tags$div(style="height:150px;overflow-y:auto;",
              uiOutput("pl_creator_list")
            )
          ),

          #Stats + Download
          tags$div(
            sec_title("Playlist Stats"),
            uiOutput("pl_creator_stats"),
            tags$br(),
            downloadButton("pl_download_csv",
              label="Download Playlist as CSV",
              style=paste0("width:100%;padding:11px;border-radius:10px;font-size:13px;",
                           "font-weight:700;background:linear-gradient(135deg,#2BB5A0,#1F8A70);",
                           "color:#fff;border:none;cursor:pointer;margin-top:8px;",
                           "text-decoration:none;display:block;text-align:center;")),
            tags$button(
              onclick="Shiny.setInputValue(\'pl_clear_click\', Math.random());",
              style=paste0("width:100%;padding:11px;border-radius:10px;font-size:13px;",
                           "font-weight:700;background:transparent;color:#E11D2A;",
                           "border:1.5px solid #E11D2A;cursor:pointer;margin-top:8px;"),
              "✕ Clear Playlist"
            )
          )
        )
      ),

      tags$div(style="height:20px;"),

      #Audio Journey & Playlist Builder
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:20px;",

        #Audio Journey
        tags$div(style=card_style,
          sec_title("Audio Journey"),
          sec_sub("Tracks arranged slow → fast by BPM. Filter by energy level."),
          tags$div(style="display:flex;gap:8px;margin-bottom:14px;",
            lapply(c("All","Low","Medium","High"), function(e) {
              col <- c("All"="#2BB5A0","Low"="#7C8CF8","Medium"="#FBBF24","High"="#E11D2A")[e]
              tags$div(id=paste0("journey_tab_",e),
                style=paste0("padding:5px 12px;border-radius:20px;font-size:11px;font-weight:700;",
                             "cursor:pointer;border:1.5px solid ",col,";color:",col,";"),
                onclick=paste0("Shiny.setInputValue('journey_energy','",e,"',{priority:'event'});",
                               "document.querySelectorAll('[id^=journey_tab_]').forEach(function(x){x.style.background='transparent';});",
                               "this.style.background='",col,"22';"),
                e)
            })
          ),
          tags$div(style="height:55vh;overflow-y:auto;",
            uiOutput("pl_journey")
          )
        ),

        #Playlist Builder
        tags$div(style=card_style,
          sec_title("Playlist Builder"),
          sec_sub("Pick your vibe and target duration, we build the tracklist."),
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:12px;margin-bottom:16px;",
            tags$div(class="genre-select-wrap", style="width:100%;min-width:0;",
              selectInput("pl_mood", "Mood",
                choices=c("High Energy","Happy","Chill","Dark"), selected="Chill")),
            tags$div(class="genre-select-wrap", style="width:100%;min-width:0;",
              selectInput("pl_duration", "Target Duration",
                choices=c("15 min"=15,"30 min"=30,"45 min"=45,"60 min"=60),
                selected=30))
          ),
          uiOutput("pl_builder_stats"),
          tags$div(style="height:45vh;overflow-y:auto;",
            uiOutput("pl_builder_tracks")
          )
        )
      ),

      #Compatibility Matrix & Track vs Track
      tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:20px;margin-bottom:20px;",

        #Compatibility Matrix
        tags$div(style=card_style,
          sec_title("Genre Compatibility Matrix"),
          sec_sub("How similar genres are by audio features: darker = better match."),
          tags$div(style="display:flex;align-items:center;gap:10px;margin-bottom:12px;",
            tags$div(style=paste0("font-size:11px;color:",sc,";"), "Feature:"),
            tags$div(class="genre-select-wrap", style="width:160px;",
              selectInput("pl_compat_feat", NULL,
                choices=c("Danceability"="dance","Energy"="energy",
                          "Valence"="val","Acousticness"="acou"),
                selected="dance"))
          ),
          plotOutput("pl_compat_plot", height="320px")
        ),

        #Track vs Track
        tags$div(style=card_style,
          sec_title("Track vs Track"),
          sec_sub("Comparison of any two tracks in the dataset."),
          tags$div(style="display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-bottom:16px;",
            tags$div(class="genre-select-wrap tvt-select", style="width:100%;min-width:0;",
              selectInput("pl_track_a", "Track A",
                choices=track_choices, selected=track_choices[1])),
            tags$div(class="genre-select-wrap tvt-select", style="width:100%;min-width:0;",
              selectInput("pl_track_b", "Track B",
                choices=track_choices, selected=track_choices[2]))
          ),
          uiOutput("pl_versus")
        )
      ),

    )
  }
