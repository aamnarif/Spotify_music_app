# ============================================================
# DSA8045 – Applied Analytics | Assignment 1
# Group 3 – Spotify Music Analytics Dashboard
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# ── Data ─────────────────────────────────────────────────────
music <- read.csv("Group3_music.csv", stringsAsFactors = FALSE)
music$duration_min   <- round(music$duration_ms / 60000, 2)
music$mode_label     <- ifelse(music$mode == 1, "Major", "Minor")
music$explicit_label <- ifelse(music$explicit, "Explicit", "Clean")
key_labels <- c("C","C#/Db","D","D#/Eb","E","F","F#/Gb","G","G#/Ab","A","A#/Bb","B")
music$key_name <- key_labels[music$key + 1]
genre_counts  <- table(music$track_genre)
valid_genres  <- names(genre_counts[genre_counts >= 5])
music_valid   <- music[music$track_genre %in% valid_genres, ]

audio_features <- c(
  "Danceability"     = "danceability",
  "Energy"           = "energy",
  "Valence (Mood)"   = "valence",
  "Acousticness"     = "acousticness",
  "Speechiness"      = "speechiness",
  "Instrumentalness" = "instrumentalness",
  "Liveness"         = "liveness"
)

# ── Theme colours ─────────────────────────────────────────────
make_theme <- function(dark = TRUE) {
  if (dark) list(
    plot_bg   = "#1A1A1A", grid = "#2A2A2A",
    text      = "#FFFFFF", text2 = "#AAAAAA", axis_text = "#AAAAAA",
    surface2  = "#242424"
  ) else list(
    plot_bg   = "#FFFFFF", grid = "#EEEEEE",
    text      = "#0F0F0F", text2 = "#555555", axis_text = "#555555",
    surface2  = "#F0F0F0"
  )
}

# ── ggplot2 base theme — extra args passed in, no chaining needed
gg_theme <- function(t,
                     axis_x_angle = 0,
                     axis_x_hjust = 0.5,
                     axis_x_size  = 9,
                     axis_y_size  = 9,
                     no_grid      = FALSE,
                     legend_key_h = NULL,
                     hide_axes    = FALSE,
                     left_margin  = 16) {
  ax_text  <- if (hide_axes) element_blank() else element_text(colour = t$axis_text, size = 9)
  ax_textx <- if (hide_axes) element_blank() else element_text(colour = t$axis_text,
                                                               size = axis_x_size,
                                                               angle = axis_x_angle,
                                                               hjust = axis_x_hjust)
  ax_texty <- if (hide_axes) element_blank() else element_text(colour = t$axis_text, size = axis_y_size)
  ax_title <- if (hide_axes) element_blank() else element_text(colour = t$text2, face = "bold", size = 10)
  ax_ticks <- if (hide_axes) element_blank() else element_line()
  base <- theme_minimal(base_size = 12) +
    theme(
      plot.background   = element_rect(fill = t$plot_bg, colour = NA),
      panel.background  = element_rect(fill = t$plot_bg, colour = NA),
      panel.grid.major  = if (no_grid) element_blank()
                          else element_line(colour = t$grid, linewidth = 0.4),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(colour = t$text,  face = "bold", size = 14),
      plot.subtitle     = element_text(colour = t$text2, size = 10),
      axis.title        = ax_title,
      axis.text         = ax_text,
      axis.text.x       = ax_textx,
      axis.text.y       = ax_texty,
      axis.ticks        = ax_ticks,
      legend.background = element_rect(fill = t$plot_bg, colour = NA),
      legend.text       = element_text(colour = t$text2, size = 9),
      legend.title      = element_text(colour = t$text,  face = "bold", size = 9),
      plot.margin       = margin(16, 16, 16, left_margin),
      strip.text        = element_text(colour = t$text, face = "bold")
    )
  if (!is.null(legend_key_h))
    base <- base + theme(legend.key.height = unit(legend_key_h, "cm"))
  base
}

# ── CSS blocks ────────────────────────────────────────────────
css_base <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
  * { font-family:'Inter',sans-serif !important; }
  .content-wrapper { padding:16px !important; }
  .row { margin-left:-8px !important; margin-right:-8px !important; }
  [class*='col-'] { padding-left:8px !important; padding-right:8px !important; }
  .box { margin-bottom:16px !important; }
  .kpi-card { border-radius:14px; padding:20px 24px; margin-bottom:16px;
    position:relative; overflow:hidden; }
  .kpi-card::before { content:''; position:absolute; top:0; left:0;
    width:4px; height:100%; background:var(--accent); border-radius:4px 0 0 4px; }
  .kpi-value { font-size:30px; font-weight:800; line-height:1.1; }
  .kpi-label { font-size:11px; margin-top:4px; font-weight:600;
    text-transform:uppercase; letter-spacing:0.5px; }
  .section-header { font-size:22px; font-weight:800; margin:8px 0 4px 0; letter-spacing:-0.3px; }
  .section-sub    { font-size:13px; margin-bottom:16px; }
  .insight-card   { border-radius:10px; padding:14px 18px; margin-bottom:12px;
    border-left:3px solid #FF0000; }
  .insight-card h5 { font-weight:700; margin:0 0 6px 0; font-size:13px; }
  .insight-card p  { font-size:12px; margin:0; line-height:1.6; }
  .btn-dl { border:none !important; border-radius:8px !important;
    font-weight:600 !important; font-size:12px !important;
    width:100%; padding:8px; margin-top:6px; }
  .mode-row { display:flex; align-items:center; justify-content:flex-end; padding:8px 20px 0 0; }
  .toggle-btn { border-radius:20px; padding:6px 18px; cursor:pointer;
    font-size:12px; font-weight:600; border:none; }
"

css_dark <- paste0(css_base, "
  body,.wrapper                         { background:#0F0F0F !important; }
  .main-header .navbar,.main-header .logo { background:#0F0F0F !important; border-bottom:1px solid #2A2A2A !important; }
  .main-sidebar,.left-side              { background:#111111 !important; }
  .sidebar-menu>li>a                    { color:#AAAAAA !important; font-weight:500; font-size:13px; }
  .sidebar-menu>li.active>a,.sidebar-menu>li>a:hover
                                        { color:#FFFFFF !important; background:#1E1E1E !important; border-left:3px solid #FF0000 !important; }
  .content-wrapper                      { background:#0F0F0F !important; }
  .box                                  { background:#1A1A1A !important; border:1px solid #2A2A2A !important; border-radius:12px !important; box-shadow:0 4px 20px rgba(0,0,0,0.4) !important; }
  .box-header                           { background:#1E1E1E !important; border-bottom:1px solid #2A2A2A !important; border-radius:12px 12px 0 0 !important; }
  .box-title                            { color:#FFFFFF !important; font-weight:700 !important; font-size:14px !important; }
  .form-control,.selectize-input        { background:#242424 !important; border:1px solid #333 !important; color:#FFFFFF !important; border-radius:8px !important; }
  .selectize-dropdown                   { background:#242424 !important; border:1px solid #333 !important; color:#FFF !important; }
  .selectize-dropdown .option:hover     { background:#FF0000 !important; }
  .irs-bar,.irs-bar-edge                { background:#FF0000 !important; border-color:#FF0000 !important; }
  .irs-handle                           { background:#FF0000 !important; border-color:#FF0000 !important; }
  .irs-single                           { background:#FF0000 !important; }
  .irs-line                             { background:#333 !important; }
  .irs-grid-text,.irs-min,.irs-max      { color:#AAA !important; }
  .irs-min,.irs-max                     { background:#242424 !important; }
  label                                 { color:#AAAAAA !important; font-size:12px !important; font-weight:600 !important; }
  .checkbox label                       { color:#CCCCCC !important; }
  input[type=checkbox]                  { accent-color:#FF0000; }
  hr                                    { border-color:#2A2A2A !important; }
  table                                 { color:#CCCCCC !important; }
  table thead                           { background:#242424 !important; color:#FFF !important; }
  table tbody tr:nth-child(even)        { background:#1E1E1E !important; }
  table tbody tr:nth-child(odd)         { background:#1A1A1A !important; }
  table tbody tr:hover                  { background:#2A2A2A !important; }
  .kpi-card   { background:linear-gradient(135deg,#1E1E1E,#242424); border:1px solid #2A2A2A; }
  .kpi-value  { color:#FFFFFF; }
  .kpi-label  { color:#888; }
  .section-header { color:#FFFFFF; }
  .section-sub    { color:#888; }
  .insight-card   { background:#1E1E1E; border-color:#2A2A2A; }
  .insight-card h5 { color:#FFFFFF; }
  .insight-card p  { color:#AAA; }
  .btn-dl     { background:#FF0000 !important; color:#FFF !important; }
  .btn-dl:hover { background:#CC0000 !important; }
  .toggle-btn { background:#1E1E1E; color:#AAA; border:1px solid #333 !important; }
  .logo-text  { color:#FFFFFF; font-size:16px; font-weight:800; }
  .logo-red   { color:#FF0000; }
")

css_light <- paste0(css_base, "
  body,.wrapper                         { background:#F4F4F4 !important; }
  .main-header .navbar,.main-header .logo { background:#FFFFFF !important; border-bottom:1px solid #E0E0E0 !important; }
  .main-sidebar,.left-side              { background:#FFFFFF !important; border-right:1px solid #E8E8E8 !important; }
  .sidebar-menu>li>a                    { color:#555555 !important; font-weight:500; font-size:13px; }
  .sidebar-menu>li.active>a,.sidebar-menu>li>a:hover
                                        { color:#CC0000 !important; background:#FFF5F5 !important; border-left:3px solid #CC0000 !important; }
  .content-wrapper                      { background:#F4F4F4 !important; }
  .box                                  { background:#FFFFFF !important; border:1px solid #E8E8E8 !important; border-radius:12px !important; box-shadow:0 2px 12px rgba(0,0,0,0.06) !important; }
  .box-header                           { background:#FAFAFA !important; border-bottom:1px solid #EEEEEE !important; border-radius:12px 12px 0 0 !important; }
  .box-title                            { color:#0F0F0F !important; font-weight:700 !important; font-size:14px !important; }
  .form-control,.selectize-input        { background:#F5F5F5 !important; border:1px solid #DDD !important; color:#0F0F0F !important; border-radius:8px !important; }
  .selectize-dropdown                   { background:#FFFFFF !important; border:1px solid #DDD !important; color:#0F0F0F !important; }
  .selectize-dropdown .option:hover     { background:#CC0000 !important; color:#FFF !important; }
  .irs-bar,.irs-bar-edge                { background:#CC0000 !important; border-color:#CC0000 !important; }
  .irs-handle                           { background:#CC0000 !important; border-color:#CC0000 !important; }
  .irs-single                           { background:#CC0000 !important; }
  .irs-line                             { background:#DDD !important; }
  .irs-grid-text,.irs-min,.irs-max      { color:#888 !important; }
  .irs-min,.irs-max                     { background:#EEE !important; }
  label                                 { color:#555555 !important; font-size:12px !important; font-weight:600 !important; }
  .checkbox label                       { color:#333333 !important; }
  input[type=checkbox]                  { accent-color:#CC0000; }
  hr                                    { border-color:#EEEEEE !important; }
  table                                 { color:#333 !important; }
  table thead                           { background:#F5F5F5 !important; color:#0F0F0F !important; }
  table tbody tr:hover                  { background:#FFF5F5 !important; }
  .kpi-card   { background:linear-gradient(135deg,#FFFFFF,#F8F8F8); border:1px solid #E8E8E8; box-shadow:0 2px 12px rgba(0,0,0,0.06); }
  .kpi-value  { color:#0F0F0F; }
  .kpi-label  { color:#888; }
  .section-header { color:#0F0F0F; }
  .section-sub    { color:#888; }
  .insight-card   { background:#FFFFFF; border-color:#EEEEEE; box-shadow:0 1px 6px rgba(0,0,0,0.04); }
  .insight-card h5 { color:#0F0F0F; }
  .insight-card p  { color:#666; }
  .btn-dl     { background:#CC0000 !important; color:#FFF !important; }
  .btn-dl:hover { background:#AA0000 !important; }
  .toggle-btn { background:#F5F5F5; color:#555; border:1px solid #DDD !important; }
  .logo-text  { color:#0F0F0F; font-size:16px; font-weight:800; }
  .logo-red   { color:#CC0000; }
")

# ── JS to swap theme stylesheet ───────────────────────────────
theme_js <- sprintf("
  var DARK_CSS  = %s;
  var LIGHT_CSS = %s;
  Shiny.addCustomMessageHandler('switch_theme', function(dark) {
    document.getElementById('dyn_css').innerHTML = dark ? DARK_CSS : LIGHT_CSS;
  });
",
  paste0('"', gsub('"', '\\\\"', gsub('\n', ' ', css_dark)), '"'),
  paste0('"', gsub('"', '\\\\"', gsub('\n', ' ', css_light)), '"')
)

# ── KPI card ──────────────────────────────────────────────────
kpi <- function(value, label, accent) {
  tags$div(class="kpi-card", style=paste0("--accent:",accent,";"),
    tags$div(class="kpi-value", value),
    tags$div(class="kpi-label", label)
  )
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════
ui <- dashboardPage(
  skin = "black",

  dashboardHeader(
    title = tags$span(class="logo-text",
      tags$span(class="logo-red", "♫ "), "Solace"
    ),
    titleWidth = 240,
    tags$li(class="dropdown",
      tags$div(style="display:flex; align-items:center; height:50px; padding-right:16px;",
        actionButton("toggle_mode", "☀ Light Mode", class="toggle-btn")
      )
    )
  ),

  dashboardSidebar(
    width = 210,
    sidebarMenu(id="tabs",
      menuItem("🏠  Home",           tabName="overview"),
      menuItem("🎛  Audio Explorer", tabName="audio"),
      menuItem("⭐  Popularity",     tabName="popularity"),
      menuItem("🔍  What's Missing", tabName="missing"),
      menuItem("📊  Correlations",   tabName="heatmap"),
      menuItem("💡  Conclusions",    tabName="conclusions")
    ),
    tags$hr(),
    tags$div(style="padding:0 14px;",
      tags$p(style="color:#666;font-size:10px;font-weight:600;text-transform:uppercase;letter-spacing:1px;","Dataset"),
      tags$p(style="color:#AAA;font-size:11px;line-height:1.6;","700 tracks · 114 genres · 20 variables")
    )
  ),

  dashboardBody(
    # Static base CSS + dark default
    tags$head(
      tags$style(id="dyn_css", HTML(css_dark)),
      tags$script(HTML(theme_js))
    ),

    tabItems(

      # ══════════════════════════════════════════════════
      # TAB 1 – HOME
      # ══════════════════════════════════════════════════
      tabItem(tabName="overview",
        fluidRow(column(12,
          tags$div(class="section-header","Spotify Music Dashboard"),
          tags$div(class="section-sub","Exploring 700 tracks across 114 genres — the data behind the music")
        )),
        fluidRow(
          column(3, uiOutput("kpi_tracks")),
          column(3, uiOutput("kpi_genres")),
          column(3, uiOutput("kpi_popularity")),
          column(3, uiOutput("kpi_danceability"))
        ),
        fluidRow(
          box(width=3, title="🎚 Controls", solidHeader=TRUE, status="danger",
            sliderInput("overview_n","Top N Genres:", min=5, max=30, value=15, step=1),
            checkboxInput("overview_explicit","Include Explicit Tracks", value=TRUE),
            tags$hr(),
            downloadButton("dl_overview","⬇ Download Data", class="btn-dl")
          ),
          box(width=9, title="🎵 Track Count by Genre", solidHeader=TRUE, status="danger",
            plotOutput("plot_genre_bar", height="360px"))
        ),
        fluidRow(
          box(width=4, title="🔞 Explicit vs Clean", solidHeader=TRUE, status="warning",
            plotOutput("plot_explicit_pie", height="280px")),
          box(width=8, title="🥁 Tempo Distribution", solidHeader=TRUE, status="warning",
            plotOutput("plot_tempo_hist", height="280px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 2 – AUDIO EXPLORER
      # ══════════════════════════════════════════════════
      tabItem(tabName="audio",
        fluidRow(column(12,
          tags$div(class="section-header","Audio Feature Explorer"),
          tags$div(class="section-sub","Discover how Spotify measures the sound and feel of every track")
        )),
        fluidRow(
          box(width=3, title="🎚 Scatter Controls", solidHeader=TRUE, status="warning",
            selectInput("audio_feature_x","X-Axis:", choices=audio_features, selected="danceability"),
            selectInput("audio_feature_y","Y-Axis:", choices=audio_features, selected="energy"),
            selectInput("audio_genre_filter","Genre Filter:",
                        choices=c("All Genres", sort(valid_genres)), selected="All Genres"),
            tags$hr(),
            downloadButton("dl_audio","⬇ Download Data", class="btn-dl")
          ),
          box(width=9, title="🔵 Feature Scatter Plot", solidHeader=TRUE, status="warning",
            plotOutput("plot_scatter", height="350px"))
        ),
        fluidRow(
          box(width=3, title="🎚 Boxplot Controls", solidHeader=TRUE, status="primary",
            selectInput("boxplot_feature","Feature:", choices=audio_features, selected="valence"),
            sliderInput("boxplot_n_genres","No. of Genres:", min=5, max=20, value=10)
          ),
          box(width=9, title="📦 Feature Distribution by Genre", solidHeader=TRUE, status="primary",
            plotOutput("plot_boxplot", height="400px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 3 – POPULARITY
      # ══════════════════════════════════════════════════
      tabItem(tabName="popularity",
        fluidRow(column(12,
          tags$div(class="section-header","Popularity Analysis"),
          tags$div(class="section-sub","What makes a track blow up on Spotify?")
        )),
        fluidRow(
          box(width=3, title="🎚 Controls", solidHeader=TRUE, status="danger",
            sliderInput("pop_genre_n","Top N Genres:", min=5, max=25, value=15),
            checkboxGroupInput("pop_explicit_filter","Include:",
                               choices=c("Explicit","Clean"), selected=c("Explicit","Clean")),
            tags$hr(),
            downloadButton("dl_popularity","⬇ Download Data", class="btn-dl")
          ),
          box(width=9, title="🏆 Average Popularity by Genre", solidHeader=TRUE, status="danger",
            plotOutput("plot_pop_genre", height="380px"))
        ),
        fluidRow(
          box(width=3, title="🎚 Feature Controls", solidHeader=TRUE, status="primary",
            selectInput("pop_feature","Feature vs Popularity:", choices=audio_features, selected="danceability"),
            sliderInput("pop_top_n","Top N Tracks in Table:", min=5, max=50, value=10, step=5)
          ),
          box(width=5, title="📈 Feature vs Popularity", solidHeader=TRUE, status="primary",
            plotOutput("plot_pop_scatter", height="320px")),
          box(width=4, title="🎤 Top Tracks", solidHeader=TRUE, status="primary",
            div(style="overflow-x:auto;max-height:340px;overflow-y:auto;",
                tableOutput("table_top_tracks")))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 4 – WHAT'S MISSING
      # ══════════════════════════════════════════════════
      tabItem(tabName="missing",
        fluidRow(column(12,
          tags$div(class="section-header","What Spotify Doesn't Show You"),
          tags$div(class="section-sub","These insights exist in your music — but Spotify and YouTube Music hide them.")
        )),
        fluidRow(
          box(width=3, title="🎚 Genre Comparison", solidHeader=TRUE, status="danger",
            selectInput("missing_genre1","Genre A:", choices=sort(valid_genres), selected="pop"),
            selectInput("missing_genre2","Genre B:", choices=sort(valid_genres), selected="hip-hop")
          ),
          box(width=9, title="🎭 Audio DNA — Genre vs Genre", solidHeader=TRUE, status="danger",
            plotOutput("plot_mood_compare", height="280px"))
        ),
        fluidRow(
          box(width=2, title="🎚 Audio Profile", solidHeader=TRUE, status="warning",
            selectInput("radar_genre","Select Genre:", choices=sort(valid_genres), selected="pop")
          ),
          box(width=5, title="🕸 Audio Profile by Genre", solidHeader=TRUE, status="warning",
            plotOutput("plot_radar", height="300px")),
          box(width=2, title="🎚 Speechiness", solidHeader=TRUE, status="primary",
            sliderInput("speech_n","No. of Genres:", min=5, max=20, value=12)
          ),
          box(width=3, title="🗣 Speechiness by Genre", solidHeader=TRUE, status="primary",
            plotOutput("plot_speechiness", height="300px"))
        ),
        fluidRow(
          column(3, tags$div(class="insight-card",
            tags$h5("🎭 Mood by Genre"),
            tags$p("Spotify never shows the average emotional tone of a genre. Valence varies wildly — some genres are consistently upbeat, others dark.")
          )),
          column(3, tags$div(class="insight-card",
            tags$h5("⚡ Energy vs Popularity Gap"),
            tags$p("High energy doesn't always mean high popularity. Many high-energy genres rank low — a gap the algorithm ignores.")
          )),
          column(3, tags$div(class="insight-card",
            tags$h5("🎸 The Acoustic Paradox"),
            tags$p("Acoustic tracks score low on energy but often high on valence. Neither platform surfaces this relationship.")
          )),
          column(3, tags$div(class="insight-card",
            tags$h5("🗣 Speechiness & Identity"),
            tags$p("Hip-hop and spoken word have uniquely high speechiness — invisible to users but it defines the genre's sonic identity.")
          ))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 5 – CORRELATIONS
      # ══════════════════════════════════════════════════
      tabItem(tabName="heatmap",
        fluidRow(column(12,
          tags$div(class="section-header","Correlation Matrix"),
          tags$div(class="section-sub","How strongly do audio features relate to each other and to popularity?")
        )),
        fluidRow(
          box(width=3, title="🎚 Controls", solidHeader=TRUE, status="primary",
            selectInput("heatmap_genre","Filter by Genre:",
                        choices=c("All Genres", sort(valid_genres)), selected="All Genres"),
            checkboxGroupInput("heatmap_features","Features:",
                               choices=audio_features, selected=unname(audio_features)),
            checkboxInput("heatmap_popularity","Include Popularity", value=TRUE),
            tags$hr(),
            downloadButton("dl_heatmap","⬇ Download Data", class="btn-dl")
          ),
          box(width=9, title="📊 Correlation Heatmap", solidHeader=TRUE, status="primary",
            plotOutput("plot_heatmap", height="520px"))
        )
      ),

      # ══════════════════════════════════════════════════
      # TAB 6 – CONCLUSIONS
      # ══════════════════════════════════════════════════
      tabItem(tabName="conclusions",
        fluidRow(column(12,
          tags$div(class="section-header","Key Findings & Conclusions"),
          tags$div(class="section-sub","What the data tells us about music on Spotify")
        )),
        fluidRow(
          column(6,
            tags$div(class="insight-card", tags$h5("🎵 Genre Distribution"),
              tags$p("700 tracks span 114 genres. Dancehall, country and Turkish music are most represented. 647 of 700 tracks (92.4%) are non-explicit, suggesting the dataset reflects mainstream, radio-friendly content.")),
            tags$div(class="insight-card", tags$h5("⚡ Audio Features"),
              tags$p("Danceability (mean 0.58) and energy (mean 0.64) dominate. A strong negative correlation exists between acousticness and energy — acoustic tracks are consistently less energetic.")),
            tags$div(class="insight-card", tags$h5("🔗 Correlations"),
              tags$p("Energy and loudness are strongly positively correlated. Instrumentalness negatively correlates with popularity — vocal tracks consistently outperform instrumental ones."))
          ),
          column(6,
            tags$div(class="insight-card", tags$h5("⭐ Popularity Drivers"),
              tags$p("Alt-rock, grunge and k-pop lead on average popularity. No single feature predicts popularity alone. Top track: 'Left and Right' feat. Jung Kook (BTS) at score 92.")),
            tags$div(class="insight-card", tags$h5("🔍 What Spotify Hides"),
              tags$p("Users cannot see audio DNA profiles per genre, mood distributions, or why tracks are recommended. This dashboard fills that gap by exposing the raw feature data behind the algorithm.")),
            tags$div(class="insight-card", tags$h5("⚠ Limitations"),
              tags$p("700 tracks is a small sample. Popularity scores are dynamic snapshots. Genre labels may not fully reflect listener perception."))
          )
        ),
        fluidRow(
          box(width=12, solidHeader=FALSE,
            tags$p(style="color:#888;font-size:11px;margin:0;",
              tags$b("Dataset:")," Spotify Music — Group 3, DSA8045 | 700 tracks · 20 variables · 114 genres",
              tags$br(), tags$b("Tools:")," R · Shiny · ggplot2 · dplyr · shinydashboard"
            )
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

  # ── Dark/light mode ───────────────────────────────────
  dark_mode <- reactiveVal(TRUE)

  observeEvent(input$toggle_mode, {
    new_dark <- !dark_mode()
    dark_mode(new_dark)
    updateActionButton(session, "toggle_mode",
                       label = if (new_dark) "☀ Light Mode" else "🌙 Dark Mode")
    session$sendCustomMessage("switch_theme", new_dark)
  })

  # ── Reactive theme ─────────────────────────────────────
  theme <- reactive({ make_theme(dark_mode()) })

  # ── Reactive datasets ──────────────────────────────────
  overview_data <- reactive({
    df <- music
    if (!input$overview_explicit) df <- df[!df$explicit, ]
    df
  })

  # ── Dynamic KPI cards ──────────────────────────────────
  output$kpi_tracks <- renderUI({
    df <- overview_data()
    kpi(as.character(nrow(df)), "Total Tracks", "#FF0000")
  })
  output$kpi_genres <- renderUI({
    df <- overview_data()
    kpi(as.character(length(unique(df$track_genre))), "Unique Genres", "#1DB954")
  })
  output$kpi_popularity <- renderUI({
    df <- overview_data()
    kpi(as.character(round(mean(df$popularity, na.rm=TRUE), 1)), "Avg Popularity", "#FF9800")
  })
  output$kpi_danceability <- renderUI({
    df <- overview_data()
    kpi(as.character(round(mean(df$danceability, na.rm=TRUE), 2)), "Avg Danceability", "#2196F3")
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

  # ── Downloads ──────────────────────────────────────────
  output$dl_overview   <- downloadHandler(filename=function() paste0("spotify_overview_",  Sys.Date(),".csv"), content=function(f) write.csv(overview_data(),   f, row.names=FALSE))
  output$dl_audio      <- downloadHandler(filename=function() paste0("spotify_audio_",     Sys.Date(),".csv"), content=function(f) write.csv(scatter_data(),    f, row.names=FALSE))
  output$dl_popularity <- downloadHandler(filename=function() paste0("spotify_popularity_",Sys.Date(),".csv"), content=function(f) write.csv(popularity_data(), f, row.names=FALSE))
  output$dl_heatmap    <- downloadHandler(filename=function() paste0("spotify_heatmap_",   Sys.Date(),".csv"), content=function(f) write.csv(heatmap_data(),    f, row.names=FALSE))

  # ── PLOT: Genre bar ────────────────────────────────────
  output$plot_genre_bar <- renderPlot({
    t  <- theme()
    df <- overview_data() %>% count(track_genre, sort=TRUE) %>% slice_head(n=input$overview_n)
    ggplot(df, aes(x=reorder(track_genre,n), y=n, fill=n)) +
      geom_col(width=0.72, show.legend=FALSE) +
      scale_fill_gradient(low="#FF6B6B", high="#FF0000") +
      coord_flip() +
      labs(title=paste("Top", input$overview_n,"Genres by Track Count"),
           subtitle=ifelse(!input$overview_explicit,"Explicit excluded","All tracks"),
           x="Genre", y="Tracks") +
      gg_theme(t)
  }, bg="transparent")

  # ── PLOT: Pie chart ────────────────────────────────────
  output$plot_explicit_pie <- renderPlot({
    t  <- theme()
    df <- music %>% count(explicit_label) %>%
      mutate(pct=round(n/sum(n)*100,1), lbl=paste0(explicit_label,"\n",pct,"%"))
    ggplot(df, aes(x=2, y=n, fill=explicit_label)) +
      geom_col(width=1, colour=t$plot_bg, linewidth=2) +
      coord_polar(theta="y", start=0) +
      xlim(0.5, 2.5) +
      scale_fill_manual(values=c("Clean"="#1DB954","Explicit"="#FF0000"),
                        guide="none") +
      geom_text(aes(label=lbl), position=position_stack(vjust=0.5),
                size=4, fontface="bold", colour="white") +
      labs(title="Explicit vs Clean", x=NULL, y=NULL) +
      gg_theme(t, no_grid=TRUE, hide_axes=TRUE)
  }, bg="transparent")

  # ── PLOT: Tempo histogram ──────────────────────────────
  output$plot_tempo_hist <- renderPlot({
    t <- theme()
    ggplot(music, aes(x=tempo, fill=explicit_label)) +
      geom_histogram(binwidth=10, colour=t$plot_bg, linewidth=0.3,
                     alpha=0.9, position="identity") +
      scale_fill_manual(values=c("Clean"="#1DB954","Explicit"="#FF0000")) +
      labs(title="Tempo Distribution (BPM)", subtitle="Explicit vs Clean",
           x="Tempo (BPM)", y="Tracks", fill=NULL) +
      gg_theme(t)
  }, bg="transparent")

  # ── PLOT: Scatter ──────────────────────────────────────
  output$plot_scatter <- renderPlot({
    t     <- theme()
    df    <- scatter_data()
    x_var <- input$audio_feature_x
    y_var <- input$audio_feature_y
    x_lab <- names(audio_features)[audio_features == x_var]
    y_lab <- names(audio_features)[audio_features == y_var]
    ggplot(df, aes_string(x=x_var, y=y_var, colour="track_genre")) +
      geom_point(alpha=0.65, size=2.2) +
      geom_smooth(method="lm", se=TRUE, colour="#FF0000",
                  linetype="dashed", linewidth=1, alpha=0.12) +
      scale_colour_viridis_d(option="turbo", guide="none") +
      labs(title=paste(x_lab,"vs",y_lab),
           subtitle=paste("Genre:", input$audio_genre_filter,"| n =",nrow(df)),
           x=x_lab, y=y_lab) +
      gg_theme(t)
  }, bg="transparent")

  # ── PLOT: Boxplot ──────────────────────────────────────
  output$plot_boxplot <- renderPlot({
    t        <- theme()
    feat     <- input$boxplot_feature
    feat_lab <- names(audio_features)[audio_features == feat]
    top_g <- music_valid %>%
      group_by(track_genre) %>%
      summarise(med=median(.data[[feat]], na.rm=TRUE)) %>%
      slice_max(order_by=med, n=input$boxplot_n_genres) %>%
      pull(track_genre)
    df <- music_valid[music_valid$track_genre %in% top_g, ]
    ggplot(df, aes_string(x="reorder(track_genre,-get(feat))", y=feat, fill="track_genre")) +
      geom_boxplot(outlier.shape=21, outlier.size=1.5, show.legend=FALSE,
                   alpha=0.85, colour=t$text2, linewidth=0.4) +
      scale_fill_viridis_d(option="plasma") +
      coord_flip() +
      labs(title=paste("Distribution of", feat_lab),
           subtitle="Top genres by median", x="Genre", y=feat_lab) +
      gg_theme(t)
  }, bg="transparent")

  # ── PLOT: Popularity by genre ──────────────────────────
  output$plot_pop_genre <- renderPlot({
    t  <- theme()
    df <- popularity_data() %>%
      group_by(track_genre) %>%
      summarise(avg_pop=mean(popularity,na.rm=TRUE), n=n()) %>%
      filter(n>=3) %>% slice_max(order_by=avg_pop, n=input$pop_genre_n)
    ggplot(df, aes(x=reorder(track_genre,avg_pop), y=avg_pop, fill=avg_pop)) +
      geom_col(width=0.72, show.legend=FALSE) +
      geom_text(aes(label=round(avg_pop,1)), hjust=-0.1, size=3, colour=t$text2) +
      scale_fill_gradient(low="#FF6B6B", high="#FF0000") +
      coord_flip() +
      scale_y_continuous(limits=c(0,82), expand=expansion(mult=c(0,0.05))) +
      labs(title=paste("Top",input$pop_genre_n,"Genres by Avg Popularity"),
           subtitle="Min 3 tracks per genre", x="Genre", y="Avg Popularity (0–100)") +
      gg_theme(t)
  }, bg="transparent")

  # ── PLOT: Feature vs Popularity ────────────────────────
  output$plot_pop_scatter <- renderPlot({
    t    <- theme()
    df   <- popularity_data()
    feat <- input$pop_feature
    lab  <- names(audio_features)[audio_features == feat]
    ggplot(df, aes_string(x=feat, y="popularity", colour="explicit_label")) +
      geom_point(alpha=0.5, size=1.8) +
      geom_smooth(method="lm", se=TRUE, colour="#FF9800",
                  linetype="dashed", linewidth=1, alpha=0.12) +
      scale_colour_manual(values=c("Clean"="#1DB954","Explicit"="#FF0000")) +
      labs(title=paste(lab,"vs Popularity"), subtitle=paste("n =",nrow(df),"tracks"),
           x=lab, y="Popularity (0–100)", colour=NULL) +
      gg_theme(t)
  }, bg="transparent")

  # ── TABLE: Top tracks ──────────────────────────────────
  output$table_top_tracks <- renderTable({
    popularity_data() %>%
      arrange(desc(popularity)) %>% slice_head(n=input$pop_top_n) %>%
      mutate(Rank=row_number(), Track=track_name, Artist=artists,
             Genre=track_genre, Score=popularity, Explicit=explicit_label) %>%
      select(Rank, Track, Artist, Genre, Score, Explicit)
  }, striped=TRUE, bordered=TRUE, hover=TRUE, width="100%")

  # ── PLOT: Genre comparison ─────────────────────────────
  output$plot_mood_compare <- renderPlot({
    t   <- theme()
    g1  <- input$missing_genre1
    g2  <- input$missing_genre2
    df  <- music_valid[music_valid$track_genre %in% c(g1,g2), ]
    feats <- names(audio_features)
    vals  <- unname(audio_features)
    long  <- do.call(rbind, lapply(seq_along(vals), function(i)
      data.frame(Feature=feats[i], Value=df[[vals[i]]], Genre=df$track_genre)
    ))
    avg <- long %>% group_by(Feature, Genre) %>%
      summarise(Mean=mean(Value, na.rm=TRUE), .groups="drop")
    ggplot(avg, aes(x=Feature, y=Mean, fill=Genre)) +
      geom_col(position="dodge", width=0.65, alpha=0.9) +
      scale_fill_manual(values=setNames(c("#FF0000","#1DB954"), c(g1,g2))) +
      scale_y_continuous(limits=c(0,1.05), expand=expansion(mult=c(0,0.02))) +
      labs(title=paste("Audio DNA:", g1,"vs",g2),
           subtitle="Average score per feature (0–1)", x=NULL, y="Score", fill="Genre") +
      gg_theme(t, axis_x_angle=25, axis_x_hjust=1, axis_x_size=9)
  }, bg="transparent")

  # ── PLOT: Audio profile radar ──────────────────────────
  output$plot_radar <- renderPlot({
    t     <- theme()
    genre <- input$radar_genre
    df    <- music_valid[music_valid$track_genre == genre, ]
    vals  <- unname(audio_features)
    feats <- names(audio_features)
    means <- sapply(vals, function(v) mean(df[[v]], na.rm=TRUE))
    radar_df <- data.frame(Feature=factor(feats, levels=feats), Value=means)
    ggplot(radar_df, aes(x=Feature, y=Value, fill=Value)) +
      geom_col(width=0.7, show.legend=FALSE, alpha=0.9) +
      geom_text(aes(label=round(Value,2)), vjust=-0.4, size=3, colour=t$text2) +
      scale_fill_gradient(low="#FF6B6B", high="#FF0000") +
      scale_y_continuous(limits=c(0,1.15), expand=expansion(mult=c(0,0))) +
      labs(title=paste("Audio Profile:", genre),
           subtitle="Mean feature scores (0–1)", x=NULL, y="Score") +
      gg_theme(t, axis_x_angle=25, axis_x_hjust=1, axis_x_size=8)
  }, bg="transparent")

  # ── PLOT: Speechiness ──────────────────────────────────
  output$plot_speechiness <- renderPlot({
    t  <- theme()
    df <- music_valid %>%
      group_by(track_genre) %>%
      summarise(mean_speech=mean(speechiness, na.rm=TRUE)) %>%
      slice_max(order_by=mean_speech, n=input$speech_n)
    ggplot(df, aes(x=reorder(track_genre,mean_speech), y=mean_speech, fill=mean_speech)) +
      geom_col(width=0.72, show.legend=FALSE) +
      scale_fill_gradient(low="#FF9800", high="#E91E63") +
      coord_flip() +
      labs(title="Speechiness by Genre",
           subtitle="Higher = more spoken words",
           x=NULL, y="Avg Speechiness (0–1)") +
      gg_theme(t)
  }, bg="transparent")

  # ── PLOT: Heatmap ──────────────────────────────────────
  output$plot_heatmap <- renderPlot({
    t  <- theme()
    df <- heatmap_data()
    validate(
      need(ncol(df) >= 2, "Please select at least 2 features."),
      need(nrow(df) >= 5, "Not enough tracks for this genre.")
    )
    cor_mat    <- cor(df, use="pairwise.complete.obs")
    feat_names <- colnames(cor_mat)
    cor_long   <- data.frame(
      Var1  = rep(feat_names, each=length(feat_names)),
      Var2  = rep(feat_names, times=length(feat_names)),
      value = as.vector(cor_mat)
    )
    display <- c(danceability="Danceability", energy="Energy", valence="Valence",
                 acousticness="Acousticness", speechiness="Speechiness",
                 instrumentalness="Instrumentalness", liveness="Liveness",
                 popularity="Popularity")
    cor_long$Var1 <- ifelse(cor_long$Var1 %in% names(display), display[cor_long$Var1], cor_long$Var1)
    cor_long$Var2 <- ifelse(cor_long$Var2 %in% names(display), display[cor_long$Var2], cor_long$Var2)
    ggplot(cor_long, aes(x=Var1, y=Var2, fill=value)) +
      geom_tile(colour=t$plot_bg, linewidth=1) +
      geom_text(aes(label=sprintf("%.2f",value)), size=3.5, fontface="bold",
                colour=ifelse(abs(cor_long$value)>0.4,"white",t$text2)) +
      scale_fill_gradient2(low="#1565C0", mid=t$surface2, high="#C62828",
                           midpoint=0, limits=c(-1,1), name="r") +
      scale_x_discrete(expand=c(0,0)) +
      scale_y_discrete(expand=c(0,0)) +
      labs(title="Audio Feature Correlation Matrix",
           subtitle=paste("Genre:", input$heatmap_genre,"| n =",nrow(df),"tracks"),
           x=NULL, y=NULL) +
      gg_theme(t, axis_x_angle=35, axis_x_hjust=1, axis_x_size=10,
               axis_y_size=10, no_grid=TRUE, legend_key_h=1.4)
  }, bg="transparent")
}

shinyApp(ui=ui, server=server)
