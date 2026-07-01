# ════════════════════════════════════════════════════════════
# SOLACE — "Music Beat" inspired Home page
# Sidebar nav + hero carousel + dual trending lists +
# recommended row + bottom player bar + dark/light toggle
# ════════════════════════════════════════════════════════════

library(shiny)
library(dplyr)
library(ggplot2)

music <- read.csv("Group3_music.csv", stringsAsFactors = FALSE)
clean_artist <- function(x) trimws(strsplit(x, "[;,]")[[1]][1])
fmt_dur <- function(ms) {
  s <- as.integer(round(ms / 1000))
  sprintf("%d:%02d", s %/% 60L, s %% 60L)
}

ACCENT_DARK <- "#1F8A70"   # teal-green, like reference logo
ACCENT_GRAD1 <- "#2BB5A0"
ACCENT_GRAD2 <- "#1F8A70"

# ── Derived data ─────────────────────────────────────────
hero_tracks <- music %>% arrange(desc(popularity)) %>% slice_head(n = 20) %>%
  mutate(main_artist = sapply(artists, clean_artist), dur = fmt_dur(duration_ms))

genre_pop <- music %>% group_by(track_genre) %>%
  summarise(avg_pop = mean(popularity, na.rm=TRUE), n = dplyr::n()) %>%
  filter(n >= 5) %>% arrange(desc(avg_pop))

all_genres_sorted <- genre_pop$track_genre   # every genre with 5+ tracks, ranked by avg popularity
genre_A_default <- all_genres_sorted[1]
genre_B_default <- all_genres_sorted[2]

get_genre_top <- function(g, n = 4) {
  music %>% filter(track_genre == g) %>% arrange(desc(popularity)) %>%
    slice_head(n = n) %>% mutate(main_artist = sapply(artists, clean_artist), dur = fmt_dur(duration_ms))
}

recommended <- music %>% arrange(desc(popularity)) %>% slice(10:14) %>%
  mutate(main_artist = sapply(artists, clean_artist))

recommended_full <- music %>% arrange(desc(popularity)) %>% slice(10:50) %>%
  mutate(main_artist = sapply(artists, clean_artist), dur = fmt_dur(duration_ms))

audio_features_map <- c(
  "Danceability"     = "danceability",
  "Energy"           = "energy",
  "Valence (Mood)"   = "valence",
  "Acousticness"     = "acousticness",
  "Speechiness"      = "speechiness",
  "Instrumentalness" = "instrumentalness",
  "Liveness"         = "liveness"
)

grad_colors <- c("#2BB5A0","#FF8FA3","#FFB199","#7C8CF8","#FBBF24","#60A5FA",
                  "#F472B6","#34D399","#F87171","#A78BFA")
grad_for <- function(i) {
  c1 <- grad_colors[(i-1) %% length(grad_colors) + 1]
  c2 <- grad_colors[i %% length(grad_colors) + 1]
  paste0("linear-gradient(135deg,",c1,",",c2,")")
}
initials <- function(x) toupper(substr(x,1,2))

key_names <- c("C","C#/Db","D","D#/Eb","E","F","F#/Gb","G","G#/Ab","A","A#/Bb","B")

# ── CSS ───────────────────────────────────────────────────
css_base <- "
  @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700;800&display=swap');
  * { font-family:'Inter',sans-serif !important; box-sizing:border-box; }
  body { margin:0; overflow-x:hidden; }
  .shell { display:flex; min-height:100vh; max-width:100vw; overflow-x:hidden; }

  .nav { width:210px; flex-shrink:0; padding:22px 16px; }
  .nav-logo { display:flex; align-items:center; gap:9px; padding:0 4px 26px 4px; }
  .nav-logo-text { font-size:16px; font-weight:800; }
  .search-box { display:flex; align-items:center; gap:9px; padding:10px 12px; border-radius:10px;
    font-size:12.5px; margin-bottom:0; }
  .search-box input { border:none; outline:none; background:transparent; font-size:12.5px; flex:1; }
  .search-icon-svg { width:15px; height:15px; flex-shrink:0; opacity:0.6; display:flex; align-items:center; color:inherit; }
  .search-icon-svg svg { width:100%; height:100%; }
  .search-wrap { position:relative; margin-bottom:20px; }
  .search-results { position:absolute; top:calc(100% + 6px); left:0; right:0; border-radius:10px;
    max-height:280px; overflow-y:auto; z-index:50; box-shadow:0 8px 24px rgba(0,0,0,0.18); }
  .search-result-row { display:flex; align-items:center; gap:10px; padding:9px 10px; cursor:pointer; }
  .search-result-cover { width:30px; height:30px; border-radius:7px; flex-shrink:0; display:flex;
    align-items:center; justify-content:center; color:#fff; font-size:10px; font-weight:800; }
  .search-result-name { font-size:11.5px; font-weight:700; }
  .search-result-sub { font-size:10px; opacity:0.55; }
  .search-no-results { padding:12px 10px; font-size:11.5px; opacity:0.5; }

  /* Track Detail Modal */
  .td-header { display:flex; gap:18px; align-items:center; margin-bottom:20px; }
  .td-cover { width:84px; height:84px; border-radius:16px; flex-shrink:0; display:flex; align-items:center;
    justify-content:center; color:#fff; font-weight:800; font-size:26px; }
  .td-title { font-size:19px; font-weight:800; line-height:1.25; margin-bottom:4px; }
  .td-artist { font-size:13px; opacity:0.6; margin-bottom:6px; }
  .td-meta-row { display:flex; gap:10px; flex-wrap:wrap; }
  .td-chip { font-size:10.5px; font-weight:700; padding:4px 10px; border-radius:20px; }
  .td-stats-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:12px; margin-bottom:20px; }
  .td-stat { border-radius:12px; padding:12px 14px; text-align:center; }
  .td-stat-value { font-size:18px; font-weight:800; }
  .td-stat-label { font-size:9.5px; font-weight:600; opacity:0.5; text-transform:uppercase; letter-spacing:0.04em; margin-top:3px; }
  .td-feature-row { display:flex; align-items:center; gap:10px; margin-bottom:9px; }
  .td-feature-label { font-size:11px; font-weight:600; width:120px; flex-shrink:0; opacity:0.7; }
  .td-feature-bar-wrap { flex:1; height:7px; border-radius:4px; position:relative; }
  .td-feature-bar-fill { position:absolute; left:0; top:0; height:100%; border-radius:4px; }
  .td-feature-value { font-size:11px; font-weight:700; width:34px; text-align:right; flex-shrink:0; }
  .td-section-title { font-size:12px; font-weight:700; margin:18px 0 10px 0; text-transform:uppercase;
    letter-spacing:0.04em; opacity:0.6; }
  .nav-item { display:flex; align-items:center; gap:12px; padding:11px 12px; border-radius:11px;
    font-size:13.5px; font-weight:600; margin-bottom:4px; cursor:pointer; }
  .nav-item.active { background:linear-gradient(135deg,#2BB5A0,#1F8A70); color:#fff !important; }
  .nav-item .ic { width:16px; text-align:center; display:flex; align-items:center; justify-content:center; }
  .nav-item .ic svg { width:16px; height:16px; }

  .main { flex:1; padding:22px 26px 26px 26px; min-width:0; }
  .top-bar { display:flex; align-items:center; justify-content:flex-end; gap:16px; margin-bottom:22px; }
  .mode-switch { width:42px; height:24px; border-radius:14px; position:relative; cursor:pointer; }
  .mode-switch .knob { width:18px; height:18px; border-radius:50%; background:#fff; position:absolute;
    top:3px; transition:left 0.2s ease; }
  .topbar-icon { font-size:15px; cursor:pointer; }
  .profile-mini { display:flex; align-items:center; gap:8px; }
  .profile-mini-avatar { width:34px; height:34px; border-radius:50%; background:linear-gradient(135deg,#FFB199,#FF8FA3);
    display:flex; align-items:center; justify-content:center; color:#fff; font-weight:700; font-size:12px; }
  .profile-mini-name { font-size:12.5px; font-weight:700; }

  /* Stat boxes (KPI cards) */
  .stats-sub { font-size:13px; opacity:0.55; margin:0 0 18px 0; }
  .kpi-row { display:flex; gap:16px; margin-bottom:30px; }
  .kpi-card { flex:1; border-radius:16px; padding:18px 20px; position:relative; overflow:hidden; }
  .kpi-card::before { content:''; position:absolute; top:0; left:0; width:4px; height:100%; background:var(--kpi-accent); }
  .kpi-value { font-size:26px; font-weight:800; line-height:1; }
  .kpi-label { font-size:11px; font-weight:600; opacity:0.55; margin-top:6px; text-transform:uppercase; letter-spacing:0.05em; }

  /* Hero carousel — paginated, 5 visible cards per page */
  .hero-section { margin-bottom:30px; display:flex; flex-direction:column; align-items:center; }
  .hero-viewport { position:relative; overflow:hidden; height:260px; box-sizing:content-box; max-width:100%; margin:0 auto; }
  .hero-track { display:flex; gap:14px; transition:transform 0.45s cubic-bezier(0.4,0,0.2,1); width:max-content; }
  .hero-card { flex-shrink:0; height:260px; width:218px; border-radius:20px; overflow:hidden;
    display:flex; flex-direction:column; justify-content:flex-end; padding:18px; color:#fff;
    box-shadow:0 6px 16px rgba(0,0,0,0.12); }
  .hero-card .hc-genre { font-size:9px; letter-spacing:0.12em; text-transform:uppercase; opacity:0.8; margin-bottom:auto; margin-top:14px; }
  .hero-card .hc-title { font-size:15px; font-weight:800; line-height:1.25; }
  .hero-card .hc-artist { font-size:11px; opacity:0.85; margin-top:2px; }
  .hero-dots { display:flex; justify-content:center; gap:7px; margin-top:14px; }
  .hero-dot { width:7px; height:7px; border-radius:50%; cursor:pointer; transition:all 0.25s ease; }
  .hero-dot.active { width:20px; border-radius:4px; }

  .sec-head { display:flex; align-items:center; justify-content:space-between; margin-bottom:16px; flex-wrap:wrap; gap:10px; }
  .sec-title { font-size:17px; font-weight:800; margin:0; }
  .view-all { font-size:12px; font-weight:600; opacity:0.55; cursor:pointer; text-decoration:none !important; }
  .view-all:hover { opacity:0.9; }
  .genre-select-wrap { width:170px; }
  .genre-select-wrap .selectize-input { min-height:34px !important; padding:6px 12px !important; font-size:12px !important;
    border-radius:9px !important; }
  .genre-select-wrap label { display:none; }
  .genre-select-wrap.full-width { width:100%; }
  .genre-select-wrap.full-width .selectize-control { width:100%; }

  /* Top Trending + Genre Comparison — side by side */
  .trend-compare-grid { display:grid; grid-template-columns:1fr 1.3fr; gap:20px; margin-bottom:30px; align-items:stretch; }
  .trend-panel, .compare-panel { border-radius:16px; padding:20px; display:flex; flex-direction:column; min-width:0; }
  .trend-panel .sec-head, .compare-panel .sec-head { margin-bottom:14px; }
  .compare-pickers { display:flex; gap:12px; margin-bottom:16px; min-width:0; }
  .compare-pickers .genre-select-wrap { flex:1 1 0; width:auto; min-width:0; max-width:50%; }
  .compare-pickers .genre-select-wrap .selectize-control,
  .compare-pickers .genre-select-wrap .selectize-input {
    width:100% !important; max-width:100% !important; box-sizing:border-box !important; }
  .compare-subtitle { font-size:11px; opacity:0.5; margin-bottom:14px; text-align:left; }
  .dna-bar-wrap { width:100%; flex:1; }

  .modal-row { display:flex; align-items:center; gap:14px; padding:10px 4px; }
  .modal-row:last-child { border-bottom:none; }
  .modal-rank { font-size:13px; font-weight:700; width:22px; }
  .modal-cover { width:46px; height:46px; border-radius:10px; flex-shrink:0; display:flex; align-items:center;
    justify-content:center; color:#fff; font-weight:800; font-size:13px; }
  .modal-info { flex:1; min-width:0; }
  .modal-name { font-size:13px; font-weight:700; }
  .modal-sub { font-size:11px; }
  .modal-pop { font-size:11px; font-weight:700; white-space:nowrap; }

  .trend-cover { width:100%; padding:14px 16px; border-radius:12px; display:flex; align-items:center;
    font-weight:800; font-size:13px; letter-spacing:0.04em; margin-bottom:14px; }
  .trend-list { width:100%; }
  .trend-row { display:flex; align-items:center; gap:10px; padding:7px 8px; border-radius:10px; cursor:pointer; transition:background 0.2s; }
  .trend-row.active { background:var(--row-active); }
  .trend-rank { font-size:12px; font-weight:700; opacity:0.4; width:14px; }
  .trend-info { flex:1; min-width:0; }
  .trend-name { font-size:12.5px; font-weight:700; white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
  .trend-sub { font-size:10.5px; opacity:0.5; }

  .reco-row { display:flex; gap:18px; margin-bottom:30px; }
  .reco-card { flex:1; }
  .reco-cover { width:100%; aspect-ratio:1; border-radius:14px; display:flex; align-items:center;
    justify-content:center; color:#fff; font-weight:800; font-size:22px; margin-bottom:8px; }
  .reco-name { font-size:12px; font-weight:700; }
  .reco-artist { font-size:10.5px; opacity:0.5; }
"

css_light <- paste0(css_base, "
  :root { --row-active:#EFF8F6; --border:#ECECEF; --accent-grad:linear-gradient(135deg,", ACCENT_GRAD1, ",", ACCENT_GRAD2, "); }
  body { background:#FAFAFB; color:#15161A; }
  .nav { background:#fff; border-right:1px solid #ECECEF; }
  .nav-logo-text { color:", ACCENT_DARK, "; }
  .search-box { background:#F4F5F7; border:1px solid #ECECEF; color:#15161A; }
  .search-box input { color:#15161A; }
  .search-results { background:#FFFFFF; border:1px solid #ECECEF; }
  .search-result-row:hover { background:#F4F5F7; }
  .search-result-name { color:#15161A; }
  .nav-item { color:#6B6E76; }
  .mode-switch { background:#E5E7EB; }
  .mode-switch .knob { left:3px; }
  .topbar-icon { color:#6B6E76; }
  .profile-mini-name { color:#15161A; }
  .sec-title { color:#15161A; }
  .trend-name, .reco-name { color:#15161A; }
  .modal-content { background:#FFFFFF !important; color:#15161A !important; }
  .modal-header, .modal-footer { border-color:#ECECEF !important; }
  .modal-title, .modal-dialog h4 { color:#15161A !important; }
  .modal-row { border-bottom:1px solid #ECECEF; }
  .modal-rank { color:#9A9CA3; }
  .modal-name { color:#15161A; }
  .modal-sub { color:#8A8DA3; }
  .modal-pop { color:#4A4D63; }
  .close { color:#15161A !important; }
  .stats-sub { color:#8A8DA3; }
  .kpi-card { background:#FFFFFF; border:1px solid #ECECEF; box-shadow:0 1px 4px rgba(16,24,40,0.04); }
  .kpi-value { color:#15161A; }
  .hero-dot { background:#D9DBE3; }
  .hero-dot.active { background:", ACCENT_DARK, "; }
  .genre-select-wrap .selectize-input { background:#F4F5F7 !important; border:1px solid #ECECEF !important; color:#15161A !important; }
  .genre-select-wrap .selectize-dropdown { background:#FFFFFF !important; border:1px solid #ECECEF !important; color:#15161A !important; }
  .compare-panel, .trend-panel { background:#FFFFFF; border:1px solid #ECECEF; box-shadow:0 1px 4px rgba(16,24,40,0.04); }
  .compare-subtitle { color:#8A8DA3; }
  .td-title { color:#15161A; }
  .td-artist { color:#6B6E76; }
  .td-stat { background:#F4F5F7; }
  .td-stat-value { color:#15161A; }
  .td-feature-label, .td-section-title { color:#15161A; }
  .td-feature-bar-wrap { background:#ECECEF; }
")

css_dark <- paste0(css_base, "
  :root { --row-active:#1A2E29; --border:#2A2A2A; --accent-grad:linear-gradient(135deg,", ACCENT_GRAD1, ",", ACCENT_GRAD2, "); }
  body { background:#121212; color:#fff; }
  .nav { background:#1A1A1A; border-right:1px solid #232323; }
  .nav-logo-text { color:#2BB5A0; }
  .search-box { background:#1E1E1E; border:1px solid #2A2A2A; color:#fff; }
  .search-box input { color:#fff; }
  .search-results { background:#1E1E1E; border:1px solid #2A2A2A; }
  .search-result-row:hover { background:#242424; }
  .search-result-name { color:#fff; }
  .nav-item { color:#9A9A9A; }
  .mode-switch { background:#1F8A70; }
  .mode-switch .knob { left:21px; }
  .topbar-icon { color:#B5B5B5; }
  .profile-mini-name { color:#fff; }
  .sec-title { color:#fff; }
  .trend-name, .reco-name { color:#fff; }
  .modal-content { background:#1A1A1A !important; color:#FFFFFF !important; }
  .modal-header, .modal-footer { border-color:#2A2A2A !important; }
  .modal-title, .modal-dialog h4 { color:#FFFFFF !important; }
  .modal-row { border-bottom:1px solid #2A2A2A; }
  .modal-rank { color:#777777; }
  .modal-name { color:#FFFFFF; }
  .modal-sub { color:#9A9A9A; }
  .modal-pop { color:#CCCCCC; }
  .close { color:#FFFFFF !important; opacity:0.8 !important; text-shadow:none !important; }
  .stats-sub { color:#9A9A9A; }
  .kpi-card { background:#1A1A1A; border:1px solid #2A2A2A; }
  .kpi-value { color:#FFFFFF; }
  .hero-dot { background:#3A3A3A; }
  .hero-dot.active { background:#2BB5A0; }
  .genre-select-wrap .selectize-input { background:#1E1E1E !important; border:1px solid #2A2A2A !important; color:#FFFFFF !important; }
  .genre-select-wrap .selectize-dropdown { background:#1E1E1E !important; border:1px solid #2A2A2A !important; color:#FFFFFF !important; }
  .genre-select-wrap .selectize-dropdown .option:hover { background:#2BB5A0 !important; }
  .compare-panel, .trend-panel { background:#1A1A1A; border:1px solid #2A2A2A; }
  .compare-subtitle { color:#9A9A9A; }
  .td-title { color:#FFFFFF; }
  .td-artist { color:#9A9A9A; }
  .td-stat { background:#242424; }
  .td-stat-value { color:#FFFFFF; }
  .td-feature-label, .td-section-title { color:#FFFFFF; }
  .td-feature-bar-wrap { background:#2A2A2A; }
")

theme_js <- sprintf("
  var DARK=%s, LIGHT=%s;
  Shiny.addCustomMessageHandler('switch_theme', function(dark){
    document.getElementById('dyn_css').innerHTML = dark ? DARK : LIGHT;
  });
",
  paste0('"', gsub('"','\\\\"', gsub('\n',' ', css_dark)), '"'),
  paste0('"', gsub('"','\\\\"', gsub('\n',' ', css_light)), '"')
)

# ── UI ────────────────────────────────────────────────────
ui <- fluidPage(
  title = "Solace",
  tags$head(
    tags$style(id="dyn_css", HTML(css_dark)),
    tags$script(HTML(theme_js)),
    tags$link(rel="stylesheet", href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css")
  ),

  tags$div(class="shell",

    tags$div(class="nav",
      tags$div(class="nav-logo", icon("waveform-lines", style="color:#2BB5A0;"), tags$span(class="nav-logo-text","Solace")),
      tags$div(class="search-wrap",
        tags$div(class="search-box",
                 tags$span(class="search-icon-svg", HTML('<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M10.77 18.3C9.2807 18.3 7.82485 17.8584 6.58655 17.031C5.34825 16.2036 4.38311 15.0275 3.81318 13.6516C3.24325 12.2757 3.09413 10.7616 3.38468 9.30096C3.67523 7.84029 4.39239 6.49857 5.44548 5.44548C6.49857 4.39239 7.84029 3.67523 9.30096 3.38468C10.7616 3.09413 12.2757 3.24325 13.6516 3.81318C15.0275 4.38311 16.2036 5.34825 17.031 6.58655C17.8584 7.82485 18.3 9.2807 18.3 10.77C18.3 11.7588 18.1052 12.738 17.7268 13.6516C17.3484 14.5652 16.7937 15.3953 16.0945 16.0945C15.3953 16.7937 14.5652 17.3484 13.6516 17.7268C12.738 18.1052 11.7588 18.3 10.77 18.3ZM10.77 4.74999C9.58331 4.74999 8.42327 5.10189 7.43657 5.76118C6.44988 6.42046 5.68084 7.35754 5.22672 8.45389C4.77259 9.55025 4.65377 10.7566 4.88528 11.9205C5.11679 13.0844 5.68824 14.1535 6.52735 14.9926C7.36647 15.8317 8.43556 16.4032 9.59945 16.6347C10.7633 16.8662 11.9697 16.7474 13.0661 16.2933C14.1624 15.8391 15.0995 15.0701 15.7588 14.0834C16.4181 13.0967 16.77 11.9367 16.77 10.75C16.77 9.15869 16.1379 7.63257 15.0126 6.50735C13.8874 5.38213 12.3613 4.74999 10.77 4.74999Z" fill="currentColor"/><path d="M20 20.75C19.9015 20.7504 19.8038 20.7312 19.7128 20.6934C19.6218 20.6557 19.5392 20.6001 19.47 20.53L15.34 16.4C15.2075 16.2578 15.1354 16.0697 15.1388 15.8754C15.1422 15.6811 15.221 15.4958 15.3584 15.3583C15.4958 15.2209 15.6812 15.1422 15.8755 15.1388C16.0698 15.1354 16.2578 15.2075 16.4 15.34L20.53 19.47C20.6704 19.6106 20.7493 19.8012 20.7493 20C20.7493 20.1987 20.6704 20.3893 20.53 20.53C20.4608 20.6001 20.3782 20.6557 20.2872 20.6934C20.1962 20.7312 20.0985 20.7504 20 20.75Z" fill="currentColor"/></svg>')),
                 tags$input(id="search_query", type="text", placeholder="Search artist, title...",
                            oninput="Shiny.setInputValue('search_query', this.value)")),
        uiOutput("search_results")
      ),
      tags$div(class="nav-item active", tags$span(class="ic", HTML('<svg viewBox="0 0 48 48" fill="currentColor" xmlns="http://www.w3.org/2000/svg"> <title>dashboard-tile-solid</title> <g id="Layer_2" data-name="Layer 2"> <g id="invisible_box" data-name="invisible box"> <rect width="48" height="48" fill="none"/> </g> <g id="icons_Q2" data-name="icons Q2"> <g> <path d="M20,30H8a2,2,0,0,0-2,2V42a2,2,0,0,0,2,2H20a2,2,0,0,0,2-2V32a2,2,0,0,0-2-2Z"/> <path d="M20,4H8A2,2,0,0,0,6,6V24a2,2,0,0,0,2,2H20a2,2,0,0,0,2-2V6a2,2,0,0,0-2-2Z"/> <path d="M40,4H28a2,2,0,0,0-2,2V16a2,2,0,0,0,2,2H40a2,2,0,0,0,2-2V6a2,2,0,0,0-2-2Z"/> <path d="M40,22H28a2,2,0,0,0-2,2V42a2,2,0,0,0,2,2H40a2,2,0,0,0,2-2V24a2,2,0,0,0-2-2Z"/> </g> </g> </g> </svg>')), "Dashboard"),
      tags$div(class="nav-item", tags$span(class="ic", HTML('<svg fill="currentColor" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 100 100" enable-background="new 0 0 100 100" xml:space="preserve"> <g> <path d="M61.8,29.4l8.9,8.9l0,0c2,1.9,2,5.1,0,7l0,0L47.5,68.4V47.3V36.6l7.2-7.3C56.6,27.4,59.9,27.4,61.8,29.4z" /> </g> <path d="M37.5,20H25c-2.8,0-5,2.2-5,5v43.8C20,75,25,80,31.2,80s11.2-5,11.2-11.2V25C42.5,22.2,40.2,20,37.5,20z M31.2,73.8c-2.8,0-5-2.2-5-5s2.2-5,5-5s5,2.2,5,5S34,73.8,31.2,73.8z"/> <g> <path d="M75,57.5h-8.8l-6,6H74L73.9,74H49.8l-6,6H75c2.8,0,5-2.2,5-5V62.5C80,59.8,77.8,57.5,75,57.5L75,57.5z"/> </g> </svg>')), "Category"),
      tags$div(class="nav-item", tags$span(class="ic", HTML('<svg fill="currentColor" viewBox="0 0 32 32" enable-background="new 0 0 32 32" id="Glyph" version="1.1" xml:space="preserve" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"><path d="M29.895,12.52c-0.235-0.704-0.829-1.209-1.549-1.319l-7.309-1.095l-3.29-6.984C17.42,2.43,16.751,2,16,2 s-1.42,0.43-1.747,1.122l-3.242,6.959l-7.357,1.12c-0.72,0.11-1.313,0.615-1.549,1.319c-0.241,0.723-0.063,1.507,0.465,2.046 l5.321,5.446l-1.257,7.676c-0.125,0.767,0.185,1.518,0.811,1.959c0.602,0.427,1.376,0.469,2.02,0.114l6.489-3.624l6.581,3.624 c0.646,0.355,1.418,0.311,2.02-0.114c0.626-0.441,0.937-1.192,0.811-1.959l-1.259-7.686l5.323-5.436 C29.958,14.027,30.136,13.243,29.895,12.52z" id="XMLID_328_"/></svg>')), "Popular"),
      tags$div(class="nav-item", tags$span(class="ic", HTML('<svg viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"> <path opacity="0.5" fill-rule="evenodd" clip-rule="evenodd" d="M2.25 5C2.25 4.58579 2.58579 4.25 3 4.25H15C15.4142 4.25 15.75 4.58579 15.75 5C15.75 5.41421 15.4142 5.75 15 5.75H3C2.58579 5.75 2.25 5.41421 2.25 5ZM2.25 9C2.25 8.58579 2.58579 8.25 3 8.25H13C13.4142 8.25 13.75 8.58579 13.75 9C13.75 9.41421 13.4142 9.75 13 9.75H3C2.58579 9.75 2.25 9.41421 2.25 9ZM2.25 13C2.25 12.5858 2.58579 12.25 3 12.25H9C9.41421 12.25 9.75 12.5858 9.75 13C9.75 13.4142 9.41421 13.75 9 13.75H3C2.58579 13.75 2.25 13.4142 2.25 13ZM2.25 17C2.25 16.5858 2.58579 16.25 3 16.25H8C8.41421 16.25 8.75 16.5858 8.75 17C8.75 17.4142 8.41421 17.75 8 17.75H3C2.58579 17.75 2.25 17.4142 2.25 17Z" fill="currentColor"/> <path d="M19.3446 5.99292C19.6232 5.89026 19.9559 5.80218 20.3149 5.86786C20.7572 5.94876 21.1513 6.19678 21.4156 6.56049C21.6302 6.85575 21.6948 7.19372 21.7228 7.48934C21.75 7.77705 21.75 8.13584 21.75 8.54466L21.75 8.57898L21.7501 8.6546C21.7509 8.9547 21.7518 9.27521 21.644 9.5705C21.559 9.8036 21.4254 10.016 21.252 10.1935C21.0324 10.4184 20.7431 10.5563 20.4722 10.6855L20.404 10.7181L18.6185 11.5751C18.25 11.752 17.9266 11.9073 17.6554 12.0072C17.3936 12.1036 17.0842 12.1872 16.75 12.1425V16.4286C16.75 18.2429 15.3147 19.7501 13.5 19.7501C11.6853 19.7501 10.25 18.2429 10.25 16.4286C10.25 14.6143 11.6853 13.1072 13.5 13.1072C14.1477 13.1072 14.747 13.2992 15.25 13.6286V10.0001H15.2529C15.25 9.83123 15.25 9.64894 15.25 9.45548L15.25 9.42114L15.2499 9.34553C15.2491 9.04542 15.2482 8.72491 15.356 8.42963C15.441 8.19652 15.5746 7.98416 15.748 7.80664C15.9676 7.58176 16.2569 7.44382 16.5278 7.31467L16.596 7.28206L18.3814 6.42504C18.75 6.24812 19.0734 6.09284 19.3446 5.99292Z" fill="currentColor"/> </svg>')), "Playlist"),
      tags$div(class="nav-item", tags$span(class="ic", HTML('<svg fill="currentColor" version="1.1" id="Capa_1" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" viewBox="0 0 294 294" xml:space="preserve"> <g> <path d="M59,15c0-8.284-6.716-15-15-15c-8.284,0-15,6.716-15,15v19h30V15z"/> <path d="M29,279c0,8.284,6.716,15,15,15c8.284,0,15-6.716,15-15V152H29V279z"/> <path d="M67.5,54h-47c-5.247,0-9.5,4.253-9.5,9.5v59c0,5.247,4.253,9.5,9.5,9.5h47c5.247,0,9.5-4.253,9.5-9.5v-59 C77,58.253,72.747,54,67.5,54z"/> <path d="M132,279c0,8.284,6.716,15,15,15c8.284,0,15-6.716,15-15v-15h-30V279z"/> <path d="M162,15c0-8.284-6.716-15-15-15c-8.284,0-15,6.716-15,15v131h30V15z"/> <path d="M170.5,166h-47c-5.247,0-9.5,4.253-9.5,9.5v59c0,5.247,4.253,9.5,9.5,9.5h47c5.247,0,9.5-4.253,9.5-9.5v-59 C180,170.253,175.747,166,170.5,166z"/> <path d="M235,279c0,8.284,6.716,15,15,15c8.284,0,15-6.716,15-15v-83h-30V279z"/> <path d="M265,15c0-8.284-6.716-15-15-15c-8.284,0-15,6.716-15,15v63h30V15z"/> <path d="M273.5,98h-47c-5.247,0-9.5,4.253-9.5,9.5v59c0,5.247,4.253,9.5,9.5,9.5h47c5.247,0,9.5-4.253,9.5-9.5v-59 C283,102.253,278.747,98,273.5,98z"/> </g> </svg>')), "Statistics")
    ),

    tags$div(class="main",

      tags$div(class="top-bar",
        tags$div(class="mode-switch", id="mode_switch",
                 onclick="Shiny.setInputValue('toggle_mode', Math.random())",
                 tags$div(class="knob")),
        tags$div(class="profile-mini",
          tags$div(class="profile-mini-avatar","G3"),
          tags$span(class="profile-mini-name","Group 3")
        )
      ),

      # Stat boxes (KPI cards) — shown first
      tags$p(class="stats-sub",
        paste0("Exploring ", nrow(music), " tracks across ", length(unique(music$track_genre)),
               " genres — the data behind the music")),
      tags$div(class="kpi-row",
        tags$div(class="kpi-card", style="--kpi-accent:#2BB5A0;",
          tags$div(class="kpi-value", nrow(music)), tags$div(class="kpi-label","Total Tracks")),
        tags$div(class="kpi-card", style="--kpi-accent:#FFB199;",
          tags$div(class="kpi-value", length(unique(music$track_genre))), tags$div(class="kpi-label","Unique Genres")),
        tags$div(class="kpi-card", style="--kpi-accent:#7C8CF8;",
          tags$div(class="kpi-value", round(mean(music$popularity),1)), tags$div(class="kpi-label","Avg Popularity")),
        tags$div(class="kpi-card", style="--kpi-accent:#FBBF24;",
          tags$div(class="kpi-value", round(mean(music$danceability),2)), tags$div(class="kpi-label","Avg Danceability"))
      ),

      # Hero carousel — paginated, 5 cards per page, up to 20 tracks
      tags$div(class="hero-section",
        tags$div(class="hero-viewport",
          tags$div(class="hero-track", id="hero_track",
            lapply(seq_len(nrow(hero_tracks)), function(i) {
              h <- hero_tracks[i,]
              tags$div(class="hero-card", style=paste0("background:",grad_for(i),";"),
                tags$div(class="hc-genre", h$track_genre),
                tags$div(class="hc-title", h$track_name),
                tags$div(class="hc-artist", h$main_artist)
              )
            })
          )
        ),
        tags$div(class="hero-dots", id="hero_dots")
      ),

      tags$script(HTML(sprintf("
        (function(){
          var viewport = document.querySelector('.hero-viewport');
          var track = document.getElementById('hero_track');
          var dotsWrap = document.getElementById('hero_dots');
          var cardW = 218, gapW = 14;
          var cardWidth = cardW + gapW;
          var perPage = 5;
          var totalCards = %d;
          var totalPages, page = 0;

          function computeLayout(){
            var available = viewport.parentElement.clientWidth;
            // How many whole cards fit in the available width (at least 1)
            var fitCount = Math.max(1, Math.floor((available + gapW) / cardWidth));
            perPage = Math.min(5, fitCount);
            var exactWidth = perPage * cardW + (perPage - 1) * gapW;
            viewport.style.width = exactWidth + 'px';
            totalPages = Math.ceil(totalCards / perPage);
            page = Math.min(page, totalPages - 1);
          }
          computeLayout();

          function renderDots(){
            dotsWrap.innerHTML = '';
            for (var p = 0; p < totalPages; p++) {
              var dot = document.createElement('div');
              dot.className = 'hero-dot' + (p === page ? ' active' : '');
              dot.dataset.page = p;
              dot.onclick = (function(pp){ return function(){ goToPage(pp); }; })(p);
              dotsWrap.appendChild(dot);
            }
          }
          renderDots();

          window.addEventListener('resize', function(){
            computeLayout();
            renderDots();
            goToPage(page);
          });

          function updateDots(){
            var dots = dotsWrap.querySelectorAll('.hero-dot');
            dots.forEach(function(d,i){ d.classList.toggle('active', i === page); });
          }
          function goToPage(p){
            page = Math.max(0, Math.min(totalPages - 1, p));
            var startIndex = page * perPage;
            var offset = -(startIndex * cardWidth);
            track.style.transform = 'translateX(' + offset + 'px)';
            updateDots();
          }
        })();
      ", nrow(hero_tracks)))),

      # Top Trending This Week + Genre Comparison — side by side
      tags$div(class="trend-compare-grid",

        # ── Left: Top Trending (boxed panel) ──
        tags$div(class="trend-panel",
          tags$div(class="sec-head",
            tags$div(class="sec-title","Top Trending This Week"),
            actionLink("view_all_trending", "View all", class="view-all")
          ),
          tags$div(class="genre-select-wrap full-width",
            selectInput("genre_select_A", NULL, choices=all_genres_sorted, selected=genre_A_default)),
          uiOutput("trend_cover_A"),
          uiOutput("trend_list_A")
        ),

        # ── Right: Genre Comparison (boxed panel) ──
        tags$div(class="compare-panel",
          tags$div(class="sec-head",
            tags$div(class="sec-title","Genre Comparison")
          ),
          tags$div(class="compare-pickers",
            tags$div(class="genre-select-wrap",
              selectInput("compare_genre1", NULL, choices=all_genres_sorted, selected=genre_A_default)),
            tags$div(class="genre-select-wrap",
              selectInput("compare_genre2", NULL, choices=all_genres_sorted, selected=genre_B_default))
          ),
          tags$div(class="compare-subtitle", textOutput("compare_subtitle", inline=TRUE)),
          tags$div(class="dna-bar-wrap", plotOutput("plot_genre_compare", height="280px"))
        )
      ),

      # Recommended for you
      tags$div(class="sec-head",
        tags$div(class="sec-title","Recommended for you"),
        actionLink("view_all_reco", "View all", class="view-all")
      ),
      tags$div(class="reco-row",
        lapply(seq_len(nrow(recommended)), function(i) {
          r <- recommended[i,]
          tags$div(class="reco-card",
            tags$div(class="reco-cover", style=paste0("background:",grad_for(i+4),";"), initials(r$track_genre)),
            tags$div(class="reco-name", r$track_name),
            tags$div(class="reco-artist", r$main_artist)
          )
        })
      )
    )
  )
)

server <- function(input, output, session) {
  dark_mode <- reactiveVal(TRUE)
  observeEvent(input$toggle_mode, {
    dark_mode(!dark_mode())
    session$sendCustomMessage("switch_theme", dark_mode())
  })

  # ── Live search (filters by track name or artist, prefix match) ──
  output$search_results <- renderUI({
    q <- input$search_query
    if (is.null(q) || nchar(trimws(q)) < 1) return(NULL)

    # Escape regex metacharacters one literal substitution at a time (fixed=TRUE, no regex parsing)
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

  # ── Reactive Top Trending panel (driven by single dropdown) ──
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

  # ── Genre Comparison DNA chart ─────────────────────────
  output$compare_subtitle <- renderText({
    paste0("DNA: ", input$compare_genre1, " vs ", input$compare_genre2, " — score per feature (0–1)")
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

  # "View all" modal — lets user browse every genre's top tracks
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

  # ── Track Detail Modal (reusable — opened from search or trending list) ──
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

shinyApp(ui, server)
