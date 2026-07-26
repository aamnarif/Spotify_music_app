# CSS
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
  .tvt-select .selectize-input { height:38px !important; min-height:38px !important;
    padding:8px 12px !important; font-size:11.5px !important; border-radius:9px !important;
    white-space:nowrap; overflow:hidden; text-overflow:ellipsis; }
  .tvt-select .selectize-control { width:100% !important; }
  .tvt-select .selectize-dropdown { min-width:100% !important; font-size:11.5px !important; z-index:9999 !important; }
  .tvt-select .selectize-dropdown-content { max-height:240px !important; overflow-y:auto !important; overflow-x:hidden !important; }
  .tvt-select .selectize-input.dropdown-active { border-bottom-left-radius:0 !important; border-bottom-right-radius:0 !important; }
  #pl_download_csv i { display:none !important; }
  #pl_download_csv { box-shadow:none !important; }
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

  /* Category page */
  .cat-stat-chip { border-radius:10px; padding:8px 14px; font-size:11.5px; font-weight:700; white-space:nowrap; }
  [id^='mood_tab_'] { transition:background 0.15s; }"

css_light <- paste0(css_base, "
  :root { --row-active:#EFF8F6; --border:#ECECEF; --accent-grad:linear-gradient(135deg,", ACCENT_GRAD1, ",", ACCENT_GRAD2, "); }
  body { background:#FAFAFB; color:#15161A; }
  :root { --card:#FFFFFF; --border:#ECECEF; }
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
  :root { --card:#1A1A1A; --border:#2A2A2A; }
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
