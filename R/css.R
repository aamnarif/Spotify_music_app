# ============================================================
# R/css.R – CSS strings and JavaScript for dynamic theming
# ============================================================

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

# JavaScript to swap stylesheets dynamically
theme_js <- sprintf("
  var DARK_CSS  = %s;
  var LIGHT_CSS = %s;
  Shiny.addCustomMessageHandler('switch_theme', function(dark) {
    document.getElementById('dyn_css').innerHTML = dark ? DARK_CSS : LIGHT_CSS;
  });
",
  paste0('"', gsub('"', '\\\\"', gsub('\n', ' ', css_dark)),  '"'),
  paste0('"', gsub('"', '\\\\"', gsub('\n', ' ', css_light)), '"')
)