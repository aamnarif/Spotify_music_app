# ============================================================
# app.R – Entry Point
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================

source("global.R")
source("R/theme.R")
source("R/mod_overview.R")
source("R/mod_audio.R")
source("R/mod_popularity.R")
source("R/mod_conclusions.R")
source("R/ui.R")
source("R/server.R")

shinyApp(ui = ui, server = server)