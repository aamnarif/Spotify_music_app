# ============================================================
# global.R – Load libraries, source all modules
# ============================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# ── Source utility files ──────────────────────────────────
source("R/data_loader.R")
source("R/theme.R")
source("R/helpers.R")

# ── Source tab UI definitions ─────────────────────────────
source("R/tabs/overview_tab.R")
source("R/tabs/audio_tab.R")
source("R/tabs/popularity_tab.R")
source("R/tabs/missing_tab.R")
source("R/tabs/heatmap_tab.R")
source("R/tabs/conclusions_tab.R")

# ── Source main UI and server ─────────────────────────────
source("ui.R")
source("server.R")