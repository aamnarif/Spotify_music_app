# ============================================================
# R/server.R – Main Server
# Calls each module's server function
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================

server <- function(input, output, session) {
  
  # Each module server handles its own tab's outputs
  overview_server(input, output)      # Tab 1 plots
  audio_server(input, output)         # Tab 2 plots
  popularity_server(input, output)    # Tab 3 plots + table
  
  # Note: conclusions_server not needed — Tab 4 is static content only
  
}