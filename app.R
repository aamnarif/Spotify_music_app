# ============================================================
# app.R - Main application entry point
# ============================================================

# ============================================================
# 1. Load packages, data, colours and shared objects
# ============================================================
source("src/data_setup.R", local = FALSE)

# ============================================================
# 2. Load helper functions
# ============================================================
source("src/helpers.R", local = FALSE)

# ============================================================
# 3. Load theme objects
#
# Depends on colour constants from data_setup.R
#
# Creates:
# - css_base
# - css_dark
# - css_light
# - theme_js
# ============================================================
source("src/components/theme.R", local = FALSE)

# ============================================================
# 4. Validate theme objects before loading UI
# ============================================================
required_theme_objects <- c(
  "css_dark",
  "css_light",
  "theme_js"
)

missing_theme_objects <- required_theme_objects[
  !vapply(
    required_theme_objects,
    exists,
    logical(1),
    inherits = TRUE
  )
]

if (length(missing_theme_objects) > 0) {
  stop(
    paste(
      "Missing theme objects:",
      paste(missing_theme_objects, collapse = ", ")
    )
  )
}

# ============================================================
# 5. Load UI
#
# ui.R can now safely use:
# - css_dark
# - css_light
# - theme_js
# ============================================================
source("src/ui.R", local = FALSE)

# ============================================================
# 6. Validate UI object
# ============================================================
if (!exists("ui", inherits = TRUE)) {
  stop(
    "The object 'ui' was not created by src/ui.R"
  )
}

# ============================================================
# 7. Load server
# ============================================================
source("src/server.R", local = FALSE)

# ============================================================
# 8. Validate server object
# ============================================================
if (!exists("server", inherits = TRUE)) {
  stop(
    "The object 'server' was not created by src/server.R"
  )
}

# ============================================================
# 9. Start Shiny application ONCE
# ============================================================
shiny::shinyApp(
  ui = ui,
  server = server
)