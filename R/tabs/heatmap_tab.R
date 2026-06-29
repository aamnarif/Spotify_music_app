# ============================================================
# R/tabs/heatmap_tab.R – Correlation Heatmap tab UI
# ============================================================

heatmap_tab_ui <- tabItem(tabName = "heatmap",
  fluidRow(column(12,
    tags$div(class = "section-header", "Correlation Matrix"),
    tags$div(class = "section-sub",
             "How strongly do audio features relate to each other and to popularity?")
  )),
  fluidRow(
    box(width = 3, title = "🎚 Controls", solidHeader = TRUE, status = "primary",
      selectInput("heatmap_genre", "Filter by Genre:",
                  choices = c("All Genres", sort(valid_genres)), selected = "All Genres"),
      checkboxGroupInput("heatmap_features", "Features:",
                         choices  = audio_features,
                         selected = unname(audio_features)),
      checkboxInput("heatmap_popularity", "Include Popularity", value = TRUE),
      tags$hr(),
      downloadButton("dl_heatmap", "⬇ Download Data", class = "btn-dl")
    ),
    box(width = 9, title = "📊 Correlation Heatmap", solidHeader = TRUE, status = "primary",
      plotOutput("plot_heatmap", height = "520px"))
  )
)