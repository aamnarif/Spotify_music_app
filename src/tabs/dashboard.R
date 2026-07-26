# ============================================================
# R/tabs/dashboard.R
# Exact dashboard_page_ui code extracted from the original working app.R
# ============================================================

dashboard_page_ui <- function() {
    tagList(
    tags$div(style="font-size:22px;font-weight:800;margin-bottom:18px;", "Dashboard"),
    # Stat boxes (KPI cards) — shown first
    tags$p(class="stats-sub",
      paste0("Exploring ", nrow(music), " tracks across ", length(unique(music$track_genre)),
           " genres, the data behind the music")),
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

    # Hero carousel
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

    #Top Trending & Genre Comparison
    tags$div(class="trend-compare-grid",

      #Top Trending 
      tags$div(class="trend-panel",
        tags$div(class="sec-head",
        tags$div(class="sec-title","Top Trending This Week"),
        actionLink("view_all_trending", "View", class="view-all")
        ),
        tags$div(class="genre-select-wrap full-width",
        selectInput("genre_select_A", NULL, choices=all_genres_sorted, selected=genre_A_default)),
        uiOutput("trend_cover_A"),
        uiOutput("trend_list_A")
      ),

      #Genre Comparison
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

    #Recommended
    tags$div(class="sec-head",
      tags$div(class="sec-title","Recommended for you"),
      actionLink("view_all_reco", "View", class="view-all")
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
  }
