# ============================================================
# R/tabs/category.R
# Exact category_page_ui code extracted from the original working app.R
# ============================================================

category_page_ui <- function(dark) {
    bg_card   <- if (dark) "#1A1A1A" else "#FFFFFF"
    border    <- if (dark) "#2A2A2A" else "#ECECEF"
    text_col  <- if (dark) "#FFFFFF" else "#15161A"
    sub_col   <- if (dark) "#9A9A9A" else "#8A8DA3"
    input_bg  <- if (dark) "#1E1E1E" else "#F4F5F7"

    tagList(
      tags$div(style=paste0("font-size:22px;font-weight:800;color:",text_col,";margin-bottom:18px;"), "Category"),
      #Dynamic subtitle
      uiOutput("cat_subtitle"),

      #Filter controls
      tags$div(style="display:flex;align-items:center;gap:14px;margin-bottom:20px;flex-wrap:wrap;",
        #Mood tabs
        tags$div(style="display:flex;gap:6px;flex-wrap:wrap;",
          lapply(c("All","High Energy","Happy","Chill","Dark"), function(m) {
            col <- if (m == "All") "#2BB5A0" else mood_colors[m]
            tags$div(id=paste0("mood_tab_",gsub(" ","_",m)),
              style=paste0("padding:6px 12px;border-radius:20px;font-size:11.5px;font-weight:700;",
                           "cursor:pointer;border:1.5px solid ",col,";color:",col,";"),
              onclick=paste0("Shiny.setInputValue('cat_mood','",m,"',{priority:'event'});",
                             "document.querySelectorAll('[id^=mood_tab_]').forEach(function(x){x.style.background='transparent';});",
                             "this.style.background='",col,"22';"),
              m
            )
          })
        ),
        
        tags$div(style=paste0("display:flex;align-items:center;gap:8px;padding:9px 14px;border-radius:10px;",
                              "background:",input_bg,";border:1px solid ",border,";width:220px;flex-shrink:0;"),
          tags$span(style=paste0("opacity:0.5;color:",text_col,";display:flex;align-items:center;"),
            HTML('<svg width="15" height="15" viewBox="0 0 24 24" fill="none" xmlns="http://www.w3.org/2000/svg"><path d="M10.77 18.3C9.2807 18.3 7.82485 17.8584 6.58655 17.031C5.34825 16.2036 4.38311 15.0275 3.81318 13.6516C3.24325 12.2757 3.09413 10.7616 3.38468 9.30096C3.67523 7.84029 4.39239 6.49857 5.44548 5.44548C6.49857 4.39239 7.84029 3.67523 9.30096 3.38468C10.7616 3.09413 12.2757 3.24325 13.6516 3.81318C15.0275 4.38311 16.2036 5.34825 17.031 6.58655C17.8584 7.82485 18.3 9.2807 18.3 10.77C18.3 11.7588 18.1052 12.738 17.7268 13.6516C17.3484 14.5652 16.7937 15.3953 16.0945 16.0945C15.3953 16.7937 14.5652 17.3484 13.6516 17.7268C12.738 18.1052 11.7588 18.3 10.77 18.3ZM10.77 4.74999C9.58331 4.74999 8.42327 5.10189 7.43657 5.76118C6.44988 6.42046 5.68084 7.35754 5.22672 8.45389C4.77259 9.55025 4.65377 10.7566 4.88528 11.9205C5.11679 13.0844 5.68824 14.1535 6.52735 14.9926C7.36647 15.8317 8.43556 16.4032 9.59945 16.6347C10.7633 16.8662 11.9697 16.7474 13.0661 16.2933C14.1624 15.8391 15.0995 15.0701 15.7588 14.0834C16.4181 13.0967 16.77 11.9367 16.77 10.75C16.77 9.15869 16.1379 7.63257 15.0126 6.50735C13.8874 5.38213 12.3613 4.74999 10.77 4.74999Z" fill="currentColor"/><path d="M20 20.75C19.9015 20.7504 19.8038 20.7312 19.7128 20.6934C19.6218 20.6557 19.5392 20.6001 19.47 20.53L15.34 16.4C15.2075 16.2578 15.1354 16.0697 15.1388 15.8754C15.1422 15.6811 15.221 15.4958 15.3584 15.3583C15.4958 15.2209 15.6812 15.1422 15.8755 15.1388C16.0698 15.1354 16.2578 15.2075 16.4 15.34L20.53 19.47C20.6704 19.6106 20.7493 19.8012 20.7493 20C20.7493 20.1987 20.6704 20.3893 20.53 20.53C20.4608 20.6001 20.3782 20.6557 20.2872 20.6934C20.1962 20.7312 20.0985 20.7504 20 20.75Z" fill="currentColor"/></svg>')
          ),
          tags$input(id="cat_search", type="text", placeholder="Filter genres...",
                     oninput="Shiny.setInputValue('cat_search', this.value)",
                     style=paste0("border:none;outline:none;background:transparent;",
                                  "font-size:12.5px;color:",text_col,";flex:1;"))
        )
      ),

      tags$div(style="display:grid;grid-template-columns:1fr 380px;gap:20px;align-items:start;",
        # Genre grid
        uiOutput("cat_genre_grid"),
        # Detail panel
        uiOutput("cat_detail_panel")
      )
    )
  }
