library(shiny)
library(dplyr)
library(ggplot2)
library(tidyr)

music <- read.csv("Group3_music.csv", stringsAsFactors = FALSE)
clean_artist <- function(x) trimws(strsplit(x, "[;,]")[[1]][1])
fmt_dur <- function(ms) {
  s <- as.integer(round(ms / 1000))
  sprintf("%d:%02d", s %/% 60L, s %% 60L)
}

ACCENT_DARK <- "#1F8A70"   
ACCENT_GRAD1 <- "#2BB5A0"
ACCENT_GRAD2 <- "#1F8A70"

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
