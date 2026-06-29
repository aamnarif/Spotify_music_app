# ============================================================
# R/data_loader.R – Data loading and preprocessing
# ============================================================

music <- read.csv("data/Group3_music.csv", stringsAsFactors = FALSE)

music$duration_min   <- round(music$duration_ms / 60000, 2)
music$mode_label     <- ifelse(music$mode == 1, "Major", "Minor")
music$explicit_label <- ifelse(music$explicit, "Explicit", "Clean")

key_labels   <- c("C","C#/Db","D","D#/Eb","E","F","F#/Gb","G","G#/Ab","A","A#/Bb","B")
music$key_name <- key_labels[music$key + 1]

genre_counts  <- table(music$track_genre)
valid_genres  <- names(genre_counts[genre_counts >= 5])
music_valid   <- music[music$track_genre %in% valid_genres, ]

audio_features <- c(
  "Danceability"     = "danceability",
  "Energy"           = "energy",
  "Valence (Mood)"   = "valence",
  "Acousticness"     = "acousticness",
  "Speechiness"      = "speechiness",
  "Instrumentalness" = "instrumentalness",
  "Liveness"         = "liveness"
)