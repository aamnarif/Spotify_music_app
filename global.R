# ============================================================
# global.R – Packages, Data Loading & Shared Variables
# DSA8045 – Applied Analytics | Group 3 – Spotify Dashboard
# ============================================================

# ── Packages ─────────────────────────────────────────────────
library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)

# ── Load Data ─────────────────────────────────────────────────
# Make sure Group3_music.csv is inside your data/ folder
music <- read.csv("data/Group3_music.csv", stringsAsFactors = FALSE)

# ── Derived Columns ───────────────────────────────────────────
music$duration_min   <- round(music$duration_ms / 60000, 2)
music$mode_label     <- ifelse(music$mode == 1, "Major", "Minor")
music$explicit_label <- ifelse(music$explicit, "Explicit", "Clean")

# Key names (standard pitch-class notation)
key_labels   <- c("C","C#/Db","D","D#/Eb","E","F","F#/Gb","G","G#/Ab","A","A#/Bb","B")
music$key_name <- key_labels[music$key + 1]

# ── Filtered Dataset (genres with 5+ tracks for boxplots) ─────
genre_counts  <- table(music$track_genre)
valid_genres  <- names(genre_counts[genre_counts >= 5])
music_valid   <- music[music$track_genre %in% valid_genres, ]

# ── Audio Feature Choices (used across all tabs) ──────────────
audio_features <- c(
  "Danceability"     = "danceability",
  "Energy"           = "energy",
  "Valence (Mood)"   = "valence",
  "Acousticness"     = "acousticness",
  "Speechiness"      = "speechiness",
  "Instrumentalness" = "instrumentalness",
  "Liveness"         = "liveness"
)