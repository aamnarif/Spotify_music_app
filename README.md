# Group_3_Applied_Analytics

# Solace — Spotify Music Analytics Dashboard

Solace is an interactive music analytics dashboard developed using R Shiny. The application transforms Spotify track data into an accessible visual experience for exploring music trends, audio characteristics, genre patterns, popularity, playlists, and statistical relationships.

The project was developed as part of the DSA8045 Applied Analytics module at Queen's University Belfast.

---

## Project Overview

Music datasets often contain a large number of numerical and categorical variables that can be difficult to interpret directly. Solace provides an interactive dashboard that allows users to explore the dataset visually and understand patterns across tracks, artists, genres, popularity scores, and audio features.

The dashboard combines descriptive analytics, interactive visualisations, filtering, search functionality, and statistical exploration within a single Shiny application.

The application is organised into five main analytical sections:

- Dashboard
- Categories
- Popularity
- Playlist
- Statistics

---

## Key Features

### Dashboard

The Dashboard provides a high-level overview of the music dataset.

It includes:

- Total number of tracks
- Number of unique artists
- Average track popularity
- Number of genres
- Top tracks
- Featured track spotlight
- Popularity distribution
- Audio feature profile
- Genre overview
- Dataset-wide summary statistics

The dashboard acts as the main entry point for understanding the overall structure of the dataset.

---

### Categories

The Categories section allows users to explore tracks by genre.

Users can:

- Browse top genres
- Select a genre
- View the number of tracks in the selected genre
- Compare genre popularity with the overall dataset
- Explore energy and danceability levels
- View top tracks within the selected genre
- Compare genre audio characteristics against dataset averages

This section helps reveal how different genres vary in popularity and musical characteristics.

---

### Popularity

The Popularity section focuses on track popularity patterns.

It includes:

- Popularity distribution
- Top-ranked tracks
- Track popularity scores
- Artist information
- Genre information
- Energy levels
- Danceability values
- Interactive track detail exploration

Users can select tracks and inspect additional information through interactive detail views.

---

### Playlist

The Playlist section provides a personalised track discovery experience.

Users can:

- Filter tracks by genre
- Set a minimum popularity threshold
- Filter by energy level
- Filter by danceability
- Generate a personalised playlist
- Shuffle playlist results
- Inspect individual tracks
- View playlist audio characteristics

The playlist generator helps users explore tracks based on selected musical preferences.

---

### Statistics

The Statistics section provides deeper analytical exploration of the dataset.

It includes:

- Correlation matrix
- Interactive scatter plots
- Variable selection
- Linear trend lines
- R² model strength
- Pearson correlation
- Statistical significance testing
- Top tracks ranked by popularity
- Explanations of analytical results

This section helps users examine relationships between Spotify audio features and track popularity.

---

## Search Functionality

Solace includes an interactive search system that allows users to search for tracks and artists.

The search interface provides:

- Live search suggestions
- Track title matching
- Artist name matching
- Popularity information
- Genre information
- Interactive track detail modal

Selecting a search result opens a detailed track view containing additional audio and metadata information.

---

## Theme Support

The application supports both dark and light themes.

The interface includes:

- Dynamic theme switching
- Custom dark-mode styling
- Custom light-mode styling
- Theme-aware charts
- Theme-aware controls
- Responsive navigation components

The selected theme is applied dynamically across the dashboard.

---

## Technology Stack

The project is built using:

- R
- Shiny
- dplyr
- ggplot2
- plotly
- tidyr
- DT
- scales
- htmltools
- Custom CSS
- JavaScript

---

## Project Structure

```text
Group_3_Applied_Analytics/
│
├── app.R
├── Group3_music.csv
├── README.md
│
├── app_files/
│   ├── ui.R
│   └── server.R
│
├── R/
│   ├── data_setup.R
│   ├── helpers.R
│   │
│   ├── components/
│   │   ├── theme.R
│   │   ├── search.R
│   │   └── modals.R
│   │
│   └── tabs/
│       ├── dashboard.R
│       ├── category.R
│       ├── popular.R
│       ├── playlist.R
│       └── statistics.R
│
└── www/
