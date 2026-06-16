# ♫ Solace — Spotify Music Analytics Dashboard
### DSA8045 Applied Analytics | Assignment 1 | Group 3

---

## Overview

**Solace** is an interactive R Shiny dashboard built for DSA8045 Applied Analytics. It explores a dataset of 700 Spotify tracks across 114 genres, exposing audio feature insights, popularity trends, and genre comparisons that mainstream platforms like Spotify and YouTube Music don't surface to their users.

The dashboard is designed for a non-technical audience, with a modern dark/light-mode UI inspired by YouTube Music.

---

## Dataset

| Property | Detail |
|---|---|
| File | `Group3_music.csv` |
| Tracks | 700 |
| Genres | 114 unique |
| Variables | 20 |
| Missing values | None |

Key variables used: `track_name`, `artists`, `track_genre`, `popularity`, `danceability`, `energy`, `valence`, `acousticness`, `speechiness`, `instrumentalness`, `liveness`, `tempo`, `explicit`, `duration_ms`

---

## Requirements

### R Packages
Install all required packages by running this once in RStudio:

```r
install.packages(c("shiny", "shinydashboard", "ggplot2", "dplyr"))
```

### R Version
Developed and tested on R 4.x. No additional system dependencies required.

---

## File Structure

```
/
├── app.R               # Main Shiny application
├── Group3_music.csv    # Spotify dataset (must be in same folder as app.R)
└── README.md           # This file
```

> Both `app.R` and `Group3_music.csv` **must be in the same folder** for the app to run.

---

## How to Run

1. Open **RStudio**
2. Go to **File → Open File** and open `app.R`
3. Click the **Run App** button (top right of the editor)
4. The dashboard will open in your browser or RStudio viewer

---

## How to Export HTML (for submission)

1. Run the app and click **Open in Browser**
2. In your browser, press `Ctrl+S` (Windows) or `Cmd+S` (Mac)
3. Save as **Webpage, Complete**

---

## Dashboard Structure

| Tab | Description |
|---|---|
| Home | Dataset overview, dynamic KPI cards, genre distribution, tempo, and explicit breakdown |
| Audio Explorer | Scatter plot and boxplot of audio features with genre filtering |
| Popularity | Genre popularity rankings, feature vs popularity scatter, top tracks table |
| What's Missing | Unique tab — genre vs genre audio DNA comparison, audio profile per genre, speechiness analysis |
| Correlations | Full correlation heatmap of audio features with genre filter |
| Conclusions | Key findings, recommendations, and dashboard limitations |

---

## Features

- **Dark / Light mode toggle** — full theme switch including sidebar, cards, plots, and controls
- **Dynamic KPI cards** — Total Tracks, Unique Genres, Avg Popularity, Avg Danceability all react to the explicit content filter
- **Download buttons** — filtered data downloadable as CSV on every tab
- **Reactive visualisations** — all plots update instantly based on user input
- **"What's Missing" tab** — exposes insights Spotify and YouTube Music don't show users:
  - Side-by-side genre audio DNA comparison
  - Full audio feature profile per genre
  - Speechiness as a hidden genre identity marker

---

## Interactive Controls Summary

| Tab | Controls |
|---|---|
| Home | Top N genres slider, explicit content checkbox |
| Audio Explorer | X/Y axis feature dropdowns, genre filter, boxplot feature selector, N genres slider |
| Popularity | Top N genres slider, explicit filter checkboxes, feature selector, top N tracks slider |
| What's Missing | Genre A/B dropdowns, audio profile genre selector, speechiness N slider |
| Correlations | Genre filter dropdown, feature checkboxes, popularity toggle |

---

## Design

- Font: **Inter** (Google Fonts)
- Dark theme: `#0F0F0F` background, `#FF0000` accent
- Light theme: `#F4F4F4` background, `#CC0000` accent
- All ggplot2 charts use a custom `gg_theme()` function that adapts to dark/light mode reactively
