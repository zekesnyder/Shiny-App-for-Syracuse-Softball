# Syracuse Softball Analytics Dashboard

An interactive R Shiny app that turns softball game data into hitting and pitching analysis that coaches and players can explore without writing code. It is styled in Syracuse orange and blue.



## What It Does

- **Hitting analysis:** view batter performance and compare players, with splits against left-handed and right-handed pitching
- **Pitching analysis:** evaluate pitchers by results, pitch outcomes, and opponent
- **Spray charts:** see where each hitter puts the ball in play
- **Leaderboards:** rank players across batting, pitching, and advanced metrics
- **Filters:** narrow any view by date range, opponent, inning

## Why I Built It

Coaches often have plenty of game data but no quick way to ask questions of it, like how a hitter does against lefties or where she tends to hit the ball. I built this app so those questions can be answered in a few clicks instead of a spreadsheet session.

## Data

- **Source:** Game-By-Game data
- **Coverage:** 2024 Season
- **Preparation:** Cleaned and standardized in R with the tidyverse

> The repository contains sample data. 

## Built With

- **R** and **Shiny** (with `shinydashboard` for the layout)
- **tidyverse** (`dplyr`, `ggplot2`, `lubridate`) for data wrangling and charts
- **DT** for interactive tables
- Custom CSS for Syracuse branding

## Run It Locally

```r
# Install packages once
install.packages(c("shiny", "shinydashboard", "DT", "ggplot2", "dplyr", "lubridate"))

# Run the app from the project folder
shiny::runApp()
```

## Repository Contents

```
├── app.R            # Main Shiny app (UI and server)
├── data/            # Sample data
└── README.md
```

## Author

**Zeke Snyder**, Sport Analytics, Syracuse University
