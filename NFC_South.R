rm(list = ls()) # Clear environment

# Install packages if necessary 
#install.packages("tidyverse", type = "binary")
#install.packages("ggrepel", type = "binary")
#install.packages("nflreadr", type = "binary")
#install.packages("nflplotR", type = "binary")

library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(dplyr)

options(scipen = 9999) # Removes scientific notation

# https://nflfastr.com/articles/beginners_guide.html#get-team-wins-each-season

# Load data
games <- nflreadr::load_schedules()
str(games) # View available data

# Load home games
home <- games |>
  filter(game_type == 'REG') |>
  select(season, week, home_team, result) |>
  rename(team = home_team)
home |> head(5)

# Load away games
away <- games |>
  filter(game_type == 'REG') |>
  select(season, week, away_team, result) |>
  rename(team = away_team) |>
  mutate(result = -result)
away |> head(5)

# Combine
results <- bind_rows(home, away) |>
  arrange(week) |>
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )

# Load team wins
team_wins <- results |>
  group_by(team, season) |>
  summarize(
    wins = sum(win),
    point_diff = sum(result)) |>
  ungroup()

# Filter most wins from past 3 seasons
team_wins_s22_24 <- team_wins |>
  filter(season %in% c("2022", "2023", "2024")) |>
  arrange(-wins)
print(team_wins_s22_24, n=10)

# Filter most combined team wins from past 3 seasons
team_wins_3Y <- team_wins |>
  filter(season %in% c("2022", "2023", "2024")) |>
  group_by(team) |>
  summarize(total_wins = sum(wins, na.rm = TRUE)) |>
  arrange(-total_wins)
print(team_wins_3Y, n=32)

# Create division wins
divisions <- team_wins_3Y |>
  mutate(division = case_when(
    team %in% c("ATL", "CAR", "NO", "TB") ~ "NFCSouth",
    team %in% c("CHI", "DET", "GB", "MIN") ~ "NFCNorth",
    team %in% c("NYG", "PHI", "WAS", "DAL") ~ "NFCEast", 
    team %in% c("ARI","SEA", "SF", "LA") ~ "NFCWest",
    team %in% c("HOU", "JAX", "TEN", "IND") ~ "AFCSouth",
    team %in% c("BAL", "CIN", "CLE", "PIT") ~ "AFCNorth",
    team %in% c("BUF", "NE", "NYJ", "MIA") ~ "AFCEast",
    team %in% c("DEN", "KC", "LAC", "LV") ~ "AFCWest"))

print(divisions, n=32)

# Rank divisions by wins
div_wins <- divisions |>
  group_by(division) |>
  summarise(total_wins = sum(total_wins)) |>
  arrange(-total_wins)

print("2022-2024 Division Wins")
print(div_wins)
view(div_wins)


# Load play-by-play data
pbp_all <- load_pbp(2025)
str(pbp_all) # View all the columns available in pbp

### Yards Plot
library(nflplotR)
# get pbp and filter to regular season rush and pass plays
pbp <- nflreadr::load_pbp(2025) |>
  dplyr::filter(season_type == "REG") |>
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))
# offense yards_gained
offense_yds <- pbp |>
  dplyr::group_by(team = posteam) |>
  dplyr::summarise(off_yds = mean(yards_gained, na.rm = TRUE))
# defense yards_gained
defense_yds <- pbp |>
  dplyr::group_by(team = defteam) |>
  dplyr::summarise(def_yds = mean(yards_gained, na.rm = TRUE))
# make figure
offense_yds |>
  dplyr::inner_join(defense_yds, by = "team") |>
  ggplot2::ggplot(aes(x = off_yds, y = def_yds)) +
  # tier lines
  ggplot2::geom_abline(slope = -1.5, intercept = (4:-3)/10, alpha = .2) +
  # nflplotR magic
  nflplotR::geom_mean_lines(aes(y0 = off_yds, x0 = def_yds)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense Yds/play",
    y = "Defense Yds/play",
    caption = "Data: @nflfastR",
    title = "2025 NFL Offensive and Defensive Yards per Play"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  ggplot2::scale_y_reverse()

### EPA Plot
library(nflplotR)
# get pbp and filter to regular season rush and pass plays
pbp <- nflreadr::load_pbp(2025) |>
  dplyr::filter(season_type == "REG") |>
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))
# offense epa
offense <- pbp |>
  dplyr::group_by(team = posteam) |>
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))
# defense epa
defense <- pbp |>
  dplyr::group_by(team = defteam) |>
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
# make figure
offense |>
  dplyr::inner_join(defense, by = "team") |>
  ggplot2::ggplot(aes(x = off_epa, y = def_epa)) +
  # tier lines
  ggplot2::geom_abline(slope = -1.5, intercept = (4:-3)/10, alpha = .2) +
  # nflplotR magic
  nflplotR::geom_mean_lines(aes(y0 = off_epa, x0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2025 NFL Offensive and Defensive EPA per Play"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  ggplot2::scale_y_reverse()






