#importing libraries

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(arrow)
library("nflfastR")

#get pbp data from 2015 to 2020
seasons <- seq(2015,2020)
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.parquet"), "tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet")
  return(df)
}
)

#rb receiver plots
#team and name of player, plus props for graph
def = 'MIN'
spread = -3
prop = 60

scatter <- pbp %>%
  mutate(is_home = ifelse(home_team == def,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                            ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == def, "y", "n")) %>%
  filter(defteam == def, !is.na(epa), rush == 1, season >= 2019) %>%
  select(season, game_id, rusher, is_home, week, team_spread, yardline_100, yards_gained, epa, spread_line, total_line, touchdown, winner) %>%
  group_by(game_id, season, rusher, is_home, week, team_spread, total_line, winner) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown)) %>%
  group_by(game_id) %>%
  slice(which.max(yds_gained))

scatter %>%
  ggplot(aes(x=team_spread,y=yds_gained, color = is_home)) +
  geom_jitter(height = 0.00, width = 0.00)+
  scale_color_manual(values=c("#D7A22A","#006778")) +
  theme_bw() +
  labs(x= "Vegas Spread", y = "Rushing Yards", title = paste(def, "Defense Yards Allowed"), subtitle = "Home vs Away Splits", caption = 
         "Source: @giantsportsball | Data from @nflfastR") +
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread ), linetype = "dashed", color = "grey")
