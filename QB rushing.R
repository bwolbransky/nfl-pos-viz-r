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
team = 'BAL' 
player = 'L.Jackson'
spread = 3.5
prop = 62.5

scatter <- pbp %>%
  mutate(is_home = ifelse(home_team == posteam,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                            ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == posteam, "y", "n")) %>%
  filter(name == player, !is.na(epa), str_detect(desc, "scramble") | str_detect(rusher, player)) %>%
  select(game_id, season, name, is_home, week, team_spread, yardline_100, yards_gained, epa, spread_line, total_line, touchdown, winner) %>%
  group_by(name, game_id, winner, season,is_home, week, spread_line,team_spread, total_line) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown)) %>%
  group_by(game_id, name, season, is_home,team_spread, winner,total_line)  

#plot
scatter %>%
  filter(season >= 2019) %>%
  ggplot(aes(x=team_spread,y=yds_gained, color = is_home)) +
  geom_jitter(height = 0, width = 0)+
  scale_color_manual(values=c("#D7A22A","#006778")) +
  theme_bw() +
  labs(x= "Vegas Spread", y = "Rushing Yards", 
       title = paste(player,"Home vs Away Splits"),
       subtitle = "By Vegas Spread", 
       caption = "Source: @giantsportsball | Data from @nflfastR") +
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread ), linetype = "dashed", color = "grey")
