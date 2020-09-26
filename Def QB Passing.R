#importing libraries

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(arrow)
library(nflfastR)

#get df data from 2015 to 2020
seasons <- seq(2019,2020)
df <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.parquet"), "tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet")
  return(df)
}
)

roster <- fast_scraper_roster(seasons)

#rb receiver plots
#team and name of player, plus props for graph
def = 'DAL'
spread = -3.5
prop = 220
poz = "QB"

scatter <- df %>%
  mutate(is_home = ifelse(home_team == def,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                            ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == def, "y", "n")) %>%
  filter(defteam == def, !is.na(epa), pass_attempt == 1, season >= 2019) %>%
  select(passer_id, season, game_id, passer, is_home, week, team_spread, yardline_100, yards_gained, epa, spread_line, total_line, touchdown, winner) %>%
  group_by(passer_id, game_id, season, passer, is_home, week, team_spread, total_line, winner) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown)) %>%
  nflfastR::decode_player_ids()

scatter <- scatter %>%
  merge(roster[,c("position", "gsis_id")], by.x='passer_id', by.y= 'gsis_id') %>%
  filter(position == poz) %>%
  group_by(game_id)%>%
  slice(which.max(yds_gained))

#get cols
col1 <- teams_colors_logos %>%
  filter(team_abbr == def) %>%
  select(team_color)
col1 <- as.character(col1[1,1])

col2 <- teams_colors_logos %>%
  filter(team_abbr == def) %>%
  select(team_color2)
col2 <- as.character(col2[1,1])

col3 <- teams_colors_logos %>%
  filter(team_abbr == def) %>%
  select(team_color3)
col3 <- as.character(col3[1,1])

scatter %>%
  ggplot(aes(x=team_spread-1,y=yds_gained-3, color = is_home)) +
  geom_point(cex = 3)+
  annotate(geom = "text",x=scatter$team_spread,y=scatter$yds_gained, label = scatter$passer, cex = 2.5) +
  scale_color_manual(values=c(col2,col1)) +
  theme_bw() +
  labs(x= "Vegas Spread", y = "Passing Yards", title = paste(def, "Defense Yards Allowed"), subtitle = "Home vs Away Splits", caption = 
         "Source: @giantsportsball | Data from @nflfastR") +
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread ), linetype = "dashed", color = "grey")
