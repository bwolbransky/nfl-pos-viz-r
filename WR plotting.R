#importing libraries. Only need to run this at beginning of session

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(arrow)
library(nflfastR)

#update seasons to get get play by play (pbp) data from from (start date, end date)

seasons <- seq(2017,2020)
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.parquet"), "tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet")
  return(df)
}
)

#Receiving plots, can use RB or WR for receptions.
#team and 'F.Last' of player, plus props for graph. 
#If no props, then leave blank. Spread can be Over/Under or spread. 

player = 'J.Cook'
spread = 3.5
prop = 44.5
team = 'NO'


#Grabbing pbp data and properly format spread, home/away team, and winner.

scatter <- pbp %>%
  mutate(is_home = ifelse(home_team == posteam,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                         ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == posteam, "y", "n")) %>%
  filter(receiver == player, !is.na(epa), season >= 2017,posteam == team) %>%
  select(defteam, season, posteam, receiver, is_home, week, total_line, team_spread, yardline_100, yards_gained, epa, spread_line, touchdown, winner) %>%
  group_by(defteam, receiver,posteam, winner, season,is_home, week, total_line, spread_line,team_spread) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown), rec = n()) %>%
  group_by(receiver, posteam, defteam, season, is_home,team_spread,total_line, winner)  

#get colors
col1 <- teams_colors_logos %>%
  filter(team_abbr == team) %>%
  select(team_color)
col1 <- as.character(col1[1,1])

col2 <- teams_colors_logos %>%
  filter(team_abbr == team) %>%
  select(team_color2)
col2 <- as.character(col2[1,1])

#plot
scatter %>%
  #use total_line for over/under team_spread for spread
  ggplot(aes(x=team_spread,y=yds_gained, color = is_home)) +
  geom_jitter(width = .2)+
  scale_color_manual(values=c(col2,col1)) +
  theme_bw() +
  theme(title = element_text(size =8))+
  labs(x= "Vegas Spread", y = "Receiving Yards", title = paste(player, "Home vs Away Splits"), subtitle = "vs Vegas Spread", caption = 
         "Source: @giantsportsball | Data from @nflfastR") +
  #if no props, comment out these lines or delete them.
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread), linetype = "dashed", color = "grey")



