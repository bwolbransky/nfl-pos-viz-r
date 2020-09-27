#importing libraries. Only need to run this at beginning of session

library(tidyverse)
library(dplyr)
library(na.tools)
library(ggimage)
library(arrow)
library(nflfastR)

#update seasons to get get play by play (pbp) data from from (start date, end date)

seasons <- seq(2018,2020)
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.parquet"), "tmp.parquet")
  df <- arrow::read_parquet("tmp.parquet")
  return(df)
}
)

#Receiving plots, can use RB or WR for receptions.
#team and 'F.Last' of player, plus props for graph. 
#If no props, then leave blank. Spread can be Over/Under or spread. 
team = 'DAL'
player = 'D.Prescott'
spread = -5
prop = 301.5

#Grabbing pbp data and properly format spread, home/away team, and winner.
scatter <- pbp %>%
  mutate(is_home = ifelse(home_team == team,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                            ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == team, "y", "n")) %>%
  filter(posteam == team, passer == player, !is.na(epa), season >= 2015) %>%
  select(season, passer, is_home, week, team_spread, yardline_100, yards_gained, complete_pass, epa, total_line, touchdown, interception, winner,defteam) %>%
  group_by(passer, winner, season,is_home, week, total_line,team_spread, defteam) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown), comps = sum(complete_pass), ints = sum(interception)) %>%
  group_by(passer, season, is_home,team_spread, total_line, winner,defteam)  

#get colors
col1 <- teams_colors_logos %>%
  filter(team_abbr == team) %>%
  select(team_color)
col1 <- as.character(col1[1,1])

col2 <- teams_colors_logos %>%
  filter(team_abbr == team) %>%
  select(team_color2)
col2 <- as.character(col2[1,1])

col3 <- teams_colors_logos %>%
  filter(team_abbr == team) %>%
  select(team_color3)
col3 <- as.character(col3[1,1])


#plot
scatter %>%
  #use total_line for over/under team_spread for spread
  ggplot(aes(x=team_spread,y=yds_gained, color = is_home)) +
  geom_jitter(height = 0.00, width = 0.00, cex=1)+
  scale_color_manual(values=c(col2,col1)) +
  theme_bw()+
  theme(title = element_text(size =8)) +
  labs(x= "Vegas Spread", y = "Passing Yards", title = paste(player, "Home vs Away Splits"), subtitle = "By Vegas Spread", caption = 
         "Source: @giantsportsball | Data from @nflfastR", color = "Home or Away") +
  #if no props, comment out these lines or delete them.
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread), linetype = "dashed", color = "grey")
  
  

