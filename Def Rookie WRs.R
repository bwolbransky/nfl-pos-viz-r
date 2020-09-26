def = 'NYJ'
spread = -11.5
prop = 45
poz = "WR"

scatter <- pbp %>%
  mutate(is_home = ifelse(home_team == def,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                            ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == def, "y", "n")) %>%
  filter(defteam == def, !is.na(epa), complete_pass == 1, season >= 2015) %>%
  select(receiver_id, posteam, season, game_id, receiver, is_home, week, team_spread, yardline_100, yards_gained, epa, spread_line, total_line, touchdown, winner) %>%
  group_by(receiver_id, posteam, game_id, season, receiver, is_home, week, team_spread, total_line, winner) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown)) %>%
  nflfastR::decode_player_ids()

scatter <- scatter %>%
  merge(roster[,c("position", "gsis_id")], by.x='receiver_id', by.y= 'gsis_id') %>%
  filter(position == poz) %>%
  group_by(game_id)%>%
  slice(which.max(yds_gained))

scatter <-scatter %>%
  merge(year_roster[,c("season", "team","name", "years")], 
        by.x = c("season","posteam","receiver"), 
        by.y = c("season","team","name")) %>%
  filter(years == 0)

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
  annotate(geom = "text",x=scatter$team_spread,y=scatter$yds_gained, label = paste(scatter$receiver, "week", scatter$week), cex = 2.5) +
  scale_color_manual(values=c(col2,col1)) +
  theme_bw() +
  labs(x= "Vegas Spread", y = "Receiving Yards", title = paste(def, "Defense Yards Allowed"), subtitle = "Home vs Away Splits", caption = 
         "Source: @giantsportsball | Data from @nflfastR") +
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread ), linetype = "dashed", color = "grey")
