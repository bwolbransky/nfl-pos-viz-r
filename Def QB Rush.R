def = 'KC'
spread = -3.5
prop = 18
poz = "QB"

seasons <- 2019
pbp <- purrr::map_df(seasons, function(x) {
  download.file(glue::glue("https://raw.githubusercontent.com/guga31bb/nflfastR-data/master/data/play_by_play_{x}.parquet"), "tmp.parquet")
  pbp <- arrow::read_parquet("tmp.parquet")
  return(pbp)
}
)
  
scatter <- pbp %>%
  mutate(is_home = ifelse(home_team == def,'home','away'),
         team_spread = ifelse(is_home == 'home', spread_line, 0-spread_line),
         home_away_win = ifelse(home_score>away_score, "home",
                                ifelse(home_score<away_score, "away","tie")),
         ha_winner = ifelse(home_away_win == "home", home_team, 
                            ifelse(home_away_win == 'away',away_team,"tie")),
         winner = ifelse(ha_winner == def, "y", "n")) %>%
  filter(!is.na(epa), str_detect(defteam, def), season >= min(seasons))

roster <- fast_scraper_roster(seasons)
roster <- na.omit(roster[,c("gsis_id","position")])
scatter <- scatter %>%
  nflfastR::decode_player_ids() 

scatter <- merge(scatter, roster[,c("gsis_id","position")], by.x='id', by.y= 'gsis_id', all.x = TRUE)

scatter <- scatter %>%
  filter((str_detect(desc, "scramble")) | (str_detect(desc, "kneel")) | (rush == 1 & position == "QB")) %>%
  select(season, game_id, name, is_home, week, team_spread, yardline_100, yards_gained, epa, spread_line, total_line, touchdown, winner) %>%
  group_by(game_id, season, name, is_home, week, team_spread, total_line, winner) %>%
  summarize(yds_gained = sum(yards_gained), tds = sum(touchdown)) %>%
  na.omit()

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
  ggplot(aes(x=team_spread,y=yds_gained, color = is_home)) +
  geom_point(cex=2)+
  annotate(geom = "text",x=scatter$team_spread+1,y=scatter$yds_gained, label = scatter$name, cex = 2) +
  scale_color_manual(values=c(col2,col1)) +
  theme_bw() +
  labs(x= "Vegas Spread", y = "Rushing Yards", title = paste(def, "QB Rushing Yards Allowed"), subtitle = "Home vs Away Splits", caption = 
         "Source: @giantsportsball | Data from @nflfastR") +
  geom_hline(aes(yintercept = prop), linetype = "dashed", color = "grey") +
  geom_vline(aes(xintercept = spread ), linetype = "dashed", color = "grey")
