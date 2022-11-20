if(!require('stats')) {
  install.packages('stats')
  library('stats')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}

if(!require('tidyverse')) {
  install.packages('tidyverse')
  library('tidyverse')
}
working.dir <- getwd()
# Model cleaning and manipulation
stats_per_game <- read.csv('understat_per_game.csv')
stats_league <- read.csv('understat.com.csv')

stats_per_game <- mutate(stats_per_game, home = case_when(h_a == 'h'  ~ '1',
                                                          h_a == 'a' ~ '0')) 

stats_per_game <- mutate(stats_per_game, season = case_when(year == '2014' ~ '2014/15',
                                                            year == '2015' ~ '2015/16',
                                                            year == '2016' ~ '2016/17',
                                                            year == '2017' ~ '2017/18',
                                                            year == '2018' ~ '2018/19',
                                                            year == '2019' ~ '2019/20'))


EPL <- stats_per_game[stats_per_game$league == 'EPL',]



home_team_join <- EPL[EPL$home == '1',]
away_team_join <- EPL[EPL$home == '0',]

home_team_join <- rename(home_team_join,
                         xg_home = xG,
                         xg_away = xGA,
                         team_home = team,
                         goals_home = scored,
                         goals_away = missed,
                         home_points = pts) 

away_team_join <- select(away_team_join, xG,
                         xGA,
                         team,
                         scored,
                         missed,
                         date,
                         pts)

away_team_join <- rename(away_team_join, 
                         xg_away = xG,
                         xg_home = xGA,
                         team_away = team,
                         goals_away = scored,
                         goals_home = missed,
                         away_points = pts)

   EPL <- merge(home_team_join, away_team_join)

drop_columns <- c('h_a','npxG', 'npxGA', 'deep', 'deep_allowed', 'result', 'wins', 'draws', 'loses', 'npxGD', 'home', 'xpts',
                  'ppda_coef', 'ppda_att', 'ppda_def', 'oppda_coef', 'oppda_att', 'oppda_def', 'xG_diff', 'xGA_diff', 'xpts_diff')

EPL <- EPL[, !names(EPL) %in% drop_columns]

column_names <- c('league','team_home', 'team_away', 'goals_home', 'goals_away','xg_home','xg_away','home_points', 'away_points', 'season',
                  'year','date')

EPL <- EPL[, column_names]
EPL <- EPL[order(EPL$date),]

write.csv(EPL, file = 'EPL.csv')

