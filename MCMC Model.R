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

EPL$game_index <- seq(from = 1, to = length(EPL$date))

print(sort(unique(EPL$team_home)))

for (i in 1:count(unique(EPL$team_home))){

EPL <- mutate(EPL, team_home_index = case_when(team_home == 'Arsenal'  ~ '1',
                                               team_home == 'Aston Villa'  ~ '2',
                                               team_home == 'Bournemouth'  ~ '3',
                                               team_home == 'Brighton'  ~ '4',
                                               team_home == 'Burnley'  ~ '5',
                                               team_home == 'Cardiff'  ~ '6',
                                               team_home == 'Chelsea'  ~ '7',
                                               team_home == 'Crystal Palace'  ~ '8',
                                               team_home == 'Everton'  ~ '9',
                                               team_home == 'Fulham'  ~ '10',
                                               team_home == 'Huddersfield'  ~ '11',
                                               team_home == 'Hull'  ~ '12',
                                               team_home == 'Leicester'  ~ '13',
                                               team_home == 'Liverpool'  ~ '14',
                                               team_home == 'Manchester City'  ~ '15',
                                               team_home == 'Manchester United'  ~ '16',
                                               team_home == 'Middlesborough'  ~ '17',
                                               team_home == 'Manchester United'  ~ '18',
                                               team_home == 'Newcastle United'  ~ '19',
                                               team_home == 'Norwich'  ~ '20',
                                               team_home == 'Queens Park Rangers'  ~ '21',
                                               team_home == 'Sheffield United'  ~ '22',
                                               team_home == 'Southampton'  ~ '23',
                                               team_home == 'Stoke'  ~ '24',
                                               team_home == 'Sunderland'  ~ '25',
                                               team_home == 'Swansea'  ~ '26',
                                               team_home == 'Tottenham'  ~ '27',
                                               team_home == 'Watford'  ~ '28',
                                               team_home == 'West Bromwich Albion'  ~ '29',
                                               team_home == 'West Ham'  ~ '30',
                                               team_home == 'Wolverhampton Wanderers'  ~ '31')) 


EPL <- mutate(EPL, team_away_index = case_when(team_away == 'Arsenal'  ~ '1',
                                               team_away == 'Aston Villa'  ~ '2',
                                               team_away == 'Bournemouth'  ~ '3',
                                               team_away == 'Brighton'  ~ '4',
                                               team_away == 'Burnley'  ~ '5',
                                               team_away == 'Cardiff'  ~ '6',
                                               team_away == 'Chelsea'  ~ '7',
                                               team_away == 'Crystal Palace'  ~ '8',
                                               team_away == 'Everton'  ~ '9',
                                               team_away == 'Fulham'  ~ '10',
                                               team_away == 'Huddersfield'  ~ '11',
                                               team_away == 'Hull'  ~ '12',
                                               team_away == 'Leicester'  ~ '13',
                                               team_away == 'Liverpool'  ~ '14',
                                               team_away == 'Manchester City'  ~ '15',
                                               team_away == 'Manchester United'  ~ '16',
                                               team_away == 'Middlesborough'  ~ '17',
                                               team_away == 'Manchester United'  ~ '18',
                                               team_away == 'Newcastle United'  ~ '19',
                                               team_away == 'Norwich'  ~ '20',
                                               team_away == 'Queens Park Rangers'  ~ '21',
                                               team_away == 'Sheffield United'  ~ '22',
                                               team_away == 'Southampton'  ~ '23',
                                               team_away == 'Stoke'  ~ '24',
                                               team_away == 'Sunderland'  ~ '25',
                                               team_away == 'Swansea'  ~ '26',
                                               team_away == 'Tottenham'  ~ '27',
                                               team_away == 'Watford'  ~ '28',
                                               team_away == 'West Bromwich Albion'  ~ '29',
                                               team_away == 'West Ham'  ~ '30',
                                               team_away == 'Wolverhampton Wanderers'  ~ '31'))
 
  
  
  
  
  
  
