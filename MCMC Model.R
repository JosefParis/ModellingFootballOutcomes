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


print(sort(unique(EPL$team_home)))

LeicesterWin <- EPL[EPL$year == '2015',]

LeicesterWin$game_index <- seq(from = 1, to = length(LeicesterWin$date))

print(sort(unique(LeicesterWin$team_home)))

LeicesterWin <- mutate(LeicesterWin, team_home_index = case_when(team_home == 'Arsenal'  ~ '1',
                                               team_home == 'Aston Villa'  ~ '2',
                                               team_home == 'Bournemouth'  ~ '3',
                                               team_home == 'Chelsea'  ~ '4',
                                               team_home == 'Crystal Palace'  ~ '5',
                                               team_home == 'Everton'  ~ '6',
                                               team_home == 'Leicester'  ~ '7',
                                               team_home == 'Liverpool'  ~ '8',
                                               team_home == 'Manchester City'  ~ '9',
                                               team_home == 'Manchester United'  ~ '10',
                                               team_home == 'Newcastle United'  ~ '11',
                                               team_home == 'Norwich'  ~ '12',
                                               team_home == 'Southampton'  ~ '13',
                                               team_home == 'Stoke'  ~ '14',
                                               team_home == 'Sunderland'  ~ '15',
                                               team_home == 'Swansea'  ~ '16',
                                               team_home == 'Tottenham'  ~ '17',
                                               team_home == 'Watford'  ~ '18',
                                               team_home == 'West Bromwich Albion'  ~ '19',
                                               team_home == 'West Ham'  ~ '20'))
                                              
LeicesterWin <- mutate(LeicesterWin, team_away_index = case_when(team_away == 'Arsenal'  ~ '1',
                                                                 team_away == 'Aston Villa'  ~ '2',
                                                                 team_away == 'Bournemouth'  ~ '3',
                                                                 team_away == 'Chelsea'  ~ '4',
                                                                 team_away == 'Crystal Palace'  ~ '5',
                                                                 team_away == 'Everton'  ~ '6',
                                                                 team_away == 'Leicester'  ~ '7',
                                                                 team_away == 'Liverpool'  ~ '8',
                                                                 team_away == 'Manchester City'  ~ '9',
                                                                 team_away == 'Manchester United'  ~ '10',
                                                                 team_away == 'Newcastle United'  ~ '11',
                                                                 team_away == 'Norwich'  ~ '12',
                                                                 team_away == 'Southampton'  ~ '13',
                                                                 team_away == 'Stoke'  ~ '14',
                                                                 team_away == 'Sunderland'  ~ '15',
                                                                 team_away == 'Swansea'  ~ '16',
                                                                 team_away == 'Tottenham'  ~ '17',
                                                                 team_away == 'Watford'  ~ '18',
                                                                 team_away == 'West Bromwich Albion'  ~ '19',
                                                                 team_away == 'West Ham'  ~ '20'))

# Values for bugs
# Prepares to launch OpenBUGS
library(R2OpenBUGS)

# Creates the data list
ngames <- length(LeicesterWin$game_index)
nteams <- length(unique(LeicesterWin$team_home_index))

data <- list(ngames=ngames, nteams=nteams, xg_home=LeicesterWin$xg_home, xg_away=LeicesterWin$xg_away,
             team_home_index=LeicesterWin$team_home_index, team_away_index=LeicesterWin$team_away_index)

model_file <- "HPL.odc"
parameters <- c("ynew","home","att","def")

inits <- function(){
  list(att=rnorm(nteams,0,1),
       def=rnorm(nteams,0,1),
       home=rnorm(nteams,0,1),
       mu.att=rnorm(nteams,0,1),
       mu.def=rnorm(nteams,0,1),
       tau.att=rnorm(1),
       tau.def=rnorm(1))
       
}
inits()


# Sets the number of iterations, burnin and thinning
n.iter <- 10000
n.burnin <- 9500
n.thin <- 20

# Finally calls OpenBUGS to do the MCMC run and saves results to the object "es"
MCMC <- bugs(data=data,
           inits=inits,
           parameters.to.save=parameters,
           model.file=model_file,
           n.chains=2, n.iter, n.burnin, n.thin, DIC=TRUE, debug = TRUE)

# Displays the summary statistics
print(MCMC,digits=3,intervals=c(0.025, 0.975))

# Convergence check through traceplots (example for node p1)
plot(MCMC$sims.list$p1[1:500],t="l",col="blue",ylab="p1")
points(MCMC$sims.list$p1[501:1000],t="le",col="red")




