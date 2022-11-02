if(!require('stats')) {
  install.packages('stats')
  library('stats')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library('dplyr')
}

# Model fitting:
# Model cleaning and manipulation
stats_per_game <- read.csv('understat_per_game.csv')
stats_league <- read.csv('understat.com.csv')

EPL <- stats_per_game[stats_per_game$league == 'EPL',]

EPL <- mutate(EPL, home = case_when(h_a == 'h'  ~ '1',
                          h_a == 'a' ~ '0'))

EPL <- mutate(EPL, season = case_when(year == '2014' ~ '2014/15',
                                      year == '2015' ~ '2015/16',
                                      year == '2016' ~ '2016/17',
                                      year == '2017' ~ '2017/18',
                                      year == '2018' ~ '2018/19',
                                      year == '2019' ~ '2019/20'))

EPL$GW <- unlist(sapply(table(EPL$year), seq))

# Linear model
prediciton_matrix <- unique(EPL[, c('team', 'home')])

linear_model_xg <- glm(xG ~ team + home, family = gaussian, data = EPL)
summary(linear_model_xg)

linear_model_xga <- glm(xGA ~ team + home, family = gaussian, data = EPL)
summary(linear_model_xga)

prediciton_xg <- predict(linear_model_xg, prediciton_matrix)
prediciton_xga <- predict(linear_model_xga, prediciton_matrix)
  
prediciton_matrix$xg <- prediciton_xg
prediciton_matrix$xga <- prediciton_xga

rm('prediciton_xg','prediciton_xga')
# Poisson extraction (P(Goals| xG))

for (i in 0:max(EPL$scored)){
  column_index <- grep("^xG$", colnames(EPL))
  temp_dataset <- subset(EPL, scored == i)
  print(mean(temp_dataset[,column_index]))
  xg_means <- list()
  xg_means[i] <- mean(temp_dataset[,column_index]) 
  rm(temp_dataset)
}