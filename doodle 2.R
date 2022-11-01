#######

install.packages('ggplot2')
library(ggplot2)

install.packages('dplyr')
library(dplyr)

install.packages('nnet')
library(nnet)
#######

dataset_leagues <- read.csv('understat.com.csv')
dataset_by_game <- read.csv('understat_per_game.csv')which git

#######

dataset_by_game %>%
  rename(
    conceded = missed 
  )

dataset_leagues %>%
  rename(
    conceded = missed
  )

#######

leagues_included <- unique(dataset_by_game$league)


EPL <- dataset_by_game[dataset_by_game$league == 'EPL',]


#######

xG_hist <- ggplot(EPL, aes(x=xG)) + geom_histogram(binwidth = 0.1, color = 'black', fill = 'white') + facet_wrap( ~ scored, nrow = 3)

#######

xG_hist_1 <- dataset_by_game[dataset_by_game$league == 'EPL'& dataset_by_game$scored == 1,]
xG_hist_1 <- ggplot(xG_hist_1, aes(x=xG)) + geom_histogram(binwidth = 0.1, color = 'black', fill = 'white') + geom_vline(xintercept = 1)
xG_hist_1

EPL %>%
  mutate(home = if_else(h_a == "h", 1, 
                        ifelse(h_a == "a", 0)))






