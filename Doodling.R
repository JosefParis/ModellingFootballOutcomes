rm(list=ls())



package_list <- c('dplyr', 'stringr', 'ggpubr')
install.packages(package_list)

install.packages('nnet')


library(dplyr)
library(stringr)
library(ggplot2)
library(ggpubr).

stats_per_game <- read.csv('understat_per_game.csv')
stats_league <- read.csv('understat.com.csv')
EPL <- stats_per_game[stats_per_game$league == 'EPL',]


xg_hist_facet <- ggplot(EPL, aes(x = xG)) + geom_histogram(binwidth=0.1, color = 'black', fill = 'white') + facet_wrap(~scored, ncol = 3)
xg_hist_facet

xg_hist_list <- split(EPL, EPL$scored)
xg_hist_0 <- ggplot(xg_hist_list[[1]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 0)
xg_hist_1 <- ggplot(xg_hist_list[[2]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 1)
xg_hist_2 <- ggplot(xg_hist_list[[3]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 2)
xg_hist_3 <- ggplot(xg_hist_list[[4]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 3)
xg_hist_4 <- ggplot(xg_hist_list[[5]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 4)
xg_hist_5 <- ggplot(xg_hist_list[[6]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 5)
xg_hist_6 <- ggplot(xg_hist_list[[7]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white')
xg_hist_7 <- ggplot(xg_hist_list[[8]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 7)
xg_hist_8 <- ggplot(xg_hist_list[[9]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 8)
xg_hist_9 <- ggplot(xg_hist_list[[10]], aes(x=xG)) + geom_histogram(binwidth=0.05, color = 'black', fill = 'white') + geom_vline(xintercept = 9)
 

for (i in 0:max(EPL$scored)){
  column_index <- grep("^xG$", colnames(EPL))
  temp_dataset <- subset(EPL, scored == i)
  print(mean(temp_dataset[,column_index]))
  xg_means[i] <- mean(temp_dataset[,column_index]) 
  rm(temp_dataset)
  }
x <- 0:4 
 
xg_hist_0 <- ggplot(xg_hist_list[[1]], aes(0:5)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.1, color = 'black', fill = 'white') + 
  geom_density() +
  geom_vline(xintercept = 0)
xg_hist_0

ggplot(data = data.frame(x = 0:20)) +
  lapply(lambdas, function(l) geom_point(aes(x = x, y = dpois(x, lambda = l), col = factor(l)))) +
  lapply(lambdas, function(l) stat_function(fun = dnorm, args = list(mean = l, sd = sqrt(l)), 
                                            aes(x = x, col = factor(l)))

  
EPL$home <- []         
             
EPL %>%
  mutate(home = ifelse(EPL$h_a == 'h') = 1, 0)




         
??case_when

mulitnomial_xg <- multinom(xG ~ home)

