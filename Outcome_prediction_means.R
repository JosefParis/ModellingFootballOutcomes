if(!require('extraDistr')) {
  install.packages('extraDistr')
  library('extraDistr')
}
if(!require('scales')) {
  install.packages('scales')
  library('scales')
}
library(ggplot2)

clubs <- as.list(unique(EPL_ELO$team_away))
mean_xg_home <- numeric(0)
mean_xg_away <- numeric(0)

for(i in 1:30) {
    team_subset <- EPL_ELO[EPL_ELO$team_home == clubs[[i]],]
    mean_xg_home <- c(mean_xg_home, mean(team_subset$xg_home))
    team_subset <- EPL_ELO[EPL_ELO$team_away == clubs[[i]],]
    mean_xg_away <- c(mean_xg_away, mean(team_subset$xg_away))
}
df <- data.frame(row.names = seq(1,length(clubs)))
df$team <- clubs
df$mean_xg_home <- mean_xg_home
df$mean_xg_away <- mean_xg_away


x <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
y <- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

ggplot(transform(data.frame(x=c(0:10)), y=dpois(x, df$mean_xg_home[15])), aes(x, y)) + 
  geom_bar(stat="identity") 

outcomes <- as.array(dpois(x, df$mean_xg_home[15])) %*% t(as.array(as.array(dpois(x, df$mean_xg_away[2]))))
colnames(outcomes) <- c(0:10)
rownames(outcomes) <- c(0:10)

home_win_matrix <- lower.tri(outcomes, diag = FALSE)
away_win_matrix <- upper.tri(outcomes, diag = FALSE)
draw_matrix <- lower.tri(outcomes, diag = TRUE) * upper.tri(outcomes, diag = TRUE)

sum(outcomes)
home_win_percentage <- sum(outcomes * home_win_matrix) 
away_win_percentage <- sum(outcomes * away_win_matrix) 
draw_percentage <- sum(outcomes * draw_matrix) 

home_win_percentage
away_win_percentage
draw_percentage




