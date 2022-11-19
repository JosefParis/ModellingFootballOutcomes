# Prepares to launch OpenBUGS
library(R2OpenBUGS)

working.dir <- getwd()
# Creates the data list
ngames <- length(LeicesterWin$game_index)
nteams <- length(unique(LeicesterWin$team_home_index))

team_home_index <- as.numeric(LeicesterWin$team_home_index)
team_away_index <- as.numeric(LeicesterWin$team_away_index)


data <- list(ngames=ngames, nteams=nteams, xg_home=LeicesterWin$goals_home, xg_away=LeicesterWin$goals_away,
             team_home_index=team_home_index, team_away_index=team_away_index)

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
MCMC <- bugs(data=data, inits=NULL, parameters.to.save=parameters,
             model.file=model_file,
             n.chains=2, n.iter, n.burnin, n.thin, DIC=TRUE, debug = TRUE)

# Displays the summary statistics
print(MCMC,digits=3,intervals=c(0.025, 0.975))

# Convergence check through traceplots (example for node p1)
plot(MCMC$sims.list$p1[1:500],t="l",col="blue",ylab="p1")
points(MCMC$sims.list$p1[501:1000],t="le",col="red")




