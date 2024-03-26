###Simulation_Sandbox.R

#necessary objects
all.teams.data
positions_data

###Obtaining all player averages by minutes played (not advanced/team data)
ag_p_data <- aggregate.players.data(all.teams.data, "12-20-2020", "06-02-2021")
##getting per minute stats and sd over season
p.df.per.mp <- select(ag_p_data[[1]], -c('Players', 'MP', 
                                         'season.day')) / ag_p_data[[1]]$MP
p.df.per.mp <- bind_cols(select(ag_p_data[[1]], c('Players', 'MP', 'season.day')),
                         p.df.per.mp)
players.mp.averages <- get.players.averages(p.df.per.mp, ag_data2[[2]], ag_data2[[3]])


#Gathering relevant averages 
all.teams.mp.avgs <- c()
all.teams.mp.loc <- c()
all.teams.mp.mov <- c()
for (t in teams) {
  print(t)
  ###Aggregating data, transforming by MP, filtering on certain criteria
  ag_data <- aggregate.data.team(all.teams.data[[t]])
  ##getting per minute stats and sd for season, n-game, and home/away
  df.per.mp <- select(ag_data[[1]], -c('Players', 'MP', 'season.day', 
                                       'is.home')) / ag_data[[1]]$MP
  df.per.mp <- bind_cols(select(ag_data[[1]], c('Players', 'MP', 'season.day', 'is.home')), df.per.mp)
  #filtering
  #home game only data
  df.per.mp.home <- filter(df.per.mp, is.home == 1)
  team.df.home <- filter(ag_data[[3]], is.home==1)
  adv.home <- filter(ag_data[[2]], is.home==1)
  #away game only data
  df.per.mp.away <- filter(df.per.mp, is.home == 0)
  team.df.away <- filter(ag_data[[3]], is.home==0)
  adv.away <- filter(ag_data[[2]], is.home==0)
  
  #computing averages for aggreagated data
  all.teams.mp.avgs[[t]] <- season.averages(all.teams.data[[t]],df.per.mp,ag_data[[2]], ag_data[[3]], ag_data[[5]])
  home.avgs <- season.averages(all.teams.data[[t]], df.per.mp.home,adv.home, team.df.home, ag_data[[5]])
  away.avgs <- season.averages(all.teams.data[[t]], df.per.mp.away, adv.away, team.df.away, ag_data[[5]])
  all.teams.mp.loc[[t]] <- list(home.avgs, away.avgs)
  all.teams.mp.mov[[t]] <- n.game.averages(all.teams.data[[t]], df.per.mp, ag_data[[2]], ag_data[[3]], ag_data[[5]])

}

for (t in teams) {
  ag_data <- aggregate.data.team(all.teams.data[[t]])
  df.per.mp <- select(ag_data[[1]], -c('Players', 'MP', 'season.day', 
                                       'is.home')) / ag_data[[1]]$MP
  df.per.mp <- bind_cols(select(ag_data[[1]], c('Players', 'MP', 'season.day', 'is.home')), df.per.mp)
  all.teams.mp.mov[[t]] <- n.game.averages(all.teams.data[[t]], df.per.mp, ag_data[[2]], ag_data[[3]], ag_data[[5]])
}

#team for simulating seaosn
teams.error <- c()
alpha.error <- c()
alphas <- seq(0,5.5, 0.5)
team = 'NYK'
#for (team in teams) {
for (alpha in alphas) {
  print(team)
  #getting games in teams season to loop through and simulate
  games <- names(all.teams.avgs[[team]]@player.basic.mean)
  games <- games[15:length(games)] 
  pred.error <- c()
  #l1 <- c()
  #l2 <- c()
  #for each game
  for (game in games) {
    print(game)
    sim.team <- simulate.game(team, game, all.teams.data, players.mp.averages, all.teams.mp.avgs[[team]], positions_data, defense = "Group", alpha = alpha)
    #simulating opponent
    opp <-filter(all.teams.data[[team]]@team.basic[[game]], Team != team)[,1]
    sim.opp <- simulate.game(opp, game, all.teams.data, players.mp.averages, all.teams.mp.avgs[[opp]], positions_data, defense = "Group", alpha = alpha)
    #predicted diff in score
    pred.diff <- mean(sim.team) - mean(sim.opp)
    #actual scores
    team.score <- filter(all.teams.data[[team]]@team.basic[[game]], Team == team)[,'PTS']
    opp.score <- filter(all.teams.data[[opp]]@team.basic[[game]], Team == opp)[,'PTS']
    actual.diff <- team.score - opp.score
    
    #appending to pred error list
    pred.error[game] <- as.numeric((pred.diff/abs(pred.diff)) == (actual.diff/abs(actual.diff)))
    #l1[game] <- pred.diff
    #l2[game] <- actual.diff
    
  }
  #mean(pred.error)
  teams.error[team] <- mean(pred.error)
  alpha.error[alpha*2] <- mean(pred.error)
}
plot(alpha.error)









#tuning knicks for regualr averages and moving averages
team = "NYK"
ratio <- seq(0, 1, 0.05)
errors <- c()
errors.pts <- c()
errors.opp.pts <- c()
for (i in 1:length(ratio)) {
  W1 <- 1
  W3 <- 0
  W2 = 0
  print(ratio[i])
  W.T <- ratio[i]
  #getting games in teams season to loop through and simulate
  games <- names(all.teams.avgs[[team]]@player.basic.mean)
  games <- games[15:length(games)] 
  pred.error <- c()
  spread.errors <- c()
  spread.errors.opp <- c()
  #for each game
  for (game in games) {
    print(game)
    sim.team <- simulate.game(team, game, all.teams.data, players.mp.averages, all.teams.mp.avgs[[team]], positions_data, 
                              moving.avgs =all.teams.mp.mov[[team]], W1 = W1,W2=W2, W3=W3, defense = "Total", W.T = W.T)
    #simulating opponent
    opp <-filter(all.teams.data[[team]]@team.basic[[game]], Team != team)[,1]
    sim.opp <- simulate.game(opp, game, all.teams.data, players.mp.averages, all.teams.mp.avgs[[opp]], positions_data,
                             moving.avgs = all.teams.mp.mov[[opp]], W1 = W1,W2=W2, W3 = W3, defense = "Total", W.T = W.T)
    #predicted diff in score
    pred.diff <- mean(sim.team) - mean(sim.opp)
    #actual scores
    team.score <- filter(all.teams.data[[team]]@team.basic[[game]], Team == team)[,'PTS']
    opp.score <- filter(all.teams.data[[opp]]@team.basic[[game]], Team == opp)[,'PTS']
    actual.diff <- team.score - opp.score
    
    #appending to pred error list
    pred.error[game] <- as.numeric((pred.diff/abs(pred.diff)) == (actual.diff/abs(actual.diff)))
    
    #spread error
    spread.errors[game] <- abs(team.score - mean(sim.team))
    spread.errors.opp[game] <- abs(opp.score - mean(sim.opp))
  }
  
  errors[i] <- mean(pred.error)
  errors.pts[i] <- mean(spread.errors)
  errors.opp.pts[i] <- mean(spread.errors.opp)
}
plot(errors.pts)
plot(errors)
plot(errors.opp.pts)






prepped.sns.avgs <- prep.season.avgs(team, game, 15, all.teams.data[[team]], all.teams.mp.avgs[[team]],players.mp.averages)
prepped.mov.avgs <- prep.moving.avgs(team, game, 5, all.teams.data[[team]], all.teams.mp.mov[[team]], prepped.sns.avgs)
s <- simulate.players.stat(team, game, all.teams.data, all.players.averages, 
                                       positions_data, prepped.sns.avgs, 
                                       prepped.mov.avgs = prepped.mov.avgs, weights = c(0.2, 0, 0.8), itr = 1000)





