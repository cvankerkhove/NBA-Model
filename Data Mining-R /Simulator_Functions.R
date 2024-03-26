#File Name: Simulator_Functions.R
#Description: This file contains functions that are called for generating simulations
  #with all pre-processing completed
#Author(s): Chris VanKerkhove

####Function for Simulating an Entire Game####
simulate.game <- function(team, game, all.teams.data, players.mp.averages, season.avgs, positions_data,
                          location.avgs = NULL, moving.avgs = NULL, W1 = 1, W2 = NULL, W3 = NULL,
                          itr = 1000, defense = 'Total', alpha = 1 , W.T = 0) {
  
  ##Getting prepped minutes per game averages for "team" prior to "game"
  prepped.sns.avgs <- prep.season.avgs(team, game, 15, all.teams.data[[team]], season.avgs, players.mp.averages)
  #checking if to factor in home/away averages for players
  if (!(is.null(location.avgs))) {
    prepped.loc.avgs <- prep.location.avgs(team, game, 12, all.teams.data[[team]], location.avgs, prepped.sns.avgs)
  }
  else {
    prepped.loc.avgs <- NULL
  }
  #checking if to factor in moving averages for players
  if (!(is.null(moving.avgs))) {
    prepped.mov.avgs <- prep.moving.avgs(team, game, 5, all.teams.data[[team]], moving.avgs, prepped.sns.avgs)
  }
  else {
    prepped.mov.avgs <- NULL
  }
  #weights for different averages
  if (is.null(prepped.loc.avgs) && is.null(prepped.mov.avgs)) {
    weights <- NULL
  }
  else {
    weights <- c(W1, W2, W3)
  }
  
  #simulating expected player performance over "itr" iterations
  players.exp.pts <- simulate.players.stat(team, game, all.teams.data, players.mp.averages, positions_data, 
                                           prepped.sns.avgs, prepped.loc.avgs, prepped.mov.avgs, 
                                           weights = weights, defense = defense, alpha = alpha)
  
  #summing across each row for spread of total team points
  team.pts <- apply(players.exp.pts ,1, sum)
  
  #adjusting for recent averages of this teams scoring 
  avg.team.pts <- filter(season.avgs@team.basic.mean[[game]], Team == team)[['PTS']]
  team.pts <- (team.pts * (1- W.T)) + avg.team.pts * W.T
  
  return (team.pts)
}



####Function for Simulating Players Stats###
simulate.players.stat <- function(team, game, all.teams.data, all.players.averages, 
                            positions_data, prepped.sns.avgs, prepped.loc.avgs = NULL, 
                          prepped.mov.avgs = NULL, weights = NULL, itr = 1000, 
                          defense = 'Total', alpha = 1) {
  #This function takes in prepped data for simulation and draws "itr" iterations 
    #of random variables for Points per Minute based on averages provided
  #ARG(s):
    #team: three letter abbreviation of team
    #game: char object of this game
    #all.teams.data: list of objects "All Season Data" for all teams
    #all.players.averages: object of class "All Player Averages" (used for advanced averages of defenders)
    #positions_data: return DF from function "get.pos.data" (see Helpful_Functions.R)
    #prepped.sns.avgs: return value of prep.season.avgs (see Model_Data_Prep.R)
    #prepped.loc.avgs: similar to above
    #prepped.mov.avgs: """
    #weights: Either a list of W1, W2, W3 for diff averages or NULL
    #itr: number of iterations for simulation
    #defense: Type of way to apply defense to players offensive score ("Total", "Group")
    #alpha: exponential weight for when applying defense
  #Value: A Dataframe with "itr" rows and a column for each player with values 
    #representing their simulated PTS
  
  ###Drawing "itr" simulations
  #players we are simulating for this team
  players <- prepped.sns.avgs[[1]]$Players
  #data.frame with columns representing players and row being iterations in sim
  pred.df <- data.frame(matrix(ncol=length(players), nrow=1000))
  colnames(pred.df) <- players
  #looping through each pkayer and simulating scoring
  for (i in 1:nrow(prepped.sns.avgs[[1]])) {
    #vector 1 from season averages
    player <- prepped.sns.avgs[[1]][i, 'Players']
    pts.pm <- prepped.sns.avgs[[1]][i, 'PTS']
    pts.pm.sd <- prepped.sns.avgs[[2]][i, 'PTS']
    v1 <- rnorm(itr, mean=pts.pm, sd=pts.pm.sd^2)
    #setting floor at 0 for pts.pm
    v1 <- sapply(v1, function(x){return(max(x,0))})
    #getting weight for this vector
    if (is.null(weights)) {
      W1 <- 1
    }
    else {
      W1 <- weights[1]
    }
    
    ##case for if we are considering home/away averages
    if (!(is.null(prepped.loc.avgs))) {
      #vector 2 from location averages
      pts.pm <- prepped.loc.avgs[[1]][i, 'PTS']
      pts.pm.sd <- prepped.loc.avgs[[2]][i, 'PTS']
      v2 <- rnorm(itr, mean=pts.pm, sd=pts.pm.sd^2)
      #setting floor at 0 for pts.pm
      v2 <- sapply(v2, function(x){return(max(x,0))})
      #weight for this vector
      W2 <- weights[2]
    }
    else {
      v2 <- v1
      W2 <- 0
    }
    
    ##case for if we are considering moving averages
    if (!(is.null(prepped.mov.avgs))) {
      #vector 3 from moving averages
      pts.pm <- prepped.mov.avgs[[1]][i, 'PTS']
      pts.pm.sd <- prepped.mov.avgs[[2]][i, 'PTS']
      v3 <- rnorm(itr, mean=pts.pm, sd=pts.pm.sd^2)
      #setting floor at 0 for pts.pm
      v3 <- sapply(v2, function(x){return(max(x,0))})
      #weight for this vector
      W3 <- weights[3]
    }
    else {
      v3 <- v1
      W3 <- 0
    }
    #combining vectors based on weights
    v <- v1 * W1 + v2 * W2 + v3 * W3
    #adding final vector to DF
    tot.pts <- v * prepped.sns.avgs[[1]][i, 'MP']
    pred.df[[player]] <- tot.pts
  }
  
  ###Defense Adjustment 
  #defensive rating numbers
  DRtg <- prep.DRtg.data(team, game, all.teams.data, all.player.averages, positions_data)
  if (defense == 'Total') {
    pred.df <- pred.df * (DRtg[[1]]/113.2)^alpha
  }
  else if (defense == 'Group') {
  #getting position grouping for each player and appling defense adjustment
    team.pos <- filter(positions_data, Tm == team) 
    for (player in players) {
      grp <- filter(team.pos, Player == player)[,'Group']
      #making sure this player has an assigned position
      if (length(grp) > 0 && grp %in% DRtg[[2]]$Group) {
        drtg <- filter(DRtg[[2]], Group == grp)[['DRtg']]
        pred.df[player] <- pred.df[player] * (drtg/113.2)^alpha
      }
      else {
        pred.df[player] <- pred.df[player] * (DRtg[[1]]/113.2)^alpha
      }
    }
  }
  #returning data.frame of player pts prediction for each 
  return (pred.df)
}

