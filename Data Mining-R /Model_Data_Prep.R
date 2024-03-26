#File Name: Model_Data_Prep.R
#Description: This file contains functions that are called upon prior to simulation
  #as they prep a dataframe of various types of players averages prior to said game
  #and therefore provide relevant numbers for easy drawing of R.V.
#Author(s): Chris VanKerkhove


####Prep Function for Overall Averages#####
prep.season.avgs <- function(team, game, min.n, team.data, season.avgs, players.mp.avgs) { 
  #This function takes in raw averages of players of a given team prior to a given 
   #game and prepares the data into a valid averages and SD by minutes played DF
   #for easy simulation of player stats for this game
  #ARG(s):
    #team: three letter abbreviation of team
    #game: char object of this game
    #min.n: minimum number of games played for a players averages to be considered 
    #team.data: s4 object of class "All Season Data"
    #season.avgs: object of class "Season Averages" by minutes played
    #players.mp.avgs: object of class "All Players Averages" by minutes played
  #Value: A list of dataframes. First index a dataframe of Player per MP averages,
    #and second index a data.frame of Player per MP Standard Deviations
  
  #player box scores for this game and our team
  basic <- select(team.data@player.basic[[game]], c('Players', 'MP'))
  #dataframe of averages matched to each player of this game
  avgs <- filter(season.avgs@player.basic.mean[[game]], Players %in% basic$Players)
  basic.w.avgs <- left_join(basic, select(avgs, -c('MP', 'is.home')), by = c('Players' = 'Players'))
  #dataframe of SD matched same way
  sd <- filter(season.avgs@player.basic.sd[[game]], Players %in% basic$Players)
  basic.w.sd <- left_join(basic, select(sd, -c('MP', 'is.home')), by = c('Players' = 'Players')) %>%
    select(-c('MP'))
  
  
  #tracker minutes of players we dont have suffice averages for
  min.redist <- 0
  #dataframe of rows to add back into main basic averages 
  update.df.mean <- NULL
  for (i in 1:nrow(basic.w.avgs)) {
    #this loop iterates through each player in box score and checks if there are valid 
    #games for significant averages. If not this will check overall season stats
    #and if not again, this code will redistribute minutes equally to remaining players
    if (basic.w.avgs[i, 'games'] < min.n | is.na(basic.w.avgs[i, 'games'])) {
      player.avgs <- filter(players.mp.avgs@basic.mean[[game]], Players == basic.w.avgs[i, 'Players'])
      #if player does not have at least min.n total games this season
      if (nrow(player.avgs) == 0) {
        min.redist <- min.redist + time.to.numeric(basic.w.avgs[i, 'MP'])
      }
      else if (player.avgs[['games']] < min.n){
        min.redist <- min.redist + time.to.numeric(basic.w.avgs[i, 'MP'])
      }
      #else use averages from all.player.averages
      else {
        #row to add back into averages
        player.mean <- select(player.avgs, -c('Players', 'MP', 'season.day', 'is.home'))
        player.mean <- bind_cols(select(player.avgs, c('Players', 'MP')), player.mean) %>%
          mutate(games = player.avgs$games)
        #making 'MP' value a numeric of actual minutes played
        player.mean[,'MP'] <- time.to.numeric(basic.w.avgs[i, 'MP'])
        #row for standard deviation
        player.sd <- filter(players.mp.avgs@basic.sd[[game]], Players == basic.w.avgs[i, 'Players']) %>%
          select(-c('MP', 'season.day', 'is.home'))
        #check if dataframes are initalized yet
        if (is.null(update.df.mean)) {
          update.df.mean <- player.mean
          update.df.sd <- player.sd
        }
        else {
          update.df.mean <- bind_rows(update.df.mean, player.mean)
          update.df.sd <- bind_rows(update.df.sd, player.sd)
        }
      }
    }
    #changing minutes played to a numeric value
    basic.w.avgs[i, 'MP'] <- time.to.numeric(basic.w.avgs[i, 'MP'])
  
  }
  
  ## Final Mean Dataframe
  #changing dataframe column type to numeric for 'MP'
  basic.w.avgs[,'MP'] <- as.numeric(basic.w.avgs[,'MP'])
  #filter out columns with insufficent games
  #mean df
  basic.w.avgs <- filter(basic.w.avgs, games >= min.n) 
  basic.w.sd <- filter(basic.w.sd, games >= min.n)
  if (!(is.null(update.df.mean))) {
    basic.w.avgs <- bind_rows(basic.w.avgs, update.df.mean)
    basic.w.sd <- bind_rows(basic.w.sd, update.df.sd)
  }
  #redistribute minutes taken from players with insufficent averages
  basic.w.avgs$MP <- basic.w.avgs$MP + (min.redist/nrow(basic.w.avgs))
  
  return (list(arrange(basic.w.avgs, Players), arrange(basic.w.sd, Players)))
}



####Prep Function for Averages by location (Home vs. Away)####
prep.location.avgs <- function(team, game, min.n, team.data, location.avgs, prepped.sns.avgs) {
  #This function takes in raw averages by home/away of players of a given team prior to a given 
  #game and depending on if given game is home or away, preps the minutes played 
  #averages.(Note this function can only be called with a prior call to prepped.sns.avgs 
  #for the same team & game)
  #ARG(s):
    #team: three letter abbreviation of team
    #game: char object of this game
    #min.n: minimum number of games played for a players averages to be considered 
    #team.data: s4 object of class "All Season Data"
    #location.avgs: list of object of class "Season Averages" filtered by home/away
    #prepped.sns.avgs: return object of prep.season.avgs
  #Value: A list of dataframes. First index a dataframe of Player per MP averages,
    #and second index a data.frame of Player per MP Standard Deviations
  
  #getting prepped season averages to use as baseline
  basic.w.avgs <- prepped.sns.avgs[[1]]
  basic.w.sd <- prepped.sns.avgs[[2]]
  #1 if home, 0 if away
  loc <- filter(team.data@team.basic[[game]], Team == team)[,2]
  #geting index for location avgs
  idx <- (loc * -1) + 2
  #this location averages filtered on players in this game and for at least min.n games
  loc.mean <- select(location.avgs[[idx]]@player.basic.mean[[game]], -c('is.home')) %>%
    filter(Players %in% basic.w.avgs$Players) %>%
    filter(games >= min.n)
  #for sd
  loc.sd <- select(location.avgs[[idx]]@player.basic.sd[[game]], -c('is.home', 'MP')) %>%
    filter(Players %in% loc.mean$Players)
  
  #iterate through season.avgs dataframe, and replace where available
  add.df.mean <- NULL
  add.df.sd <- NULL
  for (i in 1:nrow(basic.w.avgs)) {
    if (!(basic.w.avgs[i, 'Players'] %in% loc.mean$Players)) {
      row.mean <- filter(basic.w.avgs, Players == basic.w.avgs[i, 'Players'])
      row.sd <- filter(basic.w.sd, Players == basic.w.sd[i, 'Players'])
      #checking if DF is initialized yet
      if (is.null(add.df.mean)) {
        add.df.mean <- row.mean
        add.df.sd <- row.sd
      }
      else {
        add.df.mean <- bind_rows(add.df.mean, row.mean)
        add.df.sd <- bind_rows(add.df.sd, row.sd)
      }
    }
  }
  
  loc.mean <- as.data.frame(bind_rows(loc.mean, add.df.mean))
  loc.sd <- as.data.frame(bind_rows(loc.sd, add.df.sd))
  
  return(list(arrange(loc.mean, Players), arrange(loc.sd, Players)))
}



####Prep Function for Moving Averages####
prep.moving.avgs <- function(team, game, min.n, team.data, moving.avgs, prepped.sns.avgs) {
  #This function takes in raw n-game moving averages of players of a given team prior to a given 
  #game and preps the moving averages minutes played 
  #averages.(Note this function can only be called with a prior call to prepped.sns.avgs 
  #for the same team & game)
  #ARG(s):
    #team: three letter abbreviation of team
    #game: char object of this game
    #min.n: minimum number of games in players moving avereages to be considered
    #team.data: s4 object of class "All Season Data"
    #moving.avgs: object of "N Averages" by minutes played
    #prepped.sns.avgs: return object of prep.season.avgs
  #Value: A list of dataframes. First index a dataframe of Player per MP averages,
    #and second index a data.frame of Player per MP Standard Deviations
  
  #getting prepped season averages to act as baseline
  basic.w.avgs <- prepped.sns.avgs[[1]]
  basic.w.sd <- prepped.sns.avgs[[2]]
  
  #this location averages filtered on players in this game and for at least min.n games
  mov.mean <- select(moving.avgs@player.basic.mean[[game]], -c('is.home')) %>%
    filter(Players %in% basic.w.avgs$Players) %>%
    filter(games >= min.n)
  #for sd
  mov.sd <- select(moving.avgs@player.basic.sd[[game]], -c('is.home', 'MP')) %>%
    filter(Players %in% mov.mean$Players)
  
  #iterate through season.avgs dataframe, and replace where available
  add.df.mean <- NULL
  add.df.sd <- NULL
  for (i in 1:nrow(basic.w.avgs)) {
    if (!(basic.w.avgs[i, 'Players'] %in% mov.mean$Players)) {
      row.mean <- filter(basic.w.avgs, Players == basic.w.avgs[i, 'Players'])
      row.sd <- filter(basic.w.sd, Players == basic.w.sd[i, 'Players'])
      #checking if DF is initialized yet
      if (is.null(add.df.mean)) {
        add.df.mean <- row.mean
        add.df.sd <- row.sd
      }
      else {
        add.df.mean <- bind_rows(add.df.mean, row.mean)
        add.df.sd <- bind_rows(add.df.sd, row.sd)
      }
    }
  }
  
  mov.mean <- as.data.frame(bind_rows(mov.mean, add.df.mean))
  mov.sd <- as.data.frame(bind_rows(mov.sd, add.df.sd))
  
  return(list(arrange(mov.mean, Players), arrange(mov.sd, Players)))
}


####Prep function for getting expected DRtg####
prep.DRtg.data <- function(team, game, all.teams.data, all.player.averages,
                                      positions_data, min.n = 10) {
  #This function takes in a team and a game provides a weighted expectation 
  #of defense by the opponent based on the opponent players DRtg average by this
  #point
  #ARG(s):
    #team: three letter abbreviation of team
    #game: char object of this game
    #all.teams.data: list of s4 objects of class "All Season Data"
    #positions_data: return DF from function "get.pos.data" (see Helpful_Functions.R)
    #min.n: minimum number of games played for a players averages to be considered
  #Value: A list of dataframes. First index a dataframe of Player per MP averages,
    #and second index a data.frame of Player per MP Standard Deviations
  
  #opponent 3 letter abbreviation
  opp <-filter(all.teams.data[[team]]@team.basic[[game]], Team != team)[,1]
  #basic box score (only selecting Players and MP column)
  basic <- select(all.teams.data[[opp]]@player.basic[[game]], c('Players', 'MP'))
  #joining player Dtrg averages with minutes played this game
  game.def <- inner_join(basic, select(all.player.averages@advanced.mean[[game]], c('Players', 'DRtg', 'games')),
                                              by = c('Players'))
  #getting positions and group pos for players on this team
  positions <- select(filter(positions_data, Tm == opp), c('Player', 'Pos', 'Group'))
  
  game.def <- inner_join(game.def, positions, by = c('Players' = 'Player'))
  
  #changing minutes played to a numeric value for each row
  for (i in 1:nrow(game.def)) {
    game.def[i, 'MP'] <- time.to.numeric(game.def[i, 'MP'])
  }
  game.def$MP <- as.numeric(game.def$MP)
  
  #grouping by tot and by group for overall weighted expected defense performance
  DRtg.tot <- weighted.mean(game.def$DRtg, game.def$MP)
  DRtg.group <- group_by(select(game.def, c('MP', 'DRtg', 'Group')), Group) %>%
    summarise_each(funs(weighted.mean(., MP)), -MP)
  
  return (list(DRtg.tot, DRtg.group))
}





prep.defense.data <- function(team, game, all.teams.data, def.scores, min.n = 6) { 
  #opponent 3 letter abbreviation
  opp <-filter(all.teams.data[[team]]@team.basic[[game]], Team != team)[,1]
  #basic box score (only selecting Players and MP column)
  basic <- select(all.teams.data[[opp]]@player.basic[[game]], c('Players', 'MP'))
  
  #defensive score averages for this team at this date
    #filtering for only players that played this game, and with sufficient samples
  opp.def.tot <- filter(def.scores[[opp]]@impact.averages.tot[[game]], Players %in% basic$Players)
  opp.def.group <- filter(def.scores[[opp]]@impact.averages.group[[game]], Players %in% basic$Players)
  opp.def.pos <- filter(def.scores[[opp]]@impact.averages.pos[[game]], Players %in% basic$Players)
  #defense stats for the 3 stratefications
  opp.def.tot<- left_join(basic, opp.def.tot, by = c('Players' = 'Players')) %>%
    filter(n > min.n)
  opp.def.group <- left_join(basic, opp.def.group, by = c('Players' = 'Players')) %>%
    filter(n > min.n)
  opp.def.pos <- left_join(basic, opp.def.pos, by = c('Players' = 'Players')) %>%
    filter(n > min.n)
  
  for (i in 1:nrow(opp.def.tot)) {
    opp.def.tot[i, 'MP'] <- time.to.numeric(opp.def.tot[i, 'MP'])
    if (i <= nrow(opp.def.group)) {
      opp.def.group[i, 'MP'] <- time.to.numeric(opp.def.group[i, 'MP'])
    }
    if (i <= nrow(opp.def.pos)) {
      opp.def.pos[i, 'MP'] <- time.to.numeric(opp.def.pos[i, 'MP'])
    }
    
  }
  opp.def.tot$MP <- as.numeric(opp.def.tot$MP)
  opp.def.group$MP <- as.numeric(opp.def.group$MP)
  opp.def.pos$MP <- as.numeric(opp.def.pos$MP)
  #taking weighted mean for each stat
  opp.def.tot$Pos <- 'ABC'
  opp.def.tot <- group_by(select(opp.def.tot, -c('Players', 'n')), Pos) %>%
    summarise_each(funs(weighted.mean(.,MP)), -MP)
  opp.def.group <- group_by(select(opp.def.group,-c('Players', 'n')), Pos) %>%
    summarise_each(funs(weighted.mean(., MP)), -MP)
  opp.def.pos <- group_by(select(opp.def.pos, -c('Players', 'n')), Pos) %>%
    summarise_each(funs(weighted.mean(.,MP)), -MP)
  
  return (list(opp.def.tot, opp.def.group, opp.def.pos))
}

