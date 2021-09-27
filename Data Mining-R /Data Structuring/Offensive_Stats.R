#File Name: Offensive_Stats.R
#Description: This file contains classes and functions used to take in game box 
  #scores, aggregate the data and take averages and standard deviations of this 
  #data, storing these averages at dates throughout the season in an organized 
  #fashion
#Author(s): Chris VanKerkhove
library(dplyr)


####Class and Functions for getting player and team averages through season for a given team####
#S4 Class for storing averages and standard deviations of basic stats for players 
#and overall team for a single team
#SLOTS:
  #name: Three letter character of team
  #min.n: minimum number of games to start taking averages for
  #player.basic.mean: list of means of players basic stats
  #player.basic.sd: list of SD of players basic stats
  #team.basic.mean: list of means of teams basic stats
  #team.basic.sd: list of SD of teams basic stats

setClass("Season Averages", slots=list(name = "character", min.n = "numeric",
                                       player.basic.mean = "list", player.basic.sd = "list",
                                       team.basic.mean = "list", team.basic.sd = "list"))

season.averages <- function(team.data, player.df, team.df, dates, min.n = 5) {
  #Takes in data.frames of aggregated game data for a single team and computes 
  #averages and standard deviations for player and team basic stats, storing them 
  #into an object of class "Season Averages"
  #for input team data
  #(Note get inputs from return of "aggregate.data.team")
  #ARG(s):
    #team.data: An S4 class object "All Season Data" 
    #player.df: a dataframe of the aggregated player stats over games
    #team.df: a dataframe of the aggregated team stats over games
    #dates: a list of dates used for collecting aggregated data to match game num
    #min.n: min amount of games to start recording averages (default=5)
  #VALUE: A populated S4 object for "Season Averages" of given team
  
  player.avgs.list <- c()
  player.sd.list <- c()
  team.avgs.list <- c()
  team.sd.list <- c()
  
  #date of all games for this team
  games <- names(team.data@player.basic)
  #iterator for number of games in averages so far
  i <- 0
  #iterating through dates chronillogically
  for (date in dates) {
    if (date %in% games) {
      if (i >= min.n) {
        game.num <- match(date, dates)
        ##Averaging Player Stats
        #filter to include all games prior to this game
        df1 <- filter(player.df, season.day < game.num) %>%
          select(-c('season.day'))
        #games played for each player
        games.played <- count(df1, Players)
        #dataframe of averages of each stat for players
        mean.df1 <- group_by(df1, Players) %>%
          summarise_each(funs(mean)) %>%
          mutate(games=games.played$n)
        mean.df1 <- mean.df1[order(mean.df1$PTS, decreasing=TRUE),]
        #dataframe of standard deviations of each stat for players
        sd.df1 <- group_by(df1, Players) %>%
          summarise_each(funs(sd)) %>%
          mutate(games=games.played$n)
        sd.df1 <- sd.df1[order(sd.df1$PTS, decreasing=TRUE),]
        
        ##Averaging Team Stats
        df2 <- filter(team.df, season.day < game.num) %>%
          select(-c("season.day"))
        #dataframe of averages for team as a whole
        mean.df2 <- group_by(df2, Team) %>%
          summarise_each(funs(mean)) %>%
          mutate(games=i)
        #dataframe of standard deviations for team stats
        sd.df2 <- group_by(df2, Team) %>%
          summarise_each(funs(sd)) %>%
          mutate(games=i)
        
        #appending to our lists
        player.avgs.list[[date]] <- mean.df1
        player.sd.list[[date]] <- sd.df1
        team.avgs.list[[date]] <- mean.df2
        team.sd.list[[date]] <- sd.df2
      }
      i <- i + 1
    }
  }
  #S4 class object of Season Averages
  s4 <- new("Season Averages", name = team.data@name, min.n = min.n, 
            player.basic.mean=player.avgs.list, player.basic.sd=player.sd.list,
            team.basic.mean = team.avgs.list, team.basic.sd=team.sd.list)
  return (s4)
}

####Class and function for storing N Game Moving Averages (Single Team)####

#S4 Class for storing averages and standard deviations of basic stats for players 
#and overall team for their last N games at points throughout the season
#SLOTS:
#name: Three letter character of team
#n: number of prev games averages are taken over
#player.basic.mean: list of means of players basic stats
#player.basic.sd: list of SD of players basic stats
#team.basic.mean: list of means of teams basic stats
#team.basic.sd: list of SD of teams basic stats
setClass("N Averages", slots=list(name = "character", n = "numeric",
                                  player.basic.mean = "list", player.basic.sd = "list",
                                  team.basic.mean = "list", team.basic.sd = "list"))

n.game.averages <- function(team.data, player.df, team.df, dates, n = 7) {
  #Takes in data.frames of aggregated game data for a single team and computes 
  #averages and standard deviations for player and team basic stats, storing them 
  #into an object of class "N Averages"
  #for input team data
  #(Note get inputs from return of "aggregate.data.team")
  #ARG(s):
  #team.data: An S4 class object "All Season Data" 
  #player.df: a dataframe of the aggregated player stats over games
  #team.df: a dataframe of the aggregated team stats over games
  #dates: a list of dates used for collecting aggregated data to match game num
  #n: number of prev games to take average over
  #VALUE: A populated S4 object for "N Averages" of given team
  player.avgs.list <- c()
  player.sd.list <- c()
  team.avgs.list <- c()
  team.sd.list <- c()
  #date of all games for this team
  games <- names(team.data@player.basic)
  #stack for golding games chronilogically
  games.stack <- c()
  #index for earliest game in average
  idx1 <- 1
  #index  or latest game in average
  idx2 <- 1
  #iterating through dates chronillogically
  for (date in dates) {
    if (date %in% games) {
      games.stack[idx2] <- date
      idx2 <- idx2 + 1
      #if there are at least 7 games to average
      if (idx2-2 >= n) {
        #first game for this average
        game.first <- games.stack[idx1]
        #latest game for this average
        game.last <- games.stack[idx2-2]
        #game nums as attached in stats dataframe
        game.num.first <- match(game.first, dates)
        game.num.last <- match(game.last, dates)
        ##Averaging Player Stats
        #filter to include all games prior to this game
        df1 <- filter(player.df, season.day %in% seq(game.num.first, game.num.last,1)) %>%
          select(-c('season.day'))
        #games played for each player
        games.played <- count(df1, Players)
        #dataframe of averages of each stat for players
        mean.df1 <- group_by(df1, Players) %>%
          summarise_each(funs(mean)) %>%
          mutate(games=games.played$n)
        mean.df1 <- mean.df1[order(mean.df1$PTS, decreasing=TRUE),]
        #dataframe of standard deviations of each stat for players
        sd.df1 <- group_by(df1, Players) %>%
          summarise_each(funs(sd)) %>%
          mutate(games=games.played$n)
        sd.df1 <- sd.df1[order(sd.df1$PTS, decreasing=TRUE),]
        
        ##Averaging Team Stats
        df2 <- filter(team.df, season.day %in% seq(game.num.first, game.num.last,1)) %>%
          select(-c("season.day"))
        #games played to ensure these averages are n game running
        games.played <- count(df2, Team)
        #dataframe of averages for team as a whole
        mean.df2 <- group_by(df2, Team) %>%
          summarise_each(funs(mean)) %>%
          mutate(games=games.played$n)
        #dataframe of standard deviations for team stats
        sd.df2 <- group_by(df2, Team) %>%
          summarise_each(funs(sd)) %>%
          mutate(games=games.played$n)
        
        #appending to our lists
        player.avgs.list[[date]] <- mean.df1
        player.sd.list[[date]] <- sd.df1
        team.avgs.list[[date]] <- mean.df2
        team.sd.list[[date]] <- sd.df2
        
        #iterating first index
        idx1 <- idx1+1
      }
    }
  }
  #S4 class object of Season Averages
  s4 <- new("N Averages", name = team.data@name, n = n, 
            player.basic.mean=player.avgs.list, player.basic.sd=player.sd.list,
            team.basic.mean = team.avgs.list, team.basic.sd=team.sd.list)
  return (s4)
}


###Aggregating function for getting inputs to the two above averaging functions
aggregate.data.team <- function(team.data, start_date= "12-21-2020", 
                                end_date = "05-25-2021") {
  #Takes in all games for a specified team over a given date range and aggregates
  #the basic and advanced stats for both players and teams into sing data.frames
  #for each and returns these dataframes
  #ARG(s):
    #team.data: object of class "All Season Data"
    #start_date: starting date to begin collecting data
    #enddate: ending date for when to stop collecting data
  #VALUE: ##list of 5; 
    #1st-4th indices: Dataframe of agg data for basic player, advanced player, 
      #basic team and advanced team ,respectively
    #5th: list of dates for matching game day and date value
  dates <- c(end_date)
  prev <- prev.day(end_date)
  while (!(start_date %in% dates)) {
    index <- length(dates) + 1
    dates[index] <- prev
    prev <- prev.day(prev)
  }
  dates <- rev(dates)
  #initalizaing data.frames to collect basic and advanced stats
  b.players.df <- NA
  a.players.df <- NA
  b.team.df <- NA
  a.team.df <- NA
  
  games <- names(team.data@player.basic)
  #looping through all games for this team in specified range
  for (game in games) {
    ##Collecting data and Adding to Agg Dataframes
    #data:
    basic.player <- team.data@player.basic[[game]]
    advanced.player <- team.data@player.advanced[[game]]
    basic.team <- team.data@team.basic[[game]]
    advanced.team <- team.data@team.advanced[[game]]
    #getting value for if team was home or away
    is_home <- filter(basic.team, Team == team.data@name)[,2]
    #marking what day in the season this is for the team and overall
    sn.day <- match(game, dates)
    basic.player <- mutate(basic.player, season.day=sn.day, is.home=is_home)
    advanced.player <- mutate(advanced.player, season.day=sn.day, is.home=is_home)
    basic.team <- mutate(basic.team, season.day=sn.day, is.home=is_home)
    advanced.team <- mutate(advanced.team, season.day=sn.day, is.home=is_home)
    #adding rows to main dfs
    if (is.data.frame(b.players.df)) {
      b.players.df<- bind_rows(b.players.df, basic.player)
      a.players.df<- bind_rows(a.players.df, advanced.player)
      b.team.df <- bind_rows(b.team.df, basic.team)
      a.team.df <- bind_rows(a.team.df, advanced.team)
    }
    else {
      b.players.df <- basic.player
      a.players.df <- advanced.player
      b.team.df <- basic.team
      a.team.df <- advanced.team
    }
  }
  
  ##Cleaning up data to allow for easy averaging 
  #Changing Opponent team name to 'OPP'
  for (i in 1:nrow(b.team.df)) {
    if (b.team.df[i, 'Team'] != team.data@name) {
      b.team.df[i, "Team"] <- 'OPP'
    }
  }
  #Modifying minutes played into numeric values
  for (i in 1:nrow(b.players.df)) {
    b.players.df[i, 'MP'] <- time.to.numeric(b.players.df[i, 'MP'])
    a.players.df[i, 'MP'] <- time.to.numeric(a.players.df[i, 'MP'])
  }
  b.players.df[,'MP'] <- as.numeric(b.players.df[,'MP'])
  a.players.df[,'MP'] <- as.numeric(a.players.df[,'MP'])
  b.players.df <- select(b.players.df, -c('FG.', 'X3P.', 'FT.'))
  #removing NAs (for players who didnt take a shot) for simplicity
  a.players.df <- a.players.df[complete.cases(a.players.df),]
  
  return(list(b.players.df, a.players.df, b.team.df, a.team.df, dates))
}



####Class and functions for Storing ALL Players averages across a season (All Teams)####
#(This average includes if a player has games with multiple teams this season)

#S4 Class for storing averages (basic and advanced) at each date in season 
#for a player (independent of teams)
#SLOTS:
  #basic.mean: list of basic players averages at point in season
  #advanced.mean: same as above but with advanced player stats
  #basic.sd: list of standard deviation for player stats at each point in season
setClass("All Player Averages", slots=list(basic.mean = "list",
                                           advanced.mean = "list",
                                           basic.sd = "list"))

get.players.averages <- function(b.all.players.df, a.all.players.df, dates) {
  #Takes in data.frames of aggregated game data for a all teams and computes 
  #averages and standard deviations for all players, storing them into and object
  #of class "All Player Averages". 
  #(Note get inputs from return of "aggregate.players.data")
  #ARG(s):
    #b.all.players.df: a dataframe of the aggregated basic player stats over games
    #a.all.players.df: a dataframe of the aggregated team stats over games
    #dates: a list of dates used for collecting aggregated data to match game num
  #VALUE: A populated S4 object for "All Player Averages" 
  
  b.avgs.list <- c()
  b.sd.list <- c()
  a.avgs.list <- c()
  for (i in 2:length(dates)) {
    ##Basic Stats
    df1 <- filter(b.all.players.df, season.day < i)
    games.played.b <- count(df1, Players)
    #dataframe of averages of each stat
    mean.df1 <- group_by(df1, Players) %>%
      summarise_each(funs(mean)) %>%
      mutate(games=games.played.b$n)
    mean.df1 <- mean.df1[order(mean.df1$PTS, decreasing=TRUE),]
    #dataframe of standard deviations of each stat
    sd.df1 <- group_by(df1, Players) %>%
      summarise_each(funs(sd)) %>%
      mutate(games=games.played.b$n)
    sd.df1 <- sd.df1[order(sd.df1$PTS, decreasing=TRUE),]
    
    b.avgs.list[[dates[i]]] <- mean.df1
    b.sd.list[[dates[i]]] <- sd.df1
    
    ##Advanced Stats
    df2 <- filter(a.all.players.df, season.day < i)
    games.played.a <- count(df2, Players)
    df2 <- group_by(df2, Players) %>%
      summarise_each(funs(mean)) %>%
      mutate(games=games.played.a$n)
    df2 <- df2[order(df2$USG., decreasing=TRUE),]
    a.avgs.list[[dates[i]]] <- df2
  }
  #S4 class object of All Player Averages
  s4 <- new("All Player Averages", basic.mean = b.avgs.list, advanced.mean=a.avgs.list,
            basic.sd = b.sd.list)
  
  return (s4)
}


###Aggregating function for an input to the above averages function
aggregate.players.data <- function(all.teams.data, start_date, end_date) {
  #Takes in all games for all teams over a given date range and aggregates
  #the basic and advanced stats for both players
  #ARG(s):
    #all.teams.data: A list of objects of class "All Season Data" for all teams
    #start_date: starting date to begin collecting data
    #end_date: ending date for when to stop collecting data
  #VALUE: ##list of 3; 
    #1st Index: Basic Player data aggregated
    #2nd Index: Advanced player data aggregated
    #3rd index: dates list used in aggregation
  
  #getting list of dates to collect over
  dates <- c(end_date)
  prev <- prev.day(end_date)
  while (!(start_date %in% dates)) {
    index <- length(dates) + 1
    dates[index] <- prev
    prev <- prev.day(prev)
  }
  dates <- rev(dates)
  #initalizaing data.frames to collect basic and advanced stats
  b.all.players.df <- NA
  a.all.players.df <- NA
  
  #looping through all teams and all games for each team to populate dfs
  teams <- names(all.teams.data)
  for (t in teams) {
    print(t)
    games <- names(all.teams.data[[t]]@player.basic)
    for (game in games) {
      ##getting data
      basic <- all.teams.data[[t]]@player.basic[[game]]
      advanced <- all.teams.data[[t]]@player.advanced[[game]]
      #getting value for if team was home or away
      is_home <- filter(all.teams.data[[t]]@team.basic[[game]], Team == t)[,2]
      #marking what day in the season this is for the team and overall
      sn.day <- match(game, dates)
      basic <- mutate(basic, season.day=sn.day, is.home=is_home)
      advanced <- mutate(advanced, season.day=sn.day, is.home=is_home)
      #adding rows to main dfs
      if (is.data.frame(b.all.players.df)) {
        b.all.players.df<- bind_rows(b.all.players.df, basic)
        a.all.players.df<- bind_rows(a.all.players.df, advanced)
      }
      else {
        b.all.players.df <- basic
        a.all.players.df <- advanced
      }
    }
  }
  
  #cleaning up data to allow for averaging
  for (i in 1:nrow(b.all.players.df)) {
    b.all.players.df[i, 'MP'] <- time.to.numeric(b.all.players.df[i, 'MP'])
    a.all.players.df[i, 'MP'] <- time.to.numeric(a.all.players.df[i, 'MP'])
  }
  
  #final data pre-processing prior to averaging
  b.all.players.df[,'MP'] <- as.numeric(b.all.players.df[,'MP'])
  a.all.players.df[,'MP'] <- as.numeric(a.all.players.df[,'MP'])
  b.all.players.df <- select(b.all.players.df, -c('FG.', 'X3P.', 'FT.'))
  #removing NAs (for players who didnt take a shot) for simplicity
  a.all.players.df <- a.all.players.df[complete.cases(a.all.players.df),]
  
  return (list(b.all.players.df, a.all.players.df, dates))
}
