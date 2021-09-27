#File Name: Seasons_Class.R
#Description: This file contains class definitions and functions necessary 
  #for getting box score data for NBA teams in a specified season
  #(Note data is scraped from the web and stored using a python script)
#Author(s): Chris VanKerkhove
library(dplyr)
source("Averaging_Functions.R")





####Class for storing N Game Moving Averages (Single Team)####

#S4 Class for storing moving averages (basic and advanced) 
#at each point in season for teams and players based on "n" past games
#SLOTS:
  #name: Three letter character of team
  #season: character of the year of season for this data
  #player.basic: list of basic players n moving averages at point in season
  #player.advanced: same as above but with advanced player stats
  #team.basic: same as above but for basic team stats
  #team.advanced: same as above but for advanced team stats
setClass("N Averages", slots=list(name = "character", n = "numeric",
                                  player.basic = "list", player.advanced = "list",
                                  team.basic = "list", team.advanced="list"))

season.moving.averages <- function(team.data, n) {
  #function that creates and populates an S4 class object "N Averages"
  #for input team data
  #ARG(s):
  #team.data: An S4 class object "All Season Data" of a single team
  #n: number of games to compute moving average off of 
  #VALUE: A populated S4 object for team "team"
  
  #getting all past games through team.data
  num <- length(team.data@team.basic)
  current.date <- paste(substr(Sys.Date(), 6,7), substr(Sys.Date(),9,10), 
                        substr(Sys.Date(),1,4), sep='-')
  games <- n.past.games(team.data, current.date, num)
  #list of player basic, advanced averages
  b.p.avgs <- c()
  a.p.avgs <- c()
  #list of team basic, advanced averages
  b.t.avgs <- c()
  a.t.avgs <- c()
  #Looping through games (after first n games of seasons)
  for (i in (n+1):num) {
    #adding team averages to this list
    t.avgs <- compute.team.mov.avg(team.data, games[i], n)
    b.t.avgs[[games[i]]] <- t.avgs[[1]]
    a.t.avgs[[games[i]]] <- t.avgs[[2]]
    #adding player averages to this list
    p.avgs <- compute.player.mov.avg(team.data, games[i],n)
    b.p.avgs[[games[i]]] <- p.avgs[[1]]
    a.p.avgs[[games[i]]] <- p.avgs[[2]]
  }
  #current day n-moving averages up to today
  avgs <- compute.team.mov.avg(team.data, current.date, n, including=TRUE)
  b.t.avgs[[current.date]] <- avgs[[1]]
  a.t.avgs[[current.date]] <- avgs[[2]]
  avgs <- compute.player.mov.avg(team.data, current.date, n, including=TRUE)
  b.p.avgs[[current.date]] <- avgs[[1]]
  a.p.avgs[[current.date]] <- avgs[[2]]
  #S4 class object of Team Averages
  s4 <- new("N Averages", name = team.data@name, n = n, player.basic = b.p.avgs, 
            player.advanced=a.p.avgs, team.basic = b.t.avgs, team.advanced=a.t.avgs)
  return (s4)
}


####Class for storing Averages over entire Season (Single Team)####
##TOODO: Rehaul season.averages function***

#S4 Class for storing averages (basic and advanced) at each point in season 
#for players on a specified team
#SLOTS:
  #name: Three letter character of team
  #season: character of the year of season for this data
  #player.basic: list of basic players averages at point in season
  #player.advanced: same as above but with advanced player stats
  #team.basic: same as above but for basic team stats
  #team.advanced: same as above but for advanced team stats
setClass("Season Averages", slots=list(name = "character", min.n = "numeric",
                                  player.basic = "list", player.advanced = "list",
                                  team.basic = "list", team.advanced="list"))

season.averages <- function(team.data, min.n = 5) {
  #function that creates and populates an S4 class object "Season Averages"
  #for input team data
  #ARG(s):
  #team.data: An S4 class object "All Season Data" of a single team
  #min.n: min amount of games to start recording averages (default=5)
  #VALUE: A populated S4 object for team "team"
  
  #getting all games data
  num <- length(team.data@team.basic)
  current.date <- paste(substr(Sys.Date(), 6,7), substr(Sys.Date(),9,10), 
                        substr(Sys.Date(),1,4), sep='-')
  games <- n.past.games(team.data, current.date, num)
  #list of player basic, advanced averages
  b.p.avgs <- c()
  a.p.avgs <- c()
  #list of team basic, advanced averages
  b.t.avgs <- c()
  a.t.avgs <- c()
  for (i in (min.n+1):num) {
    #adding team averages to this list
    t.avgs <- compute.team.mov.avg(team.data, games[i], i-1)
    b.t.avgs[[games[i]]] <- t.avgs[[1]]
    a.t.avgs[[games[i]]] <- t.avgs[[2]]
    #adding player averages to this list
    p.avgs <- compute.player.mov.avg(team.data, games[i], i-1)
    b.p.avgs[[games[i]]] <- p.avgs[[1]]
    a.p.avgs[[games[i]]] <- p.avgs[[2]]
  }
  #current averages up to today
  avgs <- compute.team.mov.avg(team.data, current.date, num, including=TRUE)
  b.t.avgs[[current.date]] <- avgs[[1]]
  a.t.avgs[[current.date]] <- avgs[[2]]
  avgs <- compute.player.mov.avg(team.data, current.date, num, including=TRUE)
  b.p.avgs[[current.date]] <- avgs[[1]]
  a.p.avgs[[current.date]] <- avgs[[2]]
  
  #S4 class object of Team Averages
  s4 <- new("Season Averages", name = team.data@name, min.n = min.n, player.basic = b.p.avgs, 
            player.advanced=a.p.avgs, team.basic = b.t.avgs, team.advanced=a.t.avgs)
  return (s4)
}







####Class for Storing ALL Players averages across a season (All Teams)####
#(This average includes if a player has games with multiple teams this season)

#S4 Class for storing averages (basic and advanced) at each date in season 
#for a player (independent of teams)
#SLOTS:
#season: character of the year of season for this data
#player.basic: list of basic players verages at point in season
#player.advanced: same as above but with advanced player stats
#team.basic: same as above but for basic team stats
#team.advanced: same as above but for advanced team stats
setClass("All Player Averages", slots=list(basic = "list",
                                           advanced = "list"))

get.players.averages <- function(start.date, all.teams.data) {
  
  #getting dates to collect averages over
  current.date <- paste(substr(Sys.Date(), 6,7), substr(Sys.Date(),9,10), 
                        substr(Sys.Date(),1,4), sep='-')
  dates <- c(current.date)
  prev <- prev.day(current.date)
  while (!(start.date %in% dates)) {
    index <- length(dates) + 1
    dates[index] <- prev
    prev <- prev.day(prev)
  }
  #initalizaing data.frames to collect basic and advanced stats
  #x <- all.teams.data$LAL@player.basic$`12-25-2020`
  #x2 <- all.teams.data$LAL@player.advanced$`12-25-2020`
  b.all.players.df <- NA
  a.all.players.df <- NA
  
  #looping through all teams and all games for each team to populate dfs
  teams <- names(all.teams.data)
  for (i in 1:length(teams)) {
    games <- names(all.teams.data[[teams[i]]]@player.basic)
    for (i2 in 1:length(games)) {
      #getting data
      basic <- all.teams.data[[teams[i]]]@player.basic[[games[i2]]]
      advanced <- all.teams.data[[teams[i]]]@player.advanced[[games[i2]]]
      #marking what day in the season this is
      n <- length(dates) - match(games[i2], dates) + 1
      col.n <- rep(n, nrow(basic))
      col2.n <- rep(n, nrow(advanced))
      basic["n.day"] <- col.n
      advanced["n.day"] <- col2.n
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
  #populating S4 object that holds players averages at each point in season
  
  b.avgs.list <- c()
  a.avgs.list <- c()
  for (i in 1:length(dates)) {
    df1 <- filter(b.all.players.df, n.day <= i)
    games.played.b <- count(df1, Players)
    df1 <- group_by(df1, Players) %>%
      summarise_each(funs(mean)) %>%
      mutate(games=games.played.b$n)
    df1 <- df1[order(df1$PTS, decreasing=TRUE),]
    b.avgs.list[[dates[length(dates)-(i-1)]]] <- df1
    
    df2 <- filter(a.all.players.df, n.day <= i)
    games.played.a <- count(df2, Players)
    df2 <- group_by(df2, Players) %>%
      summarise_each(funs(mean)) %>%
      mutate(games=games.played.a$n)
    df2 <- df2[order(df2$USG., decreasing=TRUE),]
    a.avgs.list[[dates[length(dates)-(i-1)]]] <- df2
  }
  #S4 class object of All Player Averages
  s4 <- new("All Player Averages", basic = b.avgs.list, advanced=a.avgs.list)
  
  return (s4)
}




















####Not Yet Implemented Using Aggreatted Data Class s
season.averages2 <- function(team.data, min.n = 5) {
  #function that creates and populates an S4 class object "N Averages"
  #for input team data
  #ARG(s):
  #team.data: An S4 class object "All Season Data" of a single team
  #n: number of games to compute moving average off of 
  #VALUE: A populated S4 object for team "team"
  
  #getting all past games through team.data
  num <- length(team.data@team.basic)
  current.date <- paste(substr(Sys.Date(), 6,7), substr(Sys.Date(),9,10), 
                        substr(Sys.Date(),1,4), sep='-')
  games <- n.past.games(team.data, current.date, num)
  #Initializing an S4 class object of "Aggregated Data"
  df1 <- team.data@player.basic[[games[1]]] %>%
    mutate(game.n = rep(1, nrow(team.data@player.basic[[games[1]]])))
  df2 <- team.data@player.advanced[[games[1]]] %>%
    mutate(game.n = rep(1, nrow(team.data@player.advanced[[games[1]]])))
  df3 <- team.data@team.basic[[games[1]]] %>%
    mutate(game.n = rep(1,nrow(team.data@team.basic[[games[1]]])))
  df4 <- team.data@team.advanced[[games[1]]] %>%
    mutate(game.n = rep(1,nrow(team.data@team.advanced[[games[1]]])))
  for (i in 2:n) {
    df1.1 <- team.data@player.basic[[games[i]]] %>%
      mutate(game.n = rep(i, nrow(team.data@player.basic[[games[i]]])))
    df2.1 <- team.data@player.advanced[[games[i]]] %>%
      mutate(game.n = rep(i, nrow(team.data@player.advanced[[games[i]]])))
    df3.1 <- team.data@team.basic[[games[i]]] %>%
      mutate(game.n = rep(i,nrow(team.data@team.basic[[games[i]]])))
    df4.1 <- team.data@team.advanced[[games[i]]] %>%
      mutate(game.n = rep(i,nrow(team.data@team.advanced[[games[i]]])))
    df1 <- bind_rows(df1, df1.1)
    df2 <- bind_rows(df2, df2.1)
    df3 <- bind_rows(df3, df3.1)
    df4 <- bind_rows(df4, df4.1)
  }
  ag.data <- new("Aggregated Data", name=team.data@name, player.basic = df1,
                 player.advanced = df2, team.basic = df3, team.advanced=df4)
  
  #list of player basic, advanced averages
  b.p.avgs <- c()
  a.p.avgs <- c()
  #list of team basic, advanced averages
  b.t.avgs <- c()
  a.t.avgs <- c()
  #Looping through games (after first n games of seasons)
  for (i in (n+1):num) {
    #adding team averages to this list
    #t.out <- compute.team.mov.avg(team.data, games[i], i, )
    #b.t.avgs[[games[i]]] <- t.out[[1]]
    #a.t.avgs[[games[i]]] <- t.out[[2]]
    #ag.data <- t.out[[3]]
    #adding player averages to this list
    p.out <- player.mov.avg(team.data, ag.data, games[i], i, i, entire.season = TRUE)
    b.p.avgs[[games[i]]] <- p.out[[1]]
    a.p.avgs[[games[i]]] <- p.out[[2]]
    ag.data <- p.out[[3]]
  }
  
  #S4 class object of Team Averages
  s4 <- new("Season Averages", name = team.data@name, min.n = min.n, player.basic = b.p.avgs, 
            player.advanced=a.p.avgs, team.basic = b.t.avgs, team.advanced=a.t.avgs)
  return (s4)
  
}



####Functions for computing Moving Averages of Teams and Players####
#(for a specified team)

compute.player.mov.avg <- function(team.data, date, n, including = FALSE) {
  #Collects data of n-past games from a specified data and averages player stats
  #ARG(s):
  #team.data: object of S4 class Team Data
  #date: string input of what game to compute average prior to ("MM-DD-YYYY") 
  #n: number of most recent games for this moving average
  #dates of n most recent games from date
  #VALUES:
  #list of 2 dataframes; 1st index = basic player averages
  #2nd index = advanced player averages
  
  dates <- n.past.games(team.data, date, n, including)
  #aggregate player data (basic and advanced) over time period
  b.df <- team.data@player.basic[[dates[1]]]
  a.df <- team.data@player.advanced[[dates[1]]]
  for (i in 2:n) {
    b.data <- team.data@player.basic[[dates[i]]]
    a.data <- team.data@player.advanced[[dates[i]]]
    #binding rows together to overall data
    b.df <- bind_rows(b.df, b.data)
    a.df <- bind_rows(a.df, a.data)
  }
  for (i in 1:nrow(b.df)) {
    b.df[i, 'MP'] <- time.to.numeric(b.df[i, 'MP'])
    a.df[i, 'MP'] <- time.to.numeric(a.df[i, 'MP'])
  }
  
  #final data pre-processing prior to averaging
  b.df[,'MP'] <- as.numeric(b.df[,'MP'])
  a.df[,'MP'] <- as.numeric(a.df[,'MP'])
  b.df <- select(b.df, -c('FG.', 'X3P.', 'FT.'))
  #removing NAs (for players who didnt take a shot) for simplicity
  a.df <- a.df[complete.cases(a.df),]
  
  ##Averaging
  #averaging for players over n games
  games.played.b <- count(b.df, Players)
  games.played.a <- count(a.df, Players)
  #basiv averages
  b.df.avg <- group_by(b.df, Players) %>%
    summarise_each(funs(mean)) %>%
    mutate(games=games.played.b$n)
  #advanced averages
  a.df.avg <- group_by(a.df, Players) %>%
    summarise_each(funs(mean)) %>%
    mutate(games=games.played.a$n)
  
  return (list(b.df.avg, a.df.avg, b.df))
}


###Function for Computing Team Averages and Opponent Averages 
compute.team.mov.avg <- function(team.data, date, n, including = FALSE) {
  #Collects data of n-past games from a specified data and averages specified
  #teams and their opponents stats
  #ARG(s):
  #team.data: object of S4 class Team Data
  #date: string input of what game to compute average prior to ("MM-DD-YYYY") 
  #n: number of most recent games for this moving average
  #including: boolean for it to include input date in averages
  #VALUES: #list of 2 dataframes; 1st index = basic teams averages
  #2nd index = advanced teams averages
  
  #dates of n most recent games from date
  dates <- n.past.games(team.data, date, n, including)
  #creating data.frames to store rows for each game used in computing average
  b.team.df <- tibble(team.data@team.basic[[dates[1]]][1,])
  b.opponent.df <- tibble(team.data@team.basic[[dates[1]]][2,])
  a.team.df <- tibble(team.data@team.advanced[[dates[1]]][1,])
  a.opponent.df <- tibble(team.data@team.advanced[[dates[1]]][2,])
  #adding rows to data.frames
  for (i in 2:n) {
    b.team.df <- b.team.df %>% 
      add_row(team.data@team.basic[[dates[i]]][1,])
    b.opponent.df <- b.opponent.df %>%
      add_row(team.data@team.basic[[dates[i]]][2,])
    a.team.df <- a.team.df %>%
      add_row(team.data@team.advanced[[dates[i]]][1,])
    a.opponent.df %>%
      add_row(team.data@team.advanced[[dates[i]]][2,])
  }
  
  #averaging basic stats for both team and opponent
  b.df <- summarise_all(b.team.df[-c(1,2)],mean)
  b.df <- b.df %>%
    add_row(summarise_all(b.opponent.df[-c(1,2)],mean))
  b.df$Type = c('Team', 'Opponent')
  b.df2 <- b.df %>%
    select(Type, everything())
  #averaging advanced stats for both team and opponent
  a.df <- summarise_all(a.team.df[-c(1,2)],mean)
  a.df <- a.df %>%
    add_row(summarise_all(a.opponent.df[-c(1,2)],mean))
  a.df$Type = c('Team', 'Opponent')
  a.df2 <- a.df %>%
    select(Type, everything())
  return (list(b.df2, a.df2))
}
























####TODO: Functions using Agregated Data for Imporved Effeciency####
#S4 Class to help compute the season averages in a computational efficient way
#by binding rows to the same data.frame for each box score statistic
#SLOTS:
#name: Three letter character of team
#player.basic: data.frame of basic player data (updated over games)
#player.advanced: same as above but with advanced player stats
#team.basic: same as above but for basic team stats
#team.advanced: same as above but for advanced team stats
setClass("Aggregated Data", slots=list(name = "character", player.basic = "data.frame", 
                                       player.advanced = "data.frame", team.basic = "data.frame", 
                                       team.advanced="data.frame"))


player.mov.avg <- function(team.data, ag.data, date,game.index, n, entire.season=FALSE) {
  #Collects data of n-past games from a specified data and averages player stats
  #ARG(s):
  #team.data: object of S4 class Team Data
  #ag.data: the aggreated data.frames of box scores up to date "date"
  #date: string input of what game to compute average prior to ("MM-DD-YYYY") 
  #i: index game number in the season we are copmuting average for
  #n: number of most recent games for this moving average
  #dates of n most recent games from date
  #VALUES:
  #list of 3 dataframes; 1st index = basic player averages
  #2nd index = advanced player averages
  #3rd index = updated ag.data object
  
  ###Data retrieval and Processing
  #case for if computing averages over entire season
  if (entire.season) {
    b.df <- ag.data@player.basic
    a.df <- ag.data@player.advanced
  }
  #case if copmuting moving averages
  else {
    #getting games in this n game stretch from ag.data
    games.i <- seq(game.index-n, game.index-1, 1)
    b.df <- ag.data@player.basic %>%
      filter(game.n %in% games.i)
    a.df <- ag.data@player.advanced %>%
      filter(game.n %in% games.i)
  }
  #converting MP to numeric for averaging
  for (i in 1:nrow(b.df)) {
    b.df[i, 'MP'] <- time.to.numeric(b.df[i, 'MP'])
    a.df[i, 'MP'] <- time.to.numeric(a.df[i, 'MP'])
  }
  #final data pre-processing prior to averaging
  b.df[,'MP'] <- as.numeric(b.df[,'MP'])
  a.df[,'MP'] <- as.numeric(a.df[,'MP'])
  b.df <- select(b.df, -c('FG.', 'X3P.', 'FT.'))
  #removing NAs (for players who didnt take a shot) for simplicity
  a.df <- a.df[complete.cases(a.df),]
  
  ###Averaging
  #games played by player in this stretch
  games.played.b <- group_by(b.df, Players) %>%
    summarise(player.n=n(), .groups='drop')
  games.played.a <- group_by(a.df, Players) %>%
    summarise(player.n=n(), .groups='drop')
  #basic data averaging
  b.df.avg <- group_by(b.df, Players) %>%
    summarise_each(funs(mean)) %>%
    mutate(games=games.played.b$player.n)
  #advanced data averaging
  a.df.avg <- group_by(a.df, Players) %>%
    summarise_each(funs(mean)) %>%
    mutate(games=games.played.a$player.n)
  
  ###Updating ag.data with game on this input data
  #new basic player data
  new.b.df <- team.data@player.basic[[date]] %>%
    mutate(game.n = rep(game.index, nrow(team.data@player.basic[[date]])))
  #new advnaced player data
  new.a.df <- team.data@player.advanced[[date]] %>%
    mutate(game.n = rep(game.index, nrow(team.data@player.advanced[[date]])))
  #adding new data to ag.data
  ag.data@player.basic <- bind_rows(ag.data@player.basic, new.b.df)
  ag.data@player.advanced <- bind_rows(ag.data@player.advanced, new.a.df)
  
  return (list(b.df.avg, a.df.avg, ag.data))
}




