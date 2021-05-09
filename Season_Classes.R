#File Name: Data_Getter.R
#Description: This file contains class definitions and functions necessary 
  #for getting box score data for NBA teams in a specified season
  #(Note data is scraped from the web and stored using a python script)
#Author(s): Chris VanKerkhove
library(dplyr)
source("Averaging_Functions.R")


####Class for storing gathered Box Scores####

#All Season Data is an S4 object that stores basic,advanced box score data for
#all games by a specified Team from a specified year
#SLOTS:
  #name: Three letter character of team
  #season: character of the year of season for this data
  #player.basic: named list of data.frames with basic player stats from named date
  #player.advanced: same as above but with advanced player stats
  #team.basic: same as above but for basic team stats
  #team.advanced: same as above but for advanced team stats
setClass("All Season Data", slots=list(name = "character",season="character", player.basic="list", 
                                 player.advanced="list", team.basic="list",
                                 team.advanced="list"))


get.team.data <- function(team, season) {
  #function that searches through a specified NBA team games (in a specified season)
  #and executes processes for storing information in a S4 R class object ('All Data')
  #Arg(s):
    #team: 3 letter character representing NBA team
    #season: Date of season (year it ends in)
  pth <- paste("~/Documents/Github/NBA-Model/Seasons_Data", season, team, sep='/')
  #empty containers for box score data
  b.player <- c()
  a.player <- c()
  b.teams <- c()
  a.teams <- c()
  months <- list.files(path=pth)
  #iterating through months these teams played
  for (m in months) {
    games <- list.files(path=paste(pth, m, sep='/'))
    for (g in games) {
      b.player[[g]] = read.csv(paste(pth, m, g, "basic_player_data.csv", sep='/'))
      a.player[[g]] = read.csv(paste(pth, m, g, "advanced_player_data.csv", sep='/'))
      b.teams[[g]] = read.csv(paste(pth, m, g, "teams_basic_data.csv", sep='/'))
      a.teams[[g]] = read.csv(paste(pth, m, g, "teams_advanced_data.csv", sep='/'))
      
    }
  }
  #S4 class object: "All Season Data" 
  s4 <- new("All Season Data", name = team, season = "character",player.basic = b.player, 
            player.advanced = a.player, team.basic = b.teams, team.advanced = a.teams)
  return (s4)
} 


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

#S4 Class for storing averages (basic and advanced) at each point in season 
#for players on a specified team
#SLOTS:
  #name: Three letter character of team
  #season: character of the year of season for this data
  #player.basic: list of basic players verages at point in season
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
  
  #getting n.past.games data
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




















####Not Yet Implemented Using Aggreatted Data Class
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



