#File Name: Averaging_Functions.R
#Description: This file contains multiple functions that are either helper funcs
  #for other functions in this file, or functions that directly compute and 
  #return a dataframe of moving averages for players and teams
#Author(s): Chris VanKerkhove
library(dplyr)

####Helper Functions to computing Moving Averages####
prev.day <- function(date) {
  #function takes an input of date "MM-DD-YY format
  #and returns the previous day in same character format
  
  #Months with 31 days 
  m.31 <- c('01', '03', '05', '07', '08', '10', '12')
  #Months with 30 days
  m.30 <- c('02', '04', '06', '09', '11')
  #condition if entering a new month
  if (substr(date,4, 5 ) == '01') {
    #Updating month string
    #condition if going from January to December
    if (substr(date,1,2) == '01') {
      substr(date, 1,2) <- '12'
      substr(date,7,10) <- as.character(as.numeric(substr(date,7,10)) - 1)
    }
    #condition for staying a double digit month
    else if (as.numeric(substr(date,1,2)) > 10){
      substr(date, 1,2) <- as.character(as.integer(substr(date,1,2)) -1)
    }
    #else
    else {
      substr(date, 1,2) <- paste('0', as.character(as.integer(substr(date,1,2))-1),sep='')
    }
    #Updating Days
    if (substr(date,1,2) %in% m.31) {
      substr(date, 4,5) <- '31'
    }
    else if (substr(date,1,2) %in% m.30) {
      substr(date, 4,5) <- '30'
    }
    #February
    else {
      if (as.integer(substr(date,7,10)) %% 4 == 0 ) {
        substr(date, 4,5) <- '29'
      }
      else {
        substr(date, 4,5) <- '28'
      }
    }
  }
  #else staying in same month
  else {
    if (as.numeric(substr(date,4,5)) > 10) {
      substr(date, 4,5) <- as.character(as.integer(substr(date,4, 5 )) - 1)
    }
    else {
      substr(date, 4,5) <- paste('0', as.character(as.integer(substr(date,4,5))-1),sep='')
    }
  }
  return (date)
}


n.past.games <- function(team.data, date, n) {
  #helper function for function that computes moving averages
  #ARG(s):
    #team.data: an object of S4 class "All Seasons Data"
    #date: date to start getting averages from in "MM-DD-YYYY"
  #VALUE: a list of dates in "MM-DD-YYYY' format of the n past games from input date 
  
  date <- prev.day(date)
  #process for getting n most recent games 
  n.past.games <- rep(NA, n)
  index <- n
  while (index > 0) {
    if (date %in% names(team.data@team.basic)) {
      n.past.games[index] <- date
      index <- index - 1
    }
    date <- prev.day(date)
  }
  return (n.past.games)
}


time.to.numeric <- function(t) {
  #function that takes in a character of time and converts
  #(and returns) to numeric minutes
  #t: string input of time in minute-second
  
  #case for double digit minutes
  if (nchar(t) == 5) {
    min = as.numeric(substr(t,1,2))
    sec = as.numeric(substr(t,4,5))
  }
  #not double digit minutes
  else {
    min = as.numeric(substr(t,1,1))
    sec = as.numeric(substr(t,3,4))
  }
  return(min + sec/60)
}



####Functions for computing Moving Averages of Teams and Players####

compute.player.mov.avg <- function(team.data, date, n) {
  #Collects data of n-past games from a specified data and averages player stats
  #ARG(s):
    #team.data: object of S4 class Team Data
    #date: string input of what game to compute average prior to ("MM-DD-YYYY") 
    #n: number of most recent games for this moving average
    #dates of n most recent games from date
  #VALUES:
    #list of 2 dataframes; 1st index = basic player averages
                          #2nd index = advanced player averages
  
  dates <- n.past.games(team.data, date, n)
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
compute.team.mov.avg <- function(team.data, date, n) {
  #Collects data of n-past games from a specified data and averages specified
  #teams and their opponents stats
  #ARG(s):
  #team.data: object of S4 class Team Data
  #date: string input of what game to compute average prior to ("MM-DD-YYYY") 
  #n: number of most recent games for this moving average
  #dates of n most recent games from date
  #VALUES: #list of 2 dataframes; 1st index = basic teams averages
                                  #2nd index = advanced teams averages
  
  #dates of n most recent games from date
  dates <- n.past.games(team.data, date, n)
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



