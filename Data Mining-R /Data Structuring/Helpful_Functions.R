#File Name: Helpful_Functions.R
#Description: This file contains multiple functions that are either helper funcs
  #for other functions in this file, or functions that directly compute and 
  #return a dataframe of moving averages for players and teams
#Author(s): Chris VanKerkhove
library(dplyr)
library(ggplot2)
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


get.team.data <- function(team, season, pth) {
  #function that searches through a specified NBA team games (in a specified season)
  #and executes processes for storing information in a S4 R class object ('All Data')
  #Arg(s):
  #team: 3 letter character representing NBA team
  #season: Date of season (year it ends in)
  
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

####Helpful Functions####
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


n.past.games <- function(team.data, date, n, including = FALSE) {
  #helper function for function that computes moving averages
  #ARG(s):
    #team.data: an object of S4 class "All Seasons Data"
    #date: date to start getting averages from in "MM-DD-YYYY"
    #including: boolean if to include the input date (note if value is TRUE)
      #a list of length(n+1) will be returned (vs. n)
  #VALUE: a list of dates in "MM-DD-YYYY' format of the n past games from input date 
  
  if (!(including)) {
    date <- prev.day(date)
  }
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

get.pos.data <- function(positions_data) {
  #function that when runs returns a datafrmae with valid player positions 
  #for each player in the league
  #removing jr. suffix function for apply function
  suffix <- function(x) {
    suf <- substr(x, nchar(x)-3, nchar(x))
    if (suf == " Jr.") {
      out <- substr(x, 1, nchar(x)-4)
    }
    else {
      out <- x
    }
    return(out)
  }
  players_new <- sapply(positions_data$Player, suffix)
  positions_data$Player <- players_new
  #adding group column to dataframe
  #helper function for condensing positions
  defense.group <- function(pos) {
    if (pos %in% c('PG', 'SG')) {
      group <- 'G'
    }
    else if (pos %in% c('SF', 'PF')) {
      group <- 'F'
    }
    else {
      group <- 'C'
    }
    return(group)
  }
  positions_data <- mutate(positions_data, Group = Pos)
  positions_data$Group <- apply(positions_data['Pos'], 1, defense.group)
  
  return(positions_data)
}


####Plotting####
plot_multi_histogram <- function(df, feature, label_column) {
  plt <- ggplot(df, aes(x=eval(parse(text=feature)), fill=eval(parse(text=label_column)))) +
    geom_histogram(alpha=0.7, position="identity", aes(y = ..density..), color="black") +
    geom_density(alpha=0.7) +
    geom_vline(aes(xintercept=mean(eval(parse(text=feature)))), color="black", linetype="dashed", size=1) +
    labs(x=feature, y = "Density")
  plt + guides(fill=guide_legend(title=label_column))
}



