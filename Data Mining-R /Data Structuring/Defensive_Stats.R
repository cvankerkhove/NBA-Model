#File Name: Defensive_Stats.R
#Description: This file contains functions and a class used in calculating 
  #and storing defensive impact scores by position ('PG', 'SG', 'SF', 'PF', 'C')
  # and by group ('G', 'F', 'C'), and overall (no grouping)
#Author(s): Chris VanKerkhove
library(dplyr)

####Defensive Impact Class####

#S4 Class for storing defense impact scores and averages for each player on a 
#given team for each game in the season
#SLOTS:
  #team: Three letter character of team
  #min.n: minimum number of games to start averaging at
  #location: If this data object represents just Home games, Away games, or All
  #def.impact.pos: list of defense impact scores by position for players
  #def.impact.group: list of defense impact score by group
  #impact.averages.pos: list of averages of impact scores by position 
  #impact.averages.group: list of averages of impact scores by group
setClass("Defensive Impact", slots=list(name = "character", min.n = "numeric", location="character",
                                        def.impact.tot = "list", impact.averages.tot = "list",
                                        def.impact.pos = "list", impact.averages.pos = "list",
                                        def.impact.group = "list", impact.averages.group="list"))


def.impact.averages <- function(team, all.teams.data, diffs_tot, diffs_pos, 
                                diffs_group, positions_data, min.n=7, loc = NULL) {
  
  #Averages and stores defensive impact averages by position for players on a given
  #team over the course of a season (or specified input)
  #ARG(s):
    #team: three letter abbreviation of team (char)
    #all.teams.data: list of s4 objects "All Seasons Data"
    #diffs_tot: list of dataframes for def game scores overall
    #diffs_pos: list of dataframes for def game scores by position
    #opp_group: list of dataframes for def game scores by group
    #positions_data: dataframe of players and their positions
    #min.n: the minimum number of games to start averaging def scores 
    #loc: binary indicator if to average data for only home (1) or away (2) games 
      #if NULL no data filtering is performed
  #VALUES:
    #returns and s4 object "Defensive Impact"
  
  #filtering data if there is an indicator for home/away
  if (!(is.null(loc))) {
    diffs.tot <- c()
    diffs.pos <- c()
    diffs.group <- c()
    for (g in names(diffs_tot)) {
      if (diffs_tot[[g]]$is.home == loc) {
        diffs.tot[[g]] <- diffs_tot[[g]]
        diffs.pos[[g]] <- diffs_pos[[g]]
        diffs.group[[g]] <- diffs_group[[g]]
      }
    }
  }
  else {
    diffs.tot <- diffs_tot
    diffs.pos <- diffs_pos
    diffs.group <- diffs_group
  }
  
  #helper function for using inside loop when condesing positions
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
  
  #By total 
  #list to store each game defensive score by player
  def.impact.tot <- c()
  #list of defensive averages score 
  impact.averages.tot <- c()
  #BY Position ('PG', 'SG', 'SF', 'PF', 'C')
  def.impact.pos <- c()
  impact.averages.pos <- c()
  #By Grouping ('G', 'F', 'C')
  def.impact.group <- c()
  impact.averages.group <- c()
  
  ##getting games for this team
  games <- names(diffs.tot)
  #positions for this team
  positions <- filter(positions_data, Tm == team) %>%
    select(Player, Pos)
  
  #getting dates to collect averages over
  start.date <- '12-18-2020'
  end.date <- '06-21-2021'
  dates <- c(end.date)
  prev <- prev.day(end.date)
  while (!(start.date %in% dates)) {
    index <- length(dates) + 1
    dates[index] <- prev
    prev <- prev.day(prev)
  }
  dates <- rev(dates)
  
  #iterator
  i <- 0
  for (game in dates) {
    ###Averages operations (leading up to this game/date) therefore prior to new 
    #data operations on this game
    if (i >= min.n) {
      ##Averaging operations for data by total
      #vector for number of games for each player
      n.game <- count(averages.tot.df, Players)
      df.tot <- group_by(select(averages.tot.df,-c(Pos)), Players) %>%
        summarise_each(funs(weighted.mean(., MP)), -MP) %>%
        mutate(n=n.game[2]) %>%
        left_join(positions, by = c("Players" = "Player"))
      #append the player defensive averages prior to this game
      impact.averages.tot[[game]] <- select(df.tot, -c('X...', 'is.home'))
      
      ##Averaging operations for data by position
      n.game <- count(averages.pos.df, Players)
      df.pos <- group_by(select(averages.pos.df,-c(Pos)), Players) %>%
        summarise_each(funs(weighted.mean(., MP)), -MP) %>%
        mutate(n=n.game[2]) %>%
        left_join(positions, by = c("Players" = "Player"))
      #append the player defensive averages prior to this game
      impact.averages.pos[[game]] <- select(df.pos, -c('X...', 'is.home'))
      
      ##Averaging operations for data by grouping
      n.game <- count(averages.group.df, Players)
      df.group <- group_by(select(averages.group.df,-c(Pos)), Players) %>%
        summarise_each(funs(weighted.mean(.,MP)), -MP) %>%
        mutate(n=n.game[2]) %>%
        left_join(positions, by = c("Players" = "Player"))
      #append the averages to list for games leading up to this date
      impact.averages.group[[game]] <- select(df.group, -c('X...', 'is.home'))
    } 
    
    
    ###Operations to attach defense scores to players based on their positions
    if (game %in% games) {
      i <- i + 1
      #Box score information for our team this game
      basic <- all.teams.data[[team]]@player.basic[[game]]
      ##converting minutes played to numeric value
      for (i2 in 1:nrow(basic)) {
        basic[i2, 'MP'] <- time.to.numeric(basic[i2, 'MP'])
      }
      ##removing non averageable columns
      basic[,'MP'] <- as.numeric(basic[,'MP'])
      team.pos <- basic %>%
        left_join(positions, by = c("Players" = "Player")) %>%
        select(c('Players', 'Pos', 'MP'))
      
      ##Getting scores for total team performance
      team.all <- team.pos
      team.all$Pos <- 'All'
      def.score.tot <- left_join(team.all, diffs.tot[[game]], by = 'Pos')
      #appending dataframe to list
      def.impact.tot[[game]] <- def.score.tot
      
      ##Getting scores by pos for each player on team
      def.score.pos <- left_join(team.pos, diffs.pos[[game]], by = 'Pos')
      def.score.pos <- def.score.pos[complete.cases(def.score.pos),]
      #appending dataframe to list
      def.impact.pos[[game]] <- def.score.pos
      
      ##Getting scores by group for each player
      ##grouping into 'G', 'F', 'C' 
      team.group <- team.pos
      team.group$Pos <- apply(team.pos['Pos'], 1, defense.group)
      #joining defensive scores df with players
      def.score.group <- left_join(team.group, diffs.group[[game]], by = 'Pos')
      def.score.group <- def.score.group[complete.cases(def.score.group),]
      #appending dataframe to list
      def.impact.group[[game]] <- def.score.group
      
      #checking if averages data.frames have been initialized yet
      if (i == 1) {
        averages.tot.df <- def.score.tot
        averages.pos.df <- def.score.pos
        averages.group.df <- def.score.group
      }
      #if it has simply append new rows to respective data.frames
      else {
        averages.tot.df <- bind_rows(averages.tot.df, def.score.tot)
        averages.pos.df <- bind_rows(averages.pos.df, def.score.pos)
        averages.group.df <- bind_rows(averages.group.df, def.score.group)
      }
    }
  }
  
  #create s4 object "Defensive Impact Scores"
  s4 <- new("Defensive Impact", name = team, min.n = min.n, location = as.character(loc),
            def.impact.tot = def.impact.tot, impact.averages.tot = impact.averages.tot, 
            def.impact.pos = def.impact.pos, impact.averages.pos = impact.averages.pos, 
            def.impact.group = def.impact.group, impact.averages.group= impact.averages.group)
  return (s4)
}


opponents.stat.diffs <- function(team, all.teams.data, all.teams.avgs, positions_data) {
  #Calculates and stores differences in expected stats for each opponent a team 
  #has had, collects these dataframes in multiple lists and returns them (see below)
  #ARG(s):
    #team: three letter abbreviation of team (char)
    #all.teams.data: list of s4 objects "All Seasons Data"
    #all.team.avgs: list of objects "Season Averages" for all team
    #positions_data: dataframe of players and their positions
  #VALUES:
    ##list of 3 lists; 1st Index = list of scores summed  tot (no stratifying)
    #2nd index = list of scores by position (PG, SG, SF, PF, C)
    #3rd index = list of scores by grouping (G, F, C)
  
  #list for storing stat differences for oppoents 
  diffs_tot <- c()
  diffs_pos <- c()
  diffs_group <- c()
  #list of all games in this s4 object
  games <- names(all.teams.data[[team]]@player.basic)
  #helper function for using inside loop when condesing positions
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
  
  ###Iterating through all games and computing and storing performance
  for (game in games) {
    #getting the game opponents team ID
    t1 <- all.teams.data[[team]]@team.basic[[game]][1,1]
    t2 <- all.teams.data[[team]]@team.basic[[game]][2,1]
    if (t1 == team) {
      opp <- t2
    }
    else {
      opp <- t1
    }
    
    if (game %in% names(all.teams.avgs[[opp]]@player.basic.mean)) {
      ###Retrieving and cleaning basic and advanced box score, and player averages 
      #basic stats box score from this game
      basic <- all.teams.data[[opp]]@player.basic[[game]]
      ##converting minutes played to numeric value
      for (i in 1:nrow(basic)) {
        basic[i, 'MP'] <- time.to.numeric(basic[i, 'MP'])
      }
      ##removing non averageable columns
      basic[,'MP'] <- as.numeric(basic[,'MP'])
      basic <- select(basic, -c( 'FG.', 'X3P.', 'FT.'))
      
      #averages for opponent players who played in this game
      all.avgs <- all.teams.avgs[[opp]]@player.basic.mean[[game]]%>%
        select(-c('games', 'is.home')) %>%
        filter(Players %in% basic$Players)
      #averages of these players adjusted to a per minute basis
      avgs.per.mp <- bind_cols(all.avgs[1], select(all.avgs, -c('Players')) / all.avgs$MP)
      #joining in player names
      avgs.per.mp <- left_join(select(avgs.per.mp, -c('MP')), select(basic, c('Players', 'MP')), by = c('Players'='Players'))
      #expected stats based on minutes played
      expected.stats<- bind_cols(avgs.per.mp[1], select(avgs.per.mp, -c('Players')) * avgs.per.mp$MP) %>%
        select(-c('MP')) %>%
        arrange(Players)
      #arranging basic by players so rows match up to each dataframe
      basic <- arrange(basic, Players) %>%
        filter(Players %in% expected.stats$Players)
      #calculating adjusted differences dataframe
      adj.diff <- bind_cols(basic[1], select(expected.stats, -c('Players')) - select(basic, -c('Players', 'MP')))
      
      diffs_pos[[game]] <- adj.diff
      
      
      ###Aggregating by Position, Group and on no grouping and summing over rows
      ##NO GROUPING
      diff.tot <- summarise_each(select(adj.diff, -c('Players')),sum) %>%
        mutate(Pos = 'All')
      ##BY POSITION
      #positions data for opponent team
      positions <- filter(positions_data, Tm == opp) %>%
        select(Player, Pos)
      ##match positions to players in weighted scores df
      diff.by.pos <- adj.diff %>%
        left_join(positions, by = c("Players" = "Player")) %>%
        select(-c('Players'))
      diff.by.pos <- group_by(diff.by.pos, Pos) %>%
        summarise_each(sum)
      ##removing if position unknown
      diff.by.pos <- diff.by.pos[as.vector(!(is.na(diff.by.pos[,1]))),]
      
      ##BY DEFENSIVE GROUP ('G', 'F', 'C')
      diff.by.group <- diff.by.pos
      diff.by.group$Pos <- apply(diff.by.pos['Pos'], 1, defense.group)
      #grouping by position
      diff.by.group<- group_by(diff.by.group, Pos) %>%
        summarise_each(sum)
      
      #attaching is.home column to each df
      #getting value for if team was home or away
      is_home <- filter(all.teams.data[[team]]@team.basic[[game]], Team == team)[,2]
      diff.tot <- mutate(diff.tot, is.home = is_home)
      diff.by.pos <- mutate(diff.by.pos, is.home = is_home)
      diff.by.group <- mutate(diff.by.group, is.home = is_home)
      #append to list for storing data
      diffs_tot[[game]] <- diff.tot
      diffs_pos[[game]] <- diff.by.pos
      diffs_group[[game]] <- diff.by.group
    }
  }
  return (list(diffs_tot, diffs_pos, diffs_group))
}
