setwd("~/Users/cvankerkhove/Documents/Github/NBA-Model")
library(dplyr)
source("Season_Classes.R")

#Getting all game data from 20201 season
pth <- "Seasons_Data/2021"
#list that contains an S4 object for each team
all.teams.data <- c()
teams <- list.files(path=pth)
for (t in teams) {
  all.teams.data[[t]] = get.team.data(t, '2021')
}

#Getting n-game moving averages at each point in 2021 season
n <- 5
all.teams.nmov.avgs <- c()
for (t in teams) {
  print(t)
  all.teams.nmov.avgs[[t]] = season.moving.averages(all.teams.data[[t]], n)
}
#Warriors 10 day moving averages
gsw.mv <- season.moving.averages(all.teams.data$GSW, n)

#Nets moving averages
nets.mv <- season.moving.averages(all.teams.data$BRK, 5)

pac.mv <- season.moving.averages(all.teams.data$IND, 6)
wiz.mv <- season.moving.averages(all.teams.data$WAS, 6)


#Getting season averages at each point in 2021 season
#min.n games to start averaging
min.n <- 5
all.teams.season.avgs <- c()
for (t in teams) {
  print(t)
  all.teams.season.avgs[[t]] = season.averages(all.teams.data[[t]], min.n)
}
#Warriors averages at each point in the season
gsw <- season.averages(all.teams.data$GSW)
clip <- season.averages(all.teams.data$LAC)
net <- season.averages(all.teams.data$BRK)


#####PLAYER AVERAGES OVER SEASON
all.player.averages <- get.players.averages("12-22-2020", all.teams.data)


source("Defensive_Impact_Scoring.R")
###Defensive Impact Scores mess around
#####Script for Getting Player Positions####
#getting valid player positions data
positions_data <- read.csv("NBA_player_position.csv")
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



impact.scores.all <- c()
impact.scores.home <- c()
impact.scores.away <- c()
for (t in teams) {
  print(t)
  team.games <- opponents.stat.diffs(t, all.teams.data, all.player.averages, positions_data)
  all.games <- def.impact.averages(t, all.teams.data, team.games[[1]], team.games[[2]], team.games[[3]], positions_data)
  home.games <- def.impact.averages(t, all.teams.data, team.games[[1]], team.games[[2]], team.games[[3]], positions_data, loc=1)
  away.games <- def.impact.averages(t, all.teams.data, team.games[[1]], team.games[[2]], team.games[[3]], positions_data, loc=0)
  impact.scores.all[[t]] <- all.games
  impact.scores.home[[t]] <- home.games
  impact.scores.away[[t]] <- away.games
}
##Non-position Scores (comparing how teams do overall home vs. away)
#for only home games


#scores by position for all
scores.pos <- impact.scores$ATL@impact.averages.pos
player.scores.pos <- select(scores.pos[[length(scores.pos)]], c('PTS', 'Players', 'Pos')) %>%
  mutate(team = 'ATL')
#scores by grouping for all 
scores.group <- impact.scores$ATL@impact.averages.group
player.scores.group <- select(scores.group[[length(scores.group)]], c('PTS', 'Players', 'Pos')) %>%
  mutate(team = 'ATL')

for (t in names(impact.scores)) {
  if (t != 'ATL') {
    scores.pos <- impact.scores[[t]]@impact.averages.group
    new.scores.pos <- select(scores.pos[[length(scores.pos)]], c('PTS', 'Players', 'Pos')) %>%
      mutate(team = t)
    player.scores.pos <- bind_rows(player.scores.pos, new.scores.pos)
    #scores by grouping for all 
    scores.group <- impact.scores[[t]]@impact.averages.group
    new.scores.group<- select(scores.group[[length(scores.group)]], c('PTS', 'Players', 'Pos')) %>%
      mutate(team = t)
    player.scores.group <- bind_rows(player.scores.group, new.scores.group)
  }
}


guard <- filter(player.scores.group, Pos %in% c('PG', 'SG'))
forward <- filter(player.scores.group, Pos %in% c('SF', 'PF'))
center <- filter(player.scores.group, Pos == 'C')
#histograms of stratefied scores
hist(guard$PTS)
hist(forward$PTS)
hist(center$PTS)

#filter on two teams
knicks <- filter(player.scores.pos, team == 'PHI')
wizrds <- filter(player.scores.group, team == "WAS")
hist(knicks$PTS)
hist(wizrds$PTS)

team.pts <- c()
for (t in teams) {
  tm <- filter(player.scores.group, team == t)
  team.pts[[t]] <- mean(tm$PTS)
}


o <- opponents.stat.diffs('DEN', all.teams.data, all.player.averages, positions_data)
l <- def.impact.averages('DEN', all.teams.data, o[[1]], o[[2]], o[[3]], positions_data)
l.home <- def.impact.averages('DEN', all.teams.data, o[[1]], o[[2]], o[[3]], positions_data, loc = 1)
l.away <- def.impact.averages('DEN', all.teams.data, o[[1]], o[[2]], o[[3]], positions_data, loc = 0)


df.pos <- group_by(select(averages.df,-c(Pos)), Players) %>%
  summarise_each(funs(weighted.mean(.,TOV)),-TOV)


#Time Series James Harden Assist,Pts, Reb over time
first <- all.player.averages@basic$`12-23-2020`
games <- c(1)
assit.avgs <- c(filter(first, Players=='Stephen Curry')['AST'])
pts.avgs <- c(filter(first, Players=='Stephen Curry')['PTS'])
rbs.avgs <- c(filter(first, Players=='Stephen Curry')['TRB'])
for (i in 6:length(all.player.averages@basic)) {
  day <- names(all.player.averages@basic)[i]
  game <- filter(all.player.averages@basic[[day]], Players=='Stephen Curry')['games']
  if (game != games[length(games)]) {
  pts <- filter(all.player.averages@basic[[day]], Players=='Stephen Curry')['PTS']
  ast <- c(filter(all.player.averages@basic[[day]], Players=='Stephen Curry')['AST'])
  rbs <- c(filter(all.player.averages@basic[[day]], Players=='Stephen Curry')['TRB'])
  assit.avgs[length(assit.avgs)+1] <- ast$AST
  pts.avgs[length(pts.avgs)+1] <- pts$PTS
  rbs.avgs[length(assit.avgs)+1] <- rbs$TRB
  games[length(games)+1] <- game$games
  }
}


#Getting all player averages object
ag_data2 <- aggregate.players.data(all.teams.data, "12-20-2020", "05-28-2021")
all.player.averages <- get.players.averages(ag_data2[[1]], ag_data2[[2]], ag_data2[[3]])

