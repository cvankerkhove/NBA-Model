setwd("~/Documents/Github/NBA-Model")
library(dplyr)
source("Season_Classes.R")

#Getting all game data from 20201 season
pth <- "~/Documents/Github/NBA-Model/Seasons_Data/2021"
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



player.stats.df <- data.frame()
for (t in team) {
  all.teams.data[[t]]@player.basic
  
}

#####PLAYER AVERAGES OVER SEASON
all.player.averages <- get.players.averages("12-22-2020", all.teams.data)

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




