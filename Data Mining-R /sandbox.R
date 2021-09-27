setwd("~/Documents/GitHub /NBA-Model")
source("Helpful_Functions.R")
source("Offensive_Stats.R")
source("Defensive_Stats.R")

#Getting all game data from 20201 season
pth <- "Seasons_Data/2021"
#list that contains an S4 object for each team
all.teams.data <- c()
teams <- list.files(path=pth)
for (t in teams) {
  all.teams.data[[t]] = get.team.data(t, '2021')
}

###Offensive Stats###
#Getting season averages at each point in 2021 season
all.teams.avgs <- c()
for (t in teams) {
  print(t)
  ag_data <- aggregate.data.team(all.teams.data[[t]])
  all.teams.avgs[[t]] = season.averages(all.teams.data[[t]], ag_data[[1]], ag_data[[3]], ag_data[[5]])
}

###Defensive Stats###
#getting positions data
positions_data <- get.pos.data()
def.scores <- c()
for (t in teams) {
  print(t)
  team.games <- opponents.stat.diffs(t, all.teams.data, all.teams.avgs, positions_data)
  all.games <- def.impact.averages(t, all.teams.data, team.games[[1]], team.games[[2]], team.games[[3]], positions_data)
  def.scores[[t]] <- all.games
}



#Getting all player averages object
ag_data2 <- aggregate.players.data(all.teams.data, "12-20-2020", "05-28-2021")
all.player.averages <- get.players.averages(ag_data2[[1]], ag_data2[[2]], ag_data2[[3]])
