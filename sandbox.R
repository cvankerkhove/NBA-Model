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
n <- 7
all.teams.nmov.avgs <- c()
for (t in teams) {
  print(t)
  all.teams.nmov.avgs[[t]] = season.moving.averages(all.teams.data[[t]], n)
}

#Getting season averages at each point in 2021 season
#min.n games to start averaging
min.n <- 5
all.teams.season.avgs <- c()
for (t in teams) {
  print(t)
  all.teams.season.avgs[[t]] = season.averages(all.teams.data[[t]], min.n)
}
#Warriors averages at each point in the season
gsw <- season.averages(all.teams.data$GSW, min.n)

