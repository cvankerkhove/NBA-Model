
create.predictor.df <- function(Team, all.teams.data, all.teams.nmov.avgs, 
                                all.player.averages, n.players = 7) {
  #function that creates a datframe of predictors vs. prediction for a input team
  #to be used in model fitting
  #ARG(s):
  #Team: Three letter string of input team
  #all.teams.data: list of S4 object "All Season Data" for all teams
  #all.teams.nmov.avgs: list of S4 objects "N Averages"
  #all.player.averages: list of S4 objetcs "All Player Averages"
  #n.players: The number of players to average for player averages (by most minutes played)
  
  df.master <- NULL
  #first game in dataset
  first <- names(all.teams.nmov.avgs[['Team']]@team.basic)[1]
  #our teams averages going into this game (their averages and the averages of their opponentS)
  team.avgs1 <- all.teams.nmov.avgs[['Team']]@team.basic[[first]][1,]
  op.avgs1 <- all.teams.nmov.avgs[['Team']]@team.basic[[first]][2,]
  
}


#ALL averages stats for input Team going into game
#KEY: 
  #'t-t': means this teams averages
  #'t-o': means the averages of opponents this team has played
df1 <- data.frame(team.stats.b[1,]) %>% 
  select(-c('Type')) %>%
  rename_all(funs(paste(., 't-t', sep='.')))

  
df2 <- data.frame(team.stats.a[1,]) %>%
  select(-c('Type')) %>%
  rename_all(funs(paste(., 't-t', sep='.')))


df3 <- data.frame(team.stats.b[2,]) %>%
  select(-c('Type')) %>%
  rename_all(funs(paste(., 't-o', sep='.'))) 

df4 <- data.frame(team.stats.a[2,]) %>%
  select(-c('Type')) %>%
  rename_all(funs(paste(., 't-o', sep='.')))

df <- bind_cols(df1, df2, df3, df4)
