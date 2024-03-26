source('Model_Data_Prep.R')

#required objects
all.teams.data




#Simple model with simple predictors
input.df <- NULL
for (team in teams) {
  print(team)
  #getting games to loop through and simulate
  games <- names(all.teams.avgs[[team]]@player.basic.mean)
  games <- games[10:length(games)]
  for (game in games) {
    
    ###Main Team Stats 
    opp <- filter(all.teams.data[[team]]@team.basic[[game]], Team != team)[,1]
    is.home <- filter(all.teams.data[[team]]@team.basic[[game]], Team == team)[2]
    #overall teams averages
    preds1 <- filter(all.teams.avgs[[team]]@team.basic.mean[[game]], Team==team) %>%
      select(c('PTS', 'TOV', 'win', 'STL', 'BLK'))
    #overall teams moving averages
    preds2 <- filter(all.teams.mov.avgs[[team]]@team.basic.mean[[game]], Team==team) %>%
      select(c('PTS', 'TOV', 'win')) %>%
      rename_all(funs(paste(., 'mov', sep='.')))
    #opposing team defense
    preds3 <- filter(all.teams.avgs[[opp]]@team.basic.mean[[game]], Team=='OPP') %>%
      select('PTS') %>%
      rename_all(funs(paste(., 'opp.allows', sep='.')))
    
    new_row <- bind_cols(preds1, preds2, preds3, is.home)
    ###Opp team stats
    #overall teams averages
    preds1.1 <- filter(all.teams.avgs[[opp]]@team.basic.mean[[game]], Team==opp) %>%
      select(c('PTS', 'TOV', 'win', 'STL', 'BLK')) %>%
      rename_all(funs(paste(., 'opp', sep='.')))
    #overall teams moving averages
    preds2.1 <- filter(all.teams.mov.avgs[[opp]]@team.basic.mean[[game]], Team==opp) %>%
      select(c('PTS', 'TOV', 'win')) %>%
      rename_all(funs(paste(., 'opp.mov', sep='.')))
    #opposing team defense
    preds3.1 <- filter(all.teams.avgs[[team]]@team.basic.mean[[game]], Team=='OPP') %>%
      select('PTS') %>%
      rename_all(funs(paste(., 'opp.opp.allows', sep='.')))
    
    new_row.1 <- bind_cols(preds1.1, preds2.1, preds3.1)
    
    row <- bind_cols(new_row, new_row.1)
    
    #1 for main team win, 0 for lost
    main.score <- filter(all.teams.data[[team]]@team.basic[[game]], Team == team)[,'PTS']
    opp.score <- filter(all.teams.data[[team]]@team.basic[[game]], Team != team)[,'PTS']
    row['outcome'] <- as.numeric(main.score > opp.score)
    
    #checking if input.df has been initialized yet
    if (is.null(input.df)) {
      input.df <- row
    }
    else {
      input.df <- bind_rows(input.df, row)
    }
  }
}

input.df$Home.Team.. <- as.factor(input.df$Home.Team..)
train_ind <- sample(1:nrow(input.df), 0.65*nrow(input.df))
train <- input.df[train_ind,]                    
test <- input.df[-train_ind,]

logit <- glm(outcome~., data = train, family = binomial)
#summary(logit)
#running test data through model
pred <- predict(logit,test , type = "response")
pred.bin <- as.numeric(pred >= 0.5)
mean(pred.bin == test$outcome)


#KNN
library("class")
knn.pred=knn(train, test, train$outcome, k=1)
table(knn.pred, test$outcome)
1-mean(knn.pred==test$outcome)

#SVM
library(e1071)
tune.out <- tune(svm, outcome~., data=train, kernel="radial", 
         ranges=list(cost=c(0.001, 0.01, 0.1,1,5,10,100)),
         gamma=c(0.5,1,2,3,4), scale=T)
bestmod <- tune.out$best.model

ypred <- predict(bestmod, test)
pred.bin <- as.numeric(pred >=0.5)
mean(pred.bin == test$outcome)






