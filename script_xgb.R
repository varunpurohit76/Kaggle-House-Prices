test<-read.csv("test.csv")
train<-read.csv("train.csv")

submission<- data.frame(Id=test$Id)
train$Id<-NULL
test$Id<-NULL

feature.names <- names(train)[1:ncol(train)-1]

for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

for (f in feature.names) {
  if (class(train[[f]])=="factor") {
    train[[f]] <- as.integer(train[[f]])
    test[[f]]  <- as.integer(test[[f]])
  }
}

train[is.na(train)] <- -1
test[is.na(test)]   <- -1

library(xgboost)

dtrain <- xgb.DMatrix(data.matrix(train[,feature.names]), label=train$SalePrice)
dtest <- xgb.DMatrix(data.matrix(test[,feature.names]))

#watchlist <- list(eval = dtest, train = dtrain)

param <- list(  objective           = "reg:linear", 
                booster = "gbtree",
                eta                 = 0.001,
                max_depth           = 22,  # changed from default of 6
                subsample           = 0.6,
                colsample_bytree    = 0.6,
                eval_metric         = "auc")

mod <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 7000, # changed from 300
                    verbose             = 2, 
                    #early.stop.round    = 100,
                    #watchlist           = watchlist,
                    maximize            = TRUE)
submission$SalePrice <- predict(mod, data.matrix(test[,feature.names]))
write.csv(submission, file="xgb.9.csv", row.names = FALSE)