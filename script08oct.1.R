test<-read.csv("test.csv")
train<-read.csv("train.csv")

Num_NA<-sapply(train,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(train),Count=Num_NA)

#NA_Count

train<- train[,-c(7,73,74,75)]
test<- test[,-c(7,73,74,75)]
Num<-sapply(train,is.numeric)
Num<-train[,Num]

for(i in 1:77){
  if(is.factor(train[,i])){
    train[,i]<-as.integer(train[,i])
  }
}


for(i in 1:76){
  if(is.factor(test[,i])){
    test[,i]<-as.integer(test[,i])
  }
}

submission<- data.frame(Id=test$Id)
train$Id<-NULL
test$Id<-NULL

train[is.na(train)]<-0
test[is.na(test)]<-0
Num[is.na(Num)]<-0

#library(randomForest)
#rf <- randomForest(SalePrice~.,
#                   data=train,
#                   ntree=100)
#submission$SalePrice <- predict(rf, test)
#write.csv(submission, file="script07oct.1.csv", row.names = FALSE)


library(xgboost)
train<-as.matrix(train, rownames.force=NA)
test<-as.matrix(test, rownames.force=NA)
train <- as(train, "sparseMatrix")
test <- as(test, "sparseMatrix")
dtrain <- xgb.DMatrix(train,label = train[,"SalePrice"])
dtest <- xgb.DMatrix(test)
param <- list(booster="gbtree",
              eval_metric="rmse",
              eta=0.015,
              colsample_bytree = 0.2,
              max_depth = 6,
              min_child_weight = 2,
              gamma = 0.01,
              subsample = 0.8)

mod.xgb <- xgboost(data=dtrain, params = param,nrounds = 5000)
submission$SalePrice <- predict(mod.xgb, newdata = dtest)
write.csv(submission, file="script08oct.1.csv", row.names = FALSE)