library(randomForest)

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

rf <- randomForest(SalePrice~.,
                   data=train,
                   ntree=2500)
submission$SalePrice <- predict(rf, test)

write.csv(submission, file="script13oct.2.csv", row.names = FALSE)
