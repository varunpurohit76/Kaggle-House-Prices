library(randomForest)

test<-read.csv("test.csv")
train<-read.csv("train.csv")

Num_NA<-sapply(train,function(y)length(which(is.na(y)==T)))
NA_Count<- data.frame(Item=colnames(train),Count=Num_NA)

#NA_Count

Training<- Training[,-c(7,73,74,75)]

Num<-sapply(train,is.numeric)
Num<-Training[,Num]

for(i in 1:77){
  if(is.factor(train[,i])){
    train[,i]<-as.integer(train[,i])
  }
}


rf <- randomForest(SalePrice~.,data=train)
predicted <- predict(rf, test)
print(predicted)