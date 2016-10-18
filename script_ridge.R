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

library(glmnet)

mod <- glmnet( x = as.matrix(train[,feature.names]),
               y = train$SalePrice,
               alpha = 0)

submission$SalePrice <- predict(mod, as.matrix(test), s=1000)
write.csv(submission, file="ridge.3.csv", row.names = FALSE)
