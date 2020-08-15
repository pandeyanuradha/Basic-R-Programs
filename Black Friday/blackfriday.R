blackf=read.csv("train.csv",nrow=20000)
str(blackf)
summary(blackf)

library(Amelia)
missmap(blackf,main="Missing VS Present")

blackf$Product_Category_2[which(is.na(blackf$Product_Category_2))]=mean(blackf$Product_Category_2,na.rm=TRUE)
blackf$Product_Category_3[which(is.na(blackf$Product_Category_3))]=mean(blackf$Product_Category_3,na.rm=TRUE)

library(caTools)
set.seed(88)

spl=sample.split(blackf$Purchase,SplitRatio = 0.7)
train=subset(blackf,spl==TRUE)
test=subset(blackf,spl==FALSE)

library(rpart)
library(rpart.plot)

library(caret)
folds=trainControl(method="cv",number = 10)
grids=expand.grid(.cp=seq(0.0001,0.05,0.01))
train(Purchase~.,data=train,method="rpart",trControl=folds,tuneGrid=grids)

summary(train)
m1=lm(Purchase~.,data=train)

summary(m1)
test$User_ID=NULL
test$Product_ID=NULL
summary(test)
train$User_ID=NULL
train$Product_ID=NULL
train=as.factor(train)
test=as.factor(test)
pr=predict(m1,newdata = test)
table(test$Purchase,pr>=0.9)

m1=rpart(Purchase~.,data=train,method="class",cp=0.0101,minbucket=25)
