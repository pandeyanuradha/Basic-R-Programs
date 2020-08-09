#reading data
#multivariable classification
Claims=read.csv("ClaimsData.csv")

#initial analysis
nrow(Claims)
str(Claims)
names(Claims)
summary(Claims)

#% of patient in each cost bucket,we have 5 cost buckets,as the bucket no. increases,the moeny spent on tratment increases
prop.table(table(Claims$bucket2009))*100
#most patients have low bucket cost

library(ggplot2)
library(dplyr) #for group_by
library(rpart)
library(rpart.plot)
library(formattable) 
library(caTools) #for sample.split
library(caret) #for confusionmatrix
library(e1071)
library(flexclust) # for as.kcca
library(rattle)

#some graphical analysis
names(Claims)
ggplot(Claims)+geom_histogram(aes(Claims$age),col='black',fill="coral1",binwidth = 30)+xlab("age")+ylab("no. of patients")

agebuck2008=Claims%>%group_by(age,bucket2008)%>%summarise(tot1=n())
ggplot(data=agebuck2008,aes(age,bucket2008, fill=tot1)) +geom_tile(color = "white")+scale_fill_gradient(low="yellow",high="black")

agebuck2009=Claims%>%group_by(age,bucket2009)%>%summarise(tot2=n())
ggplot(data=agebuck2009,aes(age,bucket2009, fill=tot2)) +geom_tile(color="white")+scale_fill_gradient(low="plum1",high="black")

table(Claims$depression)
(9750)/(9750+360415)
agedepr=Claims%>%group_by(age,depression)%>%summarise(tot3=n())
ggplot(data=agedepr,aes(age,depression,fill=tot3))+geom_tile(color="white")+scale_fill_gradient(low="azure1",high="black")


diseases=rbind(Claims$alzheimers+Claims$arthritis+Claims$cancer+Claims$copd+Claims$depression+Claims$diabetes+Claims$heart.failure+Claims$ihd+Claims$kidney+Claims$osteoporosis+Claims$stroke)
distable=table(diseases)
formattable(distable)
dfdis=as.data.frame(distable)
dfdis
colnames(dfdis)=c("diseases","freq")
dfdis
bar=ggplot(dfdis,aes(x="",y=freq,fill=diseases))+geom_bar(stat="identity")
bar
bar+coord_polar(theta="y")+theme_void()   

memory.limit()
memory.limit(size = 64000)

#splitting into training and testing set
set.seed(88)
split=sample.split(Claims$bucket2009,SplitRatio = 0.6)
train=subset(Claims,split==TRUE)
test=subset(Claims,split==FALSE)

#building deciscion tree using train and test
ClaimsTree = rpart(bucket2009 ~ age + alzheimers + arthritis + cancer + copd + depression + diabetes + heart.failure + ihd + kidney + osteoporosis + stroke + bucket2008 + reimbursement2008, data=train, method="class", cp=0.00005)
fancyRpartPlot(ClaimsTree)
rpart.plot(ClaimsTree,type=0)
prp(ClaimsTree)
PredictTest=predict(ClaimsTree,newdata=test,type="class")
predictTable=table(test$bucket2009,PredictTest)
confusionMatrix(predictTable)

#splitting into training and testing data,that will be clustered
limtrain=train
limtrain$bucket2009=NULL
limtest=test
limtest$bucket2009=NULL

preproctrain=preProcess(limtrain)
preproctest=preProcess(limtest)

normtrain=predict(preproctrain, limtrain)
normtest=predict(preproctest, limtest)


#hierarchial clustering se deciding how many clusters approx to build by kmeans
normtrain2=normtrain[1:10000,]

dist1=dist(normtrain2,method="euclidean")
dend1=hclust(dist1,method="ward.D")
plot(dend1)

normtest2=normtest[1:10000,]

dist2=dist(normtest2,method="euclidean")
dend2=hclust(dist2,method="ward.D")
plot(dend2)

#since on a large dataset,kmeans clustering is prefferable,
#elbow method is used to decide the appropriate number of clusters

library(factoextra)
fviz_nbclust(normtrain2, kmeans, method = "wss")

#kmeans clustering
set.seed(144)
k=4
km= kmeans(normtrain,centers=k)
str(km)
table(km$cluster)

km.kcca = as.kcca(km, normtrain)
clustertrain = predict(km.kcca)
clustertest = predict(km.kcca, newdata=normtest)

table(clustertrain)
table(clustertest)

#building our clustered datasets
train1 =subset(train, clustertrain == 1)
train2 = subset(train, clustertrain == 2)
train3 = subset(train, clustertrain == 3)
train4 = subset(train, clustertrain == 4)

test1 = subset(test, clustertest == 1)
test2 = subset(test, clustertest == 2)
test3 = subset(test, clustertest == 3)
test4 = subset(test, clustertest == 4)

summary(train1)
summary(test1)


folds=trainControl(method="cv",number = 10)
grids=expand.grid(.cp=seq(0.0001,0.05,0.01))
train(bucket2009~.-reimbursement2009,data=train1,method="rpart",trControl=folds,tuneGrid=grids)

#models and predictions
m1=rpart(bucket2009~.-reimbursement2009,data=train1,method="class",cp=0.0101)
fancyRpartPlot(m1)
rpart.plot(m1)
p1=predict(m1,newdata=test1,type="class")
t1=table(test1$bucket2009,p1)
confusionMatrix(t1)
summary(m1)

m2=rpart(bucket2009~.-reimbursement2009,data=train2,method="class",cp=0.00001)
fancyRpartPlot(m2)
rpart.plot(m2)
p2=predict(m2,newdata=test2,type="class")
t2=table(p2,test2$bucket2009)
confusionMatrix(t2)
summary(m2)

m3=rpart(bucket2009~.-reimbursement2009,data=train3,method="class",cp=0.00001)
fancyRpartPlot(m3)
prp(m3)
p3=predict(m3,newdata=test3,type="class")
t3=table(p3,test3$bucket2009)
confusionMatrix(t3)

m4=rpart(bucket2009~.-reimbursement2009,data=train4,method="class",cp=0.0101)
fancyRpartPlot(m4)
rpart.plot(m4)
p4=predict(m4,newdata=test4,type="class")
t4=table(p4,test4$bucket2009)
confusionMatrix(t4)
