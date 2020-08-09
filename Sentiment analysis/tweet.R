#load data
#binary classification

tweets=read.csv("train_E6oV3lV.csv",header = TRUE,stringsAsFactors = FALSE)

#libraries required
library(tm) #for corpus
library(SnowballC)

library(tidyr)
library(formattable)
library(data.table)
library(wordcloud)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caret) #for confusionMatrix()
library(rattle)

#overview of data
str(tweets)
summary(tweets)

#label1:tweet contains hate
#label 0:no hate

tweets$tweet[1]
tweets$tweet[2]

#Creating a corpus,on which data will be further processed,so that we dont lose our origional data
corpus=VCorpus(VectorSource(tweets$tweet))#(volatile corpus)
corpus=tm_map(corpus,PlainTextDocument)
corpus=tm_map(corpus,content_transformer(tolower))

corpus=tm_map(corpus,removeWords,c("user"))
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,removePunctuation)

corpus=tm_map(corpus,removeNumbers)
corpus=tm_map(corpus, stripWhitespace)

#converting corpus to matrix 
dtm=DocumentTermMatrix(corpus)

#removing low frequency terms
sparse=removeSparseTerms(dtm,0.995)

finaltw=as.data.frame(as.matrix(sparse))

#some graphical analysis of the words in the matrix
dtm2=as.matrix(sparse)
frequency=colSums(dtm2)
frequency=sort(frequency,decreasing = TRUE)
names(frequency)
freq= data.frame(names(frequency), frequency)
formattable(freq)


#some variables from our matrix,that do not help in predicting label are removed here
finaltw$г.в.в.=NULL
finaltw$г.вяв.в.г.вяв.вzг.вяв.в.г.вяв.в.г.вяв.в.г.вяв.в.г.вяв.в.=NULL
finaltw$urг.вяв.в.=NULL

#since our objective is to predict label,variable label is added from the loaded dataset 
finaltw$label=tweets$label

#makes valid names out of character
#?make.names

colnames(finaltw)=make.names(colnames(finaltw))
par(mfrow=c(1,1))

#drawing a wordcloud to reinforce the results of formattable
tweetcl=wordcloud(colnames(finaltw),colSums(finaltw),random.color=TRUE,scale=c(2,0.25))
library(RColorBrewer)
display.brewer.all()
tweetcl1=wordcloud(colnames(finaltw),colSums(finaltw),colors=brewer.pal(4, "Dark2") ,scale=c(1,0.25))
#negative tweets
negtweets=subset(finaltw,finaltw$label>=0.6)
tweetcl1=wordcloud(colnames(negtweets),colSums(negtweets),colors=brewer.pal(4, "Dark2") ,scale=c(4,1))

#training and testing set
set.seed(123)
split = sample.split(finaltw$label, SplitRatio=0.75)
train = subset(finaltw, split==TRUE)
test = subset(finaltw, split==FALSE)


#model2(logistic regg.)
tweetglm=glm(label~.,data=train,family ="binomial")
prglm=predict(tweetglm,newdata = test,type="response")
tglm=table(test$label,prglm>=0.5)
tglm
#calculating accuracy
(7368+105)/(7368+105+62+455)

#model1(descision tree)
tweetCART=rpart(label~.,data=train,method="class")
rpart.plot(tweetCART)
prp(tweetCART)
pr1=predict(tweetCART,newdata = test,type="class")
tCART=table(test$label,pr1)
confusionMatrix(tCART)
#calculating accuracy
#(7370+79)/(7370+79+60+481)


#model3(radomforest)
tweetrf=randomForest(label~.,data=train,ntree=10)
pr3=predict(tweetrf,newdata = test)
table(test$label,pr3>=0.5)
#calculating accuracy
(7336+135)/(7336+135+94+425)
