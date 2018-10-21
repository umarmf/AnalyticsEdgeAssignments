library(dplyr)
library(ggplot2)
library(RColorBrewer)
census <- read.csv("census.csv")
str(census)
summary(census)

#1.1
library(caTools)
set.seed(2000)
split <- sample.split(census$over50k, SplitRatio = 0.6)
train <- subset(census,split == TRUE)
test <- subset(census,split == FALSE)

logcensus <- glm(over50k~.,data = train, family="binomial")
summary(logcensus)

#1.2
pred <- predict(logcensus,newdata=test,type = "response")
predtable <- table(test$over50k,pred>=0.5)
(predtable[1,1]+predtable[2,2])/sum(predtable)

#1.3
modetargetvalue <- names(sort(table(train$over50k),decreasing = TRUE)[1])
table(test$over50k)[modetargetvalue]/sum(table(test$over50k))

#1.4
library(ROCR)
rocrpred <- prediction(pred,test$over50k)
auc <- as.numeric(performance(rocrpred,"auc")@"y.values")

#2.1-2.3
library(rpart)
library(rpart.plot)
CARTmodel = rpart(over50k ~ ., data=train, method = "class")
prp(CARTmodel)

#2.4
pred <- predict(CARTmodel, newdata = test, type = "class")
predtable <- table(test$over50k,pred)
acc <- (predtable[1,1]+predtable[2,2])/sum(predtable)

#2.5
pred <- predict(CARTmodel, newdata = test)[,2]
rocrpred <- prediction(pred,test$over50k)
perf <- performance(rocrpred,"tpr","fpr")
plot(perf)

#2.6
auc <- as.numeric(performance(rocrpred,"auc")@"y.values")

#3.1
set.seed(1)
trainsmall = train[sample(nrow(train), 2000), ]
library(randomForest)
forestcensus <- randomForest(over50k~., data = trainsmall)
predforest <- predict(forestcensus,newdata = test)
foresttable <- table(test$over50k,predforest)
(foresttable[1,1]+foresttable[2,2])/sum(foresttable)

#3.2
vu = varUsed(forestcensus, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(forestcensus$forest$xlevels[vusorted$ix]))

#3.3
varImpPlot(forestcensus)

#4.1
library(caret)
library(e1071)
set.seed(2)
numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(0.002,0.1,0.002))
train(over50k~.,data = train, method="rpart",trControl=numfolds,tuneGrid=cpgrid)

#4.2
CARTcvcensus <- rpart(over50k~., data = train, method="class",cp=0.002)
predictcv <- predict(CARTcvcensus,newdata=test,type="class")
predtable <- table(test$over50k,predictcv)
(predtable[1,1]+predtable[2,2])/sum(predtable)
prp(CARTcvcensus)
