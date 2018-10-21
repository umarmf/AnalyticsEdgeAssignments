gerber <- read.csv("gerber.csv")
str(gerber)
summary(gerber)
#1.2
gerberhawthorne <- gerber %>%  filter(hawthorne == 1)
gerbercivic <- gerber %>%  filter(civicduty == 1)
gerberself <- gerber %>%  filter(self == 1)
gerberneighbors <- gerber %>%  filter(neighbors == 1)
prop.table(table(gerberhawthorne$voting))
prop.table(table(gerbercivic$voting))
prop.table(table(gerberself$voting))
prop.table(table(gerberneighbors$voting))
#1.3
gerberlog <- glm(voting~hawthorne+civicduty+neighbors+self,data=gerber,family="binomial")
summary(gerberlog)
#1.4
predlog <- predict(gerberlog, type = "response")
predlogtable <- table(gerber$voting,predlog >=0.3)
(predlogtable[1,1]+predlogtable[2,2])/sum(predlogtable)
#1.5
predlogtable <- table(gerber$voting,predlog >=0.5)
predlogtable
predlogtable[1]/sum(predlogtable)
#1.6
library(ROCR)
rocrpred <- prediction(predlog,gerber$voting)
as.numeric(performance(rocrpred,"auc")@"y.values")
perf <- performance(rocrpred,"tpr","fpr")
plot(perf)
#2.1-2.3
library(rpart)
library(rpart.plot)
CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel)
#2.4
CARTmodel = rpart(voting ~ sex + civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel)
#3.1-3.2
CARTmodel = rpart(voting ~ control, data=gerber, cp=0.0)
prp(CARTmodel, digits = 6)
CARTmodel = rpart(voting ~ control+sex, data=gerber, cp=0.0)
0.34-0.296638
0.334176-0.290456 - (0.345818-0.302795)
#3.3
logmodelsex <- glm(voting~sex+control,data=gerber,family="binomial")
summary(logmodelsex)
#3.4
possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(logmodelsex, newdata=possibilities, type="response")
0.2908065 - 0.290456
#3.5
logmodel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(logmodel2)
#3.6
predict(logmodel2, newdata=possibilities, type="response")
0.2904558 - 0.290456
