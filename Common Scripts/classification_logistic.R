
# Classification (logistic regression)
classifmodel <- glm(targetfield~inputfield,data = clothingtrain, family=binomial)
summary(classifmodel)
pred <- predict(classifmodel,newdata=clothingtest,type = "response")

# 0.08 = cutoff %chance of targetfield = 1
predtable <- table(clothingtest$targetfield,pred>=0.08)

sens <- predtable[2,2]/sum(predtable[2,])
spec <- predtable[1,1]/sum(predtable[1,])
acc <- (predtable[1,1]+predtable[2,2])/sum(predtable)

# Get AUC and plot TPR vs FPR graph
library(ROCR)
rocrpred <- prediction(pred,clothingtest$targetfield)
auc <- as.numeric(performance(rocrpred,"auc")@"y.values")
perf <- performance(rocrpred,"tpr","fpr")
plot(perf)