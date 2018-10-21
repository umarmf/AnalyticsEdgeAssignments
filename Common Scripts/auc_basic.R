# Get AUC and plot TPR vs FPR graph
library(ROCR)
pred <- predict(model,newdata=test,type = "response")

rocrpred <- prediction(pred,test$target)
auc <- as.numeric(performance(rocrpred,"auc")@"y.values")
perf <- performance(rocrpred,"tpr","fpr")
plot(perf)


#for CART models, take second column
pred <- predict(CARTmodel, newdata = test)[,2]