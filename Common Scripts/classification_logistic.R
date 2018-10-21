# inc all variables in model
# good for seeing exact significance of variables

# Classification (logistic regression)
classifmodel <- glm(target~input,data = train, family="binomial")
summary(classifmodel)

# type = response -> probablity
# type = class -> true/false
pred <- predict(classifmodel,newdata=test,type = "response")

# 0.08 = cutoff %chance of targetfield = 1
predtable <- table(clothingtest$targetfield,pred>=0.08)

sens <- predtable[2,2]/sum(predtable[2,])
spec <- predtable[1,1]/sum(predtable[1,])
acc <- (predtable[1,1]+predtable[2,2])/sum(predtable)
