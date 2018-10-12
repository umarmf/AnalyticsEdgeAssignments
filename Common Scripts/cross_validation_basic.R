# Basic cross-validation
library(rpart)
library(caret)
library(e1071)

# get best cp
numfolds <- trainControl(method="cv", number=10)
cpgrid <- expand.grid(.cp=seq(0.01,0.5,0.01))
train(targetfield~inputfield,data = train, method="rpart",trControl=numfolds,tuneGrid=cpgrid)

# Make regression tree (calculate number or probability)
modelcv <- rpart(targetfield~inputfield, data=train, method="class",cp=0.18)
predictcv <- predict(modelcv,newdata=test,type="class")
table(test$targetfield,predictcv)
prp(modelcv)