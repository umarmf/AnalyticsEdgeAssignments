# CART trees similar & slightly worse than log reg
# Much easier to understand
# Selects only significant variables
library(rpart)
library(rpart.plot)

# Regression Tree
# Remove method = "class" for numerical regression output
# add cp or minbucket to adjust tree
CARTmodel = rpart(targetfield ~ inputfields, data=dataset, method = "class")

# Plot tree and see summary
prp(CARTmodel)
summary(CARTmodel)

# predict
# remove type = "class" to generate:
# probabilities if model's method = class
# numbers if model's method was not defined
pred <- predict(CARTmodel, newdata = test, type = "class")
predtable <- table(test$targetfield,pred)
