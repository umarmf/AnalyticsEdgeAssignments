# Build trees
library(rpart)
library(rpart.plot)

# Regression Tree
CARTmodel = rpart(targetfield ~ inputfields, data=dataset)

# Plot tree
prp(CARTmodel)