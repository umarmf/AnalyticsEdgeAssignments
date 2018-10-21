library(randomForest)
forest <- randomForest(target~input, data = train, nodesize = n, ntree = x)
predforest <- predict(forest,newdata = test)
foresttable <- table(test$target,predforest)

#variable importance: # of trees where variable was used to split
vu = varUsed(model, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(model$forest$xlevels[vusorted$ix]))

#variable imp: average reduction in impurity for each variable
varImpPlot(forestcensus)

