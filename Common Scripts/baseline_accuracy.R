#find most common value in train set
modetargetvalue <- names(sort(table(train$target),decreasing = TRUE)[1])

#check % frequency of most common value in test set
# this is baseline accuracy
table(test$target)[modetargetvalue]/sum(table(test$target))