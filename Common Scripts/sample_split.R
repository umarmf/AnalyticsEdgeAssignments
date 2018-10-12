library(caTools)
split <- sample.split(dataset$targetfield, SplitRatio = 0.8)
train <- subset(dataset,split = TRUE)
test <- subset(dataset,split = FALSE)