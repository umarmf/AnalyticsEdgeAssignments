library(dplyr)
library(ggplot2)
library(RColorBrewer)
letters <- read.csv("letters_ABPR.csv")
letters$isB <- as.factor(letters$letter == "B")

library(caTools)
set.seed(1000)
split <- sample.split(letters$isB, SplitRatio = 0.5)
ltrain <- subset(letters,split == TRUE)
ltest <- subset(letters,split == FALSE)

#1.1
isBtable <- letters$isB %>% table()
isBtable[1]/sum(isBtable)

#1.2
library(rpart)
library(rpart.plot)
CARTb = rpart(isB ~ . - letter, data=ltrain, method = "class")
summary(CARTb)
pred <- predict(CARTb, newdata = ltest, type = "class")
predtable <- table(ltest$isB, pred)
acc <- (predtable[1,1]+predtable[2,2])/sum(predtable)

#1.3
set.seed(1000)
library(randomForest)
forestb <- randomForest(isB~. - letter, data = ltrain)
predforestb <- predict(forestb,newdata = ltest)
forestbtable <- table(ltest$isB,predforestb)
(forestbtable[1,1]+forestbtable[2,2])/sum(forestbtable)

#2.1
letters$letter = as.factor( letters$letter )
set.seed(2000)
split <- sample.split(letters$letter, SplitRatio = 0.5)
ltrain <- subset(letters,split == TRUE)
ltest <- subset(letters,split == FALSE)
mostfreqtarget <- max(table(ltrain$letter))
mostfreqtarget/sum(table(ltest$letter))

#2.2
CARTletter = rpart(letter ~ . - isB, data=ltrain, method = "class")
summary(CARTletter)
pred <- predict(CARTletter, newdata = ltest, type = "class")
predtable <- table(ltest$letter, pred)
acc <- (predtable[1,1]+predtable[2,2]+predtable[3,3]+predtable[4,4])/sum(predtable)

#2.3
forestletter <- randomForest(letter~. -isB, data = ltrain)
predforestletter <- predict(forestletter,newdata = ltest)
forestlettertable <- table(ltest$letter,predforestletter)
(forestlettertable[1,1]+forestlettertable[2,2]+forestlettertable[3,3]+forestlettertable[4,4])/sum(forestbtable)
