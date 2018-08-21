flutrain <- read.csv("FluTrain.csv")
summary(flutrain)
flutrain[which.max(flutrain$Queries),]
flutrain[which.max(flutrain$ILI),]
hist(flutrain$ILI)
plot(flutrain$Queries,log(flutrain$ILI))
model1 <- lm(log(ILI)~Queries,data=flutrain)
summary(model1)
cor(log(flutrain$ILI),flutrain$Queries)
flutest <- read.csv("flutest.csv")
predtest1 = exp(predict(model1, newdata=flutest))
predtest1[which(flutest$Week == "2012-03-11 - 2012-03-17")]
(flutest$ILI[11]-predtest1[11])/flutest$ILI[11]
sqrt(mean((flutest$ILI-predtest1)^2))
install.packages("zoo")
library(zoo)
ILIlag2 = lag(zoo(flutrain$ILI), -2, na.pad=TRUE)
flutrain$ILIlag2 = coredata(ILIlag2)
plot(log(flutrain$ILIlag2), log(flutrain$ILI))
model2 <- lm(log(ILI)~log(ILIlag2)+Queries,data=flutrain)
summary(model2)
ILIlag2 = lag(zoo(flutest$ILI), -2, na.pad=TRUE)
flutest$ILIlag2 = coredata(ILIlag2)
summary(flutest$ILIlag2)
flutest$ILIlag2[1] <- flutrain$ILI[416]
flutest$ILIlag2[2] <- flutrain$ILI[417]
flutest$ILIlag2[1:2]
predtest2 = exp(predict(model2, newdata=flutest))
sqrt(mean((flutest$ILI-predtest2)^2))
