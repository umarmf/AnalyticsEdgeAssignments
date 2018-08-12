read.csv("wine.csv")
wine <- read.csv("wine.csv")
wine_test <- read.csv("wine_test.csv")
str(wine)
summary(wine)
model1 <- lm(Price~AGST,data=wine)
summary(model1)
model1$residuals
sse <- sum(model1$residuals^2)
sse
model2 <- lm(Price~AGST+HarvestRain,data=wine)
summary(model2)
sse <- sum(model2$residuals^2)
sse
model3 <- lm(Price~AGST+HarvestRain+WinterRain+Age+FrancePop,data=wine)
summary(model3)
sse <- sum(model3$residuals^2)
sse
model4 <- lm(Price~AGST+HarvestRain+WinterRain+Age,data=wine)
summary(model4)
model5 <- lm(Price~AGST+HarvestRain+WinterRain,data=wine)
summary(model5)
cor(wine)
