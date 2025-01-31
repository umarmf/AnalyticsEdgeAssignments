library(dplyr)
library(caTools)
loans <- read.csv("loans.csv")
str(loans)
summary(loans)
prop.table(table(loans$not.fully.paid))
missing = subset(loans, is.na(log.annual.inc) | is.na(days.with.cr.line) | is.na(revol.util) | is.na(inq.last.6mths) | is.na(delinq.2yrs) | is.na(pub.rec))
nrow(missing)
library(mice)
set.seed(144)
vars.for.imputation = setdiff(names(loans), "not.fully.paid")
imputed = complete(mice(loans[vars.for.imputation]))
loans[vars.for.imputation] = imputed
set.seed(144)
split <- sample.split(loans$not.fully.paid,0.7)
train <- filter(loans,split == TRUE)
test <- filter(loans,split == FALSE)
model1 <- glm(not.fully.paid~.,data=train,family=binomial)
summary(model1)
coeff <- model1$coefficients
10*coeff["fico"]
exp(10*coeff["fico"])
predicted.risk <- predict(model1,newdata=test,type="response")
summary(predicted.risk)
test["predicted.risk"] <- predicted.risk
pred1table <- table(test$not.fully.paid,test$predicted.risk>0.5)
sens <- pred1table[2,2]/sum(pred1table[2,])
spec <- pred1table[1,1]/sum(pred1table[1,])
accu <- (pred1table[1,1]+pred1table[2,2])/sum(pred1table)
prop.table(table(test$not.fully.paid))
library(ROCR)
rocrpred <- prediction(test$predicted.risk,test$not.fully.paid)
performance(rocrpred,"auc")@"y.values"
bivariate = glm(not.fully.paid~int.rate, data=train, family="binomial")
summary(bivariate)
predicted.risk <- predict(bivariate,newdata=test,type="response")
summary(predicted.risk)
rocrpred <- prediction(predicted.risk,test$not.fully.paid)
performance(rocrpred,"auc")@"y.values"
10*exp(0.06*3)
test$profit = exp(test$int.rate*3) - 1
test$profit[test$not.fully.paid == 1] = -1
summary(test$profit)
highinterest <- filter(test,int.rate>=0.15)
summary(highinterest$profit)
prop.table(table(highinterest$not.fully.paid))
cutoff = sort(highinterest$predicted.risk, decreasing=FALSE)[100]
chosenloans <- filter(highinterest,predicted.risk <= cutoff)
sum(chosenloans$profit)
table(chosenloans$not.fully.paid)
