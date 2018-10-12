str(dataset)
summary(dataset)
prop.table(table(dataset$score,dataset$bygroup))
tapply(dataset$score,dataset$bygroup,mean)
cor(dataset)

