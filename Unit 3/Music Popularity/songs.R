songs <- read.csv("songs.csv")
library(dplyr)
library(ROCR)
str(songs)
table(songs$year)
nrow(filter(songs,artistname=="Michael Jackson"))
select(filter(songs,artistname=="Michael Jackson" & Top10 == 1),songtitle)
table(songs$timesignature)
songs[which.max(songs$tempo),]
songstrain <- filter(songs,year<=2009)
songstest <- filter(songs,year==2010)
str(songstrain)
nonvars <- c("year","songtitle","artistname","songID","artistID")
songstrain <- songstrain[,!(names(songstrain)%in%nonvars)]
songstest <- songstest[,!(names(songstest)%in%nonvars)]
model1 <- glm(Top10~.,data=songstrain,family=binomial)
summary(model1)
library(corrplot)
songscor <- cor(songstrain[,1:9])
corrplot(songscor, method="circle")
model2 <- glm(Top10~.,data=songstrain[,-3],family=binomial)
summary(model2)
model3 <- glm(Top10~.,data=songstrain[,-8],family=binomial)
summary(model3)
model3pred <- predict(model3,newdata=songstest,type = "response")
model3table <- table(songstest$Top10,model3pred>0.45)
(model3table[1,1]+model3table[2,2])/sum(model3table)
prop.table(table(songstest$Top10))
model3table
sens <- model3table[2,2]/sum(model3table[2,])
sens
spec <- model3table[1,1]/sum(model3table[1,])
spec
