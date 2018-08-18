read.csv("climate_change.csv")
climate <- read.csv("climate_change.csv")
str(climate)
library(dplyr)
climate_test <- filter(climate,Year>2006)
climate_train <- filter(climate,Year<= 2006)
model1 <- lm(Temp~MEI+CO2+CH4+N2O+CFC.11+CFC.12+TSI+Aerosols,data=climate_train)
summary(model1)
install.packages("corrplot")
library(corrplot)
corrplot(cor(climate_train), type = "upper", order = "hclust",
tl.col = "black", tl.srt = 45)
cor(climate_train)

