baseball <- read.csv("baseball.csv")
str(baseball)
moneyball <- subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD <- moneyball$RS - moneyball$RA
plot(moneyball$RD, moneyball$W)
winsreg <- lm(W~RD,data=moneyball)
summary(winsreg)
runsreg <- lm(RS~OBP+SLG,data=moneyball)
summary(runsreg)
teamrank <- c(1,2,3,3,4,4,4,4,5,5)
wins2012 <- c(94,88,95,88,93,94,98,97,93,94)
wins2013 <- c(97,97,92,93,92,96,94,96,92,90)
