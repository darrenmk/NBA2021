# setwd("D:/Data/Documents/Scripts/NBA2021/") # for Darren only, comment out

library(nlme)

data <- read.csv("React4Data.csv") # Load React4Data.csv (created by Matlab script: React4.m, bad subjects already removed)
n <- dim(data)[1]/7

## Stats for Figure 4
data$sID <- as.factor(data$sID)
data$age<- as.integer(data$age)
data$gender<- as.factor(data$gender)
data$cond <- factor(data$cond,levels=c("Ves","Com","Vis"))
data$soa <- factor(data$soa,levels=c("-Inf","-100","-50","0","50","100","Inf"))
data$gameHr <- as.integer(data$gameHr)
data$driveHr <- as.integer(data$driveHr)
data$compHr <- as.integer(data$compHr)

lmm.rt <- lme(rt ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = data)
summary(lmm.rt)
anova(lmm.rt)

lmm.mr <- lme(mr ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = data)
summary(lmm.mr)
anova(lmm.mr)

## Stats for Figure 5
dataSoa <- data[(data$soa != -Inf) & (data$soa != Inf),]
dataSoa$cond <- factor(dataSoa$cond,levels=c("Com"))
dataSoa$soa <- factor(dataSoa$soa,levels=c("-100","-50","0","50","100"))

lmm.rmv <- lme(rmv ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = dataSoa)
summary(lmm.rmv)
anova(lmm.rmv)

lmm.mre <- lme(mre ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = dataSoa)
summary(lmm.mre)
anova(lmm.mre)







## Code for discussion about subjects w vestibular MR >= 80% having faster RT in SOA than vis
highVMR <- rep(data$mr[data$soa==-Inf] >= 0.8, each=7) # get subjects with vestibular MR >= 80%
myVis <- data$rt[highVMR & data$soa==Inf] # vis RTs for high VMR subjects
mySoa <- t(matrix(data$rt[highVMR & data$soa!=-Inf & data$soa!=Inf],nrow=5)) # soa RTs for high VMR subjects
myDiff <- mySoa - myVis # diff in SOA RT and vis RT, negative = soa RT is faster
myNeg <- myDiff < 0 # check if negative

## code to check whether young adults with >50% MR had diff demographic qualities
data2 <- data[data$soa==-Inf,]
data2$gameHr <- as.integer(data2$gameHr)
data2$driveHr <- as.integer(data2$driveHr)
data2$compHr <- as.integer(data2$compHr)

yaAbove50 <- colMeans(data2[data2$age < 30 & data2$mr > 0.5,10:12])
yaAbove50se <- apply(data2[data2$age < 30 & data2$mr > 0.5,10:12],2,sd)/sqrt(8)
yaBelow50 <- colMeans(data2[data2$age < 30 & data2$mr <= 0.5,10:12])
yaBelow50se <- apply(data2[data2$age < 30 & data2$mr <= 0.5,10:12],2,sd)/sqrt(25)