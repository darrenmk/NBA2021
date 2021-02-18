# setwd("D:/Data/Documents/Scripts/NBA2021/") # for Darren only, comment out

library(nlme)
library(lme4)
library(simr)

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
data$compensation <- factor(data$compensation,levels=c("Money","Credit","Declined"))

lmm.rt <- lme(rt ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = data)
summary(lmm.rt)
anova(lmm.rt)
lmm.rt2 <- lmer(rt ~ age + gameHr + driveHr + compHr + (1|sID), data=data)
summary(lmm.rt2)
# sim.rt <- powerSim(lmm.rt2,nsim=1000,seed=1995)

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
lmm.rmv2 <- lme(rmv ~ age + gameHr + driveHr + compHr, random=~1|sID, data=dataSoa)
summary(lmm.rmv2)
lmm.rmv3 <- lmer(rmv ~ age + gameHr + driveHr + compHr + (1|sID), data=dataSoa)
summary(lmm.rmv3)
# sim.rmv <- powerSim(lmm.rmv3,nsim=1000,seed=1995)

lmm.mre <- lme(mre ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = dataSoa)
summary(lmm.mre)
anova(lmm.mre)



# Control analysis 3.3.1.: effect of compensation
comp.rt <- lme(rt ~ age+compensation, random = ~1|sID, na.action = na.exclude, data = data)
anova(comp.rt)

comp.mr <- lme(mr ~ age+compensation, random = ~1|sID, na.action = na.exclude, data = data)
anova(comp.mr)

comp.rmv <- lme(rmv ~ age+compensation, random = ~1|sID, na.action = na.exclude, data = dataSoa)
anova(comp.rmv)

comp.mre <- lme(mre ~ age+compensation, random = ~1|sID, na.action = na.exclude, data = dataSoa)
anova(comp.mre)


# Control analysis 3.3.2. High miss rate in vestibular-only condition (>= 80%)
vmr <- rep(data$mr[data$soa==-Inf] >= 0.8, each=7) # get subjects with high vestibular MR >= 80%
vmrVisRT <- data$rt[highVMR & data$soa==Inf] # get vis-only RTs for this subset of participants
vmrSOART <- t(matrix(data$rt[highVMR & data$soa!=-Inf & data$soa!=Inf],nrow=5)) # get soa RTs this subset...
vmrSOART[,1] <- vmrSOART[,1] - 100 # adjust for vis 100 ms late
vmrSOART[,2] <- vmrSOART[,2] - 50 # adjust for vis 50 ms late
vmrT <- t.test(vmrSOART, vmrVisRT, alternative = "less")



## for reviewer 1: check whether young adults with >50% MR had diff demographic qualities
data2 <- data[data$soa==-Inf,]
data2$gameHr <- as.integer(data2$gameHr)
data2$driveHr <- as.integer(data2$driveHr)
data2$compHr <- as.integer(data2$compHr)

yaAbove50 <- colMeans(data2[data2$age < 30 & data2$mr > 0.5,10:12])
yaAbove50se <- apply(data2[data2$age < 30 & data2$mr > 0.5,10:12],2,sd)/sqrt(8)
yaBelow50 <- colMeans(data2[data2$age < 30 & data2$mr <= 0.5,10:12])
yaBelow50se <- apply(data2[data2$age < 30 & data2$mr <= 0.5,10:12],2,sd)/sqrt(25)