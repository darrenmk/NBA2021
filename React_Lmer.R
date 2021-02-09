library(aod)
library (readxl)
library(ggplot2)
library(multcomp)
library(lme4)
library(nlme)

data<-read.csv("React_Lmer_RT&MR_updated.csv")
data=data[data$subject!=12 & data$subject!=19 & data$subject!=25 & data$subject!=51 & data$subject!=53 & data$subject!=71,]

data$age<- as.integer(data$age)
data$subject <- as.integer(data$subject)
data$rt<- as.integer(data$rt)
data$mr<- as.integer(data$mr)

data$cond.rt<-factor(data$cond.rt)
data$cond.mr<-factor(data$cond.mr)
data$drive.hr<- factor(data$drive.hr,levels=c(1,2,3,4,5))
data$computer.hr<- factor(data$computer.hr,levels=c(1,2,3,4,5))
data$game.hr<- factor(data$game.hr,levels=c(1,2,3,4,5))

############### Mixed Linear Model for RT and MR ##############

####RT

rt_lmer <- lmer(rt ~ age * cond.rt + (1|subject), data=data)
summary(rt_lmer)

rt_lmer1 <- lmer(rt ~ age * cond.rt + (1+drive.hr|age) + (1+computer.hr|age) + (1+game.hr|age) + (1|subject), data=data)
summary(rt_lmer1)

rt_ci <- confint(rt_lmer)
rt_ci

###MR


############### Mixed Linear Model for CRE and RMV ##############
data<- read.csv("React_Lmer_CRE&RMV_updated.csv")

data$age<- as.integer(data$age)
data$subject <- as.integer(data$subject)
data$cre<- as.integer(data$cre)
data$rmv<- as.integer(data$rmv)

data$cond.cre<- factor(data$cond.cre)
data$cond.rmv<-factor(data$cond.rmv)
data$drive.hr<- factor(data$drive.hr)
data$computer.hr<- factor(data$computer.hr)
data$game.hr<- factor(data$game.hr)


