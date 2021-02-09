library(aod)
library (readxl)
library(ggplot2)
library(multcomp)
library(lme4)
library(nlme)

setwd('/Users/yaz/Desktop/NBA2021-master')
data<-read.csv("React_Lmer_RT&MR_updated.csv")
data=data[data$subject!=12 & data$subject!=19 & data$subject!=25 & data$subject!=51 & data$subject!=53 & data$subject!=71,]

data$age<- as.integer(data$age)
data$subject <- as.integer(data$subject)
data$rt<- as.integer(data$rt)
data$mr<- as.integer(data$mr)

data$cond.rt<-factor(data$cond.rt)
data$cond.mr<-factor(data$cond.mr)
data$drive.hr<- factor(data$drive.hr)
data$computer.hr<- factor(data$computer.hr)
data$game.hr<- factor(data$game.hr)

############### Mixed Linear Model for RT and MR ##############

####RT

rt_lmer <- lmer(rt ~ age * cond.rt + (1|subject), data=data)
summary(rt_lmer)

rt_lmer1 <- lmer(rt ~ age * cond.rt + (1+drive.hr|subject) + (1+computer.hr|subject) + (1+game.hr|subject), data=data)
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


