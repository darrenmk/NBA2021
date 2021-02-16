# options(contrasts=c("contr.sum","contr.poly"))
# setwd("D:/Data/Documents/Scripts/NBA2021/") # for Darren only, comment out

library(R.matlab)
library(ggplot2)
library(ggpubr)
library(nlme)

## Figure 2. Race Model Inequality per SOA

CDFs <- readMat("CDFs.mat") # Load CDFs.mat (created by Matlab script)
grandCDFs <- CDFs[["grandCDFs"]]
grandRaceModel <- CDFs[["grandRaceModel"]]
grandRMV <- CDFs[["grandRMV"]]
vesCDF <- as.matrix(grandCDFs[1,1,])
visCDF <- as.matrix(grandCDFs[1,7,])
# grandRMV <- CDFs[["grandRMV"]]

# Import CDF data into a dataframe
cdfData <- data.frame("timecourse" = t(CDFs[["timecourse"]]), "vesCDF" = vesCDF, "ves50CDF" = as.matrix(c(rep(0,500), vesCDF[1:29501,1])), "ves100CDF" = as.matrix(c(rep(0,1000), vesCDF[1:29001,1])), "visCDF" = visCDF, "vis50CDF" = as.matrix(c(rep(0,500), visCDF[1:29501,1])), "vis100CDF" = as.matrix(c(rep(0,1000), visCDF[1:29001,1])), "soa1CDF" = as.matrix(grandCDFs[1,2,]), "soa2CDF" = as.matrix(grandCDFs[1,3,]), "soa3CDF" = as.matrix(grandCDFs[1,4,]), "soa4CDF" = as.matrix(grandCDFs[1,5,]), "soa5CDF" = as.matrix(grandCDFs[1,6,]), "race1CDF" = as.matrix(grandRaceModel[1,1,]), "race2CDF" = as.matrix(grandRaceModel[1,2,]), "race3CDF" = as.matrix(grandRaceModel[1,3,]), "race4CDF" = as.matrix(grandRaceModel[1,4,]), "race5CDF" = as.matrix(grandRaceModel[1,5,]))
## Figure 2a: -100 ms SOA
cdfPlot1 <- ggplot(cdfData, aes(x=timecourse)) +
  geom_line(aes(y=vesCDF,group="A",colour="A")) +
  geom_line(aes(y=soa1CDF,group="B",colour="B")) +
  geom_line(aes(y=vis100CDF,group="C",colour="C")) +
  geom_line(aes(y=race1CDF,group="D",colour="D")) +
  geom_ribbon(data=subset(cdfData, 0 <= cdfData & cdfData <= 920), aes(ymin=race1CDF, ymax=soa1CDF), fill="grey80") +
  labs(tag="A", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="SOA: -100 ms",size=4,fontface="bold") +
  annotate("text",x=100,y=0.9,label=paste("RMV:",round(grandRMV[1],1),"ms",sep=" "),size=4) +
  scale_colour_discrete(name="Condition",labels=c("Vestibular Cue","Combined","Visual Cue","Race Model Inequality"),guide=guide_legend(nrow=4)) +
  scale_x_continuous(breaks=seq(0,2000,by=200), limits=c(0,2000)) +
  theme_classic() +
  theme(legend.position="none")
#print(cdfPlot1)

## Figure 2b: -50 ms SOA
cdfPlot2 <- ggplot(cdfData, aes(x=timecourse)) +
  geom_line(aes(y=vesCDF,group="A",colour="A")) +
  geom_line(aes(y=soa2CDF,group="B",colour="B")) +
  geom_line(aes(y=vis50CDF,group="C",colour="C")) +
  geom_line(aes(y=race2CDF,group="D",colour="D")) +
  geom_ribbon(data=subset(cdfData, 0 <= cdfData & cdfData <= 880), aes(ymin=race2CDF, ymax=soa2CDF), fill="grey80") +
  labs(tag="B", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="SOA: -50 ms",size=4,fontface="bold") +
  annotate("text",x=100,y=0.9,label=paste("RMV:",round(grandRMV[2],1),"ms",sep=" "),size=4) +
  scale_colour_discrete(name="Condition",labels=c("Vestibular Cue","Combined (-50ms SOA)","Visual Cue (50ms delay)","Race Model Inequality"),guide=guide_legend(nrow=4)) +
  scale_x_continuous(breaks=seq(0,2000,by=200), limits=c(0,2000)) +
  theme_classic() +
  theme(legend.position="none")
#print(cdfPlot2)

## Figure 2c: 0 ms SOA
cdfPlot3 <- ggplot(cdfData, aes(x=timecourse)) +
  geom_line(aes(y=vesCDF,group="A",colour="A")) +
  geom_line(aes(y=soa3CDF,group="B",colour="B")) +
  geom_line(aes(y=visCDF,group="C",colour="C")) +
  geom_line(aes(y=race3CDF,group="D",colour="D")) +
  geom_ribbon(data=subset(cdfData, 0 <= cdfData & cdfData <= 840), aes(ymin=race3CDF, ymax=soa3CDF), fill="grey80") +
  labs(tag="B", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="SOA: 0 ms",size=4,fontface="bold") +  
  annotate("text",x=100,y=0.9,label=paste("RMV:",round(grandRMV[3],1),"ms",sep=" "),size=4) +
  scale_colour_discrete(name="Condition",labels=c("Vestibular Cue","Combined","Visual Cue","Race Model Inequality"),guide=guide_legend(nrow=4)) +
  scale_x_continuous(breaks=seq(0,2000,by=200), limits=c(0,2000)) +
  theme_classic() +
  theme(legend.position=c(0.9,0.5))
#print(cdfPlot3)

## Figure 2d: 50 ms SOA
cdfPlot4 <- ggplot(cdfData, aes(x=timecourse)) +
  geom_line(aes(y=ves50CDF,group="A",colour="A")) +
  geom_line(aes(y=soa4CDF,group="B",colour="B")) +
  geom_line(aes(y=visCDF,group="C",colour="C")) +
  geom_line(aes(y=race4CDF,group="D",colour="D")) +
  geom_ribbon(data=subset(cdfData, 0 <= cdfData & cdfData <= 860), aes(ymin=race4CDF, ymax=soa4CDF), fill="grey80") +
  labs(tag="D", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="SOA: 50 ms",size=4,fontface="bold") +
  annotate("text",x=100,y=0.9,label=paste("RMV:",round(grandRMV[4],1),"ms",sep=" "),size=4) +
  scale_colour_discrete(name="Condition",labels=c("Vestibular Cue (50ms delay)","Combined (50ms SOA)","Visual Cue","Race Model Inequality"),guide=guide_legend(nrow=4)) +
  scale_x_continuous(breaks=seq(0,2000,by=200), limits=c(0,2000)) +
  theme_classic() +
  theme(legend.position="none")
#print(cdfPlot4)

## Figure 2e: 100 ms SOA
cdfPlot5 <- ggplot(cdfData, aes(x=timecourse)) +
  geom_line(aes(y=ves100CDF,group="A",colour="A")) +
  geom_line(aes(y=soa5CDF,group="B",colour="B")) +
  geom_line(aes(y=visCDF,group="C",colour="C")) +
  geom_line(aes(y=race5CDF,group="D",colour="D")) +
  geom_ribbon(data=subset(cdfData, 0 <= cdfData & cdfData <= 860), aes(ymin=race5CDF, ymax=soa5CDF), fill="grey80") +
  labs(tag="C", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="SOA: 100 ms",size=4,fontface="bold") +
  annotate("text",x=100,y=0.9,label=paste("RMV:",round(grandRMV[5],1),"ms",sep=" "),size=4) +
  scale_colour_discrete(name="Condition",labels=c("Vestibular Cue (100ms delay)","Combined (100ms SOA)","Visual Cue","Race Model Inequality"),guide=guide_legend(nrow=4)) +
  scale_x_continuous(breaks=seq(0,2000,by=200), limits=c(0,2000)) +
  theme_classic() +
  theme(legend.position="none")
print(cdfPlot5)

## Figure 2 with 3 panels: -100, 0, and 100 ms SOAs
ggarrange(cdfPlot1, cdfPlot3, cdfPlot5, nrow = 3, ncol = 1)
## Figure 2 with 5 panels: -100, -50, 0, 50, and 100 ms SOAs
#ggarrange(cdfPlot1, cdfPlot2, cdfPlot3, cdfPlot4, cdfPlot5, nrow = 5, ncol = 1)


## Figure 3. Grand RT and MR
data <- read.csv("React4Data.csv") # Load React4Data.csv (created by Matlab script: React4.m, bad subjects already removed)
n <- dim(data)[1]/7

# Calculating mean and standard error of RT, MR, RMV, and CRE
grandRT <- tapply(data$rt,data$soa,mean,na.rm=1)
grandRTse <- tapply(data$rt,data$soa,sd,na.rm=1)/sqrt(n)
grandMR <- tapply(data$mr,data$soa,mean,na.rm=1)
grandMRse <- tapply(data$mr,data$soa,sd,na.rm=1)/sqrt(n)
grandRMV <- tapply(data$rmv,data$soa,mean,na.rm=1)
grandRMVse <- tapply(data$rmv,data$soa,sd,na.rm=1)/sqrt(n)
grandMRE <- tapply(data$mre,data$soa,mean,na.rm=1)
grandMREse <- tapply(data$mre,data$soa,sd,na.rm=1)/sqrt(n)
grandSoa <- factor(c("-Inf","-100","-50","0","50","100","Inf"),levels=c("-Inf","-100","-50","0","50","100","Inf")) # manually order
grandCondition <- factor(c("Vestibular",rep("Combined",5),"Visual"),levels=c("Vestibular","Combined","Visual")) # manually order
grandMR_scaled <- (grandMR) * 1200 # scale MR by (x-50) * 10 to match the RT scale
grandMRse_scaled <- sqrt((grandMRse^2) * 1200^2) # scale MR by (x-50) * 10 to match the RT scale
grandData <- data.frame(grandRT,grandRTse,grandMR,grandMRse,grandMR_scaled,grandMRse_scaled,grandMRE,grandMREse,grandRMV,grandRMVse,grandSoa,grandCondition)

## Figure 3: Grand RT and MR
grandPlot <- ggplot(grandData,aes(x=grandSoa,y=grandRT,fill=grandSoa)) +
  geom_errorbar(aes(ymin=grandMR_scaled-grandMRse_scaled, ymax=grandMR_scaled+grandMRse_scaled), width=0.15) +
  geom_errorbar(aes(ymin=grandRT-grandRTse, ymax=grandRT+grandRTse), width=0.15) +
  geom_col(colour="black",aes(y=grandMR_scaled)) +
  geom_point(aes(shape=grandCondition),size=4) + 
  labs(x = "SOA (ms)", y = "Reaction Time (ms)") + 
  scale_shape_manual(name="Condition",labels=c("Vestibular Cue", "Combined (SOA)", "Visual Cue"),values=c(24,21,22)) +
  scale_fill_manual(name="SOA (ms)",values=c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4")) +
  scale_y_continuous(breaks = seq(600, 1200, by = 100), labels = c(600,"",800,"",1000,"",1200), limits = c(0,1200), 
                     sec.axis=sec_axis(~ ., breaks = seq(0, 600, by = 120), labels=c(0,10,20,30,40,50), name = "Miss Rate (%)")) +
  guides(fill=guide_legend(order=2,override.aes=list(shape=(c(24,21,21,21,21,21,22)), col=(c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4")))), shape=guide_legend(order=1)) +
  theme_classic(base_size = 18) +
  theme(legend.direction = "vertical", legend.position=c(0.5,0.82), legend.box="horizontal",
        legend.key = element_rect(color="black",size=0.3,linetype="dotted"), text = element_text(size=32),axis.text = element_text(colour = "black"),legend.text=element_text(size=22, colour= "black"))
print(grandPlot)

# set variable classes in dataframe
data$sID <- as.factor(data$sID)
data$age<- as.integer(data$age)
data$gender<- as.factor(data$gender)
data$cond <- factor(data$cond,levels=c("Ves","Com","Vis"))
data$soa <- factor(data$soa,levels=c("-Inf","-100","-50","0","50","100","Inf"))
data$gameHr <- factor(data$gameHr,levels=c(1,2,3,4,5))
data$driveHr <- factor(data$driveHr,levels=c(1,2,3,4,5))
data$compHr <- factor(data$compHr,levels=c(1,2,3,4,5))


## Figure 4a: Age vs RT
ageRT <- ggplot(data, aes(x=age, y=rt, fill=soa)) +
  geom_smooth(aes(colour=soa),method=lm,se=0) +
  geom_point(colour="black",aes(shape=cond,fill=soa),size=4) +
  scale_colour_manual(values=c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4"),guide=FALSE) +
  scale_shape_manual(name="Condition",labels=c("Vestibular Cue", "Combined (SOA)", "Visual Cue"),values=c(24,21,22)) +
  scale_fill_manual(name="SOA (ms)",labels=c("-Inf","-100","-50","0","50","100","Inf"),values=c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4")) +
  guides(fill=guide_legend(override.aes=list(shape=(c(24,21,21,21,21,21,22))))) +
  labs(x = "Age", y = "Reaction Time (ms)", tag="A") +
  theme_classic2(base_size=17) +
  theme(legend.direction = "vertical", legend.position=c(0.5,0.85), legend.box="horizontal", text = element_text(size=32),  legend.background = element_rect(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),  axis.line = element_line(colour = "grey30"), axis.text = element_text(colour = "black"), legend.text=element_text(size=22, colour= "black"))
print(ageRT)

lmm.rt <- lme(rt ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = data)
summary(lmm.rt)
anova(lmm.rt)


## Figure 4b: Age & MR
ageMR <- ggplot(data, aes(x=age, y=mr, fill=soa)) +
  geom_smooth(aes(colour=soa),method=lm,se=0) +
  geom_point(colour="black",aes(shape=cond,fill=soa),size=4) +
  scale_colour_manual(values=c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4"),guide=FALSE) +
  scale_shape_manual(name="Condition",labels=c("Vestibular Cue", "Combined (SOA)", "Visual Cue"),values=c(24,21,22)) +
  scale_fill_manual(name="SOA (ms)",labels=c("-Inf","-100","-50","0","50","100","Inf"),values=c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4")) +
  guides(fill=guide_legend(override.aes=list(shape=(c(24,21,21,21,21,21,22))))) +
  labs(x = "Age", y = "Miss Rate (%)", tag="B") +
  theme_classic(base_size=17) +
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=32), axis.text = element_text(colour = "black"))
print(ageMR)

lmm.mr <- lme(mr ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = data)
summary(lmm.mr)
anova(lmm.mr)

## Figure 4c: Age & RMV
# make a smaller dataframe with vest/vis-only trials exluded
dataSoa <- data[(data$soa != -Inf) & (data$soa != Inf),]
dataSoa$cond <- factor(dataSoa$cond,levels=c("Com"))
dataSoa$soa <- factor(dataSoa$soa,levels=c("-100","-50","0","50","100"))

ageRMV <- ggplot(dataSoa, aes(x=age, y=rmv, fill=soa)) +
  geom_smooth(aes(colour=soa),method=lm,se=0) +
  geom_point(colour="black",aes(shape=cond,fill=soa),size=4) +
  scale_colour_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  scale_shape_manual(values=21) +
  scale_fill_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  labs(x = "Age", y = "Race Model Violation (ms)", tag="C") +
  theme_classic(base_size=17) +
  theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=32), axis.text = element_text(colour = "black"))
print(ageRMV)

lmm.rmv <- lme(rmv ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = dataSoa)
summary(lmm.rmv)
anova(lmm.rmv)

## Figure 4d: Age & MRE
ageMRE <- ggplot(dataSoa, aes(x=age, y=mre, fill=soa)) +
  geom_smooth(aes(colour=soa),method=lm,se=0) +
  geom_point(colour="black",aes(shape=cond,fill=soa),size=4) +
  scale_colour_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  scale_shape_manual(values=21) +
  scale_fill_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  labs(x = "Age", y = "Multisensory Response Enhacement (%)", tag="D") +
  theme_classic(base_size=17) +
  theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey30"))
print(ageMRE)

lmm.mre <- lme(mre ~ age*soa + gameHr + driveHr + compHr, random = ~1|sID, na.action = na.exclude, data = dataSoa)
summary(lmm.mre)
anova(lmm.mre)

## Figure 4, all 4 panels
ggarrange(ageRT, ageMR, ageRMV, ageCRE, nrow = 2, ncol = 2)


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

