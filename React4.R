options(contrasts=c("contr.sum","contr.poly"))
setwd('/Users/yaz/Desktop/NBA2021-master')

library(R.matlab)
library(ggplot2)
library(ggpubr)
library(ggcorrplot)
library(lme4)

### Load matlab variables and subject data ###
mat <- readMat("React4Variables.mat")
subjectsAll <- read.csv("React4Subjects.csv")
keepSubjects <- !(subjectsAll$poorData==1 | subjectsAll$below90==1)
subjects <- subjectsAll[keepSubjects,] #filter out unwanted subjects
n <- dim(subjects)[1]

subjectRT <- mat[["subjectRT"]]
subjectMR <- mat[["subjectMR"]]
subjectRMV <- mat[["subjectRMV"]]
subjectCRE <- mat[["subjectCRE"]]

#mixed Linear regression between age and five SOAs and RT and MR and RMV and CRE

lm1<-lmer(RT~Age*Vestibular.MR+Mean.RMV+(1|subjects),data = data)

## Figure 2. RMV of Race Model Inequality per SOA
grandCDFs <- mat[["grandCDFs"]]
grandRaceModel <- mat[["grandRaceModel"]]
vesCDF <- as.matrix(grandCDFs[1,1,])
visCDF <- as.matrix(grandCDFs[1,7,])
#grandRMV <- mat[["grandRMV"]]

# Import CDF data into a dataframe
cdfData <- data.frame("timecourse" = t(mat[["timecourse"]]), "vesCDF" = vesCDF, "ves50CDF" = as.matrix(c(rep(0,500), vesCDF[1:29501,1])), "ves100CDF" = as.matrix(c(rep(0,1000), vesCDF[1:29001,1])), "visCDF" = visCDF, "vis50CDF" = as.matrix(c(rep(0,500), visCDF[1:29501,1])), "vis100CDF" = as.matrix(c(rep(0,1000), visCDF[1:29001,1])), "soa1CDF" = as.matrix(grandCDFs[1,2,]), "soa2CDF" = as.matrix(grandCDFs[1,3,]), "soa3CDF" = as.matrix(grandCDFs[1,4,]), "soa4CDF" = as.matrix(grandCDFs[1,5,]), "soa5CDF" = as.matrix(grandCDFs[1,6,]), "race1CDF" = as.matrix(grandRaceModel[1,1,]), "race2CDF" = as.matrix(grandRaceModel[1,2,]), "race3CDF" = as.matrix(grandRaceModel[1,3,]), "race4CDF" = as.matrix(grandRaceModel[1,4,]), "race5CDF" = as.matrix(grandRaceModel[1,5,]))
## Figure 2a: -100 ms SOA
cdfPlot1 <- ggplot(cdfData, aes(x=timecourse)) +
  geom_line(aes(y=vesCDF,group="A",colour="A")) +
  geom_line(aes(y=soa1CDF,group="B",colour="B")) +
  geom_line(aes(y=vis100CDF,group="C",colour="C")) +
  geom_line(aes(y=race1CDF,group="D",colour="D")) +
  geom_ribbon(data=subset(cdfData, 0 <= cdfData & cdfData <= 920), aes(ymin=race1CDF, ymax=soa1CDF), fill="grey80") +
  labs(tag="A", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="-100ms SOA",size=4) +
  #annotate("text",x=400,y=0.35,label="RMV: 91.19 ms",size=2) +
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
  annotate("text",x=100,y=0.99,label="-50ms SOA",size=4) +
  #annotate("text",x=400,y=0.35,label="RMV: 80.38 ms",size=2) +
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
  labs(tag="C", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="0ms SOA",size=4) +  
  #annotate("text",x=400,y=0.35,label="RMV: 70.76 ms",size=2) +
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
  annotate("text",x=100,y=0.99,label="50ms SOA",size=4) +
  #annotate("text",x=400,y=0.35,label="RMV: 72.13 ms",size=2) +
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
  labs(tag="E", x="Time (ms)", y="P(response)") + 
  annotate("text",x=100,y=0.99,label="100ms SOA",size=4) +
  #annotate("text",x=400,y=0.35,label="RMV: 57.14 ms",size=2) +
  scale_colour_discrete(name="Condition",labels=c("Vestibular Cue (100ms delay)","Combined (100ms SOA)","Visual Cue","Race Model Inequality"),guide=guide_legend(nrow=4)) +
  scale_x_continuous(breaks=seq(0,2000,by=200), limits=c(0,2000)) +
  theme_classic() +
  theme(legend.position="none")
#print(cdfPlot5)

## Figure 2 with 3 panels: -100, 0, and 100 ms SOAs
ggarrange(cdfPlot1, cdfPlot3, cdfPlot5, nrow = 3, ncol = 1)
## Figure 2 with 5 panels: -100, -50, 0, 50, and 100 ms SOAs
ggarrange(cdfPlot1, cdfPlot2, cdfPlot3, cdfPlot4, cdfPlot5, nrow = 5, ncol = 1)


## Figure 3. Grand RT and MR
# Calculating mean and standard error of RT, MR, RMV, and CRE
overallMeanRT <- apply(subjectRT,2,mean,na.rm=1)
overallSeRT <- apply(subjectRT,2,sd,na.rm=1)/sqrt(dim(subjectRT)[1])
overallMeanMR <- apply(subjectMR,2,mean,na.rm=1)
overallSeMR <- apply(subjectMR,2,sd,na.rm=1)/sqrt(dim(subjectMR)[1])
overallMeanRMV <- c(NaN,apply(subjectRMV,2,mean,na.rm=1),NaN)
overallSeRMV <- c(NaN,apply(subjectRMV,2,sd,na.rm=1)/sqrt(dim(subjectRMV)[1]),NaN)
overallMeanCRE <- c(NaN,apply(subjectCRE,2,mean,na.rm=1), NaN)
overallSeCRE <- c(NaN,apply(subjectCRE,2,sd,na.rm=1)/sqrt(dim(subjectCRE)[1]),NaN)
overallSoa <- c("-Inf","-100","-50","0","50","100","Inf")
overallSoa <- factor(overallSoa,levels=c("-Inf","-100","-50","0","50","100","Inf")) # manually order
overallCondition <- c("Vestibular",rep("Combined",5),"Visual")
overallCondition <- factor(overallCondition,levels=c("Vestibular","Combined","Visual")) # manually order
overallMeanMRscaled <- (overallMeanMR) * 1200 # scale MR by (x-50) * 10 to match the RT scale
overallSeMRscaled <- sqrt((overallSeMR^2) * 1200^2) # scale MR by (x-50) * 10 to match the RT scale
overallData <- data.frame(overallMeanRT,overallSeRT,overallMeanMR,overallSeMR,overallMeanMRscaled,overallSeMRscaled,overallMeanCRE,overallSeCRE,overallMeanRMV,overallSeRMV,overallSoa,overallCondition)

## Figure 3: Grand RT and MR
overallRT <- ggplot(overallData,aes(x=overallSoa,y=overallMeanRT,fill=overallSoa)) +
  geom_errorbar(aes(ymin=overallMeanMRscaled-overallSeMRscaled, ymax=overallMeanMRscaled+overallSeMRscaled), width=0.15) +
  geom_errorbar(aes(ymin=overallMeanRT-overallSeRT, ymax=overallMeanRT+overallSeRT), width=0.15) +
  geom_col(colour="black",aes(y=overallMeanMRscaled)) +
  geom_point(aes(shape=overallCondition),size=4) + 
  labs(x = "SOA (ms)", y = "Reaction Time (ms)") + 
  scale_shape_manual(name="Condition",labels=c("Vestibular Cue", "Combined (SOA)", "Visual Cue"),values=c(24,21,22)) +
  scale_fill_manual(name="SOA (ms)",values=c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4")) +
  scale_y_continuous(breaks = seq(600, 1200, by = 100), labels = c(600,"",800,"",1000,"",1200), limits = c(0,1200), 
                     sec.axis=sec_axis(~ ., breaks = seq(0, 600, by = 120), labels=c(0,10,20,30,40,50), name = "Miss Rate (%)")) +
  guides(fill=guide_legend(order=2,override.aes=list(shape=(c(24,21,21,21,21,21,22)), col=(c("#F8766D","#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E","#00BFC4")))), shape=guide_legend(order=1)) +
  theme_classic(base_size = 18) +
  theme(legend.direction = "vertical", legend.position=c(0.5,0.82), legend.box="horizontal",
        legend.key = element_rect(color="black",size=0.3,linetype="dotted"), text = element_text(size=32),axis.text = element_text(colour = "black"),legend.text=element_text(size=22, colour= "black"))
print(overallRT)
#ggsave(file="overallRT.eps")
#ggsave(file="overallRT.png")


## Figure 4. Age and DVs
## Figure 4a: Age vs RT
dataRT <- data.frame("age" = subjects$age, "RT0" = subjectRT[,1], "RT1" = subjectRT[,2], "RT2" = subjectRT[,3], "RT3" = subjectRT[,4], "RT4" = subjectRT[,5], "RT5" = subjectRT[,6], "RT6" = subjectRT[,7])
meltRT <- reshape2::melt(dataRT,id.var="age")
cond <- c(rep("Vest",n),rep("Comb",n*5),rep("Vis",n))
cond <- factor(cond,levels=c("Vest","Comb","Vis"))
meltRT$cond <- cond
meltRT$subj <- rep(seq(1,n),7)
meltRT <- meltRT[complete.cases(meltRT),]
colnames(meltRT) <- c("age", "soa", "rt", "cond","subj")

lmRT <- lm(rt~0+soa+soa:age, data=meltRT) 
summary(lmRT) # Linear regression for RT

ageRT <- ggplot(meltRT, aes(x=age, y=rt, fill=soa)) +
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
#ggsave(file="ageRT.eps")


## Figure 4b: Age & MR
dataMR <- data.frame("age" = subjects$age, "MR0" = round(subjectMR[,1],2), "MR1" = round(subjectMR[,2],2), "MR2" = round(subjectMR[,3],2), "MR3" = round(subjectMR[,4],2), "MR4" = round(subjectMR[,5],2), "MR5" = round(subjectMR[,6],2), "MR6" = round(subjectMR[,7],2))
meltMR <- reshape2::melt(dataMR,id.var="age",na.rm=1)
cond <- c(rep("Vest",n),rep("Comb",n*5),rep("Vis",n))
cond <- factor(cond,levels=c("Vest","Comb","Vis"))
meltMR$cond <- cond
meltMR$subj <- rep(seq(1,n),7)
colnames(meltMR) <- c("age", "soa", "mr", "cond", "subj")

lmMR <- lm(mr~0+soa+soa:age, data=meltMR) 
summary(lmMR) # linear regression for for MR

ageMR <- ggplot(meltMR, aes(x=age, y=mr, fill=soa)) +
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
#ggsave(file="ageMR.eps")


## Figure 4c: Age & RMV
dataRMV <- data.frame("age" = subjects$age, "RMV1" = subjectRMV[,1], "RMV2" = subjectRMV[,2], "RMV3" = subjectRMV[,3], "RMV4" = subjectRMV[,4], "RMV5" = subjectRMV[,5])
meltRMV <- reshape2::melt(dataRMV,id.var="age")
meltRMV$cond <- c(rep("Comb",n*5))
meltRMV$subj <- rep(seq(1,n),5)
colnames(meltRMV) <- c("age", "soa", "rmv", "cond", "subj")

lmRMV <- lm(rmv~0+soa+soa:age, data=meltRMV)
summary(lmRMV) # linear regression for RMV
lmRMV2 <- lm(rmv~age, data=meltRMV)
summary(lmRMV2) # linear regression for RMV and just age

ageRMV <- ggplot(meltRMV, aes(x=age, y=rmv,fill=soa)) +
  geom_smooth(aes(colour=soa),method=lm,se=0) +
  geom_point(colour="black",aes(shape=cond,fill=soa),size=4) +
  scale_colour_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  scale_shape_manual(values=21) +
  scale_fill_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  labs(x = "Age", y = "Race Model Violation (ms)", tag="C") +
  theme_classic(base_size=17) +
  theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), text = element_text(size=32), axis.text = element_text(colour = "black"))
print(ageRMV)
#ggsave(file="ageRMV.eps")


## Figure 4d: Age & CRE
dataCRE <- data.frame("age" = subjects$age, "CRE1" = subjectCRE[,1], "CRE2" = subjectCRE[,2], "CRE3" = subjectCRE[,3], "CRE4" = subjectCRE[,4], "CRE5" = subjectCRE[,5])
meltCRE <- reshape2::melt(dataCRE,id.var="age")
meltCRE$cond <- c(rep("Comb",n*5))
meltCRE$subj <- rep(seq(1,n),5)
colnames(meltCRE) <- c("age", "soa", "cre", "cond", "subj")

lmCRE <- lm(cre~0+soa+soa:age, data=meltCRE) 
summary(lmCRE) # linear regression for CRE

ageCRE <- ggplot(meltCRE, aes(x=age, y=cre, fill=soa)) +
  geom_smooth(aes(colour=soa),method=lm,se=0) +
  geom_point(colour="black",aes(shape=cond,fill=soa),size=2) +
  scale_colour_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  scale_shape_manual(values=21) +
  scale_fill_manual(values=c("#F0F4C3", "#C5E1A5", "#9CCC65", "#689F38", "#33691E")) +
  labs(x = "Age", y = "Crossmodal Response Enhacement (%)", tag="D") +
  theme_classic(base_size=15) +
  theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "grey30"))
print(ageCRE)
#ggsave(file="ageCRE.eps")

## Figure 4, all 4 panels
#ggarrange(ageRT, ageMR, ageRMV, ageCRE, nrow = 2, ncol = 2)



## Figure 5: Correlation matrix
subjectData <- with(subjects, data.frame("Age" = age, "Mean RT" = rowMeans(subjectRT,na.rm=1), "Vestibular MR" = subjectMR[,1], "Mean RMV" = rowMeans(subjectRMV,na.rm=1), "Motion Sickness" = motionSick, "Exercise Hours" = exerciseHours, "Video Game Hours" = videoGameHours, "Driving Hours" = drivingHours, "Computer Hours" = computerHours, "Attention Diff" = attentionBefore-attentionAfter, "Energy Diff" = energyBefore-energyAfter, "Balance After" = balanceAfter))

corr <- cor(subjectData,use="complete.obs")
p.mat <- cor_pmat(subjectData)
corrPlot <- ggcorrplot(corr,type="upper",p.mat=p.mat)
print(corrPlot)
#ggsave(file="corrMatrix.eps")
#ggsave(file="corrMatrix.png")


## code to verify point in discussion about subjects w vestibular MR >= 80% having faster RT in SOA than vis
above80VMR <- (subjectMR[,1] >= 0.8) # subjects with vestibular MR >= 80%
above80visRT <- subjectRT[,7][highVMR] # vis RTs for subjects w vest MR >= 80%
above80soaRT <- matrix(subjectRT[,2:6][highVMR],22) # SOA RTs for subjects w vestMR >= 80%
above80diffRT <- above80soaRT - above80visRT # diff in SOA RT and vis RT, negative = soa RT is faster
above80neg <- above80diffRT < 0 # check if negative

## code to verify whether young adults with >50% MR had diff demographic qualities
subjectMRves <- subjectMR[,1]
yaAbove50 <- colMeans(subjects[subjects$age < 30 & subjectMRves > 0.5,10:20])
yaAbove50se <- apply(subjects[subjects$age < 30 & subjectMRves > 0.5,10:20],2,sd)/sqrt(8)
yaBelow50 <- colMeans(subjects[subjects$age < 30 & subjectMRves <= 0.5,10:20])
yaBelow50se <- apply(subjects[subjects$age < 30 & subjectMRves <= 0.5,10:20],2,sd)/sqrt(25)


write.csv(meltRT, file = "RT.csv")
write.csv(subjectData, file = "subjectData.csv")
write.csv(meltMR, file = "MR.csv")
write.csv(meltRMV, file = "RMV.csv")
write.csv(meltCRE, file = "CRE.csv")

