### Load matlab variables and subject data ###
dd <- read.csv("React_Lmer_RT&MR.csv")

newdd <- dd
for (j in 1:82){
  if (sum(!is.na(dd[dd$subj == j,]$Video.Game.Hours)) == 1 ){
    for (i in c(1:7)){
      newdd[newdd$subj==j,]$Video.Game.Hours[i] <- dd[dd$subj == j,]$Video.Game.Hours[which(!is.na(dd[dd$subj == j,]$Video.Game.Hours))]
      newdd[newdd$subj==j,]$Driving.Hours[i] <- dd[dd$subj == j,]$Driving.Hours[which(!is.na(dd[dd$subj == j,]$Driving.Hours))]
      newdd[newdd$subj==j,]$Computer.Hours[i] <- dd[dd$subj == j,]$Computer.Hours[which(!is.na(dd[dd$subj == j,]$Computer.Hours))]
      newdd[newdd$subj==j,]$subject[i] <- dd[dd$subj == j,]$subject[which(!is.na(dd[dd$subj == j,]$subject))]
    }
  }
}

write.csv(newdd, file = 'React_Lmer_RT&MR_updated.csv')






dd1 <- read.csv("React_Lmer_CRE&RMV.csv")
newdd1 <- dd1
for (j in 1:82){
  if (sum(!is.na(dd1[dd1$subj == j,]$Video.Game.Hours)) == 1 ){
    for (i in c(1:5)){
      newdd1[newdd1$subj==j,]$Video.Game.Hours[i] <- dd1[dd1$subj == j,]$Video.Game.Hours[which(!is.na(dd1[dd1$subj == j,]$Video.Game.Hours))]
      newdd1[newdd1$subj==j,]$Driving.Hours[i] <- dd1[dd1$subj == j,]$Driving.Hours[which(!is.na(dd1[dd1$subj == j,]$Driving.Hours))]
      newdd1[newdd1$subj==j,]$Computer.Hours[i] <- dd1[dd1$subj == j,]$Computer.Hours[which(!is.na(dd1[dd1$subj == j,]$Computer.Hours))]
      newdd1[newdd1$subj==j,]$sobject[i] <- dd1[dd1$subj == j,]$sobject[which(!is.na(dd1[dd1$subj == j,]$sobject))]
    }
  }
}

write.csv(newdd1, file = 'React_Lmer_CRE&RMV_updated.csv')

