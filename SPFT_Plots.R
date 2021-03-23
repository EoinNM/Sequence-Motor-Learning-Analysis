setwd("/Users/eoin/Documents/SPFT/DATA/DATA/")
spft <- read.csv("SPFT_Data.csv", header = T) 
head(spft)
str(spft)
#import packages for plotting/analyses
library(lme4)
library(lmerTest)
library(psych)
library(ez)
library(lme4)
library(lmerTest)
library(BayesFactor)
library(cowplot)
library(ggplot2)
library(car)
library(naniar)
library(reshape2)
library(dplyr)

####### Plotting the data#########
#Split the data
spft1=spft[spft$Condition=='lrn',]
spft2=spft[spft$Condition=='smp',]
spft1$Group=as.factor(spft1$Group)
spft1$Genotype=as.factor(spft1$Genotype)
spft1$Subject=as.factor(spft1$Subject)

#Plot the assessment week
#First is blocks:
#1. Groups - LRN Accuracy
spft1_RMSE <- ggplot(spft1, aes(x=Block,y=Accuracy, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN Accuracy") + scale_color_discrete(name = "Groups")
spft1_RMSE
#2. Trials
spft1_RMSE <- ggplot(spft1, aes(x=Trial,y=Accuracy, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN Accuracy") + scale_color_discrete(name = "Groups")
spft1_RMSE
#3. Groups - SMP Accuracy
spft2_RMSE <- ggplot(spft2, aes(x=Block,y=Accuracy, shape = Condition, group=Group, colour = Group)) + 
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - SMP Accuracy") + scale_color_discrete(name = "Groups")
spft2_RMSE
#4 Trials
spft2_RMSE <- ggplot(spft2, aes(x=Trial,y=Accuracy, shape = Condition, group=Group, colour = Group)) + 
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - SMP Accuracy") + scale_color_discrete(name = "Groups")
spft2_RMSE

#5. Groups - LRN Reaction Time
spft1_SYN <- ggplot(spft1, aes(x=Block,y=RT, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN Reaction Time") + scale_color_discrete(name = "Groups")
spft1_SYN
#6 Trials
spft1_SYN <- ggplot(spft1, aes(x=Trial,y=RT, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN Reaction Time") + scale_color_discrete(name = "Groups")
spft1_SYN

#7. Groups - SMP Reaction Time
spft2_SYN <- ggplot(spft2, aes(x=Block,y=RT, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - SMP Reaction Time") + scale_color_discrete(name = "Groups")
spft2_SYN
#8 Trials
spft2_SYN <- ggplot(spft2, aes(x=Trial,y=RT, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - SMP Reaction Time") + scale_color_discrete(name = "Groups")
spft2_SYN

#Then is days:
#1. Group Differences - LRN - Accuracy
spft1_RMSE_lrn_day <- ggplot(spft1, aes(x=Day,y=Accuracy, shape = Condition, group=Group, colour = Group)) +
  geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN - Accuracy") + scale_color_discrete(name = "Groups")
spft1_RMSE_lrn_day
#2. Group Differences - LRN - Accuracy
spft1_RT_lrn_day <- ggplot(spft1, aes(x=Day,y=RT, shape = Condition, group=Group, colour = Group)) + geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN - Reaction_Time") + scale_color_discrete(name = "Groups")
spft1_RT_lrn_day
#3. Group Differences - SMP - Accuracy
spft2_RMSE_smp_day <- ggplot(spft2, aes(x=Day,y=Accuracy, shape = Condition, group=Group, colour = Group)) + geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - SMP - Accuracy") + scale_color_discrete(name = "Groups")
spft2_RMSE_smp_day
#4. Group Differences - SMP - Reaction Time
spft2_RT_smp_day <- ggplot(spft2, aes(x=Day,y=RT, shape = Condition, group=Group, colour = Group)) + geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - SMP - Reaction_Time") + scale_color_discrete(name = "Groups")
spft2_RT_smp_day

###########Genotype Plots - in the RMSE_LRN condition only#############
#1. Genotype Differences LRN Accuracy by Block
spft1_Gen_RMSE <- ggplot(spft1, aes(x=Block,y=Accuracy, shape = Condition, group=Genotype, colour = Genotype)) + geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Genotype Differences - LRN Accuracy") + scale_fill_manual(name="Genotype")
spft1_Gen_RMSE

#2 Genotype Differences LRN Accuracy by Day
spft1_Gen_RMSE <- ggplot(spft1, aes(x=Day,y=Accuracy, shape = Condition, group=Genotype, colour = Genotype)) + geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Genotype Differences - LRN Accuracy") + scale_fill_manual(name="Genotype")
spft1_Gen_RMSE

#Trials by subject
#1. RMSE LRN
spft1_RMSE <- ggplot(spft1, aes(x=Trial,y=Accuracy, shape = Subject, group=Subject, colour = Group)) + geom_line(stat='summary') + stat_summary(fun.data = mean_se, geom = "errorbar") + ggtitle("Group Differences - LRN Accuracy") + scale_color_discrete(name = "Groups")
spft1_RMSE

######Scatter with groups:
spft_lrn <- subset(spft, Condition == "lrn", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, Base_Plasma, Day7_Plasma, Force, Acc_lrn_cov, Acc_smp_cov, RT_smp_cov, RT_lrn_cov, na.rm=FALSE))
spft_smp <- subset(spft, Condition == "smp", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, Base_Plasma, Day7_Plasma, Force, Acc_lrn_cov, Acc_smp_cov, RT_smp_cov, RT_lrn_cov, na.rm=FALSE))
write.csv(spft_lrn, file = "SPFT_LRN.csv")
write.csv(spft_smp, file = "SPFT_SMP.csv")

Group_Week <- subset(spft, Day > 1, select=c(Subject, Group, Accuracy, RT, Condition, Day, Block, Trial, Acc_lrn_cov, Acc_smp_cov, RT_lrn_cov, RT_smp_cov, Force))
#Subsets - Subset these 4 days into the LRN and SMP condition
spft_lrn <- subset(Group_Week, Condition == "lrn", select=c(Subject, Group, Accuracy, RT, Condition, Day, Block, Trial, Acc_lrn_cov, Acc_smp_cov, RT_lrn_cov, RT_smp_cov, Force, na.rm=FALSE))
spft_smp <- subset(Group_Week, Condition == "smp", select=c(Subject, Group, Accuracy, RT, Condition, Day, Block, Trial, Acc_lrn_cov, Acc_smp_cov, RT_lrn_cov, RT_smp_cov, Force, na.rm=FALSE))
write.csv(spft_lrn, file = "SPFT_LRN.csv")
write.csv(spft_smp, file = "SPFT_SMP.csv")
