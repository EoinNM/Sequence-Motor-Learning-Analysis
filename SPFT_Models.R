#SPFT modelling

setwd("/Users/eoin/Documents/SPFT/DATA/DATA/")
spft <- read.csv("SPFT_Data.csv", header = T) 

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
#install.packages(effects)

#Subsets
spft_lrn <- subset(spft, Condition == "lrn", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, na.rm=FALSE))
spft_smp <- subset(spft, Condition == "smp", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, na.rm=FALSE))
#Aggregate
spft_lrn_rt = aggregate(RT       ~ Subject+Block+Group, FUN = mean, data = spft_lrn) # SYN outcome
spft_lrn_ac = aggregate(Accuracy ~ Subject+Block+Group, FUN = mean, data = spft_lrn) # RMSE outcome
spft_smp_rt = aggregate(RT       ~ Subject+Block+Group, FUN = mean, data = spft_smp) # SYN outcome
spft_smp_ac = aggregate(Accuracy ~ Subject+Block+Group, FUN = mean, data = spft_smp) # RMSE outcome

###################################################################################################
#1. 
#LME Null model - LRN Accuracy
model0 = lmer(Accuracy ~ Group + (1|Subject), data = spft_lrn_ac, REML = T)
#LME Model 1 - LRN Accuracy
model1 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_lrn_ac, REML = T)
#LME Model 2 - LRN Accuracy
model2 = lmer(Accuracy ~ Group + Block + Group*Block + (1|Subject), data = spft_lrn_ac, REML = T)
#Compare Models
anova(model0, model1, model2)

#LME Null model - LRN RT
model0 = lmer(RT ~ Group + (1|Subject), data = spft_lrn_rt, REML = T)
#LME Model 1 - LRN RT
model1 = lmer(RT ~ Group + Block + (1|Subject), data = spft_lrn_rt, REML = T)
#LME Model 2 - LRN RT
model2 = lmer(RT ~ Group + Block + Group*Block + (1|Subject), data = spft_lrn_rt, REML = T)
#Compare Models
anova(model0, model1, model2)

#LME Null model - SMP Accuracy
model0 = lmer(Accuracy ~ Group + (1|Subject), data = spft_smp_ac, REML = T)
#LME Model 1 - SMP Accuracy
model1 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_smp_ac, REML = T)
#LME Model 2 - SMP Accuracy
model2 = lmer(Accuracy ~ Group + Block + Group*Block + (1|Subject), data = spft_smp_ac, REML = T)
#Compare Models
anova(model0, model1, model2)

#LME Null model - SMP RT
model0 = lmer(Accuracy ~ Group + (1|Subject), data = spft_smp_ac, REML = T)
#LME Model 1 - SMP RT
model1 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_smp_ac, REML = T)
#LME Model 2 - SMP RT
model2 = lmer(Accuracy ~ Group + Block + Group*Block + (1|Subject), data = spft_smp_ac, REML = T)
#Compare Models
anova(model0, model1, model2)


#2. Repeated Measures
#A. LRN Accuracy
lrn.model.ac <- lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_lrn_ac, REML = T)
summary(lrn.model.ac)
#B. LRN RT
lrn.model.rt <- lmer(RT ~ Group + Block + (1|Subject), data = spft_lrn_rt, REML = T)
summary(lrn.model.rt) 
#C. SMP Accuracy
smp.model.ac <- lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_smp_ac, REML = T)
summary(smp.model.ac)
#D
smp.model.rt <- lmer(RT ~ Group + Block + (1|Subject), data = spft_smp_rt, REML = T)
summary(smp.model.rt)
###################################################################################################

###################################################################################################
#Compare lm with lmer 

#LM Null model - LRN Accuracy
model0 = lm(Accuracy ~ Group, data = spft_lrn_ac)
#LM Model 1 - LRN Accuracy
model1 = lm(Accuracy ~ Group + Block, data = spft_lrn_ac)
#LM Model 2 - LRN Accuracy
model2 = lm(Accuracy ~ Group + Block + Group*Trial, data = spft_lrn_ac)
#Compare Models
anova(model0, model1, model2)

#LME Null model - LRN Accuracy
model0 = lmer(Accuracy ~ Group + (1|Subject), data = spft_lrn_ac, REML = T)
#LME Model 1 - LRN Accuracy
model1 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_lrn_ac, REML = T)
#LME Model 2 - LRN Accuracy
model2 = lmer(Accuracy ~ Group + Block + Group*Block + (1|Subject), data = spft_lrn_ac, REML = T)
#Compare Models
anova(model0, model1, model2)

model0 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_lrn_ac)
model1 = lm(Accuracy ~ Group + Block, data = spft_lrn_ac)
anova(model0, model1) # LMER model is a better fit

lrn_model_accuracy <- lm(Accuracy ~ Group + Trial + T, data = spft_lrn_ac)
anova(lrn_model_accuracy)#Significant

model0 = lmer(Accuracy ~ Group + (1|Subject), data = spft_lrn_ac, REML = T)
model1 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_lrn_ac, REML = T)
model2 = lmer(Accuracy ~ Group + Block + (1|Subject), data = spft_lrn_ac, REML = T)
model3 = lmer(Accuracy ~ Group + Block + I(Block^2) + (1|Subject), data = spft_lrn_ac, REML = T)
anova(model0, model1, model2, model3)

#Polynomial for non-linear learning effect
lrn_model_accuracy <- lmer(Accuracy ~ Group + Block + I(Block^2) + (1|Subject), data = spft_lrn_ac, REML = T)
Anova(lrn_model_accuracy)
summary(lrn_model_accuracy)
anova(lrn_model_accuracy)
lrn_model_rt <- lmer(RT ~ Group + Block + I(Block^2) + (1|Subject), data = spft_lrn_rt, REML = T)
Anova(lrn_model_rt)
summary(lrn_model_rt)
anova(lrn_model_rt)
smp_model_accuracy <- lmer(Accuracy ~ Group + Block + I(Block^2) + (1|Subject), data = spft_smp_ac, REML = T)
Anova(smp_model_accuracy)
summary(smp_model_accuracy)
anova(smp_model_accuracy)
smp_model_rt <- lmer(RT ~ Group + Block + I(Block^2) + (1|Subject), data = spft_smp_rt, REML = T)
Anova(smp_model_rt)
summary(smp_model_rt)
anova(smp_model_rt)

#Subset the data to just the 4 days of administration
#subset into conditions
spft_lrn <- subset(spft, Condition == "lrn", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, Base_Plasma, Day7_Plasma, na.rm=FALSE))
spft_smp <- subset(spft, Condition == "smp", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, Base_Plasma, Day7_Plasma, na.rm=FALSE))
#subset conditions into days 2-5
spft_4_lrn <- subset(spft_lrn , Day > 1, select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, Base_Plasma, Day7_Plasma, na.rm=FALSE))
spft_4_SMP <- subset(spft, Condition == "smp", select=c(Subject, Trial, Block, Group, RT, Accuracy, Condition, Day, Base_Plasma, Day7_Plasma, na.rm=FALSE))

#subset the baseline - Accuracy LRN
Group_Base <- subset(spft, Day == 1, select=c(Subject, Group, Accuracy, Condition, Day, Block, Trial))
Group_Base_LRN_AC <- subset(Group_Base, Condition == 'lrn', select=c(Subject, Group, Accuracy, Condition, Day, Block, Trial))
base_scores =  aggregate(Accuracy ~ Day+Group+Subject, FUN = mean, data = Group_Base_LRN_AC)
write.csv(base_scores, file = "Acc_LRN_cov.csv")
#subset the baseline - RT LRN
Group_Base <- subset(spft, Day == 1, select=c(Subject, Group, RT, Condition, Day, Block, Trial))
Group_Base_LRN_RT <- subset(Group_Base, Condition == 'lrn', select=c(Subject, Group, RT, Condition, Day, Block, Trial))
base_scores =  aggregate(RT ~ Day+Group+Subject, FUN = mean, data = Group_Base_LRN_RT) 
write.csv(base_scores, file = "RT_LRN_cov.csv")
#subset the baseline - Accuracy SMP
Group_Base <- subset(spft, Day == 1, select=c(Subject, Group, Accuracy, Condition, Day, Block, Trial))
Group_Base_SYN_ac <- subset(Group_Base, Condition == 'smp', select=c(Subject, Group, Accuracy, Condition, Day, Block, Trial))
base_scores =  aggregate(Accuracy ~ Day+Group+Subject, FUN = mean, data = Group_Base_SYN_ac) 
write.csv(base_scores, file = "Acc_SMP_cov.csv")
#subset the baseline - RT - SMP
Group_Base <- subset(spft, Day == 1, select=c(Subject, Group, RT, Condition, Day, Block, Trial))
Group_Base_SYN_rt <- subset(Group_Base, Condition == 'smp', select=c(Subject, Group, RT, Condition, Day, Block, Trial))
base_scores =  aggregate(RT ~ Day+Group+Subject, FUN = mean, data = Group_Base_SYN_rt) 
write.csv(base_scores, file = "RT_SMP_cov.csv")

#subset the rest of the week
Group_Week <- subset(spft, Day > 1, select=c(Subject, Group, Accuracy, Condition, Day, Block, Trial))
#merge the 2

#Linear model on days 2-5 with baseline covariate included for RMSE Accuracy.
lrn_model_ac_2_5 <- lmer(Accuracy ~ Group + Block + (1|Subject), data = Group_Week, REML = T)
anova(lrn_model_ac_2_5)
