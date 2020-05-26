#Code for analysis of behavioural SPFT data Escitalopram and Motor Plasticity Project
#Code by Eoin N Molloy - Dept. Of Neurology, MPI CBS, Leipzig.
#########################################################################################
library(magrittr)
library(dplyr)
library(lme4)
library(car)
library(lmerTest)
require(MuMIn)
library(sjstats)
library(BayesFactor)
library(effsize)
library(ggpubr)
library(devtools)
library(Lahman)
library(psych)
#########################################################################################
#0 set directory
setwd('/Users/Eoin/Documents/SPFT_Analysis_All_Files/Assessment/Data_Frames/')
#########################################################################################
#1 Compare baseline to make sure groups were randomised correctly:
day_means <- read.csv("Means_Day.csv", header = T)
t.test(Lag_LRN_Base ~ Group, data=day_means,
       var.equal=TRUE,
       conf.level=0.95, paired = FALSE)
#########################################################################################
#2 Omnibus mixed effects modelling:
assess <-read.csv("Assessment.csv", header = T)
assess$block = factor(assess$Block)
assess$group = factor(assess$Group)
str(assess)
#Lag Model fitting:
#I Intercept Only
Lag_Intercept <- lmer(Lag ~  (1|Subject), data=assess, REML = F)
summary(Lag_Intercept)
#J With Block
Lag_Time <- lmer(Lag ~  block + (1|Subject), data=assess, REML = F)
summary (Lag_Time)
#K Compare A and B
anova(Lag_Intercept, Lag_Time)
#L With both main effects of group and block
Lag_Both <- lmer(Lag ~  block + group + (1|Subject), data=assess, REML = F)
summary (Lag_Both)
#M Compare B and D
anova(Lag_Time, Lag_Both)
#N With both main effects in interaction
Lag_Interaction <- lmer(Lag ~ group*block + (1|Subject), data = assess, REML = F)
summary(Lag_Interaction)
#O Compare D with F
anova(Lag_Both, Lag_Interaction)
#Anova on the full model
summary(Lag_Interaction)
anova(Lag_Interaction)
Anova(Lag_Interaction)
#3 Fixed effect effect sizes for each model in the model comparisons above:
#A For lag
Lag_Intercept <- lmer(Lag ~  (1|Subject), data=assess, REML = F)
lag_null <- lmer(Lag ~ block + (1|Subject), data = assess, REML = F)
lag_null1 <- lmer(Lag ~ group + block + (1|Subject), data = assess, REML = F)
lag <- lmer(Lag ~ group*block + (1|Subject), data = assess, REML = F)
r.squaredGLMM(Lag_Intercept)
r.squaredGLMM(lag_null)
r.squaredGLMM(lag_null1) 
r.squaredGLMM(lag) 
#########################################################################################
#########################################################################################
#4 Post-hoc t-tests at Day1, Day7
#Baseline
#Day1:
t.test(Lag_LRN_Day1 ~ Group, data=day_means,
       var.equal=TRUE,
       conf.level=0.95, paired = FALSE)
describe.by(day_means$Lag_LRN_Day1, day_means$Group)

#Day7:
t.test(Lag_LRN_Day7 ~ Group, data=day_means,
       var.equal=TRUE,
       conf.level=0.95, paired = FALSE)
#########################################################################################
#########################################################################################
#5 Bayes Factor on post-hoc 2-sample ttests:
Bayes <-read.csv("Means_Day_Bayes.csv", header = T)
#Data preparation:
# recode all character variables to numeric
#Lag for both groups
Bayes$Lag_LRN_Day1_SSRI   <- as.numeric(Bayes$Lag_LRN_Day1_SSRI)
Bayes$Lag_LRN_Day7_SSRI   <- as.numeric(Bayes$Lag_LRN_Day7_SSRI)

Bayes$Lag_LRN_Day1_PLAC   <- as.numeric(Bayes$Lag_LRN_Day1_PLAC)
Bayes$Lag_LRN_Day7_PLAC   <- as.numeric(Bayes$Lag_LRN_Day7_PLAC)

# reshape data into long format
Bayes.l <- reshape(data=Bayes, idvar="?..Index", varying = c("Lag_LRN_Day1_SSRI", "Lag_LRN_Day7_SSRI", 
                                                             "Lag_LRN_Day1_PLAC", "Lag_LRN_Day7_PLAC"),
                   v.name=c("value"), 
                   times=c("Lag_LRN_Day1_SSRI", "Lag_LRN_Day7_SSRI", 
                           "Lag_LRN_Day1_PLAC", "Lag_LRN_Day7_PLAC"),
                   new.row.names = 1:1000,
                   direction="long")
# delete index variable (only NAs)
Bayes.l <- Bayes.l[, -1]
# omit rows with NAs
Bayes.l <- na.omit(Bayes.l)

# only select relevant data for this test - first is lag at each of the 3 timepoints:
Bayes.1 <- Bayes.l[ which(Bayes.l$time=='Lag_LRN_Day1_SSRI' | Bayes.l$time=='Lag_LRN_Day1_PLAC'),]
# Cohen's d with CI
effsize::cohen.d(Bayes.1$value, Bayes.1$time, paired = F, pooled = T)
# Bayes factor t-test
ttestBF(formula=value ~ time, data = Bayes.1, mu=0, nullInterval=NULL, 
        Paired=FALSE, rscale="medium", posterior=FALSE, var.equal=FALSE)

Bayes.2 <- Bayes.l[ which(Bayes.l$time=='Lag_LRN_Day7_SSRI' | Bayes.l$time=='Lag_LRN_Day7_PLAC'),]
# Cohen's d with CI
effsize::cohen.d(Bayes.2$value, Bayes.2$time, paired = F, pooled = T)
# Bayes factor t-test
ttestBF(formula=value ~ time, data = Bayes.2, mu=0, nullInterval=NULL, 
        Paired=FALSE, rscale="medium", posterior=FALSE, var.equal=FALSE)
#########################################################################################
#########################################################################################
#6 Deltas:
Deltas <-read.csv("Deltas_BL_D7_Analysis.csv", header = T)
t.test(Lag_LRN_Delta ~ Group, data=Deltas,
       var.equal=TRUE,
       conf.level=0.95, paired = FALSE)
describe.by(Deltas$Lag_LRN_Delta, Deltas$Group)

#Bayesian TTests on Deltas:
#Bayes now on the deltas in the learning condition
Deltas$Lag_LRN_Delta<- as.numeric(Deltas$Lag_LRN_Delta)
#cohen's d
effsize::cohen.d(Deltas$Lag_LRN_Delta, Deltas$Group, paired = F, pooled = T)
# Bayes factor t-test
ttestBF(formula=Lag_LRN_Delta ~ Group, data = Deltas, mu=0, nullInterval=NULL, 
        Paired=FALSE, rscale="medium", posterior=FALSE, var.equal=FALSE)
#########################################################################################