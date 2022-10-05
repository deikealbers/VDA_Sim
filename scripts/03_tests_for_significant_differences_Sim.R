# inferential analysis
    # t-Tests to compare groups h-off and h-on, no statistic comparisons of all 3 groups
    # if levene test is signifant --> Welch t-Test
    # if data should be treated as ordinal --> Wilcoxon Rank Sum (aka Mann Whitney U)
    #     effect size is cohens d
        
#### preparations ####
rm(list = ls())
library(tidyverse)
library(car)
library(psych)
library(lsr)
setwd("~/R/VDA_Sim")

#### import data ####
# Read in files
vorbefragung <- read.csv("data/preprocessed/vorbefragung_scores.csv", encoding = "UTF-8")
nachbefragung <- read.csv("data/preprocessed/nachbefragung_scores.csv", encoding = "UTF-8")

nach_L2 <- nachbefragung %>%
  filter(group != "L0") %>%
  mutate(group = as.factor(group))

#### notes on statistical tests #### 
# Decision: no inferential statistical tests for these metrics:
#   ANOVA / Welch ANOVA (3 groups: L0 vs L2 H-on vs. L2 H-off)
#     1) 8x NDRTs <- difficult because comparison between private car with 
#                     system (L2 groups) vs private car as it is (L0 group)     --- ordinal, treated as interval
#     2) Subj. Estimation of monitoring performance               --- ordinal, treated as interval
#     3) Subj. Estimation of effect of study setting on behavior  --- ordinal, treated as interval
#   t-Test (2 groups: L2 H-on vs. L2 H-off)      
#     5b) 6x Trust in Automation subscales                        --- interval
#   --> ANOVA or Welch ANOVA (if levene test is significant)
# 
# Decision: inferential statistical tests for these metrics:
#   t-Test (2 groups: L2 H-on vs. L2 H-off)
#     1) 4x Warnmeldungen...                                      --- ordinal, treated as interval
#     2) 4x ReaktionAufWarnung...                                 --- ordinal, treated as interval
#     3) Intention To Use L2 in private car                       --- ordinal, treated as interval
#     4) 2x Intention To Use L2 components --- h-off component: only data in group h-off available     --- ordinal, treated as interval
#     5)a) 1x Trust in Automation overall score                   --- interval
#     6) 6x CTAM subscales (no overall score)                     --- interval
#     7) System Understanding overall score                       --- interval
#   --> t-Test or Welch t-Test (if levene test is significant)

c_lev <- c("lev_Df_group", "lev_Df", "lev_F", "lev_x1", "lev_p", "lev_x2")
c_tT_eqvar <- c("tT_eqvar_t", "tT_eqvar_Df", "tT_eqvar_p", "tT_eqvar_conf1", "tT_eqvar_conf2", "tT_eqvar_meanHoff", "tT_eqvar_meanHon",
           "tT_eqvar_nullvalue", "tT_eqvar_stderr", "tT_eqvar_hypothesis", "tT_eqvar_method", "tT_eqvar_variables")
c_tT_uneqvar <- c("tT_uneqvar_t", "tT_uneqvar_Df", "tT_uneqvar_p", "tT_uneqvar_conf1", "tT_uneqvar_conf2", "tT_uneqvar_meanHoff", "tT_uneqvar_meanHon",
            "tT_uneqvar_nullvalue", "tT_uneqvar_stderr", "tT_uneqvar_hypothesis", "tT_uneqvar_method", "tT_uneqvar_variables")
c_wilc <- c("wilc_W", "wilc_p", "wilc_nullvalue", "wilc_hypothesis", "wilc_method", "wilc_variables")

#### 4x Warnmeldungen ####

## Warnmeldungen.Warnungenzuhaeufig. ##
# levene (homogeneity)
lev_Warn1 <- leveneTest(Warnmeldungen.Warnungenzuhaeufig. ~ group, data = nach_L2)
tab_lev_Warn1 <- data.frame(matrix(unlist(lev_Warn1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Warn1) <- c_lev
# t-Test (equal variances)
tT_equal_Warn1 <- t.test(nach_L2$Warnmeldungen.Warnungenzuhaeufig. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Warn1 <- data.frame(matrix(unlist(tT_equal_Warn1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Warn1) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Warn1 <- t.test(nach_L2$Warnmeldungen.Warnungenzuhaeufig. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Warn1 <- data.frame(matrix(unlist(tT_unequal_Warn1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Warn1) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$Warnmeldungen.Warnungenzuhaeufig. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Warn1 <- wilcox.test(Warnmeldungen.Warnungenzuhaeufig. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Warn1 <- data.frame(matrix(unlist(wilc_Warn1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Warn1) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Warn1$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r/ sqrt(1-wilc_r^2)
# create data frame  
name <- c("Warnmeldungen.Warnungenzuhaeufig.")
test_tabWarn1 <- cbind(name,
                      tab_lev_Warn1, 
                      tab_tT_equal_Warn1, tab_tT_unequal_Warn1, tT_cD, 
                      tab_wilc_Warn1, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Warn1")))


## Warnmeldungen.SichermitWarnsystem. ##
# levene (homogeneity)
lev_Warn2 <- leveneTest(Warnmeldungen.SichermitWarnsystem. ~ group, data = nach_L2)
tab_lev_Warn2 <- data.frame(matrix(unlist(lev_Warn2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Warn2) <- c_lev
# t-Test (equal variances)
tT_equal_Warn2 <- t.test(nach_L2$Warnmeldungen.SichermitWarnsystem. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Warn2 <- data.frame(matrix(unlist(tT_equal_Warn2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Warn2) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Warn2 <- t.test(nach_L2$Warnmeldungen.SichermitWarnsystem. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Warn2 <- data.frame(matrix(unlist(tT_unequal_Warn2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Warn2) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$Warnmeldungen.SichermitWarnsystem. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Warn2 <- wilcox.test(Warnmeldungen.SichermitWarnsystem. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Warn2 <- data.frame(matrix(unlist(wilc_Warn2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Warn2) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Warn2$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("Warnmeldungen.SichermitWarnsystem.")
test_tabWarn2 <- cbind(name, 
                       tab_lev_Warn2, 
                       tab_tT_equal_Warn2, tab_tT_unequal_Warn2, tT_cD, 
                       tab_wilc_Warn2, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Warn2")))


## Warnmeldungen.FahrfremdeohneWarnun. ##
# levene (homogeneity)
lev_Warn3 <- leveneTest(Warnmeldungen.FahrfremdeohneWarnun. ~ group, data = nach_L2)
tab_lev_Warn3 <- data.frame(matrix(unlist(lev_Warn3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Warn3) <- c_lev
# t-Test (equal variances)
tT_equal_Warn3 <- t.test(nach_L2$Warnmeldungen.FahrfremdeohneWarnun. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Warn3 <- data.frame(matrix(unlist(tT_equal_Warn3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Warn3) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Warn3 <- t.test(nach_L2$Warnmeldungen.FahrfremdeohneWarnun. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Warn3 <- data.frame(matrix(unlist(tT_unequal_Warn3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Warn3) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$Warnmeldungen.FahrfremdeohneWarnun. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Warn3 <- wilcox.test(Warnmeldungen.FahrfremdeohneWarnun. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Warn3 <- data.frame(matrix(unlist(wilc_Warn3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Warn3) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Warn3$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("Warnmeldungen.FahrfremdeohneWarnun.")
test_tabWarn3 <- cbind(name, 
                       tab_lev_Warn3, 
                       tab_tT_equal_Warn3, tab_tT_unequal_Warn3, tT_cD, 
                       tab_wilc_Warn3, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Warn3")))


## Warnmeldungen.Laestig. ##
# levene (homogeneity)
lev_Warn4 <- leveneTest(Warnmeldungen.Laestig. ~ group, data = nach_L2)
tab_lev_Warn4 <- data.frame(matrix(unlist(lev_Warn4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Warn4) <- c_lev
# t-Test (equal variances)
tT_equal_Warn4 <- t.test(nach_L2$Warnmeldungen.Laestig. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Warn4 <- data.frame(matrix(unlist(tT_equal_Warn4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Warn4) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Warn4 <- t.test(nach_L2$Warnmeldungen.Laestig. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Warn4 <- data.frame(matrix(unlist(tT_unequal_Warn4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Warn4) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$Warnmeldungen.Laestig. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Warn4 <- wilcox.test(Warnmeldungen.Laestig. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Warn4 <- data.frame(matrix(unlist(wilc_Warn4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Warn4) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Warn4$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("Warnmeldungen.Laestig.")
test_tabWarn4 <- cbind(name, 
                       tab_lev_Warn4, 
                       tab_tT_equal_Warn4, tab_tT_unequal_Warn4, tT_cD, 
                       tab_wilc_Warn4, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Warn4")))

#### 4x ReaktionAufWarnung ####
## ReaktionaufWarnung.WarumertoentWarnung. ##
# levene (homogeneity)
lev_Reak1 <- leveneTest(ReaktionaufWarnung.WarumertoentWarnung. ~ group, data = nach_L2)
tab_lev_Reak1 <- data.frame(matrix(unlist(lev_Reak1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Reak1) <- c_lev
# t-Test (equal variances)
tT_equal_Reak1 <- t.test(nach_L2$ReaktionaufWarnung.WarumertoentWarnung. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Reak1 <- data.frame(matrix(unlist(tT_equal_Reak1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Reak1) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Reak1 <- t.test(nach_L2$ReaktionaufWarnung.WarumertoentWarnung. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Reak1 <- data.frame(matrix(unlist(tT_unequal_Reak1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Reak1) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$ReaktionaufWarnung.WarumertoentWarnung. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Reak1 <- wilcox.test(ReaktionaufWarnung.WarumertoentWarnung. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Reak1 <- data.frame(matrix(unlist(wilc_Reak1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Reak1) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Reak1$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r/ sqrt(1-wilc_r^2)
# create data frame  
name <- c("ReaktionaufWarnung.WarumertoentWarnung.")
test_tabReak1 <- cbind(name,
                       tab_lev_Reak1, 
                       tab_tT_equal_Reak1, tab_tT_unequal_Reak1, tT_cD, 
                       tab_wilc_Reak1, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Reak1")))


## ReaktionaufWarnung.RichtigeReaktion. ##
# levene (homogeneity)
lev_Reak2 <- leveneTest(ReaktionaufWarnung.RichtigeReaktion. ~ group, data = nach_L2)
tab_lev_Reak2 <- data.frame(matrix(unlist(lev_Reak2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Reak2) <- c_lev
# t-Test (equal variances)
tT_equal_Reak2 <- t.test(nach_L2$ReaktionaufWarnung.RichtigeReaktion. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Reak2 <- data.frame(matrix(unlist(tT_equal_Reak2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Reak2) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Reak2 <- t.test(nach_L2$ReaktionaufWarnung.RichtigeReaktion. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Reak2 <- data.frame(matrix(unlist(tT_unequal_Reak2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Reak2) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$ReaktionaufWarnung.RichtigeReaktion. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Reak2 <- wilcox.test(ReaktionaufWarnung.RichtigeReaktion. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Reak2 <- data.frame(matrix(unlist(wilc_Reak2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Reak2) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Reak2$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("ReaktionaufWarnung.RichtigeReaktion.")
test_tabReak2 <- cbind(name, 
                       tab_lev_Reak2, 
                       tab_tT_equal_Reak2, tab_tT_unequal_Reak2, tT_cD, 
                       tab_wilc_Reak2, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Reak2")))


## ReaktionaufWarnung.BewusstIgnoriert. ##
# levene (homogeneity)
lev_Reak3 <- leveneTest(ReaktionaufWarnung.BewusstIgnoriert. ~ group, data = nach_L2)
tab_lev_Reak3 <- data.frame(matrix(unlist(lev_Reak3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Reak3) <- c_lev
# t-Test (equal variances)
tT_equal_Reak3 <- t.test(nach_L2$ReaktionaufWarnung.BewusstIgnoriert. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Reak3 <- data.frame(matrix(unlist(tT_equal_Reak3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Reak3) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Reak3 <- t.test(nach_L2$ReaktionaufWarnung.BewusstIgnoriert. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Reak3 <- data.frame(matrix(unlist(tT_unequal_Reak3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Reak3) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$ReaktionaufWarnung.BewusstIgnoriert. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Reak3 <- wilcox.test(ReaktionaufWarnung.BewusstIgnoriert. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Reak3 <- data.frame(matrix(unlist(wilc_Reak3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Reak3) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Reak3$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("ReaktionaufWarnung.BewusstIgnoriert.")
test_tabReak3 <- cbind(name, 
                       tab_lev_Reak3, 
                       tab_tT_equal_Reak3, tab_tT_unequal_Reak3, tT_cD, 
                       tab_wilc_Reak3, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Reak3")))


## ReaktionaufWarnung.AufmerksamkeitaufFah. ##
# levene (homogeneity)
lev_Reak4 <- leveneTest(ReaktionaufWarnung.AufmerksamkeitaufFah. ~ group, data = nach_L2)
tab_lev_Reak4 <- data.frame(matrix(unlist(lev_Reak4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_Reak4) <- c_lev
# t-Test (equal variances)
tT_equal_Reak4 <- t.test(nach_L2$ReaktionaufWarnung.AufmerksamkeitaufFah. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_Reak4 <- data.frame(matrix(unlist(tT_equal_Reak4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_Reak4) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_Reak4 <- t.test(nach_L2$ReaktionaufWarnung.AufmerksamkeitaufFah. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_Reak4 <- data.frame(matrix(unlist(tT_unequal_Reak4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_Reak4) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$ReaktionaufWarnung.AufmerksamkeitaufFah. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_Reak4 <- wilcox.test(ReaktionaufWarnung.AufmerksamkeitaufFah. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_Reak4 <- data.frame(matrix(unlist(wilc_Reak4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_Reak4) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_Reak4$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("ReaktionaufWarnung.AufmerksamkeitaufFah.")
test_tabReak4 <- cbind(name, 
                       tab_lev_Reak4, 
                       tab_tT_equal_Reak4, tab_tT_unequal_Reak4, tT_cD, 
                       tab_wilc_Reak4, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_Reak4")))
#### 1x L2PrivNutzung  + 2x L2Komponenten ####

## L2PrivNutzung ##
# levene (homogeneity)
lev_L2Nutz1 <- leveneTest(L2PrivNutzung ~ group, data = nach_L2)
tab_lev_L2Nutz1 <- data.frame(matrix(unlist(lev_L2Nutz1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_L2Nutz1) <- c_lev
# t-Test (equal variances)
tT_equal_L2Nutz1 <- t.test(nach_L2$L2PrivNutzung ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_L2Nutz1 <- data.frame(matrix(unlist(tT_equal_L2Nutz1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_L2Nutz1) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_L2Nutz1 <- t.test(nach_L2$L2PrivNutzung ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_L2Nutz1 <- data.frame(matrix(unlist(tT_unequal_L2Nutz1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_L2Nutz1) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$L2PrivNutzung ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_L2Nutz1 <- wilcox.test(L2PrivNutzung ~ group, data = nach_L2, exact = FALSE)
tab_wilc_L2Nutz1 <- data.frame(matrix(unlist(wilc_L2Nutz1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_L2Nutz1) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_L2Nutz1$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r/ sqrt(1-wilc_r^2)
# create data frame  
name <- c("L2PrivNutzung")
test_tabL2Nutz1 <- cbind(name,
                         tab_lev_L2Nutz1, 
                         tab_tT_equal_L2Nutz1, tab_tT_unequal_L2Nutz1, tT_cD, 
                         tab_wilc_L2Nutz1, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_L2Nutz1")))


## L2Komponenten.Laengs. ##
# levene (homogeneity)
lev_L2Nutz2 <- leveneTest(L2Komponenten.Laengs. ~ group, data = nach_L2)
tab_lev_L2Nutz2 <- data.frame(matrix(unlist(lev_L2Nutz2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_L2Nutz2) <- c_lev
# t-Test (equal variances)
tT_equal_L2Nutz2 <- t.test(nach_L2$L2Komponenten.Laengs. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_L2Nutz2 <- data.frame(matrix(unlist(tT_equal_L2Nutz2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_L2Nutz2) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_L2Nutz2 <- t.test(nach_L2$L2Komponenten.Laengs. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_L2Nutz2 <- data.frame(matrix(unlist(tT_unequal_L2Nutz2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_L2Nutz2) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$L2Komponenten.Laengs. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_L2Nutz2 <- wilcox.test(L2Komponenten.Laengs. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_L2Nutz2 <- data.frame(matrix(unlist(wilc_L2Nutz2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_L2Nutz2) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_L2Nutz2$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("L2Komponenten.Laengs.")
test_tabL2Nutz2 <- cbind(name, 
                         tab_lev_L2Nutz2, 
                         tab_tT_equal_L2Nutz2, tab_tT_unequal_L2Nutz2, tT_cD, 
                         tab_wilc_L2Nutz2, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_L2Nutz2")))


## L2Komponenten.Quer. ##
# levene (homogeneity)
lev_L2Nutz3 <- leveneTest(L2Komponenten.Quer. ~ group, data = nach_L2)
tab_lev_L2Nutz3 <- data.frame(matrix(unlist(lev_L2Nutz3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_L2Nutz3) <- c_lev
# t-Test (equal variances)
tT_equal_L2Nutz3 <- t.test(nach_L2$L2Komponenten.Quer. ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_L2Nutz3 <- data.frame(matrix(unlist(tT_equal_L2Nutz3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_L2Nutz3) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_L2Nutz3 <- t.test(nach_L2$L2Komponenten.Quer. ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_L2Nutz3 <- data.frame(matrix(unlist(tT_unequal_L2Nutz3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_L2Nutz3) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$L2Komponenten.Quer. ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_L2Nutz3 <- wilcox.test(L2Komponenten.Quer. ~ group, data = nach_L2, exact = FALSE)
tab_wilc_L2Nutz3 <- data.frame(matrix(unlist(wilc_L2Nutz3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_L2Nutz3) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_L2Nutz3$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("L2Komponenten.Quer.")
test_tabL2Nutz3 <- cbind(name, 
                         tab_lev_L2Nutz3, 
                         tab_tT_equal_L2Nutz3, tab_tT_unequal_L2Nutz3, tT_cD, 
                         tab_wilc_L2Nutz3, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_L2Nutz3")))

#### 1x TiA overall score ####

## TiA_overall ##
# levene (homogeneity)
lev_TiAov <- leveneTest(TiA_overall ~ group, data = nach_L2)
tab_lev_TiAov <- data.frame(matrix(unlist(lev_TiAov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_TiAov) <- c_lev
# t-Test (equal variances)
tT_equal_TiAov <- t.test(nach_L2$TiA_overall ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_TiAov <- data.frame(matrix(unlist(tT_equal_TiAov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_TiAov) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_TiAov <- t.test(nach_L2$TiA_overall ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_TiAov <- data.frame(matrix(unlist(tT_unequal_TiAov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_TiAov) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$TiA_overall ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_TiAov <- wilcox.test(TiA_overall ~ group, data = nach_L2, exact = FALSE)
tab_wilc_TiAov <- data.frame(matrix(unlist(wilc_TiAov), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_TiAov) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_TiAov$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("TiA_overall")
test_tabTiAov <- cbind(name, 
                       tab_lev_TiAov, 
                       tab_tT_equal_TiAov, tab_tT_unequal_TiAov, tT_cD, 
                       tab_wilc_TiAov, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_TiAov")))

#### 6x CTAM subscales (no overall score) ####

## CTAM_PE ##
# levene (homogeneity)
lev_CTAM1 <- leveneTest(CTAM_PE ~ group, data = nach_L2)
tab_lev_CTAM1 <- data.frame(matrix(unlist(lev_CTAM1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_CTAM1) <- c_lev
# t-Test (equal variances)
tT_equal_CTAM1 <- t.test(nach_L2$CTAM_PE ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_CTAM1 <- data.frame(matrix(unlist(tT_equal_CTAM1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_CTAM1) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_CTAM1 <- t.test(nach_L2$CTAM_PE ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_CTAM1 <- data.frame(matrix(unlist(tT_unequal_CTAM1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_CTAM1) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$CTAM_PE ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_CTAM1 <- wilcox.test(CTAM_PE ~ group, data = nach_L2, exact = FALSE)
tab_wilc_CTAM1 <- data.frame(matrix(unlist(wilc_CTAM1), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_CTAM1) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_CTAM1$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r/ sqrt(1-wilc_r^2)
# create data frame  
name <- c("CTAM_PE")
test_tabCTAM1 <- cbind(name,
                       tab_lev_CTAM1, 
                       tab_tT_equal_CTAM1, tab_tT_unequal_CTAM1, tT_cD, 
                       tab_wilc_CTAM1, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_CTAM1")))


## CTAM_EE ##
# levene (homogeneity)
lev_CTAM2 <- leveneTest(CTAM_EE ~ group, data = nach_L2)
tab_lev_CTAM2 <- data.frame(matrix(unlist(lev_CTAM2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_CTAM2) <- c_lev
# t-Test (equal variances)
tT_equal_CTAM2 <- t.test(nach_L2$CTAM_EE ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_CTAM2 <- data.frame(matrix(unlist(tT_equal_CTAM2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_CTAM2) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_CTAM2 <- t.test(nach_L2$CTAM_EE ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_CTAM2 <- data.frame(matrix(unlist(tT_unequal_CTAM2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_CTAM2) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$CTAM_EE ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_CTAM2 <- wilcox.test(CTAM_EE ~ group, data = nach_L2, exact = FALSE)
tab_wilc_CTAM2 <- data.frame(matrix(unlist(wilc_CTAM2), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_CTAM2) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_CTAM2$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("CTAM_EE")
test_tabCTAM2 <- cbind(name, 
                       tab_lev_CTAM2, 
                       tab_tT_equal_CTAM2, tab_tT_unequal_CTAM2, tT_cD, 
                       tab_wilc_CTAM2, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_CTAM2")))


## CTAM_ATT ##
# levene (homogeneity)
lev_CTAM3 <- leveneTest(CTAM_ATT ~ group, data = nach_L2)
tab_lev_CTAM3 <- data.frame(matrix(unlist(lev_CTAM3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_CTAM3) <- c_lev
# t-Test (equal variances)
tT_equal_CTAM3 <- t.test(nach_L2$CTAM_ATT ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_CTAM3 <- data.frame(matrix(unlist(tT_equal_CTAM3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_CTAM3) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_CTAM3 <- t.test(nach_L2$CTAM_ATT ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_CTAM3 <- data.frame(matrix(unlist(tT_unequal_CTAM3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_CTAM3) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$CTAM_ATT ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_CTAM3 <- wilcox.test(CTAM_ATT ~ group, data = nach_L2, exact = FALSE)
tab_wilc_CTAM3 <- data.frame(matrix(unlist(wilc_CTAM3), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_CTAM3) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_CTAM3$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("CTAM_ATT")
test_tabCTAM3 <- cbind(name, 
                       tab_lev_CTAM3, 
                       tab_tT_equal_CTAM3, tab_tT_unequal_CTAM3, tT_cD, 
                       tab_wilc_CTAM3, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_CTAM3")))


## CTAM_FC ##
# levene (homogeneity)
lev_CTAM4 <- leveneTest(CTAM_FC ~ group, data = nach_L2)
tab_lev_CTAM4 <- data.frame(matrix(unlist(lev_CTAM4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_CTAM4) <- c_lev
# t-Test (equal variances)
tT_equal_CTAM4 <- t.test(nach_L2$CTAM_FC ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_CTAM4 <- data.frame(matrix(unlist(tT_equal_CTAM4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_CTAM4) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_CTAM4 <- t.test(nach_L2$CTAM_FC ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_CTAM4 <- data.frame(matrix(unlist(tT_unequal_CTAM4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_CTAM4) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$CTAM_FC ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_CTAM4 <- wilcox.test(CTAM_FC ~ group, data = nach_L2, exact = FALSE)
tab_wilc_CTAM4 <- data.frame(matrix(unlist(wilc_CTAM4), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_CTAM4) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_CTAM4$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("CTAM_FC")
test_tabCTAM4 <- cbind(name, 
                       tab_lev_CTAM4, 
                       tab_tT_equal_CTAM4, tab_tT_unequal_CTAM4, tT_cD, 
                       tab_wilc_CTAM4, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_CTAM4")))

## CTAM_ITU ##
# levene (homogeneity)
lev_CTAM5 <- leveneTest(CTAM_ITU ~ group, data = nach_L2)
tab_lev_CTAM5 <- data.frame(matrix(unlist(lev_CTAM5), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_CTAM5) <- c_lev
# t-Test (equal variances)
tT_equal_CTAM5 <- t.test(nach_L2$CTAM_ITU ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_CTAM5 <- data.frame(matrix(unlist(tT_equal_CTAM5), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_CTAM5) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_CTAM5 <- t.test(nach_L2$CTAM_ITU ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_CTAM5 <- data.frame(matrix(unlist(tT_unequal_CTAM5), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_CTAM5) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$CTAM_ITU ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_CTAM5 <- wilcox.test(CTAM_ITU ~ group, data = nach_L2, exact = FALSE)
tab_wilc_CTAM5 <- data.frame(matrix(unlist(wilc_CTAM5), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_CTAM5) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_CTAM5$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r/ sqrt(1-wilc_r^2)
# create data frame  
name <- c("CTAM_ITU")
test_tabCTAM5 <- cbind(name,
                       tab_lev_CTAM5, 
                       tab_tT_equal_CTAM5, tab_tT_unequal_CTAM5, tT_cD, 
                       tab_wilc_CTAM5, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_CTAM5")))


## CTAM_PS ##
# levene (homogeneity)
lev_CTAM6 <- leveneTest(CTAM_PS ~ group, data = nach_L2)
tab_lev_CTAM6 <- data.frame(matrix(unlist(lev_CTAM6), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_CTAM6) <- c_lev
# t-Test (equal variances)
tT_equal_CTAM6 <- t.test(nach_L2$CTAM_PS ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_CTAM6 <- data.frame(matrix(unlist(tT_equal_CTAM6), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_CTAM6) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_CTAM6 <- t.test(nach_L2$CTAM_PS ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_CTAM6 <- data.frame(matrix(unlist(tT_unequal_CTAM6), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_CTAM6) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$CTAM_PS ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_CTAM6 <- wilcox.test(CTAM_PS ~ group, data = nach_L2, exact = FALSE)
tab_wilc_CTAM6 <- data.frame(matrix(unlist(wilc_CTAM6), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_CTAM6) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_CTAM6$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("CTAM_PS")
test_tabCTAM6 <- cbind(name, 
                       tab_lev_CTAM6, 
                       tab_tT_equal_CTAM6, tab_tT_unequal_CTAM6, tT_cD, 
                       tab_wilc_CTAM6, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_CTAM6")))

#### 2x System Understanding subscales ####

## SU_System_sum ##
# levene (homogeneity)
lev_SU_Sys_score <- leveneTest(SU_System_sum ~ group, data = nach_L2)
tab_lev_SU_Sys_score <- data.frame(matrix(unlist(lev_SU_Sys_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_SU_Sys_score) <- c_lev
# t-Test (equal variances)
tT_equal_SU_Sys_score <- t.test(nach_L2$SU_System_sum ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_SU_Sys_score <- data.frame(matrix(unlist(tT_equal_SU_Sys_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_SU_Sys_score) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_SU_Sys_score <- t.test(nach_L2$SU_System_sum ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_SU_Sys_score <- data.frame(matrix(unlist(tT_unequal_SU_Sys_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_SU_Sys_score) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$SU_System_sum ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_SU_Sys_score <- wilcox.test(SU_System_sum ~ group, data = nach_L2, exact = FALSE)
tab_wilc_SU_Sys_score <- data.frame(matrix(unlist(wilc_SU_Sys_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_SU_Sys_score) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_SU_Sys_score$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("SU_System_sum")
test_tabSU_Sys_score <- cbind(name, 
                         tab_lev_SU_Sys_score, 
                         tab_tT_equal_SU_Sys_score, tab_tT_unequal_SU_Sys_score, tT_cD, 
                         tab_wilc_SU_Sys_score, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_SU_Sys_score")))

## SU_Role_sum ##
# levene (homogeneity)
lev_SU_Role_score <- leveneTest(SU_Role_sum ~ group, data = nach_L2)
tab_lev_SU_Role_score <- data.frame(matrix(unlist(lev_SU_Role_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_lev_SU_Role_score) <- c_lev
# t-Test (equal variances)
tT_equal_SU_Role_score <- t.test(nach_L2$SU_Role_sum ~ nach_L2$group, var.equal = TRUE, alternative = "two.sided")
tab_tT_equal_SU_Role_score <- data.frame(matrix(unlist(tT_equal_SU_Role_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_equal_SU_Role_score) <- c_tT_eqvar
# t-Test (unequal variances)
tT_unequal_SU_Role_score <- t.test(nach_L2$SU_Role_sum ~ nach_L2$group, var.equal = FALSE, alternative = "two.sided")
tab_tT_unequal_SU_Role_score <- data.frame(matrix(unlist(tT_unequal_SU_Role_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_tT_unequal_SU_Role_score) <- c_tT_uneqvar
# cohen's D effect size
tT_cD <- cohensD(nach_L2$SU_Role_sum ~ nach_L2$group)
# wilcoxon / Mann Whitney U
wilc_SU_Role_score <- wilcox.test(SU_Role_sum ~ group, data = nach_L2, exact = FALSE)
tab_wilc_SU_Role_score <- data.frame(matrix(unlist(wilc_SU_Role_score), nrow=1, byrow=TRUE),stringsAsFactors=FALSE)
names(tab_wilc_SU_Role_score) <- c_wilc
# wilcoxon effect size 
wilc_r <- abs(qnorm(wilc_SU_Role_score$p.value / 2)/sqrt(nrow(nach_L2)))
wilc_cD <- 2*wilc_r / sqrt(1-wilc_r^2)
# create data frame  
name <- c("SU_Role_sum")
test_tabSU_Role_score <- cbind(name, 
                              tab_lev_SU_Role_score, 
                              tab_tT_equal_SU_Role_score, tab_tT_unequal_SU_Role_score, tT_cD, 
                              tab_wilc_SU_Role_score, wilc_r, wilc_cD)
# delete all but test_tabXXX variable, name lists and data sets
rm(list = ls(pattern = ("_SU_Role_score")))

#### gather test results #### 
# combine data frames of tests
tests_nach0 <- bind_rows(test_tabWarn1, test_tabWarn2, test_tabWarn3, test_tabWarn4,
                         test_tabReak1, test_tabReak2, test_tabReak3, test_tabReak4,
                         test_tabL2Nutz1, test_tabL2Nutz2, test_tabL2Nutz3,
                         test_tabTiAov,
                         test_tabCTAM1, test_tabCTAM2, test_tabCTAM3, test_tabCTAM4, test_tabCTAM5, test_tabCTAM6,
                         test_tabSU_Sys_score, test_tabSU_Role_score
                         )
tests_nach <- tests_nach0 %>%
  select(-c(lev_x1, lev_x2, 
            ends_with("nullvalue"),
            ends_with("hypothesis"),
            ends_with("variables")))

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "nach_L2", 
                        "tests_nach")))

#### save results ####
write_excel_csv(tests_nach, "data/results/inferential_nachbefragung.csv")
