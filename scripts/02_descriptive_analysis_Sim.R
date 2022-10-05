#### preparations ####
rm(list = ls())
library(tidyverse)
library(skimr)
library(data.table)
setwd("~/R/VDA_Sim")

#### write function for summary ####
# adjusted from skim() function
mean_Sim = function(x) mean(x, na.rm = TRUE)
sd_Sim = function(x) sd(x, na.rm = TRUE)
median_Sim = function(x) median(x, na.rm = TRUE)
min_Sim = function(x) min(x, na.rm = TRUE)
max_Sim = function(x) max(x, na.rm = TRUE)

Sim_skim <- skim_with(numeric = sfl(n = length, mean = mean_Sim, sd = sd_Sim, median = median_Sim, min =  min_Sim, max = max_Sim), append = FALSE)

#### import data ####
# Read in files
vorbefragung <- read.csv("data/preprocessed/vorbefragung_scores.csv", encoding = "UTF-8")
nachbefragung <- read.csv("data/preprocessed/nachbefragung_scores.csv", encoding = "UTF-8")

#### notes on variables of vorbefragung ####
# ## meta variables, n = 2 ##
#     group
#     VPNr
# ## not needed variables, n = 162 ##
#     FiltHersteller... (n = 138)
#     raw data,  Fahrstil.DSQ... (n = 15)
#     raw data, Technikaffinitaet.ATIS... (n = 9)
# ## nominal data, n = 5 ##
#     Geschlecht
#     Haendigkeit
#     Sehschwaeche
#     Farbfehlsichtigkeit
#     Hoerschwaeche
# ## ordinal data, n = 10 ##
#     Fahrtfrequenz
#     Fahrtstrecke
#     FrequenzAutobahn
#     StreckeAutobahn
#     KenntnisAS.CC.
#     KenntnisAS.ACC.
#     KenntnisAS.Spurhalte.
#     KenntnisAS.Stauassistent.
#     KenntnisAS.ParkAssist.
#     KenntnisAS.Teilautomation.
# ## interval data, n = 9 ##
#     Alter
#     Fuehrerschein
#     DSQ_Speed
#     DSQ_Calmness
#     DSQ_SocialResistance
#     DSQ_Focus
#     DSQ_Planning
#     DSQ_Deviance
#     ATIS

#### subsets vorbefragung ####
vorbefragung <- vorbefragung %>% 
  rename(group = X.U.FEFF.group)

subset_vor_nominal <- vorbefragung %>%
  select(group, Geschlecht, Haendigkeit, Sehschwaeche, Farbfehlsichtigkeit, Hoerschwaeche) %>%
  mutate(Geschlecht = as.factor(Geschlecht)) %>%
  mutate(Haendigkeit = as.factor(Haendigkeit)) %>%
  mutate(Sehschwaeche = as.factor(Sehschwaeche)) %>%
  mutate(Farbfehlsichtigkeit = as.factor(Farbfehlsichtigkeit)) %>%
  mutate(Hoerschwaeche = as.factor(Hoerschwaeche))

subset_vor_ordinal_nofactors <- vorbefragung %>%
  select(group, Fahrtfrequenz, Fahrtstrecke, FrequenzAutobahn, StreckeAutobahn, starts_with("KenntnisAS."))

subset_vor_ordinal_factors <- vorbefragung %>%
  select(group, Fahrtfrequenz, Fahrtstrecke, FrequenzAutobahn, StreckeAutobahn, starts_with("KenntnisAS.")) %>%
  mutate(Fahrtfrequenz = as.factor(Fahrtfrequenz)) %>%
  mutate(Fahrtstrecke = as.factor(Fahrtstrecke)) %>%
  mutate(FrequenzAutobahn = as.factor(FrequenzAutobahn)) %>%
  mutate(StreckeAutobahn = as.factor(StreckeAutobahn)) %>%
  mutate(Fahrtfrequenz = as.factor(Fahrtfrequenz)) %>%
  mutate(KenntnisAS.CC. = as.factor(KenntnisAS.CC.)) %>%
  mutate(KenntnisAS.ACC. = as.factor(KenntnisAS.ACC.)) %>%
  mutate(KenntnisAS.Spurhalte. = as.factor(KenntnisAS.Spurhalte.)) %>%
  mutate(KenntnisAS.Stauassistent. = as.factor(KenntnisAS.Stauassistent.)) %>%
  mutate(KenntnisAS.ParkAssist. = as.factor(KenntnisAS.ParkAssist.)) %>%
  mutate(KenntnisAS.Teilautomation. = as.factor(KenntnisAS.Teilautomation.))

subset_vor_interval <- vorbefragung %>%
  select(group, Alter, Fuehrerschein, starts_with("DSQ"), ATIS)

#### descriptive summaries vorbefragung ####
summary_vor_nominal <- subset_vor_nominal %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>%
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_vor_ordinal_factors <- subset_vor_ordinal_factors %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_vor_ordinal_nofactors <- subset_vor_ordinal_nofactors %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_vor_interval <- subset_vor_interval %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_vor_ordinal <- left_join(summary_vor_ordinal_nofactors, summary_vor_ordinal_factors)

summary_vorbefragung <- bind_rows(summary_vor_nominal, summary_vor_ordinal, summary_vor_interval)

summary_headers <- c("variable", "group", "n_missing", "frequencies", "n", "mean", "sd", "median", "min", "max")
names(summary_vorbefragung) <- summary_headers

summ_vorbefragung <- summary_vorbefragung %>%
  mutate(n = n - n_missing) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 0)) %>%
  mutate(max = round(max, 0)) %>%
  relocate(frequencies, .after = "max")

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", 
                        "mean_Sim", "sd_Sim", "median_Sim", "min_Sim", "max_Sim", "Sim_skim")))

#### notes on variables of nachbefragung ####
# ## meta variables, n = 2  ##
#     group
#         interval --- not in Sim
#     VPNr
#         DatumFahrt --- not in Sim
# ## not needed variables, n = 126 ##
#     Vertrauen.TiA... (n = 19)
#     Akzeptanz... (n = 23)
#         System... (n = 57) # in Sim: 19 items instead of 16 (in FOT)
#     MatrixNDRTsFreitext... (n = 6)
#         OptionalAnmerkungenW:KommentareStudie.y (n = 21) # different variables and number in Sim 
# ## ordinal data, n = 22 ##
#     NDRTs.NDRT... (n = 8)
#     SubjUeberwachguete.1.
#     SubjEinflussSetting
#         Warnmeldungen. ... (n = 4) --- only in Sim
#         ReaktionaufWarnung. ... (n = 4) --- only in Sim
#     L2PrivNutzung
#     L2Komponenten. ... (n = 3)
#         Ranking --- not in Sim
# ## interval data, n =  15 ##
#         DauerFahrt --- not in Sim
#         L2AnteilFahrt --- not in Sim
#         KmVorFahrt --- not in Sim
#     TiA... (n = 7)
#     CTAM... (n = 6)
#     SU_System_sum
#     SU_Role_sum

#### subsets nachbefragung ####
vars_ordinal <- c("NDRTs.NDRT1.", "NDRTs.NDRT2.", "NDRTs.NDRT3.", "NDRTs.NDRT4.",
                  "NDRTs.NDRT5.", "NDRTs.NDRT6.","NDRTs.NDRT7.", "NDRTs.NDRT8.",
                  "Warnmeldungen.Warnungenzuhaeufig.", "Warnmeldungen.SichermitWarnsystem.", 
                  "Warnmeldungen.FahrfremdeohneWarnun.", "Warnmeldungen.Laestig.",
                  "ReaktionaufWarnung.WarumertoentWarnung.", "ReaktionaufWarnung.RichtigeReaktion.",
                  "ReaktionaufWarnung.BewusstIgnoriert.", "ReaktionaufWarnung.AufmerksamkeitaufFah.",
                  "SubjUeberwachguete.1.", "SubjEinflussSetting", "L2PrivNutzung",
                  "L2Komponenten.Laengs.", "L2Komponenten.Quer.", "L2Komponenten.Hoff.")

subset_nach_ordinal_factors <- nachbefragung %>%
  select(group, vars_ordinal) %>%
  mutate_at(all_of(vars_ordinal), factor)

subset_nach_ordinal_nofactors <- nachbefragung %>%
  select(group, vars_ordinal)

subset_nach_interval <- nachbefragung %>%
  select(group, starts_with("TiA"), starts_with("CTAM"), SU_System_sum, SU_Role_sum) %>%
  mutate(SU_System_sum = SU_System_sum*100) %>%
  mutate(SU_Role_sum = SU_Role_sum*100)

#### descriptive summaries nachbefragung ####
summary_nach_ordinal_factors <- subset_nach_ordinal_factors %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>% 
  select (-c(skim_type, complete_rate, factor.ordered, factor.n_unique))

summary_nach_ordinal_nofactors <- subset_nach_ordinal_nofactors %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_nach_interval <- subset_nach_interval %>%
  dplyr::group_by(group) %>%
  Sim_skim(.) %>%
  select (-c(skim_type, complete_rate))

summary_nach_ordinal <- left_join(summary_nach_ordinal_nofactors, summary_nach_ordinal_factors)

summary_nachbefragung <- bind_rows(summary_nach_ordinal, summary_nach_interval)

summary_headers <- c("variable", "group", "n_missing", "n", "mean", "sd", "median", "min", "max", "frequencies")
names(summary_nachbefragung) <- summary_headers

summ_nachbefragung <- summary_nachbefragung %>%
  mutate(n = n - n_missing) %>%
  select(-c(n_missing)) %>%
  mutate(mean = round(mean, 2)) %>%
  mutate(sd = round(sd, 2)) %>%
  mutate(median = round(median, 2)) %>%
  mutate(min = round(min, 0)) %>%
  mutate(max = round(max, 0))

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_Sim", "sd_Sim", "median_Sim", "min_Sim", "max_Sim", "Sim_skim")))
#### table familiarity with ADAS - manufacturers ####
# function for frequency tables
notemptycount_Sim <- function(x) {  (sum(x!="", na.rm = TRUE))}

list_man <- c("KeinSystem", # only in Sim
              "VW", "Opel", "Mercedes", "Ford", "BMW", 
              "Audi", "Skoda", "Renault", "Toyota", "Seat", 
              "Hyundai", "Fiat", "Nissan", "Peugeot", "Mazda",
              "Kia", "Dacia", "Citroen", "Volvo", "Mitsubishi", 
              "Tesla", # only in Sim
              "other")
## group L0
sel_ADAS_L0 <- vorbefragung %>%
  select(group, VPNr, 
         starts_with("TempomatAutomarke"),
         starts_with("ACCAutomarke"),
         starts_with("SpurhalteAutomarke"),
         starts_with("StauassAutomarke"),
         starts_with("ParkAssistAutomarke"),
         starts_with("AutomationAutomarke")) %>%
  filter(group == "L0")
ADAS_L0 <- as.data.frame(sapply(sel_ADAS_L0, notemptycount_Sim))
ADAS_L0_t <- transpose(ADAS_L0)
colnames(ADAS_L0_t) <- rownames(ADAS_L0)

L0_CC <- ADAS_L0_t %>%
  select(starts_with("TempomatAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L0", ADAS = "CC", .before = "KeinSystem")
L0_ACC <- ADAS_L0_t %>%
  select(starts_with("ACCAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L0", ADAS = "ACC", .before = "KeinSystem")
L0_LKA <- ADAS_L0_t %>%
  select(starts_with("SpurhalteAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L0", ADAS = "LKA", .before = "KeinSystem")
L0_TJA <- ADAS_L0_t %>%
  select(starts_with("StauassAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L0", ADAS = "TJA", .before = "KeinSystem")
L0_PA <- ADAS_L0_t %>%
  select(starts_with("ParkAssistAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L0", ADAS = "PA", .before = "KeinSystem")
L0_L2 <- ADAS_L0_t %>%
  select(starts_with("AutomationAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L0", ADAS = "L2", .before = "KeinSystem")

L0_man_ADAS <- bind_rows(L0_CC, L0_ACC, L0_LKA, L0_TJA, L0_PA, L0_L2)

## group L2 H-on
sel_ADAS_L2_Hon <- vorbefragung %>%
  select(group, VPNr, 
         starts_with("TempomatAutomarke"),
         starts_with("ACCAutomarke"),
         starts_with("SpurhalteAutomarke"),
         starts_with("StauassAutomarke"),
         starts_with("ParkAssistAutomarke"),
         starts_with("AutomationAutomarke")) %>%
  filter(group == "L2 H-on")
ADAS_L2_Hon <- as.data.frame(sapply(sel_ADAS_L2_Hon, notemptycount_Sim))
ADAS_L2_Hon_t <- transpose(ADAS_L2_Hon)
colnames(ADAS_L2_Hon_t) <- rownames(ADAS_L2_Hon)

L2_Hon_CC <- ADAS_L2_Hon_t %>%
  select(starts_with("TempomatAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-on", ADAS = "CC", .before = "KeinSystem")
L2_Hon_ACC <- ADAS_L2_Hon_t %>%
  select(starts_with("ACCAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-on", ADAS = "ACC", .before = "KeinSystem")
L2_Hon_LKA <- ADAS_L2_Hon_t %>%
  select(starts_with("SpurhalteAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-on", ADAS = "LKA", .before = "KeinSystem")
L2_Hon_TJA <- ADAS_L2_Hon_t %>%
  select(starts_with("StauassAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-on", ADAS = "TJA", .before = "KeinSystem")
L2_Hon_PA <- ADAS_L2_Hon_t %>%
  select(starts_with("ParkAssistAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-on", ADAS = "PA", .before = "KeinSystem")
L2_Hon_L2 <- ADAS_L2_Hon_t %>%
  select(starts_with("AutomationAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-on", ADAS = "L2", .before = "KeinSystem")

L2_Hon_man_ADAS <- bind_rows(L2_Hon_CC, L2_Hon_ACC, L2_Hon_LKA, L2_Hon_TJA, L2_Hon_PA, L2_Hon_L2)

## group L2 H-off
sel_ADAS_L2_Hoff <- vorbefragung %>%
  select(group, VPNr, 
         starts_with("TempomatAutomarke"),
         starts_with("ACCAutomarke"),
         starts_with("SpurhalteAutomarke"),
         starts_with("StauassAutomarke"),
         starts_with("ParkAssistAutomarke"),
         starts_with("AutomationAutomarke")) %>%
  filter(group == "L2 H-off")
ADAS_L2_Hoff <- as.data.frame(sapply(sel_ADAS_L2_Hoff, notemptycount_Sim))
ADAS_L2_Hoff_t <- transpose(ADAS_L2_Hoff)
colnames(ADAS_L2_Hoff_t) <- rownames(ADAS_L2_Hoff)

L2_Hoff_CC <- ADAS_L2_Hoff_t %>%
  select(starts_with("TempomatAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-off", ADAS = "CC", .before = "KeinSystem")
L2_Hoff_ACC <- ADAS_L2_Hoff_t %>%
  select(starts_with("ACCAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-off", ADAS = "ACC", .before = "KeinSystem")
L2_Hoff_LKA <- ADAS_L2_Hoff_t %>%
  select(starts_with("SpurhalteAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-off", ADAS = "LKA", .before = "KeinSystem")
L2_Hoff_TJA <- ADAS_L2_Hoff_t %>%
  select(starts_with("StauassAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-off", ADAS = "TJA", .before = "KeinSystem")
L2_Hoff_PA <- ADAS_L2_Hoff_t %>%
  select(starts_with("ParkAssistAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-off", ADAS = "PA", .before = "KeinSystem")
L2_Hoff_L2 <- ADAS_L2_Hoff_t %>%
  select(starts_with("AutomationAutomarke")) %>%
  setNames(., list_man) %>%
  add_column(group = "L2 H-off", ADAS = "L2", .before = "KeinSystem")

L2_Hoff_man_ADAS <- bind_rows(L2_Hoff_CC, L2_Hoff_ACC, L2_Hoff_LKA, L2_Hoff_TJA, L2_Hoff_PA, L2_Hoff_L2)

## combine three groups
man_ADAS <- bind_rows(L0_man_ADAS, L2_Hon_man_ADAS, L2_Hoff_man_ADAS)

## replace column other
man_ADAS_other <- vorbefragung %>% select(VPNr, group, ends_with(".other."))

man_ADAS_other <- data.frame (group = c("L0", "L0", "L0", "L0", "L0", "L0", 
                                        "L2 H-off", "L2 H-off", "L2 H-off", "L2 H-off", "L2 H-off", "L2 H-off",
                                        "L2 H-on", "L2 H-on", "L2 H-on", "L2 H-on", "L2 H-on", "L2 H-on"),
                              ADAS = c("CC", "ACC", "LKA", "TJA", "PA", "L2"),
                              Mini =      c(1, 1, 0, 0, 1, 1, 
                                            1, 0, 0, 0, 0, 0,
                                            1, 1, 1, 0, 0, 0),
                              Porsche =   c(0, 0, 0, 0, 0, 0, 
                                            0, 0, 0, 0, 0, 0,
                                            1, 1, 1, 1, 0, 1),
                              Honda =     c(1, 0, 0, 0, 0, 0, 
                                            0, 0, 0, 0, 0, 0,
                                            0, 0, 0, 0, 0, 0))

man_ADAS_complete <- left_join(man_ADAS, man_ADAS_other, by = c("group", "ADAS")) %>%
  mutate(Citroen = ifelse(group == "L0" & ADAS == "LKA", Citroen+1, Citroen)) %>%
  select(-c("other")) %>%
  replace(is.na(.), 0)

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_Sim", "sd_Sim", "median_Sim", "min_Sim", "max_Sim", "Sim_skim", 
                        "notemptycount_Sim","man_ADAS_complete")))

#### frequency tables of nominal and ordinal data: vor- and nachbefragung ####
# functions for frequency tables
count0_Sim <- function(x) {  (sum(x==0, na.rm = TRUE))}
count1_Sim <- function(x) {  (sum(x==1, na.rm = TRUE))}
count2_Sim <- function(x) {  (sum(x==2, na.rm = TRUE))}
count3_Sim <- function(x) {  (sum(x==3, na.rm = TRUE))}
count4_Sim <- function(x) {  (sum(x==4, na.rm = TRUE))}
count5_Sim <- function(x) {  (sum(x==5, na.rm = TRUE))}
count6_Sim <- function(x) {  (sum(x==6, na.rm = TRUE))}

### vorbefragung ###
## preparation
vor_variables = c("Geschlecht", "Haendigkeit", "Sehschwaeche", "Farbfehlsichtigkeit", "Hoerschwaeche", 
                  "Fahrtfrequenz", "Fahrtstrecke", "FrequenzAutobahn", "StreckeAutobahn", 
                  "KenntnisAS.CC.", "KenntnisAS.ACC.", "KenntnisAS.Spurhalte.",
                  "KenntnisAS.Stauassistent.", "KenntnisAS.ParkAssist.", "KenntnisAS.Teilautomation.")

## L0
vor_L0 <- vorbefragung %>%
  filter(group == "L0") %>%
  select(all_of(vor_variables))

vor_L00 <- as.data.frame(sapply(vor_L0, count0_Sim))
vor_L01 <- as.data.frame(sapply(vor_L0, count1_Sim))
vor_L02 <- as.data.frame(sapply(vor_L0, count2_Sim))
vor_L03 <- as.data.frame(sapply(vor_L0, count3_Sim))
vor_L04 <- as.data.frame(sapply(vor_L0, count4_Sim))
vor_L05 <- as.data.frame(sapply(vor_L0, count5_Sim))
vor_L0n <- as.data.frame(sapply(vor_L0, notemptycount_Sim))

vor_freq_L0 <- data.frame(vor_L0n, vor_L00, vor_L01, vor_L02, vor_L03, vor_L04, vor_L05) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(group = "L0", variable = vor_variables, .before = "n")

## L2 H-on
vor_Hon <- vorbefragung %>%
  filter(group == "L2 H-on") %>%
  select(all_of(vor_variables))

vor_Hon0 <- as.data.frame(sapply(vor_Hon, count0_Sim))
vor_Hon1 <- as.data.frame(sapply(vor_Hon, count1_Sim))
vor_Hon2 <- as.data.frame(sapply(vor_Hon, count2_Sim))
vor_Hon3 <- as.data.frame(sapply(vor_Hon, count3_Sim))
vor_Hon4 <- as.data.frame(sapply(vor_Hon, count4_Sim))
vor_Hon5 <- as.data.frame(sapply(vor_Hon, count5_Sim))
vor_Honn <- as.data.frame(sapply(vor_Hon, notemptycount_Sim))

vor_freq_Hon <- data.frame(vor_Honn, vor_Hon0, vor_Hon1, vor_Hon2, vor_Hon3, vor_Hon4, vor_Hon5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(group = "L2 H-on", variable = vor_variables, .before = "n")

## L2 H-off
vor_Hoff <- vorbefragung %>%
  filter(group == "L2 H-off") %>%
  select(all_of(vor_variables))

vor_Hoff0 <- as.data.frame(sapply(vor_Hoff, count0_Sim))
vor_Hoff1 <- as.data.frame(sapply(vor_Hoff, count1_Sim))
vor_Hoff2 <- as.data.frame(sapply(vor_Hoff, count2_Sim))
vor_Hoff3 <- as.data.frame(sapply(vor_Hoff, count3_Sim))
vor_Hoff4 <- as.data.frame(sapply(vor_Hoff, count4_Sim))
vor_Hoff5 <- as.data.frame(sapply(vor_Hoff, count5_Sim))
vor_Hoffn <- as.data.frame(sapply(vor_Hoff, notemptycount_Sim))

vor_freq_Hoff <- data.frame(vor_Hoffn, vor_Hoff0, vor_Hoff1, vor_Hoff2, vor_Hoff3, vor_Hoff4, vor_Hoff5) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5")) %>%
  add_column(group = "L2 H-off", variable = vor_variables, .before = "n")

## combine L0, L2 H-on & L2 H-off
vor_freq <- bind_rows(vor_freq_L0, vor_freq_Hon, vor_freq_Hoff)
rownames(vor_freq) <- c()

### nachbefragung ###
## preparation
nach_variables = c("NDRTs.NDRT1.", "NDRTs.NDRT2.", "NDRTs.NDRT3.", "NDRTs.NDRT4.", 
                   "NDRTs.NDRT5.", "NDRTs.NDRT6.", "NDRTs.NDRT7.", "NDRTs.NDRT8.",
                   "SubjUeberwachguete.1.", "SubjEinflussSetting",
                   "Warnmeldungen.Warnungenzuhaeufig.", "Warnmeldungen.SichermitWarnsystem.", 
                   "Warnmeldungen.FahrfremdeohneWarnun.", "Warnmeldungen.Laestig.",
                   "ReaktionaufWarnung.WarumertoentWarnung.", "ReaktionaufWarnung.RichtigeReaktion.",
                   "ReaktionaufWarnung.BewusstIgnoriert.", "ReaktionaufWarnung.AufmerksamkeitaufFah.",
                   "L2PrivNutzung", "L2Komponenten.Laengs.",
                   "L2Komponenten.Quer.", "L2Komponenten.Hoff.",
                   "Role_u_01", "Role_u_02", "Role_u_03", "Role_u_04",
                   "Role_u_08", "Role_u_09", "Role_u_10", "Role_u_11",
                   "System_u_01", "System_u_02", "System_u_03", "System_u_04",
                   "System_u_05", "System_u_06", "System_u_07", "System_u_08",
                   "System_u_09", "System_u_16", "System_u_17")

## L0
nach_L0 <- nachbefragung %>%
  filter(group == "L0") %>%
  select(all_of(nach_variables))

nach_L00 <- as.data.frame(sapply(nach_L0, count0_Sim))
nach_L01 <- as.data.frame(sapply(nach_L0, count1_Sim))
nach_L02 <- as.data.frame(sapply(nach_L0, count2_Sim))
nach_L03 <- as.data.frame(sapply(nach_L0, count3_Sim))
nach_L04 <- as.data.frame(sapply(nach_L0, count4_Sim))
nach_L05 <- as.data.frame(sapply(nach_L0, count5_Sim))
nach_L06 <- as.data.frame(sapply(nach_L0, count6_Sim))
nach_L0n <- as.data.frame(sapply(nach_L0, notemptycount_Sim))

nach_freq_L0 <- data.frame(nach_L0n, nach_L00, nach_L01, 
                              nach_L02, nach_L03, nach_L04, nach_L05, 
                              nach_L06) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "L0", variable = nach_variables, .before = "n")

## L2 H-on
nach_Hon <- nachbefragung %>%
  filter(group == "L2 H-on") %>%
  select(all_of(nach_variables))

nach_Hon0 <- as.data.frame(sapply(nach_Hon, count0_Sim))
nach_Hon1 <- as.data.frame(sapply(nach_Hon, count1_Sim))
nach_Hon2 <- as.data.frame(sapply(nach_Hon, count2_Sim))
nach_Hon3 <- as.data.frame(sapply(nach_Hon, count3_Sim))
nach_Hon4 <- as.data.frame(sapply(nach_Hon, count4_Sim))
nach_Hon5 <- as.data.frame(sapply(nach_Hon, count5_Sim))
nach_Hon6 <- as.data.frame(sapply(nach_Hon, count6_Sim))
nach_Honn <- as.data.frame(sapply(nach_Hon, notemptycount_Sim))

nach_freq_Hon <- data.frame(nach_Honn, nach_Hon0, nach_Hon1, 
                               nach_Hon2, nach_Hon3, nach_Hon4, nach_Hon5, 
                               nach_Hon6) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "L2 H-on", variable = nach_variables, .before = "n")

## L2 H-off
nach_Hoff <- nachbefragung %>%
  filter(group == "L2 H-off") %>%
  select(all_of(nach_variables))

nach_Hoff0 <- as.data.frame(sapply(nach_Hoff, count0_Sim))
nach_Hoff1 <- as.data.frame(sapply(nach_Hoff, count1_Sim))
nach_Hoff2 <- as.data.frame(sapply(nach_Hoff, count2_Sim))
nach_Hoff3 <- as.data.frame(sapply(nach_Hoff, count3_Sim))
nach_Hoff4 <- as.data.frame(sapply(nach_Hoff, count4_Sim))
nach_Hoff5 <- as.data.frame(sapply(nach_Hoff, count5_Sim))
nach_Hoff6 <- as.data.frame(sapply(nach_Hoff, count6_Sim))
nach_Hoffn <- as.data.frame(sapply(nach_Hoff, notemptycount_Sim))

nach_freq_Hoff <- data.frame(nach_Hoffn, nach_Hoff0, nach_Hoff1, 
                             nach_Hoff2, nach_Hoff3, nach_Hoff4, nach_Hoff5, 
                             nach_Hoff6) %>%
  setNames(., c("n", "0", "1", "2", "3", "4", "5", "6")) %>%
  add_column(group = "L2 H-off", variable = nach_variables, .before = "n")


## combine all 3 groups
nach_freq <- bind_rows(nach_freq_L0, nach_freq_Hon, nach_freq_Hoff)
rownames(nach_freq) <- c()

#### remove not needed data ####
rm(list=setdiff(ls(), c("vorbefragung", "nachbefragung", "summ_vorbefragung", "summ_nachbefragung",
                        "mean_Sim", "sd_Sim", "median_Sim", "min_Sim", "max_Sim", "Sim_skim", 
                        "man_ADAS_complete", 
                        "vor_freq", "nach_freq")))
#### save results ####
write_excel_csv(summ_vorbefragung, "data/results/summary_vorbefragung.csv")
write_excel_csv(summ_nachbefragung, "data/results/summary_nachbefragung.csv")
write_excel_csv(man_ADAS_complete, "data/results/vorbefragung_manufacturerADAS.csv")
write_excel_csv(vor_freq, "data/results/frequencies_vorbefragung.csv")
write_excel_csv(nach_freq, "data/results/frequencies_nachbefragung.csv")