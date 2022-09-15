#### note on data####
  # qualitative data
    # NDRT free text fields: "MatrixNDRTsFreitext.0."- "MatrixNDRTsFreitext.5." --- because of coding mistake in LimeSurvey only L2 groups
    # Kommentare zu Warnmeldungn: "OptionalAnmerkungenW" --- only L2 groups
    # "OptionalAnderesVerha" 
    # "KommentareStudie"
    # "KommentareFahrt" --- only L2 groups
    # "KommentareFahrtManu" --- only L0 group
    # almost all interview data (protocols, comments etc.)

#### preparations ####
rm(list = ls())
library(tidyverse)
setwd("~/R/VDA_Sim")

#### import data ####
# Read in files
raw_vor <- read.csv("data/raw/01_Sim_Rohdaten_LS_Vorbefragung.csv", encoding = "UTF-8")
raw_nach <- read.csv("data/raw/02_Sim_Rohdaten_LS_Nachbefragung.csv", encoding = "UTF-8")
raw_interview <- read.csv("data/raw/03_Sim_Rohdaten_LS_Interview.csv", encoding = "UTF-8")
VPCodes <- read.csv("data/other/VPCodes.csv", encoding = "UTF-8", sep = ';') # needed to add VP numbers to vorbefragung and h-on
VPCodes <- VPCodes %>%
  rename(VPNr = X.U.FEFF.VP.Nr.)

#### vorbefragung ####
# delete and mutate
vor_1 <- raw_vor %>%
  subset(., lastpage == 3) %>% # exclude incomplete answers
  subset(., submitdate != "2022-07-06 21:39:53" & seed != 634894935) %>% # VP55 has filled out the survey twice, second trial will be kept
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, startdate, datestamp, 
            starts_with("Kontaktdaten"))) %>%
  unite("VPCode_vor", VPCode.VornameMutter.: VPCode.TagGeburtstag., sep = "") %>%
  mutate(VPCode_vor = ifelse(VPCode_vor == "IAAd 29", "IAAd29", VPCode_vor)) %>% # different in VPCodes.csv
  mutate(VPCode_vor = ifelse(VPCode_vor == "EAPR03.03.2000", "EAPR03.03", VPCode_vor)) %>% # different in VPCodes.csv
  left_join(VPCodes, ., by = "VPCode_vor") %>%
  subset(., Ausschluss != "ja") %>% # exclude participants marked as to be excluded
  select(-c(Ausschluss, VPCode_vor, VPCode_nach)) %>% # variables are not needed anymore
  mutate(group = ifelse(group == "Manuell", "L0",
                        ifelse(group == "HandsOn", "L2 H-on",
                               ifelse(group == "HandsOff", "L2 H-off", 999)))) %>%
  relocate(VPNr, .after = group)

# rename variables and recode replies
vor_2 <- vor_1 %>%
  mutate(Geschlecht = ifelse(Geschlecht =="Maenn", 0, 
                             ifelse(Geschlecht == "Weibl", 1, 999))) %>%
  mutate(Haendigkeit = ifelse(Hand.Rechts. == "Y", 0, 
                              ifelse(Hand.Links. == "Y", 1, 
                                     ifelse(Hand.Egal. == "Y", 2, 999))), .before = Hand.Rechts.) %>%
  mutate(Sehschwaeche = ifelse(Sehhilfe.Nein. == "Y", 0, 
                              ifelse(Sehhilfe.JaVersuchJa. == "Y", 1, 
                                     ifelse(Sehhilfe.JaVersuchNein. == "Y", 2, 999))), .before = Hand.Rechts.) %>%
  mutate(Farbfehlsichtigkeit = ifelse(Farbfehler.Nein. == "Y", 0, 
                                      ifelse(Farbfehler.Rotgruen. == "Y", 1, 
                                             ifelse(Farbfehler.Blaugelb. == "Y", 2, 999))), .before = Hand.Rechts.) %>%
  mutate(Hoerschwaeche = ifelse(Hoerschwaeche.Nein. == "Y", 0, 
                               ifelse(Hoerschwaeche.JaVersuchJa. == "Y", 1, 
                                      ifelse(Hoerschwaeche.JaVersuchNein. == "Y", 2, 999))), .before = Hand.Rechts.) %>%
  mutate(JahrFuerherschein = ifelse(JahrFuerherschein == "20 Jahren ", "20", 
                                    ifelse(JahrFuerherschein == "5 Jahre ", "5" , JahrFuerherschein)), .before = JahrFuerherschein) %>%
  mutate(Fuehrerschein = 2022 - as.integer(JahrFuerherschein), .before = JahrFuerherschein) %>%
  select(-c(starts_with("Hand."), starts_with("Sehhilfe"), starts_with("Farbfehler"), starts_with("Hoerschwaeche."), JahrFuerherschein)) 

vor_3 <- vor_2 %>%
  mutate(Fahrtfrequenz = ifelse(AnzahlAutofahren.Selten. == "Y", 0, 
                                      ifelse(AnzahlAutofahren.WenigMonat. == "Y", 1, 
                                             ifelse(AnzahlAutofahren.MehrmalsMonat. == "Y", 2, 
                                                    ifelse(AnzahlAutofahren.MehrmalsWoche. == "Y", 3,
                                                           ifelse(AnzahlAutofahren.Taeglich. == "Y", 4, 
                                                                  999))))), .before = AnzahlAutofahren.Selten.) %>%
  mutate(Fahrtstrecke = ifelse(AnzahlKilometer.0. == "Y", 0, 
                                ifelse(AnzahlKilometer.5000. == "Y", 1, 
                                       ifelse(AnzahlKilometer.20000. == "Y", 2, 
                                              ifelse(AnzahlKilometer.50000. == "Y", 3,
                                                     ifelse(AnzahlKilometer.100000. == "Y", 4, 
                                                            ifelse(AnzahlKilometer.mehr100000. == "Y", 5,
                                                                   999)))))), .before = AnzahlAutofahren.Selten.) %>% 
  mutate(FrequenzAutobahn = ifelse(AnzahlAutobahnen.Selten. == "Y", 0, 
                                ifelse(AnzahlAutobahnen.WenigMonat. == "Y", 1, 
                                       ifelse(AnzahlAutobahnen.MehrmalsMonat. == "Y", 2, 
                                              ifelse(AnzahlAutobahnen.MerhmalsWoche. == "Y", 3,
                                                     ifelse(AnzahlAutobahnen.Taeglich. == "Y", 4, 
                                                            999))))), .before = AnzahlAutofahren.Selten.) %>%
  mutate(StreckeAutobahn = ifelse(AnzahlKilometerAutob.0. == "Y", 0, 
                               ifelse(AnzahlKilometerAutob.5000. == "Y", 1, 
                                      ifelse(AnzahlKilometerAutob.20000. == "Y", 2, 
                                             ifelse(AnzahlKilometerAutob.50000. == "Y", 3,
                                                    ifelse(AnzahlKilometerAutob.100000. == "Y", 4, 
                                                           ifelse(AnzahlKilometerAutob.mehr100000. == "Y", 5,
                                                                  999)))))), .before = AnzahlAutofahren.Selten.) %>% 
  select(-c(starts_with("Anzahl")))

vor_4 <- vor_3 %>%
  mutate(across(starts_with("Erfahrungen."), ~case_when(
    . == "AO01" ~"0", 
    . == "AO02" ~"1", 
    . == "AO03" ~"2", 
    . == "AO04" ~"3", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("Erfahrungen."), as.numeric)) %>% # rename so that the same names are used as in FOT
  rename(KenntnisAS.CC. = Erfahrungen.Tempomat.,
         KenntnisAS.ACC. = Erfahrungen.ACC.,
         KenntnisAS.Spurhalte. = Erfahrungen.Spurhalteass.,
         KenntnisAS.Stauassistent. = Erfahrungen.Stauassistent.,
         KenntnisAS.ParkAssist. = Erfahrungen.ParkAssist.,
         KenntnisAS.Teilautomation. = Erfahrungen.Teilautomation.)

vor_5 <- vor_4 %>% # rename so that the same names are used as in FOT
  rename(Fahrstil.DSQ1. = Fahrverhalten.TempoAutobahn.,
         Fahrstil.DSQ2. = Fahrverhalten.Schnell.,
         Fahrstil.DSQ3. = Fahrverhalten.TempoOrt.,
         Fahrstil.DSQ4. = Fahrverhalten.Aufregung.,
         Fahrstil.DSQ5. = Fahrverhalten.Ruhig.,
         Fahrstil.DSQ6. = Fahrverhalten.DruckTeilnehmer.,
         Fahrstil.DSQ7. = Fahrverhalten.Ratschlag.,
         Fahrstil.DSQ8. = Fahrverhalten.AblehnungRatschlag.,
         Fahrstil.DSQ9. = Fahrverhalten.Vorsichtig.,
         Fahrstil.DSQ10. = Fahrverhalten.Ablenkungen.,
         Fahrstil.DSQ11. = Fahrverhalten.Beifahrer.,
         Fahrstil.DSQ12. = Fahrverhalten.OhneKarte.,
         Fahrstil.DSQ13. = Fahrverhalten.Planung.,
         Fahrstil.DSQ14. = Fahrverhalten.LinkeSpur.,
         Fahrstil.DSQ15. = Fahrverhalten.AmpelRot.) %>%
  rename(Technikaffinitaet.ATIS1. = TechnischeSysteme.Beschaeftigung.,
         Technikaffinitaet.ATIS2. = TechnischeSysteme.Funktionen.,
         Technikaffinitaet.ATIS3. = TechnischeSysteme.Muss.,
         Technikaffinitaet.ATIS4. = TechnischeSysteme.IntensivAusprobieren.,
         Technikaffinitaet.ATIS5. = TechnischeSysteme.ZeitKennenlernen.,
         Technikaffinitaet.ATIS6. = TechnischeSysteme.Genug.,
         Technikaffinitaet.ATIS7. = TechnischeSysteme.Verstaendnis.,
         Technikaffinitaet.ATIS8. = TechnischeSysteme.Grundfunktionen.,
         Technikaffinitaet.ATIS9. = TechnischeSysteme.Moeglichkeit.) %>%
  mutate(across(starts_with("Fahrstil.DSQ"), ~case_when(
    . == "Nie" ~"1", 
    . == "Selte" ~"2", 
    . == "ReSel" ~"3", 
    . == "ReHae" ~"4", 
    . == "Haefi" ~"5", 
    . == "Immer" ~"6", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("Fahrstil.DSQ"), as.numeric)) %>%
  mutate(across(starts_with("Technikaffinitaet.ATIS"), ~case_when(
    . == "Garni" ~"1", 
    . == "Weitn" ~"2", 
    . == "Ehern" ~"3", 
    . == "Eher" ~"4", 
    . == "Weitg" ~"5", 
    . == "Voell" ~"6", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("Technikaffinitaet.ATIS"), as.numeric)) %>%
  mutate(Technikaffinitaet.ATIS3. = 7 - Technikaffinitaet.ATIS3.) %>% # inverse item
  mutate(Technikaffinitaet.ATIS6. = 7 - Technikaffinitaet.ATIS6.) %>% # inverse item
  mutate(Technikaffinitaet.ATIS8. = 7 - Technikaffinitaet.ATIS8.) # inverse item

#### score calculation vorbefragung ####
# DSQ:
  # values: raw scores are 1-6
  # Scoring:
    #   Speed=Q1+Q2+Q3.
    #   Calmness=14-Q4+Q5-Q6
    #   Social resistance=7-Q7+Q8
    #   Focus=Q9+Q10+Q11
    #   Planning=7-Q12+Q13
    #   Deviance=Q14+Q15
# ATI-S_
  # values: raw scores are 1-6
  # Scoring: 
    # SUM[ATI-S1:ATI-S9]/9

vorbefragung_scores <- vor_5 %>%
  add_column(DSQ_Speed = .$Fahrstil.DSQ1. + .$Fahrstil.DSQ2. + .$Fahrstil.DSQ3., .before = "TempomatAutomarke.KeinSystem.") %>% # 
  add_column(DSQ_Calmness = 14 - .$Fahrstil.DSQ4. + .$Fahrstil.DSQ5. - .$Fahrstil.DSQ6., .before = "TempomatAutomarke.KeinSystem.") %>%
  add_column(DSQ_SocialResistance = 7- .$Fahrstil.DSQ7. + .$Fahrstil.DSQ8., .before = "TempomatAutomarke.KeinSystem.") %>%
  add_column(DSQ_Focus = .$Fahrstil.DSQ9. + .$Fahrstil.DSQ10. + .$Fahrstil.DSQ11., .before = "TempomatAutomarke.KeinSystem.") %>%
  add_column(DSQ_Planning = 7- .$Fahrstil.DSQ12. + .$Fahrstil.DSQ13., .before = "TempomatAutomarke.KeinSystem.") %>%
  add_column(DSQ_Deviance = .$Fahrstil.DSQ14. + .$Fahrstil.DSQ15., .before = "TempomatAutomarke.KeinSystem.") %>%
  rowwise() %>% 
  mutate(ATIS = mean(c_across(Technikaffinitaet.ATIS1.:Technikaffinitaet.ATIS9.), na.rm = TRUE), .before = "TempomatAutomarke.KeinSystem.")

#### remove not needed data ####
rm(list=setdiff(ls(), c("raw_interview", "raw_nach", "raw_vor", "VPCodes",
  "vorbefragung_scores", "nachbefragung_scores")))

#### nachbefragung ####
nach_1 <- raw_nach %>%
  subset(., lastpage == 9) %>% # exclude incomplete answers
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, startdate, datestamp)) %>%
  unite("VPCode_nach", Probandencode.Vorname.: Probandencode.TagGeburtstag., sep = "") %>%
  left_join(VPCodes, ., by = "VPCode_nach") %>%
  subset(., Ausschluss != "ja") %>% # exclude participants marked as to be excluded
  mutate(group = ifelse(group == "Manuell", "L0",
                        ifelse(group == "HandsOn", "L2 H-on",
                               ifelse(group == "HandsOff", "L2 H-off", 999)))) %>%
  select(-c(Ausschluss, VPCode_vor, VPCode_nach, VPNummer, Fahrt.Manuell., Fahrt.HandsOn., Fahrt.HandsOff.)) # variables are not needed anymore

nach_2 <- nach_1 %>%
  rename(Vertrauen.TiA1. = VertrauenNachbefragu.Einschaetzen.,
         Vertrauen.TiA2. = VertrauenNachbefragu.ZustandSystem.,
         Vertrauen.TiA3. = VertrauenNachbefragu.KenneSysteme.,
         Vertrauen.TiA4. = VertrauenNachbefragu.Vetrauenswuerdig.,
         Vertrauen.TiA5. = VertrauenNachbefragu.Unbekanntvorsichtig.,
         Vertrauen.TiA6. = VertrauenNachbefragu.Zuverlaessig.,
         Vertrauen.TiA7. = VertrauenNachbefragu.Unvorhersehbar.,
         Vertrauen.TiA8. = VertrauenNachbefragu.Wohlergehen.,
         Vertrauen.TiA9. = VertrauenNachbefragu.Vertrauen.,
         Vertrauen.TiA10. = VertrauenNachbefragu.Ausfall.,
         Vertrauen.TiA11. = VertrauenNachbefragu.Nachvollziehen.,
         Vertrauen.TiA12. = VertrauenNachbefragu.Systemvertrauen.,
         Vertrauen.TiA13. = VertrauenNachbefragu.komplizierteAufgaben.,
         Vertrauen.TiA14. = VertrauenNachbefragu.VerlassSystem.,
         Vertrauen.TiA15. = VertrauenNachbefragu.FehlerSystem.,
         Vertrauen.TiA16. = VertrauenNachbefragu.VorhersageSystem.,
         Vertrauen.TiA17. = VertrauenNachbefragu.NutzungSystem.,
         Vertrauen.TiA18. = VertrauenNachbefragu.AutomatisierteSystem.,
         Vertrauen.TiA19. = VertrauenNachbefragu.UeberzeugungSystem.) %>%
  mutate(across(starts_with("Vertrauen.TiA"), ~case_when(
    . == "Garni" ~"1", 
    . == "Ehern" ~"2", 
    . == "Weder" ~"3", 
    . == "Eherz" ~"4", 
    . == "Vollz" ~"5", 
    . == "KA" ~"1000", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("Vertrauen.TiA"), as.numeric)) %>%
  mutate(across(starts_with("Vertrauen.TiA"), ~ifelse(. == 1000, NA, .))) %>%
  mutate(Vertrauen.TiA5. = 6 - Vertrauen.TiA5.) %>% # inverse item
  mutate(Vertrauen.TiA7. = 6 - Vertrauen.TiA7.) %>% # inverse item
  mutate(Vertrauen.TiA10. = 6 - Vertrauen.TiA10.) %>% # inverse item
  mutate(Vertrauen.TiA15. = 6 - Vertrauen.TiA15.) %>% # inverse item
  mutate(Vertrauen.TiA16. = 6 - Vertrauen.TiA16.) # inverse item

nach_3 <- nach_2 %>%
  rename(Akzeptanz.PE1. = Akzeptanz.NuetzlichFahren.,
         Akzeptanz.PE3. = Akzeptanz.Fahrleistung.,
         Akzeptanz.PE4. = Akzeptanz.ZielSicher.,
         Akzeptanz.EE1. = Akzeptanz.InteraktionSystem.,
         Akzeptanz.EE2. = Akzeptanz.LeichtesUmgang.,
         Akzeptanz.EE3. = Akzeptanz.EinfacheBedienung.,
         Akzeptanz.EE4. = Akzeptanz.ErlernenBedienung.,
         Akzeptanz.ATT1. = Akzeptanz.GuteIdee.,
         Akzeptanz.ATT2. = Akzeptanz.InteressanterFahren.,
         Akzeptanz.ATT3. = Akzeptanz.SpassSystem.,
         Akzeptanz.ATT4. = Akzeptanz.MagInteraktionSystem.,
         Akzeptanz.FC1. = Akzeptanz.SicheresFahrverhalte.,
         Akzeptanz.FC2. = Akzeptanz.NoetigeWissen.,
         Akzeptanz.FC3. = Akzeptanz.KompatibelSystem.,
         Akzeptanz.ITU1. = Akzeptanz.BeabsichtigungNutzun.,
         Akzeptanz.ITU2. = Akzeptanz.NutzungSystem.,
         Akzeptanz.ITU3. = Akzeptanz.NutzungMonate.,
         Akzeptanz.PS1. = Akzeptanz.GefaehrlichSystem.,
         Akzeptanz.PS2. = Akzeptanz.ErhoehteAufmerksamke.,
         Akzeptanz.PS3. = Akzeptanz.AblenkenVerkehr.,
         Akzeptanz.PS4. = Akzeptanz.Sicherfuehlen.,
         Akzeptanz.PS5. = Akzeptanz.VerringerungUnfall.,
         Akzeptanz.PS6. = Akzeptanz.Nichthinschauen.) %>%
  mutate(across(starts_with("Akzeptanz."), ~case_when(
    . == "Nich0" ~"1", 
    . == "Nich1" ~"2", 
    . == "Nich2" ~"3", 
    . == "Neutr" ~"4", 
    . == "Voll2" ~"5", 
    . == "Voll1" ~"6", 
    . == "Voll0" ~"7",
    TRUE ~ .))) %>%
  mutate(across(starts_with("Akzeptanz."), as.numeric)) %>%
  mutate(Akzeptanz.PS1. = 8 - Akzeptanz.PS1.) %>% # inverse item
  mutate(Akzeptanz.PS2. = 8 - Akzeptanz.PS2.) %>% # inverse item
  mutate(Akzeptanz.PS3. = 8 - Akzeptanz.PS3.) # inverse item

nach_4 <- nach_3 %>%
  rename(Systemverstaendnis.1. = Systemverstaendnis.EineHandamSteuer.,
         Systemverstaendnis.2. = Systemverstaendnis.Uebersteuern.,
         Systemverstaendnis.3. = Systemverstaendnis.StetsUeberwachen., # andere Formulierung im FOT, Sinn gleich
         Systemverstaendnis.4. = Systemverstaendnis.SystemFahrsicherheit.,
         Systemverstaendnis.5. = Systemverstaendnis.FahrfremdeTaetigkeit.,
         Systemverstaendnis.6. = Systemverstaendnis.Bewusstaktivieren.,
         Systemverstaendnis.7. = Systemverstaendnis.GeschwVorfahrzeug.,
         Systemverstaendnis.8. = Systemverstaendnis.AusgleichenFahruntau.,
         Systemverstaendnis.9. = Systemverstaendnis.AnderePersonFahraufg.,
         Systemverstaendnis.10. = Systemverstaendnis.KeineanderenFahrzeug.,
         Systemverstaendnis.11. = Systemverstaendnis.Systemgrenze.,
         Systemverstaendnis.12. = Systemverstaendnis.Lenktautomatisch.,
         Systemverstaendnis.13. = Systemverstaendnis.UebernahmeFahrsituat., # andere Formulierung im FOT, Sinn gleich
         Systemverstaendnis.14. = Systemverstaendnis.WachbleibenAutomatio.,
         Systemverstaendnis.15. = Systemverstaendnis.NiemlasUnaufgeforder.,
         Systemverstaendnis.16. = Systemverstaendnis.Schlagloecher.,
         
         Systemverstaendnis.17. = Systemverstaendnis.MindestabstandVoraus.,
         Systemverstaendnis.18. = Systemverstaendnis.EingestellteGeschwin.,
         Systemverstaendnis.19. = Systemverstaendnis.FahrstreifenwechselF.) %>%
  mutate(across(starts_with("Systemverstaendnis."), ~case_when(
    . == "Nzutr" ~"0", 
    . == "Zutre" ~"1", 
    . == "Unsic" ~"2", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("Systemverstaendnis."), as.numeric))

nach_5 <- nach_4 %>% # combine answers of L2 groups and L0 group in a new column. L0 couldn't give optional answers, there was a coding mistake in the survey
  mutate(NDRTs.NDRT1. = ifelse(group != "L0", SystemeigenesFahrzeu.Nachrichtenlesen., PrivatesFahrzeug.NachrichtenHandy.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT2. = ifelse(group != "L0", SystemeigenesFahrzeu.HandTelefonieren., PrivatesFahrzeug.TelefonierenHandy.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT3. = ifelse(group != "L0", SystemeigenesFahrzeu.FestFreisprechanlage., PrivatesFahrzeug.Freisprechanlage.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT4. = ifelse(group != "L0", SystemeigenesFahrzeu.EinstellenKlimaanlag., PrivatesFahrzeug.Infotainment.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT5. = ifelse(group != "L0", SystemeigenesFahrzeu.Essen., PrivatesFahrzeug.Essen.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT6. = ifelse(group != "L0", SystemeigenesFahrzeu.MakeUp., PrivatesFahrzeug.MakeUp.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT7. = ifelse(group != "L0", SystemeigenesFahrzeu.InteraktionBeifahrer., PrivatesFahrzeug.Beifahrer.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(NDRTs.NDRT8. = ifelse(group != "L0", SystemeigenesFahrzeu.SuchenTasche., PrivatesFahrzeug.ObjekteSuchen.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  
  mutate(MatrixNDRTsFreitext.0. = ifelse(group != "L0", OptionalNDRT.Nie., OptionalNDRTManuell.Nie.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(MatrixNDRTsFreitext.1. = ifelse(group != "L0", OptionalNDRT.Sehrs., OptionalNDRTManuell.SehrSelten.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(MatrixNDRTsFreitext.2. = ifelse(group != "L0", OptionalNDRT.Selten., OptionalNDRTManuell.Selten.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(MatrixNDRTsFreitext.3. = ifelse(group != "L0", OptionalNDRT.Geleg., OptionalNDRTManuell.Gelegentlich.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(MatrixNDRTsFreitext.4. = ifelse(group != "L0", OptionalNDRT.Oft., OptionalNDRTManuell.Oft.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(MatrixNDRTsFreitext.5. = ifelse(group != "L0", OptionalNDRT.SeOft., OptionalNDRTManuell.SehrOft.), .before = "SystemeigenesFahrzeu.Nachrichtenlesen.") %>%
  mutate(across(starts_with("NDRTs.NDRT"), ~case_when(
    . == "Nie" ~"0", 
    . == "Sehrs" ~"1",  # L2 reply
    . == "SehrS" ~ "1", # L0 reply
    . == "Selte" ~"2", 
    . == "Gele" ~"3",   # L2 reply
    . == "Geleg" ~ "3", # L0 reply
    . == "Oft" ~"4", 
    . == "SeOft" ~"5", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("NDRTs.NDRT"), as.numeric)) %>%
  select(-c(starts_with("SystemeigenesFahrzeu."), starts_with("PrivatesFahrzeug."), starts_with("OptionalNDRT")))

nach_6 <- nach_5 %>%
  mutate(across(starts_with("Warnmeldungen."), ~case_when(
    . == "Nich0" ~"0", 
    . == "Nich1" ~"1",
    . == "Nich2" ~ "2",
    . == "Neutr" ~"3", 
    . == "Voll2" ~"4",
    . == "Voll1" ~ "5",
    . == "Voll0" ~"6", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("Warnmeldungen."), as.numeric)) %>%
  mutate(across(starts_with("ReaktionaufWarnung."), ~case_when(
    . == "Nie0" ~"0", 
    . == "Nie1" ~"1",
    . == "Nie2" ~ "2",
    . == "Geleg" ~"3", 
    . == "Imme2" ~"4",
    . == "Imme1" ~ "5",
    . == "Immer" ~"6", 
    TRUE ~ .))) %>%
  mutate(across(starts_with("ReaktionaufWarnung."), as.numeric)) %>%
  mutate(UeberwachungHOffon.UeberwachungSystem. = ifelse(UeberwachungHOffon.UeberwachungSystem. == "Unauf", 0, 
                                        ifelse(UeberwachungHOffon.UeberwachungSystem. == "AO02", 1, 
                                               ifelse(UeberwachungHOffon.UeberwachungSystem. == "AO03", 2, 
                                                      ifelse(UeberwachungHOffon.UeberwachungSystem. == "AO04", 3,
                                                             ifelse(UeberwachungHOffon.UeberwachungSystem. == "AO05", 4, 
                                                                    ifelse(UeberwachungHOffon.UeberwachungSystem. == "AO06", 5,
                                                                           ifelse(UeberwachungHOffon.UeberwachungSystem. == "StAuf", 6,
                                                                                  NA)))))))) %>%
  mutate(SubjUeberwachguete.1. = ifelse(group != "L0", UeberwachungHOffon.UeberwachungSystem., UeberwachungManuell.AufmerksamManuell.), .before = UeberwachungHOffon.UeberwachungSystem.) %>%
  select(-c(UeberwachungHOffon.UeberwachungSystem., UeberwachungManuell.AufmerksamManuell.)) %>%
  rename(SubjEinflussSetting = AnderesVerhalten) %>%
  mutate(SubjEinflussSetting = ifelse(SubjEinflussSetting == "Ja", 0, 1))


#### interview + join with nachbefragung ####
processed_interview <- raw_interview %>%
  subset(., !(startdate == "2022-07-20 10:35:03" & seed == 744596640)) %>% # first interview with #33 was aborted
  select(-c(X.U.FEFF.id, submitdate, lastpage, startlanguage, seed, datestamp, startdate)) %>%
  rename(VPNr = VP) %>%
  select(-c(Fahrt.Manuell., Fahrt.HandsOn., Fahrt.HandsOff.)) %>%
  full_join(VPCodes, ., by = "VPNr") %>%
  subset(., Ausschluss != "ja") %>% # exclude participants marked as to be excluded
  select(-c(Ausschluss, VPCode_vor, VPCode_nach, group)) # variables are not needed anymore

nachbefragung_complete0 <- full_join(nach_6, processed_interview, by = "VPNr")

nachbefragung_complete <- nachbefragung_complete0 %>%
  rename(L2PrivNutzung = BewertungL2,
         L2Komponenten.Laengs. = L2PrivatesFahrzeug.ACC.,
         L2Komponenten.Quer. = L2PrivatesFahrzeug.Spurhaltung.,
         L2Komponenten.Hoff. = L2PrivatesFahrzeug.Haendefrei.) %>%
  mutate(across(starts_with("L2"), ~case_when(
    . == "Nein" ~"0",
    . == "EherN" ~"1",
    . == "Unsic" ~ "2",
    . == "EheJa" ~"3",
    . == "Ja" ~"4",
    . == "KA" ~ "99",
    . == "Manue" ~ "99",
    TRUE ~ .))) %>%
  mutate(across(starts_with("L2"), as.numeric)) %>%
  mutate(across(starts_with("L2"), ~ifelse(. == 99, NA, .)))

#### SU statements ####
SU01_on <- 1 # wrong for h-off
SU01_off <- 0
SU02 <- 1
SU03 <- 1
SU04 <- 0
SU05 <- 0
SU06 <- 1
SU07 <- 1
SU08 <- 0
SU09 <- 0
SU10 <- 0
SU11 <- 0
SU12 <- 1
SU13 <- 1
SU14 <- 1
SU15 <- 0
SU16 <- 1
SU17 <- 1 # not in FOT
SU18 <- 1 # not in FOT
SU19 <- 0 # not in FOT

#### score calculation nachbefragung ---- MISSING ####
  # notes: items for TiA and CTAM are already inverted
  # Trust TiA:
    # values: 
      # raw scores are 1-6
      # raw scores of items 5, 7, 10, 15, 16 need to be inverted
    # scoring:
      # TiA_overall = average all items
      # TiA_Reliability_Competence = sum 1, 6, 10inverse, 13, 15inverse, 19
      # TiA_Understanding-Predictability = sum 2, 7inverse, 11, 16inverse
      # TiA_Familiarity = sum 3, 17
      # TiA_IntentionOfDevelopers = sum 4, 8
      # TiA_PropensityToTrust = sum 5inverse, 12, 18
      # TiA_TrustInAutomation = 9, 14
# Acceptance CTAM:
  # values: raw scores of PS1, PS2, PS3 need to be inverted
  # scoring: 
      # Acceptance_PerformanceExpectancy_Score = sum 1, 3, 4
      # Acceptance_EffortExpectancy_Score = sum 1-4
      # Acceptance_AttitudeTowardsUsingTechnology_Score = sum 1-4
      # Acceptance_FacilitatingConditions_Score	= sum 1-3
      # Acceptance_BehavioralIntentionToUseTheSystem_Score = sum 1-3	
      # Acceptance_PerceivedSafety_Score = sum r1, r2, r3, 4-6
# Systemverstaendnis: 
  # values: subjective answers [0 = "nicht zutreffend", 1 = "zutreffend", 2 = "unsicher"]need to be transferred into "is correct"/"not correct" 
      # --- for this the values in section SU statements are used
      # 2 ("unsicher") are treated as NAs to simplify calculation of correct answers (percentage); there are no missing values except for A_on_fc---27
  # scoring:
      # SystemUnderstanding_score = sum correct answers ("unsicher" treated as wrong)

nachbefragung_scores0 <- nachbefragung_complete %>%
  add_column(System01 = ifelse(.$Systemverstaendnis.1. == 2, 0, # statement is correct for On, wrong for off
                               ifelse(.$Systemverstaendnis.1. == SU01_on & .$group == "L2 H-on", 1, 
                                      ifelse(.$Systemverstaendnis.1. == SU01_off  & .$group == "L2 H-off", 1, 
                                             0))), .before = "Systemverstaendnis.1.") %>%
  add_column(System02 = ifelse(.$Systemverstaendnis.2. == SU02, 1, ifelse(.$Systemverstaendnis.2. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System03 = ifelse(.$Systemverstaendnis.3. == SU03, 1, ifelse(.$Systemverstaendnis.3. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System04 = ifelse(.$Systemverstaendnis.4. == SU04, 1, ifelse(.$Systemverstaendnis.4. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System05 = ifelse(.$Systemverstaendnis.5. == SU05, 1, ifelse(.$Systemverstaendnis.5. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System06 = ifelse(.$Systemverstaendnis.6. == SU06, 1, ifelse(.$Systemverstaendnis.6. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System07 = ifelse(.$Systemverstaendnis.7. == SU07, 1, ifelse(.$Systemverstaendnis.7. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System08 = ifelse(.$Systemverstaendnis.8. == SU08, 1, ifelse(.$Systemverstaendnis.8. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System09 = ifelse(.$Systemverstaendnis.9. == SU09, 1, ifelse(.$Systemverstaendnis.9. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System10 = ifelse(.$Systemverstaendnis.10. == SU10, 1, ifelse(.$Systemverstaendnis.10. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System11 = ifelse(.$Systemverstaendnis.11. == SU11, 1, ifelse(.$Systemverstaendnis.11. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System12 = ifelse(.$Systemverstaendnis.12. == SU12, 1, ifelse(.$Systemverstaendnis.12. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System13 = ifelse(.$Systemverstaendnis.13. == SU13, 1, ifelse(.$Systemverstaendnis.13. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System14 = ifelse(.$Systemverstaendnis.14. == SU14, 1, ifelse(.$Systemverstaendnis.14. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System15 = ifelse(.$Systemverstaendnis.15. == SU15, 1, ifelse(.$Systemverstaendnis.15. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System16 = ifelse(.$Systemverstaendnis.16. == SU16, 1, ifelse(.$Systemverstaendnis.16. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System17 = ifelse(.$Systemverstaendnis.17. == SU17, 1, ifelse(.$Systemverstaendnis.17. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System18 = ifelse(.$Systemverstaendnis.18. == SU18, 1, ifelse(.$Systemverstaendnis.18. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System19 = ifelse(.$Systemverstaendnis.19. == SU19, 1, ifelse(.$Systemverstaendnis.19. == 2, 0, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_01 = ifelse(.$Systemverstaendnis.1. == 2, 2, # statement is correct for On, wrong for off
                                  ifelse(.$Systemverstaendnis.1. == SU01_on & .$group == "L2 H-on", 1, 
                                         ifelse(.$Systemverstaendnis.1. == SU01_off  & .$group == "L2 H-off", 1, 
                                                0))), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_02 = ifelse(.$Systemverstaendnis.2. == SU02, 1, ifelse(.$Systemverstaendnis.2. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_03 = ifelse(.$Systemverstaendnis.3. == SU03, 1, ifelse(.$Systemverstaendnis.3. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_04 = ifelse(.$Systemverstaendnis.4. == SU04, 1, ifelse(.$Systemverstaendnis.4. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_05 = ifelse(.$Systemverstaendnis.5. == SU05, 1, ifelse(.$Systemverstaendnis.5. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_06 = ifelse(.$Systemverstaendnis.6. == SU06, 1, ifelse(.$Systemverstaendnis.6. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_07 = ifelse(.$Systemverstaendnis.7. == SU07, 1, ifelse(.$Systemverstaendnis.7. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_08 = ifelse(.$Systemverstaendnis.8. == SU08, 1, ifelse(.$Systemverstaendnis.8. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_09 = ifelse(.$Systemverstaendnis.9. == SU09, 1, ifelse(.$Systemverstaendnis.9. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_10 = ifelse(.$Systemverstaendnis.10. == SU10, 1, ifelse(.$Systemverstaendnis.10. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_11 = ifelse(.$Systemverstaendnis.11. == SU11, 1, ifelse(.$Systemverstaendnis.11. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_12 = ifelse(.$Systemverstaendnis.12. == SU12, 1, ifelse(.$Systemverstaendnis.12. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_13 = ifelse(.$Systemverstaendnis.13. == SU13, 1, ifelse(.$Systemverstaendnis.13. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_14 = ifelse(.$Systemverstaendnis.14. == SU14, 1, ifelse(.$Systemverstaendnis.14. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_15 = ifelse(.$Systemverstaendnis.15. == SU15, 1, ifelse(.$Systemverstaendnis.15. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_16 = ifelse(.$Systemverstaendnis.16. == SU16, 1, ifelse(.$Systemverstaendnis.16. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_17 = ifelse(.$Systemverstaendnis.17. == SU17, 1, ifelse(.$Systemverstaendnis.17. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_18 = ifelse(.$Systemverstaendnis.18. == SU18, 1, ifelse(.$Systemverstaendnis.18. == 2, 2, 0)), .before = "Systemverstaendnis.1.") %>%
  add_column(System_u_19 = ifelse(.$Systemverstaendnis.19. == SU19, 1, ifelse(.$Systemverstaendnis.19. == 2, 2, 0)), .before = "Systemverstaendnis.1.")

nachbefragung_scores <- nachbefragung_scores0 %>%
  rowwise() %>% 
  mutate(TiA_overall = mean(c_across(Vertrauen.TiA1.:Vertrauen.TiA19.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_RC = mean(c(Vertrauen.TiA1., Vertrauen.TiA6., Vertrauen.TiA10., 
                         Vertrauen.TiA13., Vertrauen.TiA15., Vertrauen.TiA19.),  # item 10 & 15 are already inverted
                       na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_UP = mean(c(Vertrauen.TiA2., Vertrauen.TiA7., Vertrauen.TiA11., Vertrauen.TiA16.),  # item 7 & 16 are already inverted
                       na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_F = mean(c(Vertrauen.TiA3., Vertrauen.TiA17.), 
                      na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_IoD = mean(c(Vertrauen.TiA4., Vertrauen.TiA8.), 
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_PtT = mean(c(Vertrauen.TiA5., Vertrauen.TiA12., Vertrauen.TiA18.), # item 5 is already inverted
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(TiA_TiA = mean(c(Vertrauen.TiA9., Vertrauen.TiA14.), 
                        na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  mutate(CTAM_PE = mean(c_across(Akzeptanz.PE1.:Akzeptanz.PE4.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1, 3, 4
  mutate(CTAM_EE = mean(c_across(Akzeptanz.EE1.:Akzeptanz.EE4.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-4
  mutate(CTAM_ATT = mean(c_across(Akzeptanz.ATT1.:Akzeptanz.ATT4.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-4
  mutate(CTAM_FC = mean(c_across(Akzeptanz.FC1.:Akzeptanz.FC3.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-3
  mutate(CTAM_ITU = mean(c_across(Akzeptanz.ITU1.:Akzeptanz.ITU3.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # 1-3
  mutate(CTAM_PS = mean(c_across(Akzeptanz.PS1.:Akzeptanz.PS6.), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>% # r1, r2, r3, 4-6
  mutate(System_sum = mean(c_across(System01:System19), na.rm = TRUE), .before = "Vertrauen.TiA1.") %>%
  relocate(any_of(c("NDRTs.NDRT1.", "NDRTs.NDRT2.", "NDRTs.NDRT3.", "NDRTs.NDRT4.", 
                    "NDRTs.NDRT5.", "NDRTs.NDRT6.", "NDRTs.NDRT7.", "NDRTs.NDRT8.", 
                    "SubjUeberwachguete.1.", "SubjEinflussSetting",
                    "Warnmeldungen.Warnungenzuhaeufig.", "Warnmeldungen.SichermitWarnsystem.", "Warnmeldungen.FahrfremdeohneWarnun.", "Warnmeldungen.Laestig.",
                    "ReaktionaufWarnung.WarumertoentWarnung.", "ReaktionaufWarnung.RichtigeReaktion.", "ReaktionaufWarnung.BewusstIgnoriert.", "ReaktionaufWarnung.AufmerksamkeitaufFah.",
                    "L2PrivNutzung", "L2Komponenten.Laengs.", "L2Komponenten.Quer.", "L2Komponenten.Hoff."
                    )), .before = "Vertrauen.TiA1.")

#### remove not needed data ####
rm(list=setdiff(ls(), c("raw_interview", "raw_nach", "raw_vor", "VPCodes",
                        "vorbefragung_scores", "nachbefragung_scores")))

#### save ####
## caution: items are already inverted (ATIS, CTAM & TiA); 
write_excel_csv(vorbefragung_scores, "data/preprocessed/vorbefragung_scores.csv")
write_excel_csv(nachbefragung_scores, "data/preprocessed/nachbefragung_scores.csv")

# in case separate datasets for qual- and quant-analyses are wanted
nachbefragung_qual <- nachbefragung_scores %>%
  select(c(group, VPNr, 
           MatrixNDRTsFreitext.0.:MatrixNDRTsFreitext.5.,
           OptionalAnmerkungenW, OptionalAnderesVerha:KommentareStudie.y))
nachbefragung_quant <- nachbefragung_scores %>%
  select(c(VPNr:L2Komponenten.Hoff.)) # without single answers for System Understanding
write_excel_csv(nachbefragung_quant, "data/preprocessed/nachbefragung_quant.csv")
write_excel_csv(nachbefragung_qual, "data/preprocessed/nachbefragung_qual.csv")