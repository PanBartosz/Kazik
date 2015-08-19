library(plyr)
library(reshape2)

# Badanie trzecie: "Co myślimy o działaniach?"
rawData <- read.csv(file.path(date, "rawData/169464.csv")) # Wczytanie danych

#Zremapowanie kolumn: płeć, wykształcenie, wiek, filozoficzność
rawData$G <- mapvalues(rawData$G, from = c("M", "F"), to = c("Mężczyzna", "Kobieta"))
rawData$E <- mapvalues(rawData$E, from = c("A1", "A2", "A3", "A4", "A5", "A6"), to = c("Podstawowe", "Zawodowe", "Średnie", "Wyższe", "Doktorat", "Habilitacja Plus"))
rawData$F <- mapvalues(rawData$F, from = c("A1", "A2"), to = c("Tak", "Nie"))
rawData$H <- mapvalues(rawData$H, from = c("N", "Y"), to = c("Nie", "Tak"))

#Zmiana nazw kolumn
colnames(rawData)[51] <- "Gender"
colnames(rawData)[52] <- "Birth"
colnames(rawData)[53] <- "Education"
colnames(rawData)[54] <- "Education_other"
colnames(rawData)[55] <- "Philosophical_Education"
colnames(rawData)[56] <- "Previous_survey"
# Dodanie kolumny z grupą
rawData$GROUP <- ifelse(rawData$LOSUJ %in% c(1,3,5,7,9,11,13,16,18,19,210), "Harm", "Help")
rawData$GROUP <- as.factor(rawData$GROUP)

# Podział danych na badania
rawDataIgnacySlawek <- rawData[which(rawData$LOSUJ <3),]
rawDataJanek <- rawData[which(rawData$LOSUJ > 2 & rawData$LOSUJ < 9) ,]
rawDataWiktor <- rawData[which(rawData$LOSUJ > 8 & rawData$LOSUJ < 11) ,]
rawDataKasiaBartosz <- rawData[which(rawData$LOSUJ > 10 & rawData$LOSUJ < 15) ,]
rawDataIgnacySlawek2 <- rawData[which(rawData$LOSUJ > 14 & rawData$LOSUJ < 19) ,]
rawDataKasiaBartosz22 <- rawData[which(rawData$LOSUJ > 18) ,]

#Dane Ignacego i Sławka o obowiązku:

IntentionalityX <- as.numeric(rawDataIgnacySlawek$IntHARM)-1 + 
  as.numeric(rawDataIgnacySlawek$IntHELP)-1
rawDataIgnacySlawek$intentionality <- mapvalues(as.factor(IntentionalityX), from = c("1","2"), to = c("Nie", "Tak"))

Obligation_knowX <- as.numeric(rawDataIgnacySlawek$WoOHARM)-1 + 
  as.numeric(rawDataIgnacySlawek$WoOHELP)-1
rawDataIgnacySlawek$obligation_know <- mapvalues(as.factor(Obligation_knowX), from = c("1","2"), to = c("Nie", "Tak"))

Obligation_existX <-as.numeric(rawDataIgnacySlawek$IOHARM)-1 + 
  as.numeric(rawDataIgnacySlawek$IOMHELP)-1
rawDataIgnacySlawek$obligation_exist <- mapvalues(as.factor(Obligation_existX), from = c("1","2"), to = c("Nie", "Tak"))

#Zapisywanie oczyszczonych danych 
cleanDataIgnacySlawek <- rawDataIgnacySlawek[, c(1,2,5,6,51,52,53,54,55,56,62,63,64,65)]
write.csv(cleanDataIgnacySlawek, file = file.path(date, "cleanDataIgnacySlawek169464.csv"))

# Dane Janka o wymyślonych przysłówkach
# Dodanie zmiennej dotyczącej przysłówka
rawDataJanek$ADVERB <- ifelse(
  rawDataJanek$LOSUJ %in% 3:4, "Endemicznie",
  ifelse(rawDataJanek$LOSUJ %in% 5:6 , "Katrybilnie", "Pastewnie"))
rawDataJanek$ADVERB <- as.factor(rawDataJanek$ADVERB)
# Utworzenie zmiennej dotyczącej czasu
rawDataJanek$COUNTER <-rowSums(cbind(rawDataJanek$licznikISTN, 
                                     rawDataJanek$licznikZROZ,
                                     rawDataJanek$licznikSENS), na.rm = TRUE)

cleanDataJanek <- rawDataJanek[, c(1,2,5,6,51,52,53,54,55,56,62,63,64)]
rawDataJanek$RESPONSE <-as.numeric(rawDataJanek$helpWYM)-1 + 
  as.numeric(rawDataJanek$harmWYM)-1 +
  as.numeric(rawDataJanek$helpZROZ)-1 +
  as.numeric(rawDataJanek$harmZROZ)-1 +
  as.numeric(rawDataJanek$helpBEZSENS)-1 +
  as.numeric(rawDataJanek$harmBEZSENS)-1

rawDataJanek$RESPONSE <- mapvalues(as.factor(rawDataJanek$RESPONSE), from = c("1","2"), to = c("Nie", "Tak"))
cleanDataJanek <- rawDataJanek[, c(1,2,5,6,51,52,53,54,55,56,62,63,64,65)]


write.csv(cleanDataJanek, file = file.path(date, "cleanDataJanek169464.csv"))

#Dane Wiktora o postawach
rawDataWiktor$HA1 <- mapvalues(rawDataWiktor$HA1,
                               from = c("A1", "A2", "A3", "A4"),
                              to = c("A3", "A4", "A1", "A2"))
rawDataWiktor$HA1 <- factor(rawDataWiktor$HA1, levels = c("A1", "A2", "A3", "A4", "A5"))
rawDataWiktor$HE1 <- factor(rawDataWiktor$HE1, levels = c("A1", "A2", "A3", "A4", "A5"))
                            
question1 <- rowSums(cbind(as.numeric(rawDataWiktor$HE1), 
                           as.numeric(rawDataWiktor$HA1)), na.rm = TRUE)
question2 <- rowSums(cbind(as.numeric(rawDataWiktor$HE2)-1,
                           as.numeric(rawDataWiktor$HA2)-1), na.rm = TRUE)

rawDataWiktor$CHAIRMAN_ATTITUDE <- mapvalues(as.factor(question1), 
                                             from = c("1","2","3","4","5"),
                                             to = c("Dyrektor dezaprobuje pomaganie",
                                                    "Dyrektor aprobuje pomaganie",
                                                    "Dyrektor dezaprobuje szkodzenie",
                                                    "Dyrektor aprobuje szkodzenie",
                                                    "Dyrektor jest obojętny"))

rawDataWiktor$ATTITUDE <- mapvalues(as.factor(question2), 
                                             from = c("1","2","3"),
                                             to = c("Dezaprobuję postawę dyrektora",
                                                    "Aprobuję postawę dyrektora",
                                                    "Jestem obojętny wobec postawy dyrektora"))

cleanDataWiktor <- rawDataWiktor[, c(1,2,5,6,51,52,53,54,55,56,62,63,64)]
write.csv(cleanDataWiktor, file = file.path(date, "cleanDataWiktor169464.csv"))

# Dane Katarzyny i Bartosza o kolizjach norm

rawDataKasiaBartosz$ACTION <- ifelse(rawDataKasiaBartosz$LOSUJ %in% c(12,13), "Działanie", "Brak działania")
rawDataKasiaBartosz$ACTION <- as.factor(rawDataKasiaBartosz$ACTION)

# Dodanie kolumny z pytaniem o środowisko
rawDataKasiaBartosz$HARMPRIMENV <- mapvalues(rawDataKasiaBartosz$HARMPRIMENV,
                                             from = c("N", "Y"),
                                             to = c("Nie", "Tak"))

rawDataKasiaBartosz$HARMPRIMENV <- factor(rawDataKasiaBartosz$HARMPRIMENV, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$HARMBISENV <- mapvalues(rawDataKasiaBartosz$HARMBISENV,
                                             from = c("N", "Y"),
                                             to = c("Nie", "Tak"))

rawDataKasiaBartosz$HARMBISENV <- factor(rawDataKasiaBartosz$HARMBISENV, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPPRIMENV <- mapvalues(rawDataKasiaBartosz$HELPPRIMENV,
                                             from = c("N", "Y"),
                                             to = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPPRIMENV <-factor(rawDataKasiaBartosz$HELPPRIMENV, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPBISENV <- mapvalues(rawDataKasiaBartosz$HELPBISENV,
                                             from = c("N", "Y"),
                                             to = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPBISENV <- factor(rawDataKasiaBartosz$HELPBISENV, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$Environment_response <- as.factor(rowSums(
  cbind(rawDataKasiaBartosz$HARMPRIMENV, rawDataKasiaBartosz$HARMBISENV,
      rawDataKasiaBartosz$HELPPRIMENV, rawDataKasiaBartosz$HELPBISENV), na.rm = TRUE))

rawDataKasiaBartosz$Environment_response <- mapvalues(rawDataKasiaBartosz$Environment_response,
                                                      from = c("1","2"),
                                                      to = c("Nie", "Tak"))



# Dodanie kolumny z pytaniem o śmierć głodową
rawDataKasiaBartosz$HARMPRIMSTARVE <- mapvalues(rawDataKasiaBartosz$HARMPRIMSTARVE,
                                             from = c("N", "Y"),
                                             to = c("Nie", "Tak"))

rawDataKasiaBartosz$HARMPRIMSTARVE <- factor(rawDataKasiaBartosz$HARMPRIMSTARVE, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$HARMBISSTARVE <- mapvalues(rawDataKasiaBartosz$HARMBISSTARVE,
                                            from = c("N", "Y"),
                                            to = c("Nie", "Tak"))

rawDataKasiaBartosz$HARMBISSTARVE <- factor(rawDataKasiaBartosz$HARMBISSTARVE, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPPRIMSTARVE <- mapvalues(rawDataKasiaBartosz$HELPPRIMSTARVE,
                                             from = c("N", "Y"),
                                             to = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPPRIMSTARVE <-factor(rawDataKasiaBartosz$HELPPRIMSTARVE, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPBISSTARVE <- mapvalues(rawDataKasiaBartosz$HELPBISSTARVE,
                                            from = c("N", "Y"),
                                            to = c("Nie", "Tak"))

rawDataKasiaBartosz$HELPBISSTARVE <- factor(rawDataKasiaBartosz$HELPBISSTARVE, levels = c("Nie", "Tak"))

rawDataKasiaBartosz$Starvation_response <- as.factor(rowSums(
  cbind(rawDataKasiaBartosz$HARMPRIMSTARVE, rawDataKasiaBartosz$HARMBISSTARVE,
        rawDataKasiaBartosz$HELPPRIMSTARVE, rawDataKasiaBartosz$HELPBISSTARVE), na.rm = TRUE))

rawDataKasiaBartosz$Starvation_response <- mapvalues(rawDataKasiaBartosz$Starvation_response,
                                                      from = c("1","2"),
                                                      to = c("Nie", "Tak"))

cleanDataKasiaBartosz <- rawDataKasiaBartosz[, c(1,2,5,6,51,52,53,54,55,56,62,63,64,65)]
write.csv(cleanDataKasiaBartosz, file = file.path(date, "cleanKasiaBartosz169464.csv"))

#Dane Ignacego i Sławka o efekcie epistemicznym

adverbs = c("Sądził & Kauzalna", "Sądził & Kauzalna", "Sądził & Stan", "Sądził & Stan")
# Dodanie kolumny z przysłówkami
rawDataIgnacySlawek2 <- mutate(rawDataIgnacySlawek2, ADVERB = adverbs[LOSUJ-14])
# Dodanie kolumny z grupą
rawDataIgnacySlawek2$GROUP <- ifelse(rawDataIgnacySlawek2$LOSUJ %in% c(16,18), "Harm", "Help")
rawDataIgnacySlawek2$RESPONSE <-as.numeric(rawDataIgnacySlawek2$Sadzi2wdrozenieHARM)-1 + 
  as.numeric(rawDataIgnacySlawek2$Sadzi2wdrozenieHELP)-1 +
  as.numeric(rawDataIgnacySlawek2$Sadzi3stansieHARM)-1 +
  as.numeric(rawDataIgnacySlawek2$Sadzi3stansieHELP)-1

rawDataIgnacySlawek2$RESPONSE <- mapvalues(as.factor(rawDataIgnacySlawek2$RESPONSE), from = c("1","2"), to = c("Nie", "Tak"))

cleanDataIgnacySlawek2 <- rawDataIgnacySlawek2[, c(1,2,5,6,51,52,53,54,55,56,62,63,64)]
write.csv(cleanDataIgnacySlawek2, file = file.path(date, "cleanIgnacySlawek2169464.csv"))



#Dane Katarzyny i Bartosza o przysłówkach i namyśle

adverbs <- c("Z rozmysłem", "Z rozmysłem", "Umyślnie", "Umyślnie")
groups <- c("Harm", "Help", "Harm", "Help")
# Dodanie kolumny z przysłówkami
rawDataKasiaBartosz22 <- mutate(rawDataKasiaBartosz22, ADVERB = adverbs[LOSUJ-18])

# Dodanie kolumny z grupą
rawDataKasiaBartosz22 <- mutate(rawDataKasiaBartosz22, GROUP = groups[LOSUJ-18])


rawDataKasiaBartosz22$Response <-as.numeric(rawDataKasiaBartosz22$ZROZMYSLEMHARM)-1 + 
  as.numeric(rawDataKasiaBartosz22$ZROZMYSLEMHELP)-1 +
  as.numeric(rawDataKasiaBartosz22$UMYSLNIEHARM)-1 +
  as.numeric(rawDataKasiaBartosz22$UMYSLNIEHELP)-1

rawDataKasiaBartosz22$Response <- mapvalues(as.factor(rawDataKasiaBartosz22$Response), from = c("1","2"), to = c("Nie", "Tak"))

cleanDataKasiaBartosz2 <- rawDataKasiaBartosz22[, c(1,2,5,6,51,52,53,54,55,56,62,63,64)]
write.csv(cleanDataKasiaBartosz2, file = file.path(date, "cleanKasiaBartosz2169464.csv"))
