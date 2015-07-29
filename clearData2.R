library(plyr)
library(reshape2)

# Badanie drugie: "Drugie badanie zespołu prof. Paprzyckiej"
rawData <- read.csv(file.path(date, "rawData/249562.csv")) # Wczytanie danych
# Dodanie dodatkowej kolumny - który przyslówek/słowo.
adverbs = c("Sądził & Agentywna", "Wiedział & Kauzalna", "Wiedział & Stan", "BRAKPYTANIA")
groups = c("Harm", "Help")
rawData <- mutate(rawData, ADVERB = adverbs[LOSUJ])
rawData <- mutate(rawData, GROUP = groups[LOSUJV])
rawData$GROUP <- as.factor(rawData$GROUP)
rawData$ADVERB <- as.factor(rawData$ADVERB)

# Scalenie wszystkich kolumn z odpowiedziami w jedno
attach(rawData)
response <- as.numeric(IgnacyHARM) -1 +
  as.numeric(IgnacyMHELP) -1 +
  as.numeric(Wiedza2HARM) -1 +
  as.numeric(Wiedza2HELP) -1 +
  as.numeric(WIEDZAHARM) -1 +
  as.numeric(WIEDZAHELP) -1
#Scalenie wszystkich odpowiedzi o odpowiedzialności w jedną kolumnę
responsibility <- rowSums(cbind(RESPHARM, RESPHELP), na.rm = TRUE)
detach(rawData)
#Dodanie kolumn z odpowiedzią główną i z odpowiedzią na pytanie o odpowiedzialność
rawData$RESPONSE <- mapvalues(as.factor(response), from = c("1","2"), to = c("Nie", "Tak"))
rawData$RESPONSIBILITY <- responsibility
#Zremapowanie kolumn: płeć, wykształcenie, wiek, filozoficzność
rawData$G <- mapvalues(rawData$G, from = c("M", "F"), to = c("Mężczyzna", "Kobieta"))
rawData$E <- mapvalues(rawData$E, from = c("A1", "A2", "A3", "A4", "A5", "A6"), to = c("Podstawowe", "Zawodowe", "Średnie", "Wyższe", "Doktorat", "Habilitacja Plus"))
rawData$F <- mapvalues(rawData$F, from = c("A1", "A2"), to = c("Tak", "Nie"))
#Zmiana nazw kolumn
colnames(rawData)[18] <- "Gender"
colnames(rawData)[19] <- "Birth"
colnames(rawData)[20] <- "Education"
colnames(rawData)[21] <- "Education_other"
colnames(rawData)[22] <- "Philosophical_Education"

#Wybór interesujących nas kolumn
cleanData <- rawData[, c(1,2,5,6,18,19,20,21,22,23,24,25,26,27)]
#Wygenerowanie "czystych" danych.
write.csv(cleanData, file = file.path(date, "cleanData249562.csv"))