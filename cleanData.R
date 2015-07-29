library(plyr)
library(reshape2)

# Badanie pierwsze: "Badanie zespołu prof. Paprzyckiej"
rawData <- read.csv(file.path(date, "rawData/28553.csv")) # Wczytanie danych
# Dodanie dodatkowej kolumny - który przyslówek/słowo.
adverbs = c("Specjalnie", "Świadomie", "Celowo", "Umyślnie", "Brak przysłówka",
            "Chciał", "Zamierzał", "Wiedział & Agentywna", "Z rozmysłem")
groups = c("Harm", "Help")
rawData <- mutate(rawData, ADVERB = adverbs[LOSUJ])
rawData <- mutate(rawData, GROUP = groups[LOSUJV])
rawData$GROUP <- as.factor(rawData$GROUP)
rawData$ADVERB <- as.factor(rawData$ADVERB)

# Scalenie wszystkich kolumn z odpowiedziami w jedno
attach(rawData)
response <- as.numeric(SPECJALNIEHARM) -1 +
as.numeric(SPECJALNIEHELP) -1 +
as.numeric(SWIADOMIEHARM) -1 +
as.numeric(SWIADOMIEHELP) -1 +
as.numeric(CELOWOHARM) -1 +
as.numeric(CELOWOHELP) -1 +
as.numeric(UMYSLNIEHARM) -1 +
as.numeric(UMYSLNIEHELP) -1 +
as.numeric(BRAKHARM) -1 +
as.numeric(BRAKHELP) -1 +
as.numeric(CHCIALHARM) -1 +
as.numeric(CHCIALHELP) -1 +
as.numeric(ZROZMYSLEMHARM) -1 +
as.numeric(ZROZMYSLEMHELP) -1 +
as.numeric(ZAMIERZALHARM) -1 +
as.numeric(ZAMIERZALHELP) -1 +
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
colnames(rawData)[30] <- "Gender"
colnames(rawData)[31] <- "Birth"
colnames(rawData)[32] <- "Education"
colnames(rawData)[33] <- "Education_other"
colnames(rawData)[34] <- "Philosophical_Education"

#Wybór interesujących nas kolumn
cleanData <- rawData[, c(1,2,5,6,30,31,32,33,34,35,36,37,38 )]
#Wygenerowanie "czystych" danych.
write.csv(cleanData, file = file.path(date, "cleanData28553.csv"))