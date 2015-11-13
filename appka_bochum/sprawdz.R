data <- read.csv("wyniki.csv")
require(car)
# KODOWANIE URWANIA RYNNY


n1 <- rowSums(cbind(data$PIERWSZE1.1.,
                    data$PIERWSZE2.2.,
                    data$PIERWSZE3.3.,
                    data$PIERWSZE4.4.,
                    data$PIERWSZE5.5.,
                    data$PIERWSZE6.6.,
                    data$PIERWSZE7.7.,
                    data$PIERWSZE8.8.,
                    data$PIERWSZE9.9.,
                    data$PIERWSZE10.10.,
                    data$PIERWSZE11.11.,
                    data$PIERWSZE12.12.), na.rm = TRUE)

data$URWANIE_RYNNY <- NA
data[which(data$LOSUJ <13),]$URWANIE_RYNNY <- n1[which(data$LOSUJ <13)]

# KODOWANIE KOPNIECIA PIŁKI

n2 <- rowSums(cbind(data$PIERWSZE13.13.,
                    data$PIERWSZE14.14.,
                    data$PIERWSZE15.15.,
                    data$PIERWSZE16.16.,
                    data$PIERWSZE17.17.,
                    data$PIERWSZE18.18.,
                    data$PIERWSZE19.19.,
                    data$PIERWSZE20.20.,
                    data$PIERWSZE21.21.,
                    data$PIERWSZE22.22.,
                    data$PIERWSZE23.23.,
                    data$PIERWSZE24.24.), na.rm = TRUE)

data$KOPNIECIE_PILKI <- NA
data[which(!data$LOSUJ<13),]$KOPNIECIE_PILKI <- n2[which(!data$LOSUJ<13)]

# KODOWANIE KONEWKI

n3 <- rowSums(cbind(data$DRUGIE1.1,
                    data$DRUGIE2.2,
                    data$DRUGIE3.3.,
                    data$DRUGIE13.13.,
                    data$DRUGIE14.14.,
                    data$DRUGIE15.15.), na.rm = TRUE)

data$SPADNIECIE_KONEWKI <- NA
data[which(data$LOSUJ %in% c(1,2,3,13,14,15)),]$SPADNIECIE_KONEWKI <- n3[which(data$LOSUJ %in% c(1,2,3,13,14,15))]


# KODOWANIE TRAWY I WIATRU

n4 <- rowSums(cbind(data$DRUGIE4.4,
                    data$DRUGIE5.5,
                    data$DRUGIE6.6.,
                    data$DRUGIE16.16.,
                    data$DRUGIE17.17.,
                    data$DRUGIE18.18.), na.rm = TRUE)

data$BRAK_TRAWY_LUB_WIATRU <- NA
data[which(data$LOSUJ %in% c(4,5,6,16,17,18)),]$BRAK_TRAWY_LUB_WIATRU <- n4[which(data$LOSUJ %in% c(4,5,6,16,17,18))]

# KODOWANIE MALINOWSKIEJ

n5 <- rowSums(cbind(data$DRUGIE7.7,
                    data$DRUGIE8.8,
                    data$DRUGIE9.9.,
                    data$DRUGIE19.19.,
                    data$DRUGIE20.20.,
                    data$DRUGIE21.21.), na.rm = TRUE)

data$ZLORZECZENIE_MALINOWSKIEJ <- NA
data[which(data$LOSUJ %in% c(7,8,9,19,20,21)),]$ZLORZECZENIE_MALINOWSKIEJ <- n5[which(data$LOSUJ %in% c(7,8,9,19,20,21))]

# KODOWANIE ROBOTNIKÓW I RUSZTOWANIA

n6 <- rowSums(cbind(data$DRUGIE10.10,
                    data$DRUGIE11.11,
                    data$DRUGIE12.12.), na.rm = TRUE)

data$ROBOTNICY_BRAK_RUSZTOWANIA <- NA
data[which(data$LOSUJ %in% c(10,11,12)),]$ROBOTNICY_BRAK_RUSZTOWANIA <- n6[which(data$LOSUJ %in% c(10,11,12))]


# KODOWANIE MIESZKAŃCÓW I TRAWY

n7 <- rowSums(cbind(data$DRUGIE22.22,
                    data$DRUGIE23.23,
                    data$DRUGIE24.24.), na.rm = TRUE)

data$MIESZKANCY_BRAK_TRAWY <- NA
data[which(data$LOSUJ %in% c(22,23,24)),]$MIESZKANCY_BRAK_TRAWY <- n7[which(data$LOSUJ %in% c(22,23,24))]


# KODOWANIE ODPOWIEDZIALNOSCI DOZORCY

dozorcaa <- cbind(as.character(data$CZWARTE2),
                  as.character(data$CZWARTE3),
                  as.character(data$CZWARTE5),
                  as.character(data$CZWARTE6),
                  as.character(data$CZWARTE8),
                  as.character(data$CZWARTE9),
                  as.character(data$CZWARTE11),
                  as.character(data$CZWARTE12),
                  as.character(data$CZWARTE14),
                  as.character(data$CZWARTE15),
                  as.character(data$CZWARTE17),
                  as.character(data$CZWARTE18),
                  as.character(data$CZWARTE20),
                  as.character(data$CZWARTE21),
                  as.character(data$CZWARTE23),
                  as.character(data$CZWARTE24)
)
dozorcab <- character(nrow(data))

for(n in 1:nrow(data)) {
  for(x in 1:16)
    if(dozorcaa[n,x] != "") {
      dozorcab[n] <- dozorcaa[n,x]
    } 
}

data$ODPOWIEDZIALNOSC_DOZORCY <- as.factor(dozorcab)

# KODOWANIE ODPOWIEDZIALNOSCI MALINOWSKIEJ

malinowskaa <- cbind(as.character(data$TRZECIE8),
                     as.character(data$TRZECIE9),
                     as.character(data$PIATE20),
                     as.character(data$PIATE21))

malinowskab <- character(nrow(data))

for(n in 1:nrow(data)) {
  for(x in 1:4)
    if(malinowskaa[n,x] != "") {
      malinowskab[n] <- malinowskaa[n,x]
    } 
}

data$ODPOWIEDZIALNOSC_MALINOWSKIEJ <- as.factor(malinowskab)

# KODOWANIE ODPOWIEDZIALNOSCI ROBOTNIKÓW

robotnicya <- cbind(as.character(data$TRZECIE11),
                    as.character(data$TRZECIE12))

robotnicyb <- character(nrow(data))

for(n in 1:nrow(data)) {
  for(x in 1:2)
    if(robotnicya[n,x] != "") {
      robotnicyb[n] <- robotnicya[n,x]
    } 
}

data$ODPOWIEDZIALNOSC_ROBOTNIKÓW <- as.factor(robotnicyb)

# KODOWANIE ODPOWIEDZIALNOSCI CHŁOPCA

chlopieca <- cbind(as.character(data$TRZECIE14),
                   as.character(data$TRZECIE15),
                   as.character(data$TRZECIE17),
                   as.character(data$TRZECIE18),
                   as.character(data$TRZECIE20),
                   as.character(data$TRZECIE21),
                   as.character(data$TRZECIE23),
                   as.character(data$TRZECIE24))


chlopiecb <- character(nrow(data))

for(n in 1:nrow(data)) {
  for(x in 1:8)
    if(chlopieca[n,x] != "") {
      chlopiecb[n] <- chlopieca[n,x]
    } 
}

data$ODPOWIEDZIALNOSC_CHLOPCA <- as.factor(chlopiecb)


# KODOWANIE ODPOWIEDZIALNOSCI MIESZKAŃCÓW

mieszkancya <- cbind(as.character(data$PIATE23),
                     as.character(data$PIATE24))

mieszkancyb <- character(nrow(data))

for(n in 1:nrow(data)) {
  for(x in 1:2)
    if(mieszkancya[n,x] != "") {
      mieszkancyb[n] <- mieszkancya[n,x]
    } 
}

data$ODPOWIEDZIALNOSC_MIESZKANCOW <- as.factor(mieszkancyb)


#KODOWANIE PIERWSZOSCI I DRUGOŚCI PYTAŃ

data$PYTANIE_PIERWSZE <- rowSums(cbind(data$URWANIE_RYNNY, data$KOPNIECIE_PILKI), na.rm = TRUE)
data$PYTANIE_DRUGIE <- rowSums(cbind(data$SPADNIECIE_KONEWKI, data$ZLORZECZENIE_MALINOWSKIEJ, data$ROBOTNICY_BRAK_RUSZTOWANIA,
                                     data$MIESZKANCY_BRAK_TRAWY, data$BRAK_TRAWY_LUB_WIATRU), na.rm = TRUE)

# KOSMETYKA
data$SKUTEK <-character(nrow(data))
data[which(data$LOSUJ %in% c(1,4,7,10,13,16,19,22)),]$SKUTEK <- "Neutralny"
data[which(data$LOSUJ %in% c(2,5,8,11,14,17,20,23)),]$SKUTEK <- "Pozytywny"
data[which(data$LOSUJ %in% c(3,6,9,12,15,18,21,24)),]$SKUTEK <- "Negatywny"

data$PIERWSZE_OGNIWO <-character(nrow(data))
data[which(data$LOSUJ < 13),]$PIERWSZE_OGNIWO <- "Dzianie się"
data[which(data$LOSUJ > 12),]$PIERWSZE_OGNIWO <- "Działanie"

data$DRUGIE_OGNIWO <-character(nrow(data))
data[which(data$LOSUJ %in% c(1:6, 13:18)),]$DRUGIE_OGNIWO <- "Dzianie się"
data[which(!data$LOSUJ %in% c(1:6, 13:18)),]$DRUGIE_OGNIWO <- "Działanie"

data$ZMIANA_CZY_BRAK <-character(nrow(data))
data[which(data$LOSUJ %in% c(1:3, 7:9, 13:15, 19:21)),]$ZMIANA_CZY_BRAK <- "Zmiana"
data[which(!data$LOSUJ %in% c(1:3, 7:9, 13:15, 19:21)),]$ZMIANA_CZY_BRAK <- "Brak zmiany"

## KODY KRÓTKIE
data$P_P <- data$PYTANIE_PIERWSZE
data$P_D <- data$PYTANIE_DRUGIE
data$O_P <- data$PIERWSZE_OGNIWO
data$O_D <- data$DRUGIE_OGNIWO
data$S_M <- data$SKUTEK
data$Z <- data$ZMIANA_CZY_BRAK

## ANOVA

model_p1 <- glm(P_P ~ (O_P*O_D*Z*S_M), data=data, contrasts = c("contr.sum"))
model_p2 <- glm(P_D ~ (O_P*O_D*Z*S_M), data=data, contrasts = c("contr.sum"))

a_model_p1 <- aov(model_p1)
a_model_p2 <- aov(model_p2)
Anova(model_p1, type =2)
Anova(model_p2, type =2)

TukeyHSD(a_model_p1, "O_P")
plot(TukeyHSD(a_model_p1, "O_P"))
