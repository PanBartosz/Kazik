library(ggplot2)
library(scales)
library(grid)
library(ggthemes)

data <- read.csv("results-survey254952.csv")
data2 <-read.csv("results-survey129171.csv")
data3 <-read.csv("results-survey898773.csv")


data <- data[which(data$MULTIMULTI == 2),] ## Tylko BISy tzn. pytania o zachowania dyrektora
data <- data[which(data$MULTILOTEK != 5),] ## Wywalamy kruki
data <- data[which(data$MULTILOTEK != 6),] ## Wywalamy kruki cd.
data <- data[which(data$MULTILOTEK != 6),] ## Wywalamy kruki cd.
data <- data[which(data$MULTILOTEK != 2),] ## Wywalamy błędny cd.


data$ACTION <- ifelse(data$MULTILOTEK %in% c(1,3), "Introduced", "Not introduced")
data$GROUP <- ifelse(data$MULTILOTEK %in% c(1,2), "Harm", "Help")

data2$ACTION <- ifelse(data2$MULTILOTEK %in% c(1,3), "Introduced", "Not introduced")
data2$GROUP <- ifelse(data2$MULTILOTEK %in% c(1,2), "Harm", "Help")


## Suwak I

data$SUWAK_1 <-rowSums(
  cbind(data$BISHARMPRIMSUWAKI.1.,
        data$BISHARMPRIMSUWAKI.1..1,
        # data2$BISHARMBISSUWAKI.A1.,
        # data2$r713q19.1.,
        data$BISHELPPRIMSUWAKI.1.,
        data$BISHELPPRIMSUWAKI.1..1,
        data$BISHELPBISSUWAKI.1.,
        data$BISHELPBISSUWAKI.1..1), na.rm = TRUE)



data$SUWAK_2 <- rowSums(
  cbind(data$BISHARMPRIMSUWAKI.2.,
        data$BISHARMPRIMSUWAKI.2..1,
        #data2$BISHARMBISSUWAKI.A2.,
        #data2$r713q19.2.,
        data$BISHELPPRIMSUWAKI.2.,
        data$BISHELPPRIMSUWAKI.2..1,
        data$BISHELPBISSUWAKI.2.,
        data$BISHELPBISSUWAKI.2..1), na.rm = TRUE)


data$SUWAK_3 <- rowSums(
  cbind(data$BISHARMPRIMSUWAKI.3.,
        data$BISHARMPRIMSUWAKI.3..1,
        #data2$r713q19.3.,
        #data2$BISHARMBISSUWAKI.A3.,
        data$BISHELPPRIMSUWAKI.3.,
        data$BISHELPPRIMSUWAKI.3..1,
        data$BISHELPBISSUWAKI.3.,
        data$BISHELPBISSUWAKI.3..1), na.rm = TRUE)


data$SUWAK_4 <- rowSums(
  cbind(data$BISHARMPRIMSUWAKI.4.,
        data$BISHARMPRIMSUWAKI.4..1,
        #data2$BISHARMBISSUWAKI.A4.,
        #data2$r713q19.4.,
        data$BISHELPPRIMSUWAKI.4.,
        data$BISHELPPRIMSUWAKI.4..1,
        data$BISHELPBISSUWAKI.4.,
        data$BISHELPBISSUWAKI.4..1), na.rm = TRUE)

data$SUWAK_5 <- rowSums(
  cbind(data$BISHARMPRIMSUWAKI.5.,
        data$BISHARMPRIMSUWAKI.5..1,
        #data2$BISHARMBISSUWAKI.A5.,
        #data2$r713q19.5.,
        data$BISHELPPRIMSUWAKI.5.,
        data$BISHELPPRIMSUWAKI.5..1,
        data$BISHELPBISSUWAKI.5.,
        data$BISHELPBISSUWAKI.5..1), na.rm = TRUE)


## Badnie dodatkowe...

data2 <- data2[which(data2$MULTILOTEK == 2),] ## Tylko HARMBIS
data2 <- data2[which(data2$MULTIMULTI == 2),] ## Tylko BISy tzn. pytania o zachowania dyrektora

data2$SUWAK_1 <-rowSums(
  cbind( data2$BISHARMBISSUWAKI.A1.,
        data2$r713q19.1.), na.rm = TRUE)



data2$SUWAK_2 <- rowSums(
  cbind(data2$BISHARMBISSUWAKI.A2.,
        data2$r713q19.2.), na.rm = TRUE)


data2$SUWAK_3 <- rowSums(
  cbind(data2$r713q19.3.,
        data2$BISHARMBISSUWAKI.A3.), na.rm = TRUE)


data2$SUWAK_4 <- rowSums(
  cbind(data2$BISHARMBISSUWAKI.A4.,
        data2$r713q19.4.), na.rm = TRUE)

data2$SUWAK_5 <- rowSums(
  cbind(data2$BISHARMBISSUWAKI.A5.,
        data2$r713q19.5.), na.rm = TRUE)




s1 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$SUWAK_1))
s2 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$SUWAK_2))
s3 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$SUWAK_3))
s4 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$SUWAK_4))
s5 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$SUWAK_5))

s1$PYTANIE <- 1
s2$PYTANIE <- 2
s3$PYTANIE <- 3
s4$PYTANIE <- 4
s5$PYTANIE <- 5

data_s <- rbind(s1,s2,s3,s4,s5)
data_s$V3 <- as.numeric(data_s$V3)-1

data_s$PYTANIE <- as.factor(data_s$PYTANIE)

as1 <- as.data.frame(cbind(data2$GROUP, data2$ACTION, data2$SUWAK_1))
as2 <- as.data.frame(cbind(data2$GROUP, data2$ACTION, data2$SUWAK_2))
as3 <- as.data.frame(cbind(data2$GROUP, data2$ACTION, data2$SUWAK_3))
as4 <- as.data.frame(cbind(data2$GROUP, data2$ACTION, data2$SUWAK_4))
as5 <- as.data.frame(cbind(data2$GROUP, data2$ACTION, data2$SUWAK_5))

as1$PYTANIE <- 1
as2$PYTANIE <- 2
as3$PYTANIE <- 3
as4$PYTANIE <- 4
as5$PYTANIE <- 5

adata_s <- rbind(as1,as2,as3,as4,as5)
adata_s$V3 <- as.numeric(adata_s$V3)-1

adata_s$PYTANIE <- as.factor(adata_s$PYTANIE)

data_S <- rbind(data_s, adata_s)



## WYKRES ŚREDNICH W PYTANIU CZY DYREKTOR PRYCZYNIŁ SIĘ...
plot <- ggplot(data = data_S) + aes(y=V3, x=PYTANIE) + stat_summary(fun.y = mean, geom = "bar") + facet_grid(V1~V2) + coord_cartesian(ylim = c(1, 3))

## TEST WILCOXA MIĘDZY PYTANIAMI 5 i 2 DLA PAR 5:2 
wilcox.test(
data_S[which(data_S$V1=="Harm" & data_S$V2 =="Introduced" &data_S$PYTANIE == "5"),]$V3,
data_S[which(data_S$V1=="Harm" & data_S$V2 =="Introduced" &data_S$PYTANIE == "2"),]$V3) 

wilcox.test(
  data_S[which(data_S$V1=="Help" & data_S$V2 =="Introduced" &data_S$PYTANIE == "5"),]$V3,
  data_S[which(data_S$V1=="Help" & data_S$V2 =="Introduced" &data_S$PYTANIE == "2"),]$V3) 

wilcox.test(
  data_S[which(data_S$V1=="Harm" & data_S$V2 =="Not introduced" &data_S$PYTANIE == "5"),]$V3,
  data_S[which(data_S$V1=="Harm" & data_S$V2 =="Not introduced" &data_S$PYTANIE == "2"),]$V3) 

wilcox.test(
  data_S[which(data_S$V1=="Help" & data_S$V2 =="Not introduced" &data_S$PYTANIE == "5"),]$V3,
  data_S[which(data_S$V1=="Help" & data_S$V2 =="Not introduced" &data_S$PYTANIE == "2"),]$V3) 

## TEST WILCOXA DLA GLOBALNEJ RÓŻNICY
wilcox.test(
  data_S[which(data_S$V1=="Help" & data_S$V2 =="Not introduced"),]$V3,
  data_S[which(data_S$V1=="Harm" & data_S$V2 =="Not introduced"),]$V3)

wilcox.test(
  data_S[which(data_S$V1=="Help" & data_S$V2 =="Introduced"),]$V3,
  data_S[which(data_S$V1=="Harm" & data_S$V2 =="Introduced"),]$V3)



fit <- lm(V3 ~ V1*V2*PYTANIE, data=data_S)
Anova(fit, type =2)
HSD.test(fit, c("V1", "V2", "PYTANIE"), console = TRUE)



## SPRAWSTWO
data <- read.csv("results-survey254952.csv")
data2 <-read.csv("results-survey129171.csv")
data3 <-read.csv("results-survey898773.csv")


data <- data[which(data$MULTIMULTI == 2),] ## Tylko BISy tzn. pytania o zachowania dyrektora
data <- data[which(data$MULTILOTEK != 5),] ## Wywalamy kruki
data <- data[which(data$MULTILOTEK != 6),] ## Wywalamy kruki cd.


data$ACTION <- ifelse(data$MULTILOTEK %in% c(1,3), "Introduced", "Not introduced")
data$GROUP <- ifelse(data$MULTILOTEK %in% c(1,2), "Harm", "Help")
data <- data[which(data$MULTILOTEK != 2),] ## Wywalamy błędny cd. 

data$AGENCYHARMPRIM1 <- ifelse(data$AGENCYHARMPRIM.1. == "", NA, data$AGENCYHARMPRIM.1.)-2
data$AGENCYHARMPRIM2 <- ifelse(data$AGENCYHARMPRIM.2. == "", NA, data$AGENCYHARMPRIM.2.)-2
data$AGENCYHARMPRIM3 <- ifelse(data$AGENCYHARMPRIM.3. == "", NA, data$AGENCYHARMPRIM.3.)-2
data$AGENCYHARMPRIM4 <- ifelse(data$AGENCYHARMPRIM.4. == "", NA, data$AGENCYHARMPRIM.4.)-2
data$AGENCYHARMPRIM5 <- ifelse(data$AGENCYHARMPRIM.5. == "", NA, data$AGENCYHARMPRIM.5.)-2

data$AGENCYHELPPRIM1 <- ifelse(data$AGENCYHELPPRIM.1. == "", NA, data$AGENCYHELPPRIM.1.)-2
data$AGENCYHELPPRIM2 <- ifelse(data$AGENCYHELPPRIM.2. == "", NA, data$AGENCYHELPPRIM.2.)-2
data$AGENCYHELPPRIM3 <- ifelse(data$AGENCYHELPPRIM.3. == "", NA, data$AGENCYHELPPRIM.3.)-2
data$AGENCYHELPPRIM4 <- ifelse(data$AGENCYHELPPRIM.4. == "", NA, data$AGENCYHELPPRIM.4.)-2
data$AGENCYHELPPRIM5 <- ifelse(data$AGENCYHELPPRIM.5. == "", NA, data$AGENCYHELPPRIM.5.)-2

data$AGENCYHELPBIS1 <- ifelse(data$AGENCYHELPBIS.1. == "", NA, data$AGENCYHELPBIS.1.)-2
data$AGENCYHELPBIS2 <- ifelse(data$AGENCYHELPBIS.2. == "", NA, data$AGENCYHELPBIS.2.)-2
data$AGENCYHELPBIS3 <- ifelse(data$AGENCYHELPBIS.3. == "", NA, data$AGENCYHELPBIS.3.)-2
data$AGENCYHELPBIS4 <- ifelse(data$AGENCYHELPBIS.4. == "", NA, data$AGENCYHELPBIS.4.)-2
data$AGENCYHELPBIS5 <- ifelse(data$AGENCYHELPBIS.5. == "", NA, data$AGENCYHELPBIS.5.)-2

data$AGENCY1 <- rowSums(cbind(data$AGENCYHARMPRIM1, data$AGENCYHELPPRIM1, data$AGENCYHELPBIS1), na.rm = TRUE) 
data$AGENCY2 <- rowSums(cbind(data$AGENCYHARMPRIM2, data$AGENCYHELPPRIM2, data$AGENCYHELPBIS2), na.rm = TRUE) 
data$AGENCY3 <- rowSums(cbind(data$AGENCYHARMPRIM3, data$AGENCYHELPPRIM3, data$AGENCYHELPBIS3), na.rm = TRUE) 
data$AGENCY4 <- rowSums(cbind(data$AGENCYHARMPRIM4, data$AGENCYHELPPRIM4, data$AGENCYHELPBIS4), na.rm = TRUE) 
data$AGENCY5 <- rowSums(cbind(data$AGENCYHARMPRIM5, data$AGENCYHELPPRIM5, data$AGENCYHELPBIS5), na.rm = TRUE) 

agencys1 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$AGENCY1))
agencys2 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$AGENCY2))
agencys3 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$AGENCY3))
agencys4 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$AGENCY4))
agencys5 <- as.data.frame(cbind(data$GROUP, data$ACTION, data$AGENCY5))

agencys1$PYTANIE <- 1
agencys2$PYTANIE <- 2
agencys3$PYTANIE <- 3
agencys4$PYTANIE <- 4
agencys5$PYTANIE <- 5

agencydata_s <- rbind(agencys1,agencys2,agencys3,agencys4,agencys5)
agencydata_s$PYTANIE <- as.factor(agencydata_s$PYTANIE)
agencydata_s$V3 <- as.numeric(agencydata_s$V3)-1


## BŁĄÐ...

data3$AGENCYHARMBIS1 <- ifelse(data3$AGENCYHARMBIS.1. == "", NA, data3$AGENCYHARMBIS.1.)
data3$AGENCYHARMBIS2 <- ifelse(data3$AGENCYHARMBIS.2. == "", NA, data3$AGENCYHARMBIS.2.)
data3$AGENCYHARMBIS3 <- ifelse(data3$AGENCYHARMBIS.3. == "", NA, data3$AGENCYHARMBIS.3.)
data3$AGENCYHARMBIS4 <- ifelse(data3$AGENCYHARMBIS.4. == "", NA, data3$AGENCYHARMBIS.4.)
data3$AGENCYHARMBIS5 <- ifelse(data3$AGENCYHARMBIS.5. == "", NA, data3$AGENCYHARMBIS.5.)

data3 <- data3[which(data3$MULTIMULTI == 2),] ## Tylko BISy tzn. pytania o zachowania dyrektora
data3 <- data3[which(data3$MULTILOTEK == 2),] ## Wywalamy kruki



data3$ACTION <- rep("Not introduced", nrow(data3))
data3$GROUP <- rep("Harm", nrow(data3))
agencys1 <- as.data.frame(cbind(data3$GROUP, data3$ACTION, data3$AGENCYHARMBIS1))
agencys2 <- as.data.frame(cbind(data3$GROUP, data3$ACTION, data3$AGENCYHARMBIS2))
agencys3 <- as.data.frame(cbind(data3$GROUP, data3$ACTION, data3$AGENCYHARMBIS3))
agencys4 <- as.data.frame(cbind(data3$GROUP, data3$ACTION, data3$AGENCYHARMBIS4))
agencys5 <- as.data.frame(cbind(data3$GROUP, data3$ACTION, data3$AGENCYHARMBIS5))

agencys1$PYTANIE <- rep(1, nrow(agencys1))
agencys2$PYTANIE <- rep(2, nrow(agencys2))
agencys3$PYTANIE <- rep(3, nrow(agencys3))
agencys4$PYTANIE <- rep(4, nrow(agencys4))
agencys5$PYTANIE <- rep(5, nrow(agencys5))

e_agencydata_s <- rbind(agencys1,agencys2,agencys3,agencys4,agencys5)
e_agencydata_s$PYTANIE <- as.factor(e_agencydata_s$PYTANIE)
e_agencydata_s$V3 <- as.numeric(e_agencydata_s$V3)-1

agency_data <- rbind(agencydata_s, e_agencydata_s)

as.factor(agency_data$V3)

## TESTY CHIKWADAT

chisq.test(as.factor(agency_data[which(agency_data$V1=="Help" & agency_data$V2 =="Not introduced" &agency_data$PYTANIE %in% c("3","5")),]$V3),
as.factor(agency_data[which(agency_data$V1=="Help" & agency_data$V2 =="Not introduced" &agency_data$PYTANIE %in% c("3","5")),]$PYTANIE))

chisq.test(as.factor(agency_data[which(agency_data$V1=="Harm" & agency_data$V2 =="Not introduced" &agency_data$PYTANIE %in% c("3","5")),]$V3),
           as.factor(agency_data[which(agency_data$V1=="Harm" & agency_data$V2 =="Not introduced" &agency_data$PYTANIE %in% c("3","5")),]$PYTANIE))

chisq.test(as.factor(agency_data[which(agency_data$V1=="Help" & agency_data$V2 =="Introduced" &agency_data$PYTANIE %in% c("3","5")),]$V3),
           as.factor(agency_data[which(agency_data$V1=="Help" & agency_data$V2 =="Introduced" &agency_data$PYTANIE %in% c("3","5")),]$PYTANIE))

chisq.test(as.factor(agency_data[which(agency_data$V1=="Harm" & agency_data$V2 =="Introduced" &agency_data$PYTANIE %in% c("3","5")),]$V3),
           as.factor(agency_data[which(agency_data$V1=="Harm" & agency_data$V2 =="Introduced" &agency_data$PYTANIE %in% c("3","5")),]$PYTANIE))



agency_data$V4 <- as.factor(agency_data$V3)

attach(agency_data[which(agency_data$V2 == "Introduced"),])
chisq.test(V1, V3)
detach(agency_data[which(agency_data$V2 == "Introduced"),])

attach(agency_data[which(agency_data$V2 == "Not introduced"),])
chisq.test(V1, V3)
detach(agency_data[which(agency_data$V2 == "Not introduced"),])



plot2 <- ggplot(data = agency_data) + aes(y=V3, x=PYTANIE) + stat_summary(fun.y = mean, geom = "bar") + facet_grid(V1~V2)







## INTENCJONALNOŚĆ



data$INTENCJONALNOSC_STARV <-rowSums(cbind((ifelse(data$INTHARMPRIM2 == "N/A", NA, data$INTHARMPRIM2) - 2),
                                           (ifelse(data$INTHELPPRIM2 == "N/A", NA, data$INTHELPPRIM2) - 2),
                                           (ifelse(data$INTHELPBIS2 == "N/A", NA, data$INTHELPBIS2) - 2)), na.rm=TRUE)



data3$INTENCJONALNOSC_STARV <- ifelse(data3$INTHARMBIS2 == "N/A", NA, data3$INTHARMBIS2) -1


data$INTENCJONALNOSC_ENV <-rowSums(cbind((ifelse(data$INTHARMPRIM1 == "N/A", NA, data$INTHARMPRIM1) - 2),
                                         (ifelse(data$INTHELPPRIM == "N/A", NA, data$INTHELPPRIM) - 2),
                                         (ifelse(data$INTHELPBIS == "N/A", NA, data$INTHELPBIS) - 2)), na.rm=TRUE)



data3$INTENCJONALNOSC_ENV <- ifelse(data3$INTHARMBIS == "N/A", NA, data3$INTHARMBIS)-1


data$I_STARV <- ifelse(data$INTENCJONALNOSC_STARV == 1, "Tak", "Nie")
data$I_ENV <- ifelse(data$INTENCJONALNOSC_ENV == 1, "Tak", "Nie")
data3$I_STARV <- ifelse(data3$INTENCJONALNOSC_STARV == 1, "Tak", "Nie")
data3$I_ENV <- ifelse(data3$INTENCJONALNOSC_ENV == 1, "Tak", "Nie")


data_intentionality <- rbind(
  as.data.frame(cbind(data$GROUP, data$ACTION, data$I_ENV, data$I_STARV)),
  as.data.frame(cbind(data3$GROUP, data3$ACTION, data3$I_ENV, data3$I_STARV)))



attach(data_intentionality)
xtable <-xtabs(~ V3 + V1 + V2, drop.unused.levels = TRUE)
xtable[1:2,1:2,1] <- prop.table(xtable[1:2,1:2,1],2)
xtable[1:2,1:2,2] <- prop.table(xtable[1:2,1:2,2],2)
detach(data_intentionality)

attach(data_intentionality)
xtable2 <-xtabs(~ V4 + V1 + V2, drop.unused.levels = TRUE)
xtable2[1:2,1:2,1] <- prop.table(xtable2[1:2,1:2,1],2)
xtable2[1:2,1:2,2] <- prop.table(xtable2[1:2,1:2,2],2)
detach(data_intentionality)

f1 <- as.data.frame(xtable)
f2 <- as.data.frame(xtable2)

f1$QUESTION <- "Environment"
f2$QUESTION <- "Starvation"
colnames(f2)[1] <- "V3"

data_intentionality_s <- rbind(f1, f2)

data_intentionality_s$V3 <- factor(data_intentionality_s$V3, levels = levels(data_intentionality_s$V3)[c(2,1)])
data_intentionality_s <- data_intentionality_s[order(data_intentionality_s$V3), ]

plot3 <- ggplot(data = data_intentionality_s) + aes(y=Freq, x=QUESTION, fill=V3) + geom_bar(stat="identity") + facet_grid(V1~V2)

  

