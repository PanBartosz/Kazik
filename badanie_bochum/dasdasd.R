leaps<-regsubsets(PYTANIE_PIERWSZE~PIERWSZE_OGNIWO+DRUGIE_OGNIWO+SKUTEK+ZMIANA_CZY_BRAK,data=data,nbest=10)

png("plott2.png")
plot(leaps,scale="r2")
dev.off()

data_s1 <- split(data, data$PIERWSZE_OGNIWO)

leaps_dzialanie<-regsubsets(PYTANIE_PIERWSZE~DRUGIE_OGNIWO+SKUTEK+ZMIANA_CZY_BRAK,data=data_s1[[1]],nbest=10)

leaps_dzianie<-regsubsets(PYTANIE_PIERWSZE~DRUGIE_OGNIWO+SKUTEK+ZMIANA_CZY_BRAK,data=data_s1[[2]],nbest=10)

plot(leaps_dzialanie, scale="r2")
plot(leaps_dzianie, scale="r2")

data_s1_dzialanie <- split(data_s1[[1]], data_s1[[1]]$DRUGIE_OGNIWO)
data_s1_dzianie <- split(data_s1[[2]], data_s1[[2]]$DRUGIE_OGNIWO)

leaps_dzialanie_dzialanie<-regsubsets(PYTANIE_PIERWSZE~SKUTEK+ZMIANA_CZY_BRAK,data=data_s1_dzialanie[[1]],nbest=10)
leaps_dzialanie_dzianie<-regsubsets(PYTANIE_PIERWSZE~SKUTEK+ZMIANA_CZY_BRAK,data=data_s1_dzialanie[[2]],nbest=10)

plot(leaps_dzialanie_dzialanie, scale="r2")
plot(leaps_dzialanie_dzianie, scale="r2")


#############

data_s1 <- split(data, data$DRUGIE_OGNIWO)

leaps_dzialanie<-regsubsets(PYTANIE_PIERWSZE~PIERWSZE_OGNIWO+SKUTEK+ZMIANA_CZY_BRAK,data=data_s1[[1]],nbest=10)

leaps_dzianie<-regsubsets(PYTANIE_PIERWSZE~PIERWSZE_OGNIWO+SKUTEK+ZMIANA_CZY_BRAK,data=data_s1[[2]],nbest=10)

plot(leaps_dzialanie, scale="r2")
plot(leaps_dzianie, scale="r2")

data_s1_dzialanie <- split(data_s1[[1]], data_s1[[1]]$PIERWSZE_OGNIWO)
data_s1_dzianie <- split(data_s1[[2]], data_s1[[2]]$PIERWSZE_OGNIWO)

leaps_dzialanie_dzialanie<-regsubsets(PYTANIE_PIERWSZE~SKUTEK+ZMIANA_CZY_BRAK,data=data_s1_dzialanie[[1]],nbest=10)
leaps_dzialanie_dzianie<-regsubsets(PYTANIE_PIERWSZE~SKUTEK+ZMIANA_CZY_BRAK,data=data_s1_dzialanie[[2]],nbest=10)

plot(leaps_dzialanie_dzialanie, scale="r2")
plot(leaps_dzialanie_dzianie, scale="r2")


#### !!!!!!!
leaps_dzianie<-regsubsets(PYTANIE_DRUGIE~DRUGIE_OGNIWO+SKUTEK,data=data_s1[[2]][which(data_s1[[2]]$ZMIANA_CZY_BRAK == "Zmiana"),],nbest=10)










Anova(glm(PYTANIE_PIERWSZE ~ (PIERWSZE_OGNIWO*SKUTEK*ZMIANA_CZY_BRAK*DRUGIE_OGNIWO), data=data, contrasts = c("contr.sum")), type = 2)
Anova(glm(PYTANIE_PIERWSZE ~ (SKUTEK*PIERWSZE_OGNIWO*ZMIANA_CZY_BRAK*DRUGIE_OGNIWO), data=data, contrasts = c("contr.sum")), type = 2)
Anova(glm(PYTANIE_PIERWSZE ~ (ZMIANA_CZY_BRAK*SKUTEK*PIERWSZE_OGNIWO*DRUGIE_OGNIWO), data=data, contrasts = c("contr.sum")), type = 2)
Anova(glm(PYTANIE_PIERWSZE ~ (DRUGIE_OGNIWO*ZMIANA_CZY_BRAK*SKUTEK*PIERWSZE_OGNIWO), data=data, contrasts = c("contr.sum")), type = 2)

