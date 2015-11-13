library(shiny)
library(ggplot2)
library(scales)

data <- read.csv("wyniki.csv")

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

f_data <- data

shinyServer(
  function(input, output) {

    output$model_pierwszy <- renderTable({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## DŁUGI I BRZYDKI KOD 
      
      ## 4
      if(input$factor_zmiana & input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo & input$factor_skutek_moralny)
      {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
      summary(model_pierwsze)
      } else
        
        ## 3
        if(input$factor_zmiana & input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo )
        {model_pierwsze <- glm(PYTANIE_PIERWSZE ~  ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
        summary(model_pierwsze)
        } else
          
          if(input$factor_zmiana & input$factor_pierwsze_ogniwo & input$factor_skutek_moralny)
          {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
          summary(model_pierwsze)
          } else
            
            if(input$factor_zmiana  & input$factor_drugie_ogniwo & input$factor_skutek_moralny)
            {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
            summary(model_pierwsze)
            } else
              
              if(input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo & input$factor_skutek_moralny)
              {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
              summary(model_pierwsze)
              } else
                
                ## 2
                
                
                if(input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo)
                {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
                summary(model_pierwsze)
                } else
                  
                  if(input$factor_pierwsze_ogniwo & input$factor_skutek_moralny)
                  {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + PIERWSZE_OGNIWO, data = f_data)
                  summary(model_pierwsze)
                  } else
                    
                    if(input$factor_drugie_ogniwo & input$factor_skutek_moralny)
                    {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + DRUGIE_OGNIWO, data = f_data)
                    summary(model_pierwsze)
                    } else
                      
                      if(input$factor_pierwsze_ogniwo & input$factor_zmiana)
                      {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
                      summary(model_pierwsze)} else
                        
                        if(input$factor_drugie_ogniwo & input$factor_zmiana)
                        {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
                        summary(model_pierwsze)
                        } else
                          
                          if(input$factor_skutek_moralny & input$factor_zmiana)
                          {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK + SKUTEK, data = f_data)
                          summary(model_pierwsze)
                          } else
                            
                            ## 1 
                            
                            if(input$factor_pierwsze_ogniwo)
                            {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ PIERWSZE_OGNIWO, data = f_data)
                            summary(model_pierwsze)
                            } else
                              
                              if(input$factor_drugie_ogniwo)
                              {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ DRUGIE_OGNIWO, data = f_data)
                              summary(model_pierwsze)
                              } else
                                
                                if(input$factor_zmiana)
                                {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK, data = f_data)
                                summary(model_pierwsze)
                                } else
                                  
                                  if(input$factor_skutek_moralny)
                                  {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK, data = f_data)
                                  summary(model_pierwsze)
                                  } 
    }
    )
    output$srednia_pierwsza <- renderTable({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      table(mean(f_data$PYTANIE_PIERWSZE))
    }
    )
    output$odchylenie_pierwsze <- renderText({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      sd(f_data$PYTANIE_PIERWSZE)
    }
    )
    
    output$model_drugi <- renderTable({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## DŁUGI I BRZYDKI KOD 
      
      ## 4
      if(input$factor_zmiana & input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo & input$factor_skutek_moralny)
      {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
      summary(model_pierwsze)
      } else
        
        ## 3
        if(input$factor_zmiana & input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo )
        {model_pierwsze <- glm(PYTANIE_DRUGIE ~  ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
        summary(model_pierwsze)
        } else
          
          if(input$factor_zmiana & input$factor_pierwsze_ogniwo & input$factor_skutek_moralny)
          {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
          summary(model_pierwsze)
          } else
            
            if(input$factor_zmiana  & input$factor_drugie_ogniwo & input$factor_skutek_moralny)
            {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
            summary(model_pierwsze)
            } else
              
              if(input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo & input$factor_skutek_moralny)
              {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
              summary(model_pierwsze)
              } else
                
                ## 2
                
                
                if(input$factor_pierwsze_ogniwo & input$factor_drugie_ogniwo)
                {model_pierwsze <- glm(PYTANIE_DRUGIE ~ PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
                summary(model_pierwsze)
                } else
                  
                  if(input$factor_pierwsze_ogniwo & input$factor_skutek_moralny)
                  {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + PIERWSZE_OGNIWO, data = f_data)
                  summary(model_pierwsze)
                  } else
                    
                    if(input$factor_drugie_ogniwo & input$factor_skutek_moralny)
                    {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + DRUGIE_OGNIWO, data = f_data)
                    summary(model_pierwsze)
                    } else
                      
                      if(input$factor_pierwsze_ogniwo & input$factor_zmiana)
                      {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
                      summary(model_pierwsze)} else
                        
                        if(input$factor_drugie_ogniwo & input$factor_zmiana)
                        {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
                        summary(model_pierwsze)
                        } else
                          
                          if(input$factor_skutek_moralny & input$factor_zmiana)
                          {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK + SKUTEK, data = f_data)
                          summary(model_pierwsze)
                          } else
                            
                            ## 1 
                            
                            if(input$factor_pierwsze_ogniwo)
                            {model_pierwsze <- glm(PYTANIE_DRUGIE ~ PIERWSZE_OGNIWO, data = f_data)
                            summary(model_pierwsze)
                            } else
                              
                              if(input$factor_drugie_ogniwo)
                              {model_pierwsze <- glm(PYTANIE_DRUGIE ~ DRUGIE_OGNIWO, data = f_data)
                              summary(model_pierwsze)
                              } else
                                
                                if(input$factor_zmiana)
                                {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK, data = f_data)
                                summary(model_pierwsze)
                                } else
                                  
                                  if(input$factor_skutek_moralny)
                                  {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK, data = f_data)
                                  summary(model_pierwsze)
                                  } 
    }
    )
    
    output$srednia_druga <- renderTable({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      table(mean(f_data$PYTANIE_DRUGIE))
    }
    )
    
    output$odchylenie_drugie <- renderText({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      sd(f_data$PYTANIE_DRUGIE)
    })
    
    output$odpowiedzialny <- renderTable({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## POKAŻ ODPOWIEDZIALNOŚĆ
      if(input$osoba == "chlopiec"){
        table(droplevels(f_data$ODPOWIEDZIALNOSC_CHLOPCA))
      } else
      
      if(input$osoba == "dozorca"){
        table(droplevels(f_data$ODPOWIEDZIALNOSC_DOZORCY))
      } else
      if(input$osoba == "malinowska"){
        table(droplevels(f_data$ODPOWIEDZIALNOSC_MALINOWSKIEJ))
      } else
      
      if(input$osoba == "robotnicy"){
        table(droplevels(f_data$ODPOWIEDZIALNOSC_ROBOTNIKÓW))
      } else
      
      if(input$osoba == "mieszkancy"){
        table(droplevels(f_data$ODPOWIEDZIALNOSC_MIESZKANCOW))
      } 
    }
    )
    
    output$odpowiedzialny_2 <- renderTable({
      f_data <- data
      
      if(!input$dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## POKAŻ ODPOWIEDZIALNOŚĆ
      if(input$osoba == "chlopiec"){
        prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_CHLOPCA)))*100
      } else
        
        if(input$osoba == "dozorca"){
          prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_DOZORCY)))*100
        } else
          if(input$osoba == "malinowska"){
            prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_MALINOWSKIEJ)))*100
          } else
            
            if(input$osoba == "robotnicy"){
              prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_ROBOTNIKÓW)))*100
            } else
              
              if(input$osoba == "mieszkancy"){
                prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_MIESZKANCOW)))*100
              } 
    }
    )
    
    
    
    
    
    output$D_model_pierwszy <- renderTable({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## DŁUGI I BRZYDKI KOD 
      
      ## 4
      if(input$D_factor_zmiana & input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
      {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
      summary(model_pierwsze)
      } else
        
        ## 3
        if(input$D_factor_zmiana & input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo )
        {model_pierwsze <- glm(PYTANIE_PIERWSZE ~  ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
        summary(model_pierwsze)
        } else
          
          if(input$D_factor_zmiana & input$D_factor_pierwsze_ogniwo & input$D_factor_skutek_moralny)
          {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
          summary(model_pierwsze)
          } else
            
            if(input$D_factor_zmiana  & input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
            {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
            summary(model_pierwsze)
            } else
              
              if(input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
              {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
              summary(model_pierwsze)
              } else
                
                ## 2
                
                
                if(input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo)
                {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
                summary(model_pierwsze)
                } else
                  
                  if(input$D_factor_pierwsze_ogniwo & input$D_factor_skutek_moralny)
                  {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + PIERWSZE_OGNIWO, data = f_data)
                  summary(model_pierwsze)
                  } else
                    
                    if(input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
                    {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK + DRUGIE_OGNIWO, data = f_data)
                    summary(model_pierwsze)
                    } else
                      
                      if(input$D_factor_pierwsze_ogniwo & input$D_factor_zmiana)
                      {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
                      summary(model_pierwsze)} else
                        
                        if(input$D_factor_drugie_ogniwo & input$D_factor_zmiana)
                        {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
                        summary(model_pierwsze)
                        } else
                          
                          if(input$D_factor_skutek_moralny & input$D_factor_zmiana)
                          {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK + SKUTEK, data = f_data)
                          summary(model_pierwsze)
                          } else
                            
                            ## 1 
                            
                            if(input$D_factor_pierwsze_ogniwo)
                            {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ PIERWSZE_OGNIWO, data = f_data)
                            summary(model_pierwsze)
                            } else
                              
                              if(input$D_factor_drugie_ogniwo)
                              {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ DRUGIE_OGNIWO, data = f_data)
                              summary(model_pierwsze)
                              } else
                                
                                if(input$D_factor_zmiana)
                                {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ ZMIANA_CZY_BRAK, data = f_data)
                                summary(model_pierwsze)
                                } else
                                  
                                  if(input$D_factor_skutek_moralny)
                                  {model_pierwsze <- glm(PYTANIE_PIERWSZE ~ SKUTEK, data = f_data)
                                  summary(model_pierwsze)
                                  } 
    }
    )
    output$D_srednia_pierwsza <- renderTable({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      table(mean(f_data$PYTANIE_PIERWSZE))
    }
    )
    output$D_odchylenie_pierwsze <- renderText({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      sd(f_data$PYTANIE_PIERWSZE)
    }
    )
    
    output$D_model_drugi <- renderTable({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## DŁUGI I BRZYDKI KOD 
      
      ## 4
      if(input$D_factor_zmiana & input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
      {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
      summary(model_pierwsze)
      } else
        
        ## 3
        if(input$D_factor_zmiana & input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo )
        {model_pierwsze <- glm(PYTANIE_DRUGIE ~  ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
        summary(model_pierwsze)
        } else
          
          if(input$D_factor_zmiana & input$D_factor_pierwsze_ogniwo & input$D_factor_skutek_moralny)
          {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
          summary(model_pierwsze)
          } else
            
            if(input$D_factor_zmiana  & input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
            {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
            summary(model_pierwsze)
            } else
              
              if(input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
              {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
              summary(model_pierwsze)
              } else
                
                ## 2
                
                
                if(input$D_factor_pierwsze_ogniwo & input$D_factor_drugie_ogniwo)
                {model_pierwsze <- glm(PYTANIE_DRUGIE ~ PIERWSZE_OGNIWO + DRUGIE_OGNIWO, data = f_data)
                summary(model_pierwsze)
                } else
                  
                  if(input$D_factor_pierwsze_ogniwo & input$D_factor_skutek_moralny)
                  {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + PIERWSZE_OGNIWO, data = f_data)
                  summary(model_pierwsze)
                  } else
                    
                    if(input$D_factor_drugie_ogniwo & input$D_factor_skutek_moralny)
                    {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK + DRUGIE_OGNIWO, data = f_data)
                    summary(model_pierwsze)
                    } else
                      
                      if(input$D_factor_pierwsze_ogniwo & input$D_factor_zmiana)
                      {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK + PIERWSZE_OGNIWO, data = f_data)
                      summary(model_pierwsze)} else
                        
                        if(input$D_factor_drugie_ogniwo & input$D_factor_zmiana)
                        {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK + DRUGIE_OGNIWO, data = f_data)
                        summary(model_pierwsze)
                        } else
                          
                          if(input$D_factor_skutek_moralny & input$D_factor_zmiana)
                          {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK + SKUTEK, data = f_data)
                          summary(model_pierwsze)
                          } else
                            
                            ## 1 
                            
                            if(input$D_factor_pierwsze_ogniwo)
                            {model_pierwsze <- glm(PYTANIE_DRUGIE ~ PIERWSZE_OGNIWO, data = f_data)
                            summary(model_pierwsze)
                            } else
                              
                              if(input$D_factor_drugie_ogniwo)
                              {model_pierwsze <- glm(PYTANIE_DRUGIE ~ DRUGIE_OGNIWO, data = f_data)
                              summary(model_pierwsze)
                              } else
                                
                                if(input$D_factor_zmiana)
                                {model_pierwsze <- glm(PYTANIE_DRUGIE ~ ZMIANA_CZY_BRAK, data = f_data)
                                summary(model_pierwsze)
                                } else
                                  
                                  if(input$D_factor_skutek_moralny)
                                  {model_pierwsze <- glm(PYTANIE_DRUGIE ~ SKUTEK, data = f_data)
                                  summary(model_pierwsze)
                                  } 
    }
    )
    
    output$D_srednia_druga <- renderTable({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      table(mean(f_data$PYTANIE_DRUGIE))
    }
    )
    
    output$D_odchylenie_drugie <- renderText({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      sd(f_data$PYTANIE_DRUGIE)
    })
    
    output$D_odpowiedzialny <- renderTable({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## POKAŻ ODPOWIEDZIALNOŚĆ
      if(input$D_osoba == "chlopiec"){
        table(droplevels(f_data$ODPOWIEDZIALNOSC_CHLOPCA))
      } else
        
        if(input$D_osoba == "dozorca"){
          table(droplevels(f_data$ODPOWIEDZIALNOSC_DOZORCY))
        } else
          if(input$D_osoba == "malinowska"){
            table(droplevels(f_data$ODPOWIEDZIALNOSC_MALINOWSKIEJ))
          } else
            
            if(input$D_osoba == "robotnicy"){
              table(droplevels(f_data$ODPOWIEDZIALNOSC_ROBOTNIKÓW))
            } else
              
              if(input$D_osoba == "mieszkancy"){
                table(droplevels(f_data$ODPOWIEDZIALNOSC_MIESZKANCOW))
              } 
    }
    )
    
    output$D_odpowiedzialny_2 <- renderTable({
      f_data <- data
      
      if(!input$D_dzialanie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Działanie"),]
      }
      if(!input$D_dzialanie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Działanie"),]
      }
      
      if(!input$D_dzianie_sie_1){
        f_data <- f_data[which(f_data$PIERWSZE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_dzianie_sie_2){
        f_data <- f_data[which(f_data$DRUGIE_OGNIWO != "Dzianie się"),]
      }
      
      if(!input$D_pozytywny){
        f_data <- f_data[which(f_data$SKUTEK != "Pozytywny"),]
      }
      if(!input$D_neutralny){
        f_data <- f_data[which(f_data$SKUTEK != "Neutralny"),]
      }
      if(!input$D_negatywny){
        f_data <- f_data[which(f_data$SKUTEK != "Negatywny"),]
      }    
      if(!input$D_zmiana){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Zmiana"),]
      }    
      if(!input$D_brak_zmiany){
        f_data <- f_data[which(f_data$ZMIANA_CZY_BRAK != "Brak zmiany"),]
      }   
      
      ## POKAŻ ODPOWIEDZIALNOŚĆ
      if(input$D_osoba == "chlopiec"){
        prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_CHLOPCA)))*100
      } else
        
        if(input$D_osoba == "dozorca"){
          prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_DOZORCY)))*100
        } else
          if(input$D_osoba == "malinowska"){
            prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_MALINOWSKIEJ)))*100
          } else
            
            if(input$D_osoba == "robotnicy"){
              prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_ROBOTNIKÓW)))*100
            } else
              
              if(input$D_osoba == "mieszkancy"){
                prop.table(table(droplevels(f_data$ODPOWIEDZIALNOSC_MIESZKANCOW)))*100
              } 
    }
    )
    
    
    
        
  }
)




