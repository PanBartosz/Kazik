library(shiny)
library(ggplot2)
library(scales)
library(rCharts)
options(RCHART_WIDTH = 1000)

data <- read.csv("cleanData28553.csv")
adverbs <-c("Specjalnie", "Świadomie", "Celowo", "Umyślnie", "Brak przysłówka",
            "Chciał", "Z rozmysłem", "Zamierzał")
data <- data[which(data$ADVERB %in% adverbs),]
data<- data[which(data$RESPONSE !=0),]
attach(data)
xtable <-xtabs(~ RESPONSE + GROUP + ADVERB, drop.unused.levels = TRUE)
xtable[1:2,1:2,1] <- prop.table(xtable[1:2,1:2,1],2)
xtable[1:2,1:2,2] <- prop.table(xtable[1:2,1:2,2],2)
xtable[1:2,1:2,3] <- prop.table(xtable[1:2,1:2,3],2)
xtable[1:2,1:2,4] <- prop.table(xtable[1:2,1:2,4],2)
xtable[1:2,1:2,5] <- prop.table(xtable[1:2,1:2,5],2)
xtable[1:2,1:2,6] <- prop.table(xtable[1:2,1:2,6],2)
xtable[1:2,1:2,7] <- prop.table(xtable[1:2,1:2,7],2)
xtable[1:2,1:2,8] <- prop.table(xtable[1:2,1:2,8],2)
detach(data)
DATA <- as.data.frame(xtable)
DATA$RESPONSE <- factor(DATA$RESPONSE, levels = levels(DATA$RESPONSE)[c(1,2)])
DATA <- DATA[order(DATA$RESPONSE), ]
DATA$RESPONSE <- factor(DATA$RESPONSE,levels(DATA$RESPONSE)[c(2,1)])

data1 <- read.csv("cleanData28553.csv")
data2 <- read.csv("cleanIgnacySlawek2169464.csv")
data3 <- read.csv("cleanData249562.csv")
data1 <- data1[,c(1,2,6,7,8,9,10,11,12,13)]
data2 <- data2[,c(1,2,6,7,8,9,10,12,13,14)]
data3 <- data3[,c(1,2,6,7,8,9,10,12,13,14)]
edata <- rbind(data1, data2, data3)
edata <- edata[which(edata$RESPONSE !=0),]
e_modifiers <- c("Wiedział & Kauzalna", "Wiedział & Agentywna", 
                 "Wiedział & Stan", "Sądził & Kauzalna", "Sądził & Agentywna",
                 "Sądził & Stan")
edata <- edata[which(edata$ADVERB %in% e_modifiers),]
attach(edata)
extable <-xtabs(~ RESPONSE + GROUP + ADVERB, drop.unused.levels = TRUE)
extable[1:2,1:2,1] <- prop.table(extable[1:2,1:2,1],2)
extable[1:2,1:2,2] <- prop.table(extable[1:2,1:2,2],2)
extable[1:2,1:2,3] <- prop.table(extable[1:2,1:2,3],2)
extable[1:2,1:2,4] <- prop.table(extable[1:2,1:2,4],2)
extable[1:2,1:2,5] <- prop.table(extable[1:2,1:2,5],2)
extable[1:2,1:2,6] <- prop.table(extable[1:2,1:2,6],2)
detach(edata)
EDATA <- as.data.frame(extable)
EDATA$RESPONSE <- factor(EDATA$RESPONSE, levels = levels(EDATA$RESPONSE)[c(1,2)])
EDATA <- EDATA[order(EDATA$RESPONSE), ]
EDATA$RESPONSE <- factor(EDATA$RESPONSE,levels(EDATA$RESPONSE)[c(2,1)])

shinyServer(
  function(input, output) {
    
    output$barchart <- renderChart2({
      if (input$tab == "intentionality") {
      filter <- c(input$specjalnie, 
                  input$swiadomie,
                  input$celowo,
                  input$umyslnie,
                  input$brak,
                  input$chcial,
                  input$zrozmyslem,
                  input$zamierzal) 
      adverbfilter <- adverbs[filter]
      DATA <- DATA[DATA$ADVERB %in% adverbfilter, ]
      if (input$present == "imodifiers") {
        rplot <- rPlot(Freq ~ ADVERB | GROUP, color = 'RESPONSE', data = DATA, type = "bar")
        rplot$facet(cols=1)
        rplot$guides(x = list(numticks = length(filter)))
        
        rplot
        } else {
          colnames(DATA)[3] <- "W"
          rplot2 <- rPlot(Freq ~ GROUP | W, color = 'RESPONSE', data = DATA, type = "bar")
          rplot2$facet(cols=length(filter[filter == TRUE])
                       )
          rplot2$guides(x = list(numticks = length(filter)))
          rplot2
        }
      
    } else {
      efilter <- c(input$wiedziec & input$kauzalna,
                  input$wiedziec & input$agentywna,
                  input$wiedziec & input$stan,
                  input$sadzic & input$kauzalna,
                  input$sadzic & input$agentywna,
                  input$sadzic & input$stan
      )
      epistemicfilter <- e_modifiers[efilter]
      EDATA <- EDATA[EDATA$ADVERB %in% epistemicfilter, ]
      if (input$present2 == "emodifiers") {
        rplot <- rPlot(Freq ~ ADVERB | GROUP, color = 'RESPONSE', data = EDATA, type = "bar")
        rplot$facet(cols=1)
        rplot$guides(x = list(numticks = length(epistemicfilter)))
        return(rplot)
              } else {
                colnames(EDATA)[3] <- "W"
                rplot2 <- rPlot(Freq ~ GROUP | W, color = 'RESPONSE', data = EDATA, type = "bar")
                rplot2$facet(cols=length(efilter[efilter==TRUE]))
                rplot2$guides(x = list(numticks = length(epistemicfilter)))
                return(rplot2)
              }
      
      }})
        
      
      
  }
)




