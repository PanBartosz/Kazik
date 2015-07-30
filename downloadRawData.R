
# Zadania programu:
# 1. Połączyć się za pomocą API do Limesurveya na serwerze Kognilabu 
# 2. Pobrać pliki ankiet za pomocą funkcji getRawData:
#   2.1. Funkcja przyjmować będzie dwa argumenty - vector z SIDów (integer) i ścieżkę
#   2.2. Ścieżką standardową będzie "/rawData/"

library(XMLRPC)
library(base64enc)
library(plyr)

getRawData <- function(SIDS){
  url <- "http://kognilab.pl/lime/index.php/admin/remotecontrol"
  key <- xml.rpc(url = url, method = "get_session_key", username, password)
  dir.create(file.path(date, "rawData"))
  for (sid in SIDS){
    results <- xml.rpc(url = url,
                       method = "export_responses", # Eksport odpowiedzi
                       key, # Klucz sesji
                       as.integer(sid), 
                       "csv", # Format
                       "pl", # Język
                       "all" # Wszystkie odpowiedzi 
    )                
    resultsbinary <- base64decode(what = results) # Odkodowanie do postaci binarnej
    writeBin(resultsbinary, file.path(date, "rawData", 
                                      paste(as.character(sid),
                                            ".csv", sep=""))) # Zapisywanie
  }
}


getSurveyInfo <- function(SIDS) {
  url <- "http://kognilab.pl/lime/index.php/admin/remotecontrol"
  key <- xml.rpc(url = url, method = "get_session_key", username, password)
  surveyInfo <- xml.rpc(url = url,
                        method = "list_surveys", # Eksport odpowiedzi
                        key # Klucz sesji
  )
  ## Magia ze stackoverflow, nawet nie pytać jak to działa
  surveyInfo<- rbind.fill(lapply(surveyInfo, function(f) {
    as.data.frame(Filter(Negate(is.null), f))
  }))
  surveyInfoClean <- surveyInfo[which(surveyInfo$sid %in% SIDS),]
  
  completed <- integer()
  all <- integer()
  
for (sid in SIDS){
    
    data <- read.csv(file.path(date, "rawData", paste(as.character(sid), ".csv", sep = "")))
    completed <- c(completed, nrow(data[data$lastpage == max(data$lastpage, na.rm = TRUE),]))
    all <- c(all, nrow(data))
}
  surveyInfoClean <- cbind(surveyInfoClean, completed, all)
  write.csv(surveyInfoClean, file = file.path(date, "info.csv"))
  
}





source("surveyList.R")


getRawData(surveyList)
getSurveyInfo(surveyList)
