
# Zadania programu:
# 1. Połączyć się za pomocą API do Limesurveya na serwerze Kognilabu 
# 2. Pobrać pliki ankiet za pomocą funkcji getRawData:
#   2.1. Funkcja przyjmować będzie dwa argumenty - vector z SIDów (integer) i ścieżkę
#   2.2. Ścieżką standardową będzie "/rawData/"

getRawData <- function(SIDS){
  library(XMLRPC)
  library(base64enc)
  url <- "http://kognilab.pl/lime/index.php/admin/remotecontrol"
  key <- xml.rpc(url = url, method = "get_session_key", "admin", "nietakszybko")
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

source("surveyList.R")

getRawData(surveyList)
