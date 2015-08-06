library(rmarkdown)
username <- commandArgs(trailingOnly = TRUE)[1]
password <- commandArgs(trailingOnly = TRUE)[2]

date <- as.character(date())
dir.create(date)

source("downloadRawData.R")

if (!file.exists("dateLog.csv")) {
  write.csv(as.data.frame(date), file = "dateLog.csv", row.names = FALSE)
  dateLog <- read.csv("dateLog.csv")
} else {
  dateLog <- read.csv("dateLog.csv")
  dateLog <- rbind(as.data.frame(date), dateLog)
  write.csv(dateLog, file = "dateLog.csv", row.names = FALSE)
}

for (d in as.character(dateLog[[1]])) {
  infoTable_date <- read.csv(file.path(d, "info.csv"))
  infoTable_date$date <- d
  if (!exists("infoTable")) {
    infoTable <<- infoTable_date
  } else {
    infoTable <<- rbind(infoTable, infoTable_date)
  }
}

write.csv(infoTable, file ="infoTable.csv")

source("cleanData.R")
source("cleanData2.R")
source("cleanData3.R")
rmarkdown::render(file.path("raportPrzyslowki.Rmd"), output_dir = file.path(date, "raports"))
rmarkdown::render(file.path("raportEfektEpistemiczny.Rmd"), output_dir = file.path(date, "raports"))
rmarkdown::render(file.path("raportWymyslonePrzyslowki.Rmd"), output_dir = file.path(date, "raports"))
rmarkdown::render(file.path("raportWiedzaINormy.Rmd"), output_dir = file.path(date, "raports"))
rmarkdown::render(file.path("raportPostawy.Rmd"), output_dir = file.path(date, "raports"))
rmarkdown::render(file.path("raportKolizjaNorm.Rmd"), output_dir = file.path(date, "raports"))
rmarkdown::render(file.path("info.Rmd"), output_dir = file.path(date, "raports"))
