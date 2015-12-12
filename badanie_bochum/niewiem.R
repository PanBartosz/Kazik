chisq.test(as.factor(
  c(agency_data[which(agency_data$V1 =="Harm" & agency_data$V2 =="Introduced" &agency_data$PYTANIE ==5),]$V3,
                     agency_data[which(agency_data$V1 =="Help" & agency_data$V2 =="Not introduced" &agency_data$PYTANIE == 5),]$V3
    )
  ) ,
                     as.factor(
                       c(
                         rep("A", length(agency_data[which(agency_data$V1 =="Harm" & agency_data$V2 =="Introduced" & agency_data$PYTANIE ==5),]$V3)),
                         rep("B", length(agency_data[which(agency_data$V1 =="Help" & agency_data$V2 =="Not introduced" & agency_data$PYTANIE == 5),]$V3))
                       )))
                         

chisq.test(as.factor(
  c(agency_data[which(agency_data$V1 =="Help" & agency_data$V2 =="Introduced"),]$V3,
    agency_data[which(agency_data$V1 =="Harm" & agency_data$V2 =="Not introduced"),]$V3
  )
) ,
as.factor(
  c(
    rep("A", length(agency_data[which(agency_data$V1 =="Help" & agency_data$V2 =="Introduced"),]$V3)),
    rep("B", length(agency_data[which(agency_data$V1 =="Harm" & agency_data$V2 =="Not introduced"),]$V3))
  )))
