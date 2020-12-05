#Across the United States, which types of events (as indicated in the |EVTYPE|}E) are most harmful with respect to population health?
# the United States, which types of events have the greatest economic consequences?


input_db <- read.csv("repdata_data_StormData.csv.bz2")

library(lubridate)
library(ggplot2)
library(tidyr)


db <- input_db
db$BGN_DATE <- mdy_hms(db$BGN_DATE)
db$END_DATE <- mdy_hms(db$END_DATE)
year_BGN <- year(db$BGN_DATE)
year_END <- year(db$END_DATE)
db <- cbind(db, year_BGN, year_END)

#FATALITIES
total_FATALITIES <- aggregate(FATALITIES ~ EVTYPE, data=db, sum)
total_FATALITIES <- total_FATALITIES[order(-total_FATALITIES$FATALITIES) , ]
total_FATALITIES_top20 <- total_FATALITIES[1:20 ,]
ggplot(total_FATALITIES_top20, aes(x = reorder(EVTYPE, -FATALITIES),  y = FATALITIES)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) + labs(x = "Events", y="total number of fatalities")
     

#INJURES
total_INJURIES <- aggregate(INJURIES ~ EVTYPE, data=db, sum)
total_INJURIES <- total_INJURIES[order(-total_INJURIES$INJURIES) , ]
total_INJURIES_top20 <- total_INJURIES[1:20 ,]
ggplot(total_INJURIES_top20, aes(x = reorder(EVTYPE, -INJURIES),  y = INJURIES)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) +labs(x = "Events", y="total number of injuries")

#greatest economic consequences
PROPDMG_M <- vector()
for (i in c(1:length(db$BGN_DATE))) {
      if (db$PROPDMGEXP[i] == "K") {PROPDMG_M[i] = db$PROPDMG[i] / 1000}
  else PROPDMG_M[i] = db$PROPDMG[i]
}
CROPDMG_M <- vector()
for (i in c(1:length(db$BGN_DATE))) {
  if (db$CROPDMGEXP[i] == "K") {CROPDMG_M[i] = db$CROPDMG[i] / 1000}
  else CROPDMG_M[i] = db$CROPDMG[i]
}

db_ECO <- cbind(db, PROPDMG_M, CROPDMG_M)  

#prop
total_PROP <- aggregate(PROPDMG_M ~ EVTYPE, data=db_ECO, sum)
total_PROP <- total_PROP[order(-total_PROP$PROPDMG_M) , ]
total_PROP_top20 <- total_PROP[1:20 ,]
ggplot(total_PROP_top20, aes(x = reorder(EVTYPE, -PROPDMG_M),  y = PROPDMG_M)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

#crop
total_CROP <- aggregate(CROPDMG_M ~ EVTYPE, data=db_ECO, sum)
total_CROP <- total_CROP[order(-total_CROP$CROPDMG_M) , ]
total_CROP_top20 <- total_CROP[1:20 ,]
ggplot(total_CROP_top20, aes(x = reorder(EVTYPE, -CROPDMG_M),  y = CROPDMG_M)) + geom_col() + theme(axis.text.x = element_text(angle = 90))

#total

total <- aggregate(CROPDMG_M+PROPDMG_M ~ EVTYPE, data=db_ECO, sum)
total <- total[order(-total$`CROPDMG_M + PROPDMG_M`) , ]
total_top20 <- total[1:20 ,]
ggplot(total_top20, aes(x = reorder(EVTYPE, -`CROPDMG_M + PROPDMG_M`),  y = `CROPDMG_M + PROPDMG_M`)) + geom_col() + theme(axis.text.x = element_text(angle = 90)) +labs(x = "Events", y="total cost in M$")


