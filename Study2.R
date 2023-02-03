#create a unique set of 10 DARs for fitting later

library(moveHMM)

tot_col <- list()

for (i in 2:11){
  
tot_10 <- mid_to_mid[[i]]
ref_time<-min(tot_10$date)#define the reference time which will be the "start point "zero time"
adptable<-addTimeDateFields (tot_10, "5 min", ref_time) #groups rows by the times' frames

subData <- adptable %>% group_by(min5)%>%
  slice(1)%>%
  ungroup() #subset the data table by the times' frames (one point every 10 min)

subData <- subData[order(as.Date(subData$date, format="%Y/%m/%d")),]

subData_col <- subData[, c(14)]
subData_col$x <- subData$LON
subData_col$y <- subData$LAT
subData_col$ID <- subData$TAG
subData_col$date <- subData$date

subData_col <- as.data.frame(subData_col)
j <- i - 1
tot_col[[j]] <- subData_col
}
