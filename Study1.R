#This code reads the movement data and split them in DARs, for which we have enough data points during the hours of interest (night in this case since nocturnal animal)

#load needed libraries
library(lubridate)
library(tidyverse)
library(toolsForAtlas)

b1 <- load(paste("/Users/ludovicaluisavissat/Desktop/DAR_ms/Owls_tracks/Tracks of GG41312.RData"))
d <- ringTrack

mid_to_mid <- d %>%
  group_by(Night) %>%
  group_split()

#extract the dates for each DARs
as_date_v <- list()
dl <- 1

for (i in 1:length(mid_to_mid)){
  as_date_v[[dl]] <- unique(as.Date(mid_to_mid[[i]]$dateTime))
  dl <- dl + 1
}

#make sure they are consecutive and that there are only two
ind_ok <- vector()

for (i in 1:length(as_date_v)){
  if ((as_date_v[[i]][2] - as_date_v[[i]][1]) == 1 && length(as_date_v[[i]]) == 2){
    ind_ok <- c(ind_ok, i)
  }
}

#select only the ones that start before 9pm and end after 2am
ind_ok_time <- vector()

for (i in ind_ok){
  h1 <- hour(mid_to_mid[[i]]$dateTime[1])
  l <- length(mid_to_mid[[i]]$dateTime)
  hf <- hour(mid_to_mid[[i]]$dateTime[l])
  if (h1 <= 21 && hf >= 2){
    ind_ok_time <- c(ind_ok_time, i)
  }
}


