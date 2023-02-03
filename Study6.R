#here I am fitting the 10 DARs all together, not separately

library(moveHMM)

tot_col_all <- data.frame()

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
  subData_col$ID <- rep(i, length(subData$TAG))
  subData_col$date <- subData$date
  
  subData_col <- as.data.frame(subData_col)
  tot_col_all <- rbind(tot_col_all, subData_col)
}

dataHMM_all <- prepData(tot_col_all, type = c("LL"), LLangle = TRUE)

set.seed(12345)
# Number of tries with different starting values
niter <- 25
# Save list of fitted models
allm <- list()

for(i in 1:niter) {
  # Step length mean
  stepMean0 <- c(runif(1, min = 0.01, max = 0.1), runif(1, min = 0.2, max = 0.5))
  # Step length standard deviation
  stepSD0 <- c(runif(1, min = 0.01, max = 0.1), runif(1, min = 0.1, max = 0.2))
  
  # Turning angle mean
  angleMean0 <- c(0, 0)
  # Turning angle concentration
  angleCon0 <- runif(2,
                     min = c(0.1, 1),
                     max = c(1, 5))
  # Fit model
  stepPar0 <- c(stepMean0, stepSD0)
  anglePar0 <- c(angleMean0, angleCon0)
  formula <- ~ID_num
  allm[[i]] <- fitHMM(data = dataHMM_all, nbStates = 2, stepPar0 = stepPar0,
                      anglePar0 = anglePar0, formula=~ID)
}

allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))
allnllk

# Index of best fitting model (smallest negative log-likelihood)
whichbest <- which.min(allnllk)
# Best fitting model
mbest <- allm[[whichbest]]
mbest
plot(mbest)

mbest10 <- mbest
plot(mbest, col = c("green", "#F8766D33"))
legend(40, 20, legend=c("CAM 1", "CAM 2"), col=c("red", "green"), lty=1, cex=0.8)

seq <- viterbi(mbest)
