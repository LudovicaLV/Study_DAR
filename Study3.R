#Using moveHMM function to fit the sequence 

best_10 <- list()
seq_10 <- list()

for (j in 1:10){
  print(j)
  
  subData_col <- as.data.frame(tot_col[[j]])
  dataHMM <- prepData(subData_col, type = c("LL"), LLangle = TRUE)
  
  set.seed(12345)
  # Number of tries with different starting values
  niter <- 25
  # Save list of fitted models
  allm <- list()
  
  for(i in 1:niter) {
    print(i)
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
    allm[[i]] <- fitHMM(data = dataHMM, nbStates = 2, stepPar0 = stepPar0,
                        anglePar0 = anglePar0)
  }
  
  allnllk <- unlist(lapply(allm, function(m) m$mod$minimum))
  allnllk
  
  # Index of best fitting model (smallest negative log-likelihood)
  whichbest <- which.min(allnllk)
  # Best fitting model
  mbest <- allm[[whichbest]]
  mbest
  #plot(mbest)
  
  best_10[[j]] <- mbest
  seq_10[[j]] <- viterbi(mbest)
}
