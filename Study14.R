#BCPA per one individual, one DAR
library(bcpa)

all_breaks_tot <- c()

m_sl <- c()
m_ta <- c()
m_ah <- c()

sd_sl <- c()
sd_ta <- c()
sd_ah <- c()

l <- 1

for (i in c(2)){
print(i)
all_breaks <- c()

data_ind <- mid_to_mid[[i]]

X <- data_ind$X 
Y <- data_ind$Y
Time <- data_ind$dateTime

#need to regularise the data

mytrack <- MakeTrack(X,Y,Time)

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "S", windowsize=50, progress=TRUE, K=2)
c <- ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

owl.ws_sl <- owl.ws
c_sl <- ChangePointSummary(owl.ws)

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "Phi", windowsize=50, progress=TRUE, K=2)
c <- ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

owl.ws_ah <- owl.ws
c_ah <- ChangePointSummary(owl.ws)

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "Theta", windowsize=50, progress=TRUE, K=2)
c <- ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

owl.ws_ta <- owl.ws
c_ta <- ChangePointSummary(owl.ws)

all_breaks <- c(c_sl$breaks$middle, c_ah$breaks$middle, c_ta$breaks$middle)
all_breaks <- sort(all_breaks)

all_breaks_tot <- c(all_breaks_tot, all_breaks)

l <- length(owl.VT$T.end)
all_breaks <- c(all_breaks, owl.VT$T.end[l])

slv <- c()
ahv <- c()
tav <- c()

j <- 1

for (i in 1:length(owl.VT$T.end)){
  if (owl.VT$T.end[i] <= all_breaks[j]){
    slv <- c(slv, owl.VT$S[i])
    ahv <- c(ahv, owl.VT$Phi[i])
    tav <- c(tav, owl.VT$Theta[i])
  }else{

    m_sl <- c(m_sl, mean(slv))
    m_ta <- c(m_ta, mean(ahv))
    m_ah <- c(m_ah, mean(tav))
    
    sd_sl <- c(sd_sl, sd(slv))
    sd_ta <- c(sd_ta, sd(ahv))
    sd_ah <- c(sd_ah, sd(tav))
    
    slv <- c()
    ahv <- c()
    tav <- c()
    j <- j + 1
  }
}
}


