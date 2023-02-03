#BCPA per one individual, one DAR

library(bcpa)

data_ind <- mid_to_mid[[2]]

for (i in 3:10){
  data_ind <- rbind(data_ind, mid_to_mid[[i]])
}

X <- data_ind$X 
Y <- data_ind$Y
Time <- data_ind$dateTime

#need to regularise the data

mytrack <- MakeTrack(X,Y,Time)
plot(mytrack)

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "V*cos(Theta/mean_sl)", windowsize=50, progress=TRUE, K=2)
c <- ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

plot(owl.ws, type="flat")
plot(owl.ws, type="flat", clusterwidth=5)


