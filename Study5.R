#using BCPA library to extract CAMs

library(bcpa)
## Loading required package: Rcpp

X <- tot_col_all$x
Y <- tot_col_all$y
Time <- tot_col_all$dateTime

mytrack <- MakeTrack(X,Y,Time)
plot(mytrack)

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "V*cos(Theta)", windowsize=50, progress=FALSE, K=2)
ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

plot(owl.ws, type="flat")
plot(owl.ws, type="flat", clusterwidth=5)

