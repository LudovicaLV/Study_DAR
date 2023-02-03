#BCPA second option

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "Phi", windowsize=50, progress=TRUE, K=2)
c <- ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

owl.ws_ah <- owl.ws
c_ah <- ChangePointSummary(owl.ws)

plot(owl.ws, type="flat")
plot(owl.ws, type="flat", clusterwidth=5)