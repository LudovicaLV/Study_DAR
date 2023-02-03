#BCPA third option

owl.VT <- GetVT(mytrack)
owl.ws <- WindowSweep(owl.VT, "Theta", windowsize=50, progress=TRUE, K=2)
c <- ChangePointSummary(owl.ws, clusterwidth=5)
PathPlot(mytrack, owl.ws, type="flat", clusterwidth = 5, main="Flat BCPA")
PhasePlot(owl.ws, clusterwidth = 5)

owl.ws_ta <- owl.ws
c_ta <- ChangePointSummary(owl.ws)

plot(owl.ws, type="flat")
plot(owl.ws, type="flat", clusterwidth=5)