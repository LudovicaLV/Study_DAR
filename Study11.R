#stats of various segments

all_breaks <- c(c_sl$breaks$middle, c_ah$breaks$middle, c_ta$breaks$middle)

all_breaks <- sort(all_breaks)

l <- length(owl.VT$T.end)
all_breaks <- c(all_breaks, owl.VT$T.end[l])

sl_l <- list()
ta_l <- list()
ah_l <- list()

lb <- length(all_breaks) 

for (j in 1:lb){
    sl_l[[j]] <- vector()
    ta_l[[j]] <- vector()
    ah_l[[j]] <- vector()
}

j <- 1
for (i in 1:length(owl.VT$T.end)){
  if (owl.VT$T.end[i] <= all_breaks[j]){
    sl_l[[j]] <- c(sl_l[[j]], owl.VT$S[i])
    ah_l[[j]] <- c(ah_l[[j]], owl.VT$Phi[i])
    ta_l[[j]] <- c(ta_l[[j]], owl.VT$Theta[i])
  }else{
    j <- j + 1
  }
}

m_sl <- c()
m_ta <- c()
m_ah <- c()

v_sl <- c()
v_ta <- c()
v_ah <- c()

for (j in 1: length(ta_l)){
  m_sl <- c(m_sl, mean(sl_l[[j]]))
  m_ah <- c(m_ah, mean(ah_l[[j]]))
  m_ta <- c(m_ta, mean(ta_l[[j]]))
  
  v_sl <- c(v_sl, var(sl_l[[j]]))
  v_ta <- c(v_ta, var(ah_l[[j]]))
  v_ah <- c(v_ah, var(ta_l[[j]]))
}

par(mfrow = c(3,1))
plot(m_sl, ylim = c(-2,10))
points(m_sl + v_sl, col = "blue")
points(m_sl - v_sl, col = "blue")

plot(m_ah, ylim = c(-3,3))
points(m_ah + v_ah, col = "blue")
points(m_ah - v_ah, col = "blue")

plot(m_ta, ylim = c(-4,4))
points(m_ta + v_ta, col = "blue")
points(m_ta - v_ta, col = "blue")

m_sl <- c()
m_ta <- c()
m_ah <- c()

sd_sl <- c()
sd_ta <- c()
sd_ah <- c()

for (j in 1: length(ta_l)){
  m_sl <- c(m_sl, mean(sl_l[[j]]))
  m_ah <- c(m_ah, mean(ah_l[[j]]))
  m_ta <- c(m_ta, mean(ta_l[[j]]))
  
  sd_sl <- c(sd_sl, sd(sl_l[[j]]))
  sd_ta <- c(sd_ta, sd(ah_l[[j]]))
  sd_ah <- c(sd_ah, sd(ta_l[[j]]))
}

par(mfrow = c(3,1))
plot(m_sl, ylim = c(-2,10))
points(m_sl + sd_sl, col = "blue")
points(m_sl - sd_sl, col = "blue")

plot(m_ah, ylim = c(-3,3))
points(m_ah + sd_ah, col = "blue")
points(m_ah - sd_ah, col = "blue")

plot(m_ta, ylim = c(-4,4))
points(m_ta + sd_ta, col = "blue")
points(m_ta - sd_ta, col = "blue")


