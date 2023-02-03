#to visualize the results

tot_10 <- c()

for (i in 1:10){
  tot_10 <- c(tot_10, seq_10[[i]])
}

plot(seq)
points(tot_10, col = "red")
table(seq==tot_10)

w_ind <- which(cc %in% c(FALSE))

plot(tot_col_all$x, tot_col_all$y, type = "l")
points(tot_col_all$x[w_ind], tot_col_all$y[w_ind], col = "red")

plot(dataHMM$step)
points(w_ind, dataHMM$step[w_ind], col = "red")

table(seq[w_ind])
table(tot_10[w_ind])

#total hist
ind1 <- which(seq %in% c(1))
ind2 <- which(seq %in% c(2))
hist(dataHMM_all$step[ind1])
hist(dataHMM_all$step[ind2])

#string hist
dataHMM_string <- data.frame()

for (j in 1:10){
  subData_col <- as.data.frame(tot_col[[j]])
  dataHMM <- prepData(subData_col, type = c("LL"), LLangle = TRUE)
  dataHMM_string <- rbind(dataHMM_string, dataHMM)
}

ind1 <- which(tot_10 %in% c(1))
ind2 <- which(tot_10 %in% c(2))
hist(dataHMM_string$step[ind1])
hist(dataHMM_string$step[ind2])

#create plot
par(mfrow = c(2,2))
hist(dataHMM_all$step[ind1], xlim = c(0,2), breaks = 2)
hist(dataHMM_all$step[ind2], xlim = c(0,2), breaks = 20)
hist(dataHMM_string$step[ind1], xlim = c(0,2), breaks = 2)
hist(dataHMM_string$step[ind2], xlim = c(0,2), breaks = 20)


ma <- c()
sa <- c()

ma <- c(ma, round(mean(dataHMM_all$step[ind1], na.rm = TRUE),5))
sa <- c(sa, round(sd(dataHMM_all$step[ind1], na.rm = TRUE),5))

ma <- c(ma, round(mean(dataHMM_all$step[ind2], na.rm = TRUE),5))
sa <- c(sa, round(sd(dataHMM_all$step[ind2], na.rm = TRUE),5))

ma <- c(ma, round(mean(dataHMM_string$step[ind1], na.rm = TRUE),5))
sa <- c(sa, round(sd(dataHMM_string$step[ind1], na.rm = TRUE),5))

ma <- c(ma, round(mean(dataHMM_string$step[ind2], na.rm = TRUE),5))
sa <- c(sa, round(sd(dataHMM_string$step[ind2], na.rm = TRUE),5))

data <- data.frame(
  name=c(1:4),
  value=ma,
  sd=sa
)

ggplot(data) +
  geom_bar( aes(x=name, y=value), stat="identity", alpha=0.7) +
  geom_errorbar( aes(x=name, ymin=value-sd, ymax=value+sd), width=0.4, colour="black", alpha=0.9, size=0.5)
