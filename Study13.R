#more clustering/analysis of the stats

library(factoextra) 

df_meas <- data.frame(m_sl = m_sl, m_ta = m_ta, m_ah = m_ah, sd_sl = sd_sl, sd_ta = sd_ta, sd_ah = sd_ah)

df_meas <- scale(df_meas)
data <- as.matrix(df_meas)

#analysis to choose the number of clusters - elbow method
fviz_nbclust(data, FUN = hcut, method = "wss", k.max = 10)

#choice of cluster number
nclust <- 4

#clustering functions
dist_mat <- dist(data, method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'ward.D')
clusters_row <- cutree(hclust_avg, k = nclust)

table(clusters_row)

col_cl <- c()
col_cl[clusters_row == 1] <- "blue"
col_cl[clusters_row == 2] <- "red"
col_cl[clusters_row == 3] <- "green"
col_cl[clusters_row == 4] <- "orange"

par(mfrow = c(3,1))
plot(m_sl, ylim = c(-2,10))
points(m_sl + sd_sl, col = col_cl)
points(m_sl - sd_sl, col = col_cl)

plot(m_ah, ylim = c(-3,3))
points(m_ah + sd_ah, col = col_cl)
points(m_ah - sd_ah, col = col_cl)

plot(m_ta, ylim = c(-4,4))
points(m_ta + sd_ta, col = col_cl)
points(m_ta - sd_ta, col = col_cl)

###scaled data

df_meas <- as.data.frame(df_meas)

par(mfrow = c(3,1))
plot(df_meas$m_sl, ylim = c(-5,5))
points(df_meas$m_sl + df_meas$sd_sl, col = col_cl)
points(df_meas$m_sl - df_meas$sd_sl, col = col_cl)

plot(df_meas$m_ah, ylim = c(-5,5))
points(df_meas$m_ah + df_meas$sd_ah, col = col_cl)
points(df_meas$m_ah - df_meas$sd_ah, col = col_cl)

plot(df_meas$m_ta, ylim = c(-5,5))
points(df_meas$m_ta + df_meas$sd_ta, col = col_cl)
points(df_meas$m_ta - df_meas$sd_ta, col = col_cl)

