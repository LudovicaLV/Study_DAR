#cluster the stat parameters

library(factoextra) 

df_meas <- data.frame(m_sl = m_sl, m_ta = m_ta, m_ah = m_ah, v_sl = v_sl, v_ta = v_ta, v_ah = v_ah)

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

df.pca <- prcomp(df_meas, center = TRUE, scale. = FALSE)
summary(df.pca)
ggbiplot(df.pca, ellipse=FALSE, groups=col_cl, alpha = 1, varname.size = 2.0)
