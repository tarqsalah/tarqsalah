library(factoextra)
library(fpc)
library(NbClust)
library(tidyverse)
library(ggdendro)


# Agglomeration hierarchical clustering implementation

#Importing Trend data:
data <- read.csv("complete_trend_data.csv", header= TRUE)


# Preparing data
data <- data %>% remove_rownames %>% column_to_rownames(var="Stations")


#Hierarchical clustering algorithm :

set.seed(123)

# Perform hierarchical clustering
HC_TR <- eclust(data, FUNcluster = "agnes", k = 3, hc_metric = "euclidean", hc_method = "complete", graph = FALSE)

fviz_dend(HC_TR, cex = 0.2, k_colors = c("green", "red", "blue"),
          xlab = "Groundwater Stations",
          main= "Dendrogram of Groundwater Clusters using\nTrend Water Level Values", 
          show_labels= FALSE) + 
  geom_text( aes(x = 25, y = 0, label= "Cluster 1"),vjust = 1.5) +
  geom_text( aes(x = 75, y = 0, label= "Cluster 2"), vjust = 1.5) +
  geom_text( aes(x = 95, y = 0, label= "Cluster 3"), vjust = 1.5) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic()


# Plot Dendrogram

dendro_data <- as.dendrogram(HC_TR)

# Plot the dendrogram

ggdendrogram(dendro_data, rotate = FALSE, labels = FALSE) +
  labs(title = "Dendrogram of AGNES using Trend values") +
  scale_color_manual(values = c("orange", "blue", "red")) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank())





# #Attributing dendrogram:
# 
# dendTR_dt <- attr(dend_TR, "dendrogram")
# dendSE_dt <- attr(dend_SE, "dendrogram")
# dendWDcs_dt <- attr(dend_WDCs, "dendrogram")
# 
# #Trend dendrogram:
# 
# dend_cuts <- cut(dendTR_dt, h=20) # two heights 11.52 (Subtree 1) & 14.90 (Subtree 2)
# 
# fviz_dend(dend_cuts$lower[[1]], main = "By trend values subtree 1 (Cluster 1)")
# fviz_dend(dend_cuts$lower[[2]][[1]], main = "By trend values subtree 2.1 (Cluster 2)")
# fviz_dend(dend_cuts$lower[[2]][[2]], main = "By trend values subtree 2.2 (Cluster 3)")
# 
# # Extract labels corresponding to the cluster 1
# Cluster1_labels <- data.frame(labels(dend_cuts$lower[[1]]))
# #cluster 2
# Cluster2_labels<- data.frame(labels(dend_cuts$lower[[2]][[1]]))
# #cluster 3
# Cluster3_labels<- data.frame(labels(dend_cuts$lower[[2]][[2]]))


write.table(Cluster1_labels,"trend_cluster_1.csv")
write.table(Cluster2_labels,"trend_cluster_2.csv")
write.table(Cluster3_labels,"trend_clsuter_3.csv")






#Cluster validation

# Silhouette_SCORE

fviz_silhouette(HC_TR,   palette = "jco", ggtheme = theme_classic())
fviz_silhouette(HC_SE,   palette = "jco", ggtheme = theme_classic())
fviz_silhouette(HC_WDCs, palette = "jco", ggtheme = theme_classic())


silinfo_TR <- HC_TR$silinfo
silinfo_SE <- HC_SE$silinfo
silinfo_WDCs <- HC_WDCs$silinfo

names(silinfo_TR)

Si_TR<-silinfo_TR$widths[, 1:3]
Si_SE<-silinfo_SE$widths[, 1:3]
Si_WDCs<-silinfo_WDCs$widths[, 1:3]

silinfo_TR$clus.avg.widths
silinfo_SE$clus.avg.widths
silinfo_WDCS$clus.avg.widths

neg_Si_index<- which(Si_TR[,"sil_width"] < 0)
neg_Si_index<- which(Si_SE[,"sil_width"] < 0)
neg_Si_index<- which(Si_WDCs[,"sil_width"] < 0)


# Within-Cluster Sum of Squares (WCSS):
fviz_cluster(HC_TR  ,geom = "point", ellipse.type = "norm",palette = "jco", ggtheme = theme_minimal())
fviz_cluster(HC_SE  ,geom = "point", ellipse.type = "norm",palette = "jco", ggtheme = theme_minimal())
fviz_cluster(HC_WDCs,geom = "point", ellipse.type = "norm",palette = "jco", ggtheme = theme_minimal())


#Other_Statistics:
HC_TR_Satistics <- cluster.stats(dist(dataTR_scale),HC_TR$cluster)
HC_SE_Satistics <- cluster.stats(dist(dataSE_scale),HC_SE$cluster)
HC_WDCs_Satistics <- cluster.stats(dist(dataWDCs_scale),HC_WDCs$cluster)




















