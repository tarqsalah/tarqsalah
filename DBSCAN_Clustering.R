library(factoextra)
library("fpc")
library("dbscan")
library("NbClust")

#importing Seasonal /Trend data
data <- read.csv("AllData Weekly_Trend.csv", header=F)
rownames(data) <- (data$V1)

data<-data[,2:53]

# #this to delete NA rows from trend data
data <- na.omit(data)

data_Trans<- t(data)


# Scaling the data
data_scale <- scale(data)

# Defining opt number of cluster
fviz_nbclust(data_scale , kmeans, method = "wss") + labs(subtitle="Elbow Method")


fviz_nbclust(data_scale, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

set.seed(123)
fviz_nbclust(data_scale, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")


# Defining opt radius
dbscan::kNNdistplot(data_scale, k = 10)
abline(h =1.5, lty = 2)


set.seed(123)

#DBSCAN
db <- fpc ::dbscan(data_scale, eps= 1.5, MinPts =10)

# Visualize the clustering algorithm results.
fviz_cluster(db, data=data_scale, stand= FALSE, geom ="point", ellipse = FALSE, show.clust.cent= FALSE, palette ="jco", ggtheme =theme_classic())
