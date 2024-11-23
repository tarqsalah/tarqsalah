##Seasonal---------

library(factoextra)

#importing Seasonal /Trend data
data <- read.csv("AllData Weekly_TrendV.2.csv")

# Preparing data
row.names(data)<- data$V1
data <- data[,2:53]

# Scaling the data
data_scale <- scale(data)

# Distance
data_dis <- dist(data_scale)

# Distance Matrix
fviz_dist(data_dis)

# Number of Clusters

# Within Sum Squares
fviz_nbclust(data_scale, kmeans, method = "wss")+
    labs(subtitle="Elbow Method")

# Kmeans
km.out <- kmeans(data_scale, centers=3,nstart=100)
print(km.out)

# Visualize the clustering algorithm results.
km.clusters<-km.out$cluster

rownames(data_scale)<-paste(1:ncol(data))

fviz_cluster(list(data=data_scale, cluster = km.clusters)) +labs( title="Clustering by Trend")

km.clusters <- data.frame(km.clusters)

write.csv(km.clusters,"clusters_Trend.csv")



#Duration_Curves--------

library(factoextra)

#importing Seasonal /Trend data
data <- read.csv("AllDataWDCs(Long_term).csv")

data_Trans<- t(data)

# Scaling the data
data_scale <- scale(data_Trans)

# Distance
data_dis <- dist(data_scale)

# Number of Clusters

# Within Sum Squares

fviz_nbclust(data_scale, kmeans, method = "wss")+
  labs(subtitle="Elbow Method")

# Kmeans
set.seed(123)
km.out <- eclust(data_scale, k=4,nstart=100, graph = FALSE)


# Visualize the clustering algorithm results.
km.clusters<-km.out$cluster

rownames(data_scale)<-paste(1:ncol(data))

fviz_cluster(km.out, geom = "point", ellipse.type= "norm", palette ="jco", ggtheme = theme_minimal()) +labs( title="Clustering by duration curves")

fviz_silhouette(km.out, palettte ="jco", ggthem =theme_classic())

library(fpc)
# Statistics for k-means clustering
km_stats <- cluster.stats(data_dis, km.out$cluster)

# Dun index
km_stats$dunn

km.clusters <- data.frame(km.clusters)

write.csv(km.clusters,"clusters_Trend.csv")
