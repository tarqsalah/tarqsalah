library(factoextra)

#importing Seasonal /Trend data
data <- read.csv("AllData Weekly_SEV.2.csv", header = F)

# Preparing data
row.names(data)<- data$V1
data <- data[,2:53]

# Scaling the data
data_scale <- scale(data)

# Number of Clusters

# Defining opt number of cluster using within sum squares
fviz_nbclust(data_scale, kmeans, method = "wss") + labs(subtitle="Elbow Method")

# Defining opt number of cluster using silhouette method
fviz_nbclust(data_scale , kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Defining opt number of cluster using Gap statistic method
fviz_nbclust(data_scale, kmeans, nstart = 25, method = "gap_stat", nboot = 500)+
  labs(subtitle = "Gap statistic method")
