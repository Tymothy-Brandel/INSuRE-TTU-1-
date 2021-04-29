install.packages("cluster")
install.packages("factoextra")
install.packages("NbClust")
install.packages("dplyr")
library(cluster)
library(factoextra)
library(NbClust)
library(dplyr)

#set up matrix to hold test data
rownames = c("A","B","C","D","E","F","G","H")
m = matrix ( nrow = 8, ncol = 8, byrow = FALSE, dimnames = list(rownames, rownames))

#initialize matrix with random data
for (i in 1:8) {
  for (j in 1:8) {
    m[i,j] = runif(1, min=0.01, max=0.07)
  }
}

#set certain values for test clusters
m[1,2] = 0.75
m[2,1] = 0.75
m[1,3] = 0.73
m[3,1] = 0.73
m[2,3] = 0.83
m[3,2] = 0.83
m[4,5] = 0.55
m[5,4] = 0.55
m[6,7] = 0.93
m[7,6] = 0.93
m[6,8] = 0.81
m[8,6] = 0.81
m[7,8] = 0.67
m[8,7] = 0.67

#run clustering algorithm on matrix
test = agnes(m, diss = inherits(x, "dist"), metric = "euclidian",
      stand = FALSE, method = "average", par.method,
      keep.diss = FALSE, keep.data = TRUE, trace.lev = 0)

m2 = as.data.frame(m)

# Elbow method to determine optimal number of clusters
elbow = fviz_nbclust(m2, hcut, method = "wss",k.max=5) +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

#extract optimal number of clusters
opt_clusters = pull(elbow$layers[[3]]$data,xintercept)

#plot tree and draw rectangles
pltree(test)
rect.hclust(test, k = opt_clusters[1], border = 2:5)

#alternate hierarchical clustering method
#d <- dist(m2, method = "euclidean")
#h_test = hclust(d, method="average")
#plot(h_test)


#cut tree into clusters
clusters = cutree(as.hclust(test), k = opt_clusters[1])


cluster_list = list()
cluster_table = cbind(rownames, clusters)

#break the clusters up and store them in vectors
#each vector in the cluster_list is an individual cluster
for (i in 1:4) {
  temp_list = vector()
  for (j in 1:8) {
    if (cluster_table[j,2] == i) {
      temp_list[length(temp_list)+1] = cluster_table[j,1]
    }
  }
  cluster_list[[length(cluster_list)+1]] = temp_list
}


#######Other methods for optimal number of clusters ################

# Silhouette method
sil = fviz_nbclust(m2, hcut, method = "silhouette", k.max=5)+
  labs(subtitle = "Silhouette method")

#Gap statistic
# nboot = 50 to keep the function speedy. 
# recommended value: nboot= 500 for your analysis.
# Use verbose = FALSE to hide computing progression.
set.seed(123)
fviz_nbclust(m2, kmeans, nstart = 25,  method = "gap_stat", nboot = 500, k.max=5)+
  labs(subtitle = "Gap statistic method")
