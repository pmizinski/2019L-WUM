library(cluster)
library(mclust)
library(ggplot2)
library(clues)

zbior<-read.csv("g2-2-100.data",header = FALSE,sep = " ")
etykiety<-read.csv("g2-2-100.labels",header = FALSE,sep = " ")

zbior<-scale(zbior)
d <- dist(zbior, method = "euclidean") # distance matrix

srednie<-kmeans(zbior,2)

# Ward Hierarchical Clustering

ward <- hclust(d, method="ward.D2")
plot(ward) # display dendogram
groups <- cutree(ward, k=2) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters

plot(zbior[,1],zbior[,2],col=etykiety$V1)

adjustedRand(srednie$cluster,etykiety$V1,c("Rand", "HA", "FM", "Jaccard"))

clusplot(zbior, srednie$cluster, color=TRUE, shade=TRUE, lines=0)

clusplot(zbior, groups, color=TRUE, shade=TRUE, lines=0)

adjustedRand(groups,etykiety$V1,c("Rand", "HA", "FM", "Jaccard"))
