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

clusplot(zbior, srednie$cluster, color=TRUE, shade=TRUE, lines=0,main ="",xlab = "",ylab = "",sub = "" )

clusplot(zbior, groups, color=TRUE, shade=TRUE, lines=0)

adjustedRand(groups,etykiety$V1,c("Rand", "HA", "FM", "Jaccard"))

ksrednie<-lapply(2:5,function(x){srednia<-kmeans(zbior,x,nstart = 25);srednia$cluster})
statystyki<-sapply(ksrednie, function(x){adjustedRand(x,etykiety$V1,c("Rand", "HA", "FM", "Jaccard"))})
data.frame(t(statystyki))


ward <- hclust(d, method="ward.D2")
ward<-lapply(2:5,function(x){srednia<-cutree(ward, k=x)})
statystyki<-sapply(ward, function(x){adjustedRand(x,etykiety$V1,c("Rand", "HA", "FM", "Jaccard"))})
ward<-data.frame(t(statystyki))

clues::get_Silhouette(zbior,ward[[1]])

library(reshape2)
reshape2::melt(ward_stat)

ggplot(data = stat,aes(x=liczba_klastrow,y=value,color=metoda,group=metoda))+geom_line()+facet_grid(. ~ variable)

