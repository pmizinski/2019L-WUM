library(readr)
library(ggplot2)
library(fpc)
library(cluster)
library(factoextra)
library(NbClust)

### WCZYTANIE I PRZESKALOWANIE DANYCH
df <- read_csv("dataset_36_segment.csv")
labels <- as.factor(df$class)
df$class <- NULL
df$`region-pixel-count` <- NULL
df <- scale(df)


### K-MEANS
n_class <- length(levels(labels))
clusters <- kmeans(df, n_class)


### K-MEDOIDS
clusters <- pam(df, n_class)
#plot(clusters)


### PORÓWNANIE OPTYMALNEJ LICZBY KLASTRÓW

fviz_nbclust(df, kmeans, method = "silhouette")
fviz_nbclust(df, pam, method = "silhouette")

fviz_nbclust(df, kmeans, method = "wss")
fviz_nbclust(df, pam, method = "wss")


### PORÓWNANIE KLASTRÓW DLA ICH OPTYMALNEJ LICZBY
pca_df <- prcomp(df)
plot(pca_df$x[,1], pca_df$x[,3], xlab="PC1", ylab = "PC2", main = "PC1 / PC2 - plot", col = clusters$cluster)

library(devtools)
install_github("ggbiplot")

library(ggbiplot)
g <- ggbiplot(ir.pca, obs.scale = 1, var.scale = 1, 
              groups = ir.species, ellipse = TRUE, 
              circle = TRUE)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g)

### PORÓWNANIE CENTRÓW KLASTRÓW
clusters <- kmeans(df, n_class)
pca_df <- prcomp(df)
centers <- predict(pca_df, clusters$centers)

plot(pca_df$x[,1], pca_df$x[,3], xlab="PC1", ylab = "PC3", main = "PC1 / PC3 - plot", col = clusters$cluster)
points(centers[,1], centers[,3], col="pink", pch=20, bg = 40, cex = 7)

#-------------------------------------
png("rplot.png", width = 1080, height = 1080) 
par(mfrow=c(4,4))
for(i in 1:4){
  for(j in 1:4){
    plot(pca_df$x[,i], pca_df$x[,j], xlab=paste0("PC ", i), ylab = paste0("PC ", j), main = paste0("PC", i, " / PC", j, "- plot"), col = clusters$cluster)
    points(centers[,i], centers[,j], col="pink", pch=20, bg = 10, cex = 10)
  }
}
dev.off()

#-------------------------------------



clusters <- pam(df, n_class)
pca_df <- prcomp(df)
centers <- predict(pca_df, clusters$medoids)
plot(pca_df$x[,1], pca_df$x[,3], xlab="PC1", ylab = "PC3", main = "PC1 / PC3 - plot", col = clusters$cluster)
points(centers[,1], centers[,3], col="pink", pch=20, bg = 40, cex = 7)
#-------------------------------------
png("rplot.png", width = 1080, height = 1080) 
par(mfrow=c(4,4))
for(i in 1:4){
  for(j in 1:4){
    plot(pca_df$x[,i], pca_df$x[,j], xlab=paste0("PC ", i), ylab = paste0("PC ", j), main = paste0("PC", i, " / PC", j, "- plot"), col = clusters$cluster)
    points(centers[,i], centers[,j], col="pink", pch=20, bg = 10, cex = 10)
  }
}
dev.off()

#-------------------------------------

### SZYBKOŚĆ ZBIEŻNOŚCI