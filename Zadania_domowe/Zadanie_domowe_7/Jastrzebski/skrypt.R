# Wczytanie zbiorów danych

library(ggplot2)
library(dplyr)
library(gridExtra)

load("data.rda")

set.seed(123)

# Plowowanie danych

## Plot wszystkich shapów
plot_shapes <- function(d1,d2,d3,d4,d5,d6,d7,d8) {
  
  p1 <- ggplot(d1, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p2 <- ggplot(d2, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p3 <- ggplot(d3, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p4 <- ggplot(d4, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p5 <- ggplot(d5, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p6 <- ggplot(d6, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p7 <- ggplot(d7, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  p8 <- ggplot(d8, aes(x = x, y = y, color = label)) +
    geom_point() + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
  
  grid.arrange(p1, p2, p3, p4, p5, p6, p7, p8, nrow = 2)
  
}

## Plotowanie dwóch obok siebie kmeans i kmedoids

plot_mean_medoid <- function(d, index_med, index_mean, med, mean, lmed, lmean) {
  
  lmed <- lmed[[index_med]] %>% as.factor()
  lmean <- lmean[[index_mean]] %>% as.factor()
  
  med <- med[[index_med]] 
  mean <- mean[[index_mean]] 
  
  d$label <- lmean
  p1 <- ggplot(d, aes(x = x, y = y, color = label)) +
    geom_point() + 
    geom_point(data = data.frame(mean$centers), color = 'red', size=3) +
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle("k-means")
  
  d$label <- lmed
  p2 <- ggplot(d, aes(x = x, y = y, color = label)) +
    geom_point() + 
    geom_point(data = d[med$medoid,], color = 'red', size=3) +
    theme_bw() + 
    theme(legend.position = 'none',
          axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) +
    ggtitle("k-medoids")
  
  grid.arrange(p1, p2, nrow = 1)
}

## Plotowanie wskaźników

plot_index <- function(s1s, s1m,
                       s2s, s2m,
                       s3s, s3m,
                       s4s, s4m,
                       s5s, s5m,
                       s6s, s6m,
                       s7s, s7m,
                       s8s, s8m, index) {
  p1s <- cbind(rand = s1s[index,], k = 2:(ncol(s1s) + 1)) %>% data.frame()
  p2s <- cbind(rand = s2s[index,], k = 2:(ncol(s2s) + 1)) %>% data.frame()
  p3s <- cbind(rand = s3s[index,], k = 2:(ncol(s3s) + 1)) %>% data.frame()
  p4s <- cbind(rand = s4s[index,], k = 2:(ncol(s4s) + 1)) %>% data.frame()
  p5s <- cbind(rand = s5s[index,], k = 2:(ncol(s5s) + 1)) %>% data.frame()
  p6s <- cbind(rand = s6s[index,], k = 2:(ncol(s6s) + 1)) %>% data.frame()
  p7s <- cbind(rand = s7s[index,], k = 2:(ncol(s7s) + 1)) %>% data.frame()
  p8s <- cbind(rand = s8s[index,], k = 2:(ncol(s8s) + 1)) %>% data.frame()
  
  p1m <- cbind(rand = s1m[index,], k = 2:(ncol(s1m) + 1)) %>% data.frame()
  p2m <- cbind(rand = s2m[index,], k = 2:(ncol(s2m) + 1)) %>% data.frame()
  p3m <- cbind(rand = s3m[index,], k = 2:(ncol(s3m) + 1)) %>% data.frame()
  p4m <- cbind(rand = s4m[index,], k = 2:(ncol(s4m) + 1)) %>% data.frame()
  p5m <- cbind(rand = s5m[index,], k = 2:(ncol(s5m) + 1)) %>% data.frame()
  p6m <- cbind(rand = s6m[index,], k = 2:(ncol(s6m) + 1)) %>% data.frame()
  p7m <- cbind(rand = s7m[index,], k = 2:(ncol(s7m) + 1)) %>% data.frame()
  p8m <- cbind(rand = s8m[index,], k = 2:(ncol(s8m) + 1)) %>% data.frame()
  
  p1 <- ggplot() + 
    geom_line(data = p1s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p1m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  p2 <- ggplot() + 
    geom_line(data = p2s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p2m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  p3 <- ggplot() + 
    geom_line(data = p3s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p3m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  p4 <- ggplot() + 
    geom_line(data = p4s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p4m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  p5 <- ggplot() + 
    geom_line(data = p5s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p5m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  p6 <- ggplot() + 
    geom_line(data = p6s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p6m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  p7 <- ggplot() + 
    geom_line(data = p7s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p7m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  p8 <- ggplot() +
    geom_line(data = p8s, aes(x = k, y = rand), color = 'skyblue') +
    geom_line(data = p8m, aes(x = k, y = rand), color = 'hotpink') + 
    theme_bw() + 
    theme(legend.position = 'none',
          axis.ticks = element_blank(),
          axis.title = element_blank())
  
  grid.arrange(p1,
               p2,
               p3,
               p4,
               p5,
               p6,
               p7,
               p8, nrow = 2)
}

# Wyliczenie labelsów

## kmeans

c1s <- lapply(2:20, FUN = function(i) kmeans(data.matrix(d1[,1:2]), i))
c2s <- lapply(2:20, FUN = function(i) kmeans(data.matrix(d2[,1:2]), i))
c3s <- lapply(2:40, FUN = function(i) kmeans(data.matrix(d3[,1:2]), i))
c4s <- lapply(2:10, FUN = function(i) kmeans(data.matrix(d4[,1:2]), i))
c5s <- lapply(2:10, FUN = function(i) kmeans(data.matrix(d5[,1:2]), i))
c6s <- lapply(2:10, FUN = function(i) kmeans(data.matrix(d6[,1:2]), i))
c7s <- lapply(2:10, FUN = function(i) kmeans(data.matrix(d7[,1:2]), i))
c8s <- lapply(2:25, FUN = function(i) kmeans(data.matrix(d8[,1:2]), i))

l1s <- lapply(2:20, FUN = function(i) c1s[[i-1]]$cluster %>% as.integer)
l2s <- lapply(2:20, FUN = function(i) c2s[[i-1]]$cluster %>% as.integer)
l3s <- lapply(2:40, FUN = function(i) c3s[[i-1]]$cluster %>% as.integer)
l4s <- lapply(2:10, FUN = function(i) c4s[[i-1]]$cluster %>% as.integer)
l5s <- lapply(2:10, FUN = function(i) c5s[[i-1]]$cluster %>% as.integer)
l6s <- lapply(2:10, FUN = function(i) c6s[[i-1]]$cluster %>% as.integer)
l7s <- lapply(2:10, FUN = function(i) c7s[[i-1]]$cluster %>% as.integer)
l8s <- lapply(2:25, FUN = function(i) c8s[[i-1]]$cluster %>% as.integer)

## kmedoids
library(kmed)

kmeddist1 <- dist(data.matrix(d1[,1:2]))
kmeddist2 <- dist(data.matrix(d2[,1:2]))
kmeddist3 <- dist(data.matrix(d3[,1:2]))
kmeddist4 <- dist(data.matrix(d4[,1:2]))
kmeddist5 <- dist(data.matrix(d5[,1:2]))
kmeddist6 <- dist(data.matrix(d6[,1:2]))
kmeddist7 <- dist(data.matrix(d7[,1:2]))
kmeddist8 <- dist(data.matrix(d8[,1:2]))

c1m <- lapply(2:20, FUN = function(i) fastkmed(kmeddist1, i))
c2m <- lapply(2:20, FUN = function(i) fastkmed(kmeddist2, i))
c3m <- lapply(2:40, FUN = function(i) fastkmed(kmeddist3, i))
c4m <- lapply(2:10, FUN = function(i) fastkmed(kmeddist4, i))
c5m <- lapply(2:10, FUN = function(i) fastkmed(kmeddist5, i))
c6m <- lapply(2:10, FUN = function(i) fastkmed(kmeddist6, i))
c7m <- lapply(2:10, FUN = function(i) fastkmed(kmeddist7, i))
c8m <- lapply(2:25, FUN = function(i) fastkmed(kmeddist8, i))

l1m <- lapply(2:20, FUN = function(i) c1m[[i-1]]$cluster %>% as.integer)
l2m <- lapply(2:20, FUN = function(i) c2m[[i-1]]$cluster %>% as.integer)
l3m <- lapply(2:40, FUN = function(i) c3m[[i-1]]$cluster %>% as.integer)
l4m <- lapply(2:10, FUN = function(i) c4m[[i-1]]$cluster %>% as.integer)
l5m <- lapply(2:10, FUN = function(i) c5m[[i-1]]$cluster %>% as.integer)
l6m <- lapply(2:10, FUN = function(i) c6m[[i-1]]$cluster %>% as.integer)
l7m <- lapply(2:10, FUN = function(i) c7m[[i-1]]$cluster %>% as.integer)
l8m <- lapply(2:25, FUN = function(i) c8m[[i-1]]$cluster %>% as.integer)

# Wyliczanie statystyk

library(clv)
library(clusterCrit)

statystyki <- function(d, pred) {
  
  s <- std.ext(as.integer(pred), as.integer(d$label))
  i <- intCriteria(data.matrix(d[,1:2]), pred, c("Gamma","Silhouette","Davies_Bouldin", "Dunn"))
  
  c(jacc = clv.Jaccard(s),
    silhouette = i$silhouette,
    davb = i$davies_bouldin,
    dunn = i$dunn)
}

s1s <-sapply(l1s, function(l) statystyki(d1, l))
s2s <-sapply(l2s, function(l) statystyki(d2, l))
s3s <-sapply(l3s, function(l) statystyki(d3, l))
s4s <-sapply(l4s, function(l) statystyki(d4, l))
s5s <-sapply(l5s, function(l) statystyki(d5, l))
s6s <-sapply(l6s, function(l) statystyki(d6, l))
s7s <-sapply(l7s, function(l) statystyki(d7, l))
s8s <-sapply(l8s, function(l) statystyki(d8, l))

s1m <-sapply(l1m, function(l) statystyki(d1, l))
s2m <-sapply(l2m, function(l) statystyki(d2, l))
s3m <-sapply(l3m, function(l) statystyki(d3, l))
s4m <-sapply(l4m, function(l) statystyki(d4, l))
s5m <-sapply(l5m, function(l) statystyki(d5, l))
s6m <-sapply(l6m, function(l) statystyki(d6, l))
s7m <-sapply(l7m, function(l) statystyki(d7, l))
s8m <-sapply(l8m, function(l) statystyki(d8, l))

# Plot shapów

shapes <- plot_shapes(d1,d2,d3,d4,d5,d6,d7,d8)

s1s['jacc',]

# Plot indeksów

jpi <- plot_index(s1s, s1m,
                  s2s, s2m,
                  s3s, s3m,
                  s4s, s4m,
                  s5s, s5m,
                  s6s, s6m,
                  s7s, s7m,
                  s8s, s8m, 'jacc')
spi <- plot_index(s1s, s1m,
                  s2s, s2m,
                  s3s, s3m,
                  s4s, s4m,
                  s5s, s5m,
                  s6s, s6m,
                  s7s, s7m,
                  s8s, s8m, 'silhouette')
dbpi <- plot_index(s1s, s1m,
                  s2s, s2m,
                  s3s, s3m,
                  s4s, s4m,
                  s5s, s5m,
                  s6s, s6m,
                  s7s, s7m,
                  s8s, s8m, 'davb')
dpi <- plot_index(s1s, s1m,
                  s2s, s2m,
                  s3s, s3m,
                  s4s, s4m,
                  s5s, s5m,
                  s6s, s6m,
                  s7s, s7m,
                  s8s, s8m, 'dunn')

# plot najlepszych wartości
# Średnie niebieskie, medoidy czerwone

jpb1 <- plot_mean_medoid(d = d1, index_med = which.max(s1m['jacc',]), index_mean = which.max(s1s['jacc',]), med = c1m, mean = c1s, lmed = l1m, lmean = l1s)
jpb2 <- plot_mean_medoid(d = d2, index_med = which.max(s2m['jacc',]), index_mean = which.max(s2s['jacc',]), med = c2m, mean = c2s, lmed = l2m, lmean = l2s)
jpb3 <- plot_mean_medoid(d = d3, index_med = which.max(s3m['jacc',]), index_mean = which.max(s3s['jacc',]), med = c3m, mean = c3s, lmed = l3m, lmean = l3s)
jpb4 <- plot_mean_medoid(d = d4, index_med = which.max(s4m['jacc',]), index_mean = which.max(s4s['jacc',]), med = c4m, mean = c4s, lmed = l4m, lmean = l4s)
jpb5 <- plot_mean_medoid(d = d5, index_med = which.max(s5m['jacc',]), index_mean = which.max(s5s['jacc',]), med = c5m, mean = c5s, lmed = l5m, lmean = l5s)
jpb6 <- plot_mean_medoid(d = d6, index_med = which.max(s6m['jacc',]), index_mean = which.max(s6s['jacc',]), med = c6m, mean = c6s, lmed = l6m, lmean = l6s)
jpb7 <- plot_mean_medoid(d = d7, index_med = which.max(s7m['jacc',]), index_mean = which.max(s7s['jacc',]), med = c7m, mean = c7s, lmed = l7m, lmean = l7s)
jpb8 <- plot_mean_medoid(d = d8, index_med = which.max(s8m['jacc',]), index_mean = which.max(s8s['jacc',]), med = c8m, mean = c8s, lmed = l8m, lmean = l8s)

spb1 <- plot_mean_medoid(d = d1, index_med = which.max(s1m['silhouette',]), index_mean = which.max(s1s['silhouette',]), med = c1m, mean = c1s, lmed = l1m, lmean = l1s)
spb2 <- plot_mean_medoid(d = d2, index_med = which.max(s2m['silhouette',]), index_mean = which.max(s2s['silhouette',]), med = c2m, mean = c2s, lmed = l2m, lmean = l2s)
spb3 <- plot_mean_medoid(d = d3, index_med = which.max(s3m['silhouette',]), index_mean = which.max(s3s['silhouette',]), med = c3m, mean = c3s, lmed = l3m, lmean = l3s)
spb4 <- plot_mean_medoid(d = d4, index_med = which.max(s4m['silhouette',]), index_mean = which.max(s4s['silhouette',]), med = c4m, mean = c4s, lmed = l4m, lmean = l4s)
spb5 <- plot_mean_medoid(d = d5, index_med = which.max(s5m['silhouette',]), index_mean = which.max(s5s['silhouette',]), med = c5m, mean = c5s, lmed = l5m, lmean = l5s)
spb6 <- plot_mean_medoid(d = d6, index_med = which.max(s6m['silhouette',]), index_mean = which.max(s6s['silhouette',]), med = c6m, mean = c6s, lmed = l6m, lmean = l6s)
spb7 <- plot_mean_medoid(d = d7, index_med = which.max(s7m['silhouette',]), index_mean = which.max(s7s['silhouette',]), med = c7m, mean = c7s, lmed = l7m, lmean = l7s)
spb8 <- plot_mean_medoid(d = d8, index_med = which.max(s8m['silhouette',]), index_mean = which.max(s8s['silhouette',]), med = c8m, mean = c8s, lmed = l8m, lmean = l8s)

dbpb1 <- plot_mean_medoid(d = d1, index_med = which.max(s1m['davb',]), index_mean = which.max(s1s['davb',]), med = c1m, mean = c1s, lmed = l1m, lmean = l1s)
dbpb2 <- plot_mean_medoid(d = d2, index_med = which.max(s2m['davb',]), index_mean = which.max(s2s['davb',]), med = c2m, mean = c2s, lmed = l2m, lmean = l2s)
dbpb3 <- plot_mean_medoid(d = d3, index_med = which.max(s3m['davb',]), index_mean = which.max(s3s['davb',]), med = c3m, mean = c3s, lmed = l3m, lmean = l3s)
dbpb4 <- plot_mean_medoid(d = d4, index_med = which.max(s4m['davb',]), index_mean = which.max(s4s['davb',]), med = c4m, mean = c4s, lmed = l4m, lmean = l4s)
dbpb5 <- plot_mean_medoid(d = d5, index_med = which.max(s5m['davb',]), index_mean = which.max(s5s['davb',]), med = c5m, mean = c5s, lmed = l5m, lmean = l5s)
dbpb6 <- plot_mean_medoid(d = d6, index_med = which.max(s6m['davb',]), index_mean = which.max(s6s['davb',]), med = c6m, mean = c6s, lmed = l6m, lmean = l6s)
dbpb7 <- plot_mean_medoid(d = d7, index_med = which.max(s7m['davb',]), index_mean = which.max(s7s['davb',]), med = c7m, mean = c7s, lmed = l7m, lmean = l7s)
dbpb8 <- plot_mean_medoid(d = d8, index_med = which.max(s8m['davb',]), index_mean = which.max(s8s['davb',]), med = c8m, mean = c8s, lmed = l8m, lmean = l8s)

dpb1 <- plot_mean_medoid(d = d1, index_med = which.max(s1m['dunn',]), index_mean = which.max(s1s['dunn',]), med = c1m, mean = c1s, lmed = l1m, lmean = l1s)
dpb2 <- plot_mean_medoid(d = d2, index_med = which.max(s2m['dunn',]), index_mean = which.max(s2s['dunn',]), med = c2m, mean = c2s, lmed = l2m, lmean = l2s)
dpb3 <- plot_mean_medoid(d = d3, index_med = which.max(s3m['dunn',]), index_mean = which.max(s3s['dunn',]), med = c3m, mean = c3s, lmed = l3m, lmean = l3s)
dpb4 <- plot_mean_medoid(d = d4, index_med = which.max(s4m['dunn',]), index_mean = which.max(s4s['dunn',]), med = c4m, mean = c4s, lmed = l4m, lmean = l4s)
dpb5 <- plot_mean_medoid(d = d5, index_med = which.max(s5m['dunn',]), index_mean = which.max(s5s['dunn',]), med = c5m, mean = c5s, lmed = l5m, lmean = l5s)
dpb6 <- plot_mean_medoid(d = d6, index_med = which.max(s6m['dunn',]), index_mean = which.max(s6s['dunn',]), med = c6m, mean = c6s, lmed = l6m, lmean = l6s)
dpb7 <- plot_mean_medoid(d = d7, index_med = which.max(s7m['dunn',]), index_mean = which.max(s7s['dunn',]), med = c7m, mean = c7s, lmed = l7m, lmean = l7s)
dpb8 <- plot_mean_medoid(d = d8, index_med = which.max(s8m['dunn',]), index_mean = which.max(s8s['dunn',]), med = c8m, mean = c8s, lmed = l8m, lmean = l8s)


save(shapes,
     jpb1,
     jpb2,
     jpb3,
     jpb4,
     jpb5,
     jpb6,
     jpb7,
     jpb8,
     spb1,
     spb2,
     spb3,
     spb4,
     spb5,
     spb6,
     spb7,
     spb8,
     dbpb1,
     dbpb2,
     dbpb3,
     dbpb4,
     dbpb5,
     dbpb6,
     dbpb7,
     dbpb8,
     dpb1,
     dpb2,
     dpb3,
     dpb4,
     dpb5,
     dpb6,
     dpb7,
     dpb8,
     jpi,
     spi,
     dbpi,
     dpi, file = "plots.rda")

c1m[[1]]
