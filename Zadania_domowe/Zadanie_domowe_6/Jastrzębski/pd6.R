library(ggplot2)
library(dplyr)
library(gridExtra)
# Przygotowanie danych

d1 <- read.delim("Aggregation.txt", col.names = c("x", "y", "label"))
d2 <- read.delim("Compound.txt", col.names = c("x", "y", "label"))
d3 <- read.delim("D31.txt", col.names = c("x", "y", "label"))
d4 <- read.delim("spiral.txt", col.names = c("x", "y", "label"))
d5 <- read.delim("flame.txt", col.names = c("x", "y", "label"))
d6 <- read.delim("jain.txt", col.names = c("x", "y", "label"))
d7 <- read.delim("pathbased.txt", col.names = c("x", "y", "label"))
d8 <- read.delim("R15.txt", col.names = c("x", "y", "label"))

plot(d1$x, d1$y, col=d1$label)
plot(d2$x, d2$y, col=d2$label)
plot(d3$x, d3$y, col=d3$label)
plot(d4$x, d4$y, col=d4$label)
plot(d5$x, d5$y, col=d5$label)
plot(d6$x, d6$y, col=d6$label)
plot(d7$x, d7$y, col=d7$label)
plot(d8$x, d8$y, col=d8$label)

# factors

d1$label <- d1$label %>% as.factor
d2$label <- d2$label %>% as.factor
d3$label <- d3$label %>% as.factor
d4$label <- d4$label %>% as.factor
d5$label <- d5$label %>% as.factor
d6$label <- d6$label %>% as.factor
d7$label <- d7$label %>% as.factor
d8$label <- d8$label %>% as.factor

save(d1,d2,d3,d4,d5,d6,d7,d8,file='data.rda')

# Plotowanie danych

plot_shapes <- function(d1,d2,d3,d4,d5,d6,d7,d8,l1,l2,l3,l4,l5,l6,l7,l8, repl = TRUE) {
  
  if(repl) {
    d1$label <- l1 %>% as.factor
    d2$label <- l2 %>% as.factor
    d3$label <- l3 %>% as.factor
    d4$label <- l4 %>% as.factor
    d5$label <- l5 %>% as.factor
    d6$label <- l6 %>% as.factor
    d7$label <- l7 %>% as.factor
    d8$label <- l8 %>% as.factor
  }
  
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

plot_shapes(d1,d2,d3,d4,d5,d6,d7,d8,0,0,0,0,0,0,0,0,repl=FALSE)

# Clustering

## hclust2

library(genie)

t1 <- hclust2(dist(data.matrix(d1[,1:2])))
t2 <- hclust2(dist(data.matrix(d2[,1:2])))
t3 <- hclust2(dist(data.matrix(d3[,1:2])))
t4 <- hclust2(dist(data.matrix(d4[,1:2])))
t5 <- hclust2(dist(data.matrix(d5[,1:2])))
t6 <- hclust2(dist(data.matrix(d6[,1:2])))
t7 <- hclust2(dist(data.matrix(d7[,1:2])))
t8 <- hclust2(dist(data.matrix(d8[,1:2])))

l1_h <- lapply(1:20, FUN = function(i) cutree(t1, i))
l2_h <- lapply(1:20, FUN = function(i) cutree(t2, i))
l3_h <- lapply(1:40, FUN = function(i) cutree(t3, i))
l4_h <- lapply(1:10, FUN = function(i) cutree(t4, i))
l5_h <- lapply(1:10, FUN = function(i) cutree(t5, i))
l6_h <- lapply(1:10, FUN = function(i) cutree(t6, i))
l7_h <- lapply(1:10, FUN = function(i) cutree(t7, i))
l8_h <- lapply(1:25, FUN = function(i) cutree(t8, i))


## kmeans

kmeans(data.matrix(d1[,1:2]), 5)

l1_k <- lapply(1:20, FUN = function(i) kmeans(data.matrix(d1[,1:2]), i)$cluster)
l2_k <- lapply(1:20, FUN = function(i) kmeans(data.matrix(d2[,1:2]), i)$cluster)
l3_k <- lapply(1:40, FUN = function(i) kmeans(data.matrix(d3[,1:2]), i)$cluster)
l4_k <- lapply(1:10, FUN = function(i) kmeans(data.matrix(d4[,1:2]), i)$cluster)
l5_k <- lapply(1:10, FUN = function(i) kmeans(data.matrix(d5[,1:2]), i)$cluster)
l6_k <- lapply(1:10, FUN = function(i) kmeans(data.matrix(d6[,1:2]), i)$cluster)
l7_k <- lapply(1:10, FUN = function(i) kmeans(data.matrix(d7[,1:2]), i)$cluster)
l8_k <- lapply(1:25, FUN = function(i) kmeans(data.matrix(d8[,1:2]), i)$cluster)

save(l1_h,
     l2_h,
     l3_h,
     l4_h,
     l5_h,
     l6_h,
     l7_h,
     l8_h,
     l1_k,
     l2_k,
     l3_k,
     l4_k,
     l5_k,
     l6_k,
     l7_k,
     l8_k,
     file = 'etykiety.rda')

# Wyliczanie statystyk

library(clv)

statystyki <- function(d, pred) {
  
  s <- std.ext(as.integer(pred), as.integer(d$label))
  i <- intCriteria(data.matrix(d[,1:2]), pred, c("Gamma", "Davies_Bouldin", "Dunn"))
  
  c(rand = clv.Rand(s),
    jacc = clv.Jaccard(s),
    folk = clv.Folkes.Mallows(s),
    gamma = i$gamma,
    davb = i$davies_bouldin,
    dunn = i$dunn)
}


s1_k <-sapply(l1_k, function(l) statystyki(d1, l))
s2_k <-sapply(l2_k, function(l) statystyki(d2, l))
s3_k <-sapply(l3_k, function(l) statystyki(d3, l))
s4_k <-sapply(l4_k, function(l) statystyki(d4, l))
s5_k <-sapply(l5_k, function(l) statystyki(d5, l))
s6_k <-sapply(l6_k, function(l) statystyki(d6, l))
s7_k <-sapply(l7_k, function(l) statystyki(d7, l))
s8_k <-sapply(l8_k, function(l) statystyki(d8, l))

s1_h <-sapply(l1_h, function(l) statystyki(d1, l))
s2_h <-sapply(l2_h, function(l) statystyki(d2, l))
s3_h <-sapply(l3_h, function(l) statystyki(d3, l))
s4_h <-sapply(l4_h, function(l) statystyki(d4, l))
s5_h <-sapply(l5_h, function(l) statystyki(d5, l))
s6_h <-sapply(l6_h, function(l) statystyki(d6, l))
s7_h <-sapply(l7_h, function(l) statystyki(d7, l))
s8_h <-sapply(l8_h, function(l) statystyki(d8, l))

plot(2:20, s['dunn',2:20], type='l')

p1_k <- cbind(data.frame(t(s1_k)), k = 1:ncol(s1_k))
p2_k <- cbind(data.frame(t(s2_k)), k = 1:ncol(s2_k))
p3_k <- cbind(data.frame(t(s3_k)), k = 1:ncol(s3_k))
p4_k <- cbind(data.frame(t(s4_k)), k = 1:ncol(s4_k))
p5_k <- cbind(data.frame(t(s5_k)), k = 1:ncol(s5_k))
p6_k <- cbind(data.frame(t(s6_k)), k = 1:ncol(s6_k))
p7_k <- cbind(data.frame(t(s7_k)), k = 1:ncol(s7_k))
p8_k <- cbind(data.frame(t(s8_k)), k = 1:ncol(s8_k))

p1_h <- cbind(data.frame(t(s1_h)), k = 1:ncol(s1_h))
p2_h <- cbind(data.frame(t(s2_h)), k = 1:ncol(s2_h))
p3_h <- cbind(data.frame(t(s3_h)), k = 1:ncol(s3_h))
p4_h <- cbind(data.frame(t(s4_h)), k = 1:ncol(s4_h))
p5_h <- cbind(data.frame(t(s5_h)), k = 1:ncol(s5_h))
p6_h <- cbind(data.frame(t(s6_h)), k = 1:ncol(s6_h))
p7_h <- cbind(data.frame(t(s7_h)), k = 1:ncol(s7_h))
p8_h <- cbind(data.frame(t(s8_h)), k = 1:ncol(s8_h))

# rand
randPlot_1 <- ggplot() + 
  geom_line(data = p1_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p1_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_2 <- ggplot() + 
  geom_line(data = p2_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p2_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_3 <- ggplot() + 
  geom_line(data = p3_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p3_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_4 <- ggplot() + 
  geom_line(data = p4_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p4_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_5 <- ggplot() + 
  geom_line(data = p5_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p5_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_6 <- ggplot() + 
  geom_line(data = p6_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p6_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_7 <- ggplot() + 
  geom_line(data = p7_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p7_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
randPlot_8 <- ggplot() +
  geom_line(data = p8_k, aes(x = k, y = rand), color = 'skyblue') +
  geom_line(data = p8_h, aes(x = k, y = rand), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())

randPlot <- grid.arrange(randPlot_1,
             randPlot_2,
             randPlot_3,
             randPlot_4,
             randPlot_5,
             randPlot_6,
             randPlot_7,
             randPlot_8, nrow = 2)

# rand
jaccPlot_1 <- ggplot() + 
  geom_line(data = p1_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p1_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_2 <- ggplot() + 
  geom_line(data = p2_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p2_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_3 <- ggplot() + 
  geom_line(data = p3_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p3_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_4 <- ggplot() + 
  geom_line(data = p4_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p4_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_5 <- ggplot() + 
  geom_line(data = p5_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p5_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_6 <- ggplot() + 
  geom_line(data = p6_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p6_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_7 <- ggplot() + 
  geom_line(data = p7_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p7_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
jaccPlot_8 <- ggplot() +
  geom_line(data = p8_k, aes(x = k, y = jacc), color = 'skyblue') +
  geom_line(data = p8_h, aes(x = k, y = jacc), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())

jaccPlot <- grid.arrange(jaccPlot_1,
             jaccPlot_2,
             jaccPlot_3,
             jaccPlot_4,
             jaccPlot_5,
             jaccPlot_6,
             jaccPlot_7,
             jaccPlot_8, nrow = 2)

#folk
folkPlot_1 <- ggplot() + 
  geom_line(data = p1_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p1_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_2 <- ggplot() + 
  geom_line(data = p2_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p2_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_3 <- ggplot() + 
  geom_line(data = p3_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p3_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_4 <- ggplot() + 
  geom_line(data = p4_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p4_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_5 <- ggplot() + 
  geom_line(data = p5_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p5_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_6 <- ggplot() + 
  geom_line(data = p6_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p6_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_7 <- ggplot() + 
  geom_line(data = p7_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p7_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
folkPlot_8 <- ggplot() +
  geom_line(data = p8_k, aes(x = k, y = folk), color = 'skyblue') +
  geom_line(data = p8_h, aes(x = k, y = folk), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())

folkPlot <- grid.arrange(folkPlot_1,
                         folkPlot_2,
                         folkPlot_3,
                         folkPlot_4,
                         folkPlot_5,
                         folkPlot_6,
                         folkPlot_7,
                         folkPlot_8, nrow = 2)

# gamma
gammaPlot_1 <- ggplot() + 
  geom_line(data = p1_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p1_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_2 <- ggplot() + 
  geom_line(data = p2_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p2_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_3 <- ggplot() + 
  geom_line(data = p3_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p3_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_4 <- ggplot() + 
  geom_line(data = p4_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p4_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_5 <- ggplot() + 
  geom_line(data = p5_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p5_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_6 <- ggplot() + 
  geom_line(data = p6_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p6_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_7 <- ggplot() + 
  geom_line(data = p7_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p7_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
gammaPlot_8 <- ggplot() +
  geom_line(data = p8_k, aes(x = k, y = gamma), color = 'skyblue') +
  geom_line(data = p8_h, aes(x = k, y = gamma), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())

gammaPlot <- grid.arrange(gammaPlot_1,
                         gammaPlot_2,
                         gammaPlot_3,
                         gammaPlot_4,
                         gammaPlot_5,
                         gammaPlot_6,
                         gammaPlot_7,
                         gammaPlot_8, nrow = 2)
# davb
davbPlot_1 <- ggplot() + 
  geom_line(data = p1_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p1_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_2 <- ggplot() + 
  geom_line(data = p2_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p2_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_3 <- ggplot() + 
  geom_line(data = p3_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p3_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_4 <- ggplot() + 
  geom_line(data = p4_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p4_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_5 <- ggplot() + 
  geom_line(data = p5_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p5_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_6 <- ggplot() + 
  geom_line(data = p6_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p6_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_7 <- ggplot() + 
  geom_line(data = p7_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p7_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
davbPlot_8 <- ggplot() +
  geom_line(data = p8_k, aes(x = k, y = davb), color = 'skyblue') +
  geom_line(data = p8_h, aes(x = k, y = davb), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())

davbPlot <- grid.arrange(davbPlot_1,
                         davbPlot_2,
                         davbPlot_3,
                         davbPlot_4,
                         davbPlot_5,
                         davbPlot_6,
                         davbPlot_7,
                         davbPlot_8, nrow = 2)
# dunn
dunnPlot_1 <- ggplot() + 
  geom_line(data = p1_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p1_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_2 <- ggplot() + 
  geom_line(data = p2_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p2_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_3 <- ggplot() + 
  geom_line(data = p3_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p3_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_4 <- ggplot() + 
  geom_line(data = p4_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p4_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_5 <- ggplot() + 
  geom_line(data = p5_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p5_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_6 <- ggplot() + 
  geom_line(data = p6_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p6_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_7 <- ggplot() + 
  geom_line(data = p7_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p7_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())
dunnPlot_8 <- ggplot() +
  geom_line(data = p8_k[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'skyblue') +
  geom_line(data = p8_h[2:nrow(p1_k),], aes(x = k, y = dunn), color = 'hotpink') + 
  theme_bw() + 
  theme(legend.position = 'none',
        axis.ticks = element_blank())

dunnPlot <- grid.arrange(dunnPlot_1,
                         dunnPlot_2,
                         dunnPlot_3,
                         dunnPlot_4,
                         dunnPlot_5,
                         dunnPlot_6,
                         dunnPlot_7,
                         dunnPlot_8, nrow = 2)

p1_k %>% head
save(randPlot,
     jaccPlot,
     folkPlot,
     gammaPlot, 
     davbPlot, 
     dunnPlot,
     shapePlot,
     file = 'statPlots.rda')


plot(gammaPlot)
shapePlot <- plot_shapes(d1,d2,d3,d4,d5,d6,d7,d8,0,0,0,0,0,0,0,0,repl=FALSE)
