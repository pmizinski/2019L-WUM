
rand_stat <- function(group1, group2){
x <- abs(sapply(group1, function(x) x - group1))
x[x > 1] <- 1
y <- abs(sapply(group2, function(x) x - group2))
y[y > 0] <- 2
xy <- (x + y)
xy <- xy[lower.tri(xy)]
SS <- sum(xy==0)
SD <- sum(xy==2)
DS <- sum(xy==1)
DD <- sum(xy==3)
M <- length(xy)
rand <- (SS + DD)/M
rand
}

jaccard_coef <- function(group1, group2){
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 0] <- 2
  xy <- (x + y)
  xy <- xy[lower.tri(xy)]
  SS <- sum(xy==0)
  SD <- sum(xy==2)
  DS <- sum(xy==1)
  DD <- sum(xy==3)
  M <- length(xy)
  jacc <- SS/(SS+SD+DS)
  jacc
}

FM <- function(group1, group2){
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 0] <- 2
  xy <- (x + y)
  xy <- xy[lower.tri(xy)]
  SS <- sum(xy==0)
  SD <- sum(xy==2)
  DS <- sum(xy==1)
  DD <- sum(xy==3)
  M <- length(xy)
  m1 <- SS + SD
  m2 <- SS + DS
  fm <- SS/sqrt(m1*m2)
  fm
}

hubert <- function(group1, group2){
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 1] <- 1
  # cor(x[lower.tri(x)],y[lower.tri(y)])
  sum((x*y)[upper.tri(x)])/sum(upper.tri(x))
}


hubert_std <- function(group1, group2){
  x <- abs(sapply(group1, function(x) x - group1))
  x[x > 1] <- 1
  y <- abs(sapply(group2, function(x) x - group2))
  y[y > 1] <- 1
  M <- sum(lower.tri(x))
  x <- x[lower.tri(x)]
  y <- y[lower.tri(y)]
  sdx <- sqrt(sum(x^2-mean(x)^2)/M)
  sdy <- sqrt(sum(y^2-mean(y)^2)/M)
  # cor(x[lower.tri(x)],y[lower.tri(y)])
  sum(((x-mean(x))*(y-mean(y))))/(M*sdx*sdy)
}


dunn_index <- function(data, cluster){
  k <- length(unique(cluster))
  max_diam <- max(sapply(1:k, function(i) max(dist(data[cluster==i, ]))))
  d <- min(sapply(1:(k-1), 
                  function(i) min(sapply(min(i+1,k):k, 
                                         function(j) min(proxy::dist(iris[cluster==i, ],iris[cluster==j, ]))/max_diam))))
  d
}

