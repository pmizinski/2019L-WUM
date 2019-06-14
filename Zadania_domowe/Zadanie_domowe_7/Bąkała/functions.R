library(OpenML)
library(factoextra)
library(NbClust)
library(cluster)
library(dplyr)
require(ggfortify)

# odczytanie danych ----

data <- getOMLDataSet(36)
data_X <- data$data %>% select(-c(class, region.pixel.count))

# poszukiwanie liczby klastrów ----

fviz_nbclust(x = data_X, kmeans, method = "wss")
fviz_nbclust(x = data_X, kmeans, method = "silhouette")
fviz_nbclust(x = data_X, kmeans, method = "gap_stat")

fviz_nbclust(x = data_X, pam, method = "wss")
fviz_nbclust(x = data_X, pam, method = "silhouette")
fviz_nbclust(x = data_X, pam, method = "gap_stat")

NbClust(data = data_X, method = "kmeans")

# modelowanie ----

set.seed(27)
kmean <- kmeans(data_X, 3)
set.seed(27)
kmed <- pam(data_X, 3)

# pca ----

autoplot(kmean, data_X, x = 1, y = 2, frame = TRUE, size = 1.5)
autoplot(kmed, data_X, x = 1, y = 2, frame = TRUE, size = 1.5)

# centra ----

kmean$centers
kmed$medoids

# porównanie klastrowań

diff <- kmean$cluster - (4 - kmed$clustering)
df <- data.frame(sum = c(sum(diff == 2), sum(diff == 1), sum(diff == -1), sum(diff == -2)),
                 label = c("diff_plus2", "diff_plus1", "diff_minus1", "diff_minus2"))
ggplot(df, aes(y = sum, x = label)) +
  geom_bar(stat = "identity")
