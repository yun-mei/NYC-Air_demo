
library("dplyr")
data <- as.data.frame(t(demogr_20))
names(data)<-data[2,]
data=data[-1:-2,]
data <- as.data.frame(t(data))
data<-data[,-1]

data <- data %>% 
  mutate_at(c(1:6), as.numeric)

sapply(data,class)

normalize <- function(x){
  return ((x-min(x))/(max(x)-min(x)))
} 

D_20n <- as.data.frame(lapply(data[,c(1,2,3,4,5,6)],normalize))
rownames(D_20n) <- rownames(data)


library(corrplot)
corrplot(cor(data), method = "number", type = "lower")

k3 <- kmeans(D_20n, centers = 3, nstart = 25)

library(factoextra)
fviz_cluster(k3, data = D_20n)

## R exercise: get more info from the clusters
k3$centers
k3$withinss
k3$tot.withinss

k2 <- kmeans(D_20n, centers = 2, nstart = 25)
k4 <- kmeans(D_20n, centers = 4, nstart = 25)
k5 <- kmeans(D_20n, centers = 5, nstart = 25)
k6 <- kmeans(D_20n, centers = 6, nstart = 25)
k7 <- kmeans(D_20n, centers = 7, nstart = 25)

fviz_cluster(k6, data = D_20n)


p1 <- fviz_cluster(k2, geom = "point", data = D_20n) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = D_20n) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = D_20n) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = D_20n) + ggtitle("k = 5")
p5 <- fviz_cluster(k6, geom = "point",  data = D_20n) + ggtitle("k = 6")
p6 <- fviz_cluster(k7, geom = "point",  data = D_20n) + ggtitle("k = 7")


library(gridExtra)
grid.arrange(p1, p2, p3, p4,p5,p6 ,nrow = 2)

plot(p5)
fviz_nbclust(data, kmeans, method = 'wss') +
  geom_vline(xintercept = 4, linetype=5, col= "darkred")

fviz_cluster(k6, data = D_20n)

k6$centers

