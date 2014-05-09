library(ggplot2)
library('ggdendro')
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)

ggplot(data.frame(x,y),aes(x, y)) +
  geom_point(colour='green',size=3) +
  geom_text(aes(x=x+0.1,y+0.1,label=as.character(1:12)))

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
nrow(dataFrame)
hClustering <- hclust(distxy)
plot(hClustering)
ggdendrogram(hClustering)

set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)

ggplot(dataFrame[sample(1:12),],aes(x,y)) +
  geom_tile() +
  scale_fill_gradient()


#K-Mean
set.seed(1234)
x <- rnorm(80, mean = rep(1:4, each = 20), sd = 0.2)
y <- rnorm(80, mean = rep(c(1, 2, 3,4,-2), each = 4), sd = 0.2)
length(x)==length(y)
ggplot(data.frame(x,y),aes(x, y)) +
  geom_point(colour='green',size=2) +
  geom_text(aes(x+0.05,y+0.05,label=as.character(1:80)),size=2)

dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 5)
names(kmeansObj)
kmeansObj$cluster
kmeansObj$centers
kmeansObj$totss #All
kmeansObj$withinss #Within cluster
kmeansObj$tot.withinss#Between cluster
kmeansObj$betweenss
kmeansObj$size
kmeansObj$iter
kmeansObj$ifault

ggplot(dataFrame,aes(x,y)) +
  geom_point(aes(colour=factor(kmeansObj$cluster))) +
  geom_point(data=data.frame(kmeansObj$centers),aes(x,y),colour="red",size=3,pch=13)
set.seed(1234)
sample1 <- sample(1:12)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
dataMatrix <- as.matrix(dataFrame)[sample1, ]
dataFrame[sample1, ]
image(t(dataMatrix)[,nrow(dataMatrix):1])
image(dataMatrix)
#Must transpose image then reversed column to get original view in coord
dataMatrix[1,1]  <- 100
dataMatrix[2,2] <- 255


setwd("/home/anhhh11/Documents/OpenIntro/ExploratoryGraphic")
download.file("https://dl.dropboxusercontent.com/u/7710864/courseraPublic/samsungData.rda"
              ,destfile="samsungData.rda",method="curl",mode="wb")
load("samsungData.rda")

set.seed(12345); par(mar=rep(0.2,4))
dataMatrix <- matrix(rnorm(400),nrow=40) # 40x10

par(mar=rep(0.2,4),mfrow=c(1,2))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1]) #10x40
image(dataMatrix)
heatmap(dataMatrix)

set.seed(678910)
for(i in 1:40){
  # flip a coin
  coinFlip <- rbinom(1,size=1,prob=0.5)
  # if coin is heads add a common pattern to that row
  if(coinFlip){
    dataMatrix[i,] <- dataMatrix[i,] + rep(c(0,3),each=5)
  }
}
heatmap(dataMatrix)
par(mar=rep(0.2,4))
image(1:10,1:40,t(dataMatrix)[,nrow(dataMatrix):1])
heatmap(dataMatrix)
hh <- hclust(dist(dataMatrix)); 
dataMatrixOrdered <- dataMatrix[hh$order,]
par(mfrow=c(1,5))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrixOrdered),xlab="Column",ylab="Column Mean",pch=19)
plot(rowMeans(dataMatrix),40:1,,xlab="Row",ylab="Row Mean",pch=19)
plot(colMeans(dataMatrix),xlab="Column",ylab="Column Mean",pch=19)


#SVD
#scale: default: scale down
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow=c(1,3))
image(t(dataMatrixOrdered)[,nrow(dataMatrixOrdered):1])
plot(svd1$u[,1],40:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svd1$v[,1],xlab="Column",ylab="First right singular vector",pch=19)