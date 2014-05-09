library('corpcor')
library('impute')
data <- matrix(1:20,4,5)
svddata <- svd(data)
ud <- (svddata$u %*% svddata$d) 
vt <- t(svddata$v)
class(ud); class(vt)
svddata$u
svddata$d
svddata$v
svddata$u %*%  diag(svddata$d) %*% t(svddata$v)
image(t(data)[,nrow(data):1])
plot(svddata$u[,1],4:1,,xlab="Row",ylab="First left singular vector",pch=19)
plot(svddata$v[,1],xlab="Column",ylab="First right singular vector",pch=19)
svddata$u[,1]
svddata$v[,1]
plot(svddata$d,xlab="Column",ylab="Singluar value",pch=19)
plot(svddata$d^2/sum(svddata$d^2),xlab="Column",ylab="Percent of variance explained",pch=19)
pca1 <- prcomp(data,scale=TRUE)
plot(pca1$rotation[,1],svddata$v[,1],pch=19,xlab="Principal Component 1",ylab="Right Singular Vector 1")
pca1$rotation[,1]
svddata$v[,1]

bigMatrix <- matrix(rnorm(1e4*40),nrow=1e4)
bigMatrixSvd <- fast.svd(scale(bigMatrix),tol=0)

data <- matrix(1:10000,100,100)
svd1 <- svd(data)
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
approx5 <- svd1$u[,1:2] %*% diag(svd1$d[1:2])%*% t(svd1$v[,1:2])
dim(svd1$u[,1:2]) + 2 + dim(svd1$v[,1:2])
ncol(data)
approxAll <- svd1$u %*% diag(svd1$d) %*% t(svd1$v)


library(ggplot2)
data(diamonds)
str(diamonds)
diamonds$cut <- as.numeric(diamonds$cut)
diamonds$clarity <- as.numeric(diamonds$clarity)
diamonds$color <- as.numeric(factor(diamonds$color,levels=rev(levels(diamonds$color))))
s <- svd(scale(diamonds))
p <- prcomp(diamonds, scale.=TRUE)
par(mfrow=c(1,2))

#p$sdev contains Transparent layer weight
#p$sdev[i] contain layer i
library('reshape2')
library('ggplot2')
plot(s$d^2 / sum(s$d^2))
plot(p$sdev^2 / sum(p$sdev^2))
sum((p$sdev^2 / sum(p$sdev^2))[1:5])
#p$ratation[,i] cointains influence of each variable in Layer i
plot(s$v[,1], pch=19)
plot(p$rotation[,1], pch=19)
names(diamonds)[c(1,7,8,9,10)]
ttable <- data.frame(p$rotation)
ttable$name <- rownames(ttable)
ttableMelted <- melt(ttable,id=c("name"))
#ggplot(data=ttableMelted,aes(value,colour=name)) +
#  geom_histogram()

#kNN impute
library('impute')
dataMatrixOrdered=matrix(1:1000,ncol=10)
dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=F)] <- NA
dataMatrix2 <- impute.knn(dataMatrix2)$data
svd1 <- svd(scale(dataMatrixOrdered)); 
svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); 
plot(svd1$v[,1],pch=19); 
plot(svd2$v[,1],pch=19)


#Reduction
download.file("https://spark-public.s3.amazonaws.com/dataanalysis/face.rda",
              destfile="./face.rda",
              method="wget")
load("./face.rda")
image(t(faceData)[,nrow(faceData):1])
svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2),pch=19,xlab="Singluar vector",ylab="Variance explained")
approx1 <- svd1$u[,1] %*% t(svd1$v[,1]) * svd1$d[1]
approx5 <- svd1$u[,1:5] %*% diag(svd1$d[1:5])%*% t(svd1$v[,1:5])
approx10 <- svd1$u[,1:10] %*% diag(svd1$d[1:10])%*% t(svd1$v[,1:10])
