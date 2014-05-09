library(ggplot2)
library(gridExtra)
library(lattice)
install.packages('lattice')
setwd("/home/anhhh11/Documents/OpenIntro/ExploratoryGraphic")
pollution <- read.csv("data/avgpm25.csv", colClasses = c("numeric", "character",
                                                         "factor", "numeric", "numeric"))
#1D
head(pollution)

summary(pollution$pm25)
#Box plot
par(mfrow=c(1,1))
boxplot(pollution$pm25,col="blue")
ggplot(pollution,aes("PM25",pm25)) + geom_boxplot() # + coord_flip()
#Histogram
hist(pollution$pm25,col="green",breaks=100)
rug(pollution$pm25)
abline(h=12)
abline(v=12,lwd=2)
abline(v=median(pollution$pm25),col="magenta",lwd=5)
ggplot(pollution,aes(pm25)) + 
  geom_histogram(colour="black",fill="green",breaks=1:2:40) +
  geom_hline(yintercept=12,
             colour="magenta",
             linetype="longdash",
             size=2) +
  geom_vline(xintercept=median(pollution$pm25),
             colour="red",
             size=3)

barplot(table(pollution$region),col="wheat",main="Number of counties in Each region")
ggplot(pollution,aes(region,fill=region)) + geom_bar(width=.5) + 
  labs(title="Number of counties in Each region",x="Region",y="Count") +

#2D
boxplot(pm25~region,data=pollution,col="red")
ggplot(pollution,aes(x=factor(region),y=pm25,fill=region)) + geom_boxplot()

par(mfrow=c(2,1),mar=c(4,4,2,1))
hist(subset(pollution,region=="east")$pm25,col="green")
hist(subset(pollution,region="west")$pm25,col="red")
h1  <- ggplot(subset(pollution,region=="east"),aes(pm25)) + geom_histogram(fill="green")
h2  <- ggplot(subset(pollution,region=="west"),aes(pm25)) + geom_histogram(fill="green")
grid.arrange(h1,h2,ncol=2)
#Scatter plot
par(mfrow=c(1,1))
with(pollution,plot(latitude,pm25,col=region))
abline(h=12,lwd=2,lty=2)
ggplot(pollution,aes(x=latitude,y=pm25,colour=region)) + 
  geom_point() +
  geom_hline(yintercept=12,linetype=2,size=1.2)
ggplot(pollution,aes(x=latitude,y=pm25,group=region,colour=region)) + 
  geom_line() +
  geom_hline(yintercept=12,linetype=2,size=1.2)

#Multiple scatter plot
par(mfrow=c(1,2),mar=c(5,4,2,1))
with(subset(pollution,region=="east"),plot(latitude,pm25,main="West"))
with(subset(pollution,region="west"),plot(latitude,pm25,main="East"))
east <- ggplot(subset(pollution,region=="east"),aes(latitude,pm25)) + geom_point()
west <- ggplot(subset(pollution,region=="west"),aes(latitude,pm25)) + geom_point()
grid.arrange(east,west,ncol=2)


#Grid
Life.Exp.Levels = cut(state$Life.Exp,3,labels=c("low","medium","high"))
state <- cbind(state,Life.Exp.Levels)
library(scales)
p1 <- ggplot(state,aes(Income,Life.Exp)) + 
  geom_point() +
  facet_wrap(region ~ Life.Exp.Levels) +
  labs(title="P1")
p2 <- ggplot(state,aes(Income,Life.Exp)) + 
  geom_point(aes(colour=region)) +
  scale_x_continuous(log2_trans()) +
  scale_y_continuous(log2_trans(),limits=c(70,80)) +
  facet_grid(region ~ Life.Exp.Levels) + 
  labs(title="P2") + 
  geom_smooth(size=2,linetype=3) 
summary(p2)
grid.arrange(p1,p2,ncol=2)

#Limit
testdat <- data.frame(x = 1:100, y = rnorm(100))
testdat[50,2] <- 100
g <- ggplot(testdat, aes(x = x, y = y))
g + geom_line() + ylim(-3,3) #Remove outlier
g + geom_line() + coord_cartesian(ylim=c(-3,3))
x11()
?Devices
library(datasets)
svg("myplot.svg")
with(faithful, qplot(eruptions, waiting) +
       labs(title = "Old Faithful Geyser datassss"))
dev.off()
dev.cur()

with(faithful, qplot(eruptions, waiting) +
       labs(title = "Old Faithful Geyser datassss"))
dev.copy(png,file="myplotCopy.png")
dev.off()



