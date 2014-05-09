library(openintro)
library(sampling)
library(ggplot2)
data(email50)
View(email50)
data(county)
View(county)
#Scatter plot
#base
with(county,plot(poverty,
                 fed_spend,
                 ylim=c(0,40),
                 xlab="Poverty Rate(Percent)",
   
with(county,plot(multiunit,homeownership))
#GGplot
p <- ggplot(county,aes(poverty,fed_spend)) + 
  ylim(0,40) + 
  xlab("Poverty Rate(Percent") +
  ylab("Federal Spending Per Capita") + 
  geom_point()
#Sampling
#Stratified: column to group, obs number
#Clustering: min cluster size, max cluster size, obs number, n-of-cluster
#TODE

#Histogram ( hallow + normal)
with(email50,plot(num_char,line_breaks))

data(cars)

plot(cars$weight,cars$price,col=fadeColor('magenta',88),pch=20,cex=1)

par(mfrow=c(1,2))

histPlot(cars$price[cars$type=='small'], probability=TRUE,
         hollow=TRUE, xlim=c(0,50))
histPlot(cars$price[cars$type=='midsize'], probability=TRUE,
         hollow=TRUE, add=TRUE, border='red', lty=3)
histPlot(cars$price[cars$type=='large'], probability=TRUE,
         hollow=TRUE, add=TRUE, border='blue', lty=4)
#Long
ggplot(cars,aes(price)) + 
  geom_histogram(data=subset(cars,type=="small"),fill="red",alpha=0.2) +
  geom_histogram(data=subset(cars,type=="midsize"),fill="blue",alpha=0.2) +
  geom_histogram(data=subset(cars,type=="large"),fill="green",alpha=0.2)
#Sort
#dat <- data.frame(xx = c(runif(100,20,50),runif(100,40,80),runif(100,0,30)),yy = rep(letters[1:3],each = 100))
ggplot(cars,aes(x=price,fill=type)) + geom_histogram(aes(y=..count../sum(..count..)))
ggplot(cars,aes(x=price,fill=type,alpha=0.8)) + geom_density() 


#Dot plot
#test
ggplot(email50, aes(x=num_char)) +   geom_dotplot(binwidth=2,stackdir="center")
#1
data(cars)
dotPlot(cars$price, cars$type, key=c('large', 'midsize', 'small'), cex=1:3)
#2
ggplot(cars, aes(x=type,y=price)) +   geom_dotplot(binaxis="y",stackdir="center")
