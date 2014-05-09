library(datasets)
library(ggplot2)
data(airquality)
airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)
library(ggplot2)
g <- ggplot(movies, aes(votes, rating))
print(g)
qplot(votes, rating, data = movies) + geom_smooth() 
?geom_smooth

n <- -100:100
a <- n*log2(n)
b <- n^(log(n*log2(n),n))

