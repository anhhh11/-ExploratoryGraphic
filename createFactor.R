data <- rep(1:3,2,each=3)
odata <- factor(data,levels=c(2,1,3),ordered=TRUE)
sum(as.numeric(odata))
sum(as.numeric(levels(odata)[odata]))

fdata <- factor(data)
levels(fdata)  <- c("a","b","c")
fdata
rdata <- factor(data,labels=c("low","med","hi"))
table(rdata)

lets = sample(letters,size=100,replace=TRUE)
lets = factor(lets)
table(lets[1:5])

wfact = cut(women$weight,3)
table(wfact)

wfact = cut(women$weight,pretty(women$weight,3))
quantile(women$weight,(0:4)/4))


#Smooth
qplot(mpg, wt, data = mtcars, geom = c("point","smooth"))
ggplot(mtcars,aes(x=mpg,y=wt)) + geom_point() + geom_smooth(method="lm") + labs(title="Smooth")
