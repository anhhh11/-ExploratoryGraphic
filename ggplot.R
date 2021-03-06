library(ggplot2)
p <- qplot(wt, mpg, data = mtcars)
p + geom_abline()
p + geom_abline(intercept=25)
coef(lm(mpg ~ wt, data = mtcars))
p + geom_abline(intercept=37,slope=-5)
p + geom_abline(intercept = 10, colour = "red", size = 2)
p + stat_smooth(method="lm", se=FALSE)
p + stat_smooth(method="lm", se=TRUE)

p <- ggplot(mtcars, aes(x = wt, y=mpg,, . ~ cyl)) + geom_point()
p <- ggplot(mtcars, aes(x = wt, y=mpg)) + geom_point()
df <- data.frame(a=rnorm(10, 25), b=rnorm(10, 0))
#Multiple aes
p + geom_abline(aes(intercept=a, slope=b), data=df)
