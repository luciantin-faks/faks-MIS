data(WWWusage)

WWWusage

summary(WWWusage)

par(mfrow=c(nrow=2, ncol=2), pty="s")

plot(WWWusage,
     main = "Internet usage per minute",
     xlab = "Minutes",
     ylab = "Usage")

hist(WWWusage,
     main = "Internet usage per minute",
     xlab = "Minutes",
     ylab = "Usage")


View(mtcars)

class(mtcars)

d <- density(mtcars$mpg)
plot(d, main="Density of mpg", xlab = "Miles", ylab = "Density", col = "blue")

polygon(d, col = "red", border = "blue")

cly.f <- factor(mtcars$cyl, labels = c("4 cyl", "6 cyl", "8 cyl"), levels = c(4,6,8))
cly.y <- factor(mtcars$cyl)

cyl.dc4 <- density(mtcars$mpg[mtcars$cyl == 4])
cyl.dc6 <- density(mtcars$mpg[mtcars$cyl == 6])
cyl.dc8 <- density(mtcars$mpg[mtcars$cyl == 8])

print(cyl.dc4)


par(mfrow=c(nrow=2, ncol=2), pty="s")

plot(cyl.dc4,
     main="Density of 4 cyl",
     xlab = "Miles",
     xlim = c(min(cyl.dc8$x,cyl.dc4$x,cyl.dc6$x),
              max(cyl.dc8$x,cyl.dc4$x,cyl.dc6$x)),
     ylim = c(min(cyl.dc8$y,cyl.dc4$y,cyl.dc6$y),
              max(cyl.dc8$y,cyl.dc4$y,cyl.dc6$y)),
     col = "red")
lines(cyl.dc6, col = "blue")
lines(cyl.dc8, col = "green")

legend("topright", legend = levels(cly.f), text.col=c("red","blue","green"))


# install.packages("ggplot2", repos='http://cran.us.r-project.org')

library(ggplot2)

cly.f <- factor(mtcars$cyl, labels = c("4 cyl", "6 cyl", "8 cyl"), levels = c(4,6,8))

ggplot(mtcars, aes(x=mpg)) +
  geom_density(aes(colour = cly.f)) +
  xlab("Miles Per Gallon") +
  ggtitle("MPG Distribution by Car Cylinders") +
  labs(color="Cylinders")




UKgas

plot.ts(window(UKgas, start = 1961, end = 1968))