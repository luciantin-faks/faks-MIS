# Title     : TODO
# Objective : TODO
# Created by: tin
# Created on: 31. 03. 2021.

library(ggplot2)
library(dplyr)

?BOD

ggplot(data = BOD,
       mapping = aes(x = Time,  # x is the independent dim
                     y = demand))

CO2 %>%
  ggplot(aes(conc, uptake, colour = Treatment)) +
  geom_point(size = 3) +
  geom_smooth()


x1 <- 1:100
y1 <- 1:100
x2 <- 101:200
y2 <- 1:100

d1 <- data.frame(x1, y1)
d2 <- data.frame(x2, y2)

d1$group1 <- 1
d2$group2 <- 2

d <- cbind(d1,d2)
d


ggplot(d, aes(y1, x2, group=interaction(group1, group2))) +
   geom_point()


d

set.seed(0)
x <- rep(1:10, 4)
y <- c(rep(1:10, 2)+rnorm(20)/5, rep(6:15, 2) + rnorm(20)/5)
treatment <- gl(2, 20, 40, labels=letters[1:2])
replicate <- gl(2, 10, 40)
d <- data.frame(x=x, y=y, treatment=treatment, replicate=replicate)
d
ggplot(d, aes(x=x, y=y, colour=treatment, shape = replicate,
  group=interaction(treatment, replicate))) +
  geom_point() + geom_line()