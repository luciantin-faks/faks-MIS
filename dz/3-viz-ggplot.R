# Title     : TODO
# Objective : TODO
# Created by: tin
# Created on: 31. 03. 2021.

library(ggplot2)
library(measurements)
library(gridExtra)

women$height <- round(conv_unit(women$height, "inch",  "cm"))
women$weight <- round(conv_unit(women$weight, "lbs",  "kg"))

plot1 <-
ggplot(women, aes(x=weight)) +
  geom_density(aes(fg = "Data")) +
  stat_function(fun = dnorm, n = 100, args = list(mean = mean(women$weight), sd = sd(women$weight)), aes(fg = "Normal")) +
  ylab("Density") +
  labs(title = "Weight Density")

plot2 <-
ggplot(women, aes(x=height)) +
  geom_density(aes( fg = "Data")) +
  stat_function(fun = dnorm, n = 100, args = list(mean = mean(women$height), sd = sd(women$height)), aes(fg = "Normal")) +
  ylab("Density") +
  labs(title = "Height Density")

grid.arrange(plot1,plot2, ncol = 2)
