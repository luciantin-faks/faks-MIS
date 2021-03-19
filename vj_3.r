# install.packages("measurements")

data(women)
library(measurements)
par(mfrow=c(row=2,col=2))

women$weight <- round(conv_unit(women$weight, 'lbs', 'kg'))
women$height <- round(conv_unit(women$height, 'inch', 'cm'))


plot(women$weight, women$height)
plot(women)

data <- women$height

plot_dist <- function (data, title) {
  x <- rnorm(length(data), mean(data), sd(data))
  x_d <- dnorm(x, mean(data), sd(data))
  plot(density(data), ylim=c(0, max(x_d)), col = "blue", main=title, xlab="")
  curve( dnorm(x, mean(data), sd(data)), add = TRUE, col = "green" )
  legend("topright", legend = c("Data", "Normal"), text.col = c("blue", "green"))
}

plot_dist(women$weight, "weight")
plot_dist(women$height, "height")

# SIM
par(mfrow=c(row=1,col=1))

# install.packages("purrr")
# install.packages("broom")
# install.packages("dplyr")
# install.packages("ggplot2")

library(purrr)
library(broom)
library(dplyr)
library(ggplot2)

# B0 = stvarna srednja vrijednost (cijele populacije) = 80
# B1 = srednja vrijednost test grupe (sd = 3) = 75
# SD - 7

n <- 100
data_mean <- 75
data_sd <- 3

# plot(x,1:n)

B0 <- 80
B1 <- -5   # 75 - 80
sigma <- 7
# plot(y, 1:length(y))

sim_fun <- function () {
  x <- rnorm(n, mean = data_mean, sd = data_sd)
  y <- B0 + B1 * x + rnorm(n, 0, sigma)
  fit <- lm(formula = y ~ x )
  return(fit)
}

summary(sim_fun())

set.seed(100)

sims <- rerun(10000, sim_fun())


summary(sims)

sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  data.frame(sigma = .) %>%
  ggplot(aes(sigma)) +
  geom_density(fill="blue", alpha=.5) +
  geom_vline(xintercept = 7)

# plot(1:100, 1:100)


sims %>%
  map_dbl(~summary(.x)$sigma) %>%
  {. < 7} %>%
  mean()

sims %>%
  map_df(tidy) %>%
  pull(p.value) %>%
  {. < 0.05} %>%
  mean()


# gg plot + 1. primjer