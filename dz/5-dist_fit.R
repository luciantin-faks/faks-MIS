library(ggplot2)
library(fitdistrplus)

# Distr. of age

data <- infert$age

plotdist(data, histo = TRUE, demp = TRUE)


# provjera dist
descdist(data = data)


# fit test ?
fits <- list(
  norm = fitdist(data, "norm"),
  exp = fitdist(data, "exp"),
  logis = fitdist(data, "logis"),
  weibull = fitdist(data, "weibull"),
  lnorm = fitdist(data, "lnorm"),
  gamma = fitdist(data, "gamma")
)

plot(fits$gamma)

vals <- sapply(fits, function (i) i$loglik)

names(fits)[vals == max(vals)]


# goodness of fit
gofstat(fits, fitnames = names(fits))

# KAZE GAMMA


dist_params <- fitdistr(data, densfun = "gamma" )
dist_params
dist_shape <- dist_params$estimate[[1]]
dist_rate <- dist_params$estimate[[2]]

samples <- 10000

rnd_data <- rgamma(rate = dist_rate, shape = dist_shape, n=samples)
# rnd_data
# plot(rnd_data)


ggplot() +
  aes(x=data) +
  # geom_histogram(aes(y=..density..), binwidth = 0.3, colour="white", fill="blue") +
  # geom_histogram(aes(x=rnd_data, y=..density..), binwidth = 0.3, colour="white", fill="green") +
  geom_density(aes(x=rnd_data),alpha=.2, fill="#FF6666") +
  geom_density(alpha=.2, fill="#6666FF")

# RED - random
# BLUE - data


# Valjda je ok