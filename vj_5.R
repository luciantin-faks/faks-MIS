library(ggplot2)
library(MASS)

data(diamonds)

ggplot(diamonds, aes(x=carat)) +
  geom_histogram(aes(y=..density..), binwidth = 0.1, colour="white", fill="blue") +
  geom_density(alpha=.2, fill="#FF6666")


library(fitdistrplus)

data <- diamonds$carat

data
plotdist(data, histo = TRUE, demp = TRUE)

descdist(data = data)

fits <- list(
  norm = fitdist(data, "norm"),
  exp = fitdist(data, "exp"),
  logis = fitdist(data, "logis"),
  weibull = fitdist(data, "weibull"),
  lnorm = fitdist(data, "lnorm"),
  gamma = fitdist(data, "gamma")
)

plot(fits$lnorm)

vals <- sapply(fits, function (i) i$loglik)

names(fits)[vals == max(vals)]


# goodness of fit
gofstat(fits, fitnames = names(fits))

# Lognormal je rijesenje
# sada mozemo nasumicno genereirati podatke
# i simulirati te podatke sa mean i sd

# mean() i sd() je za normalnu dist

log_params <- fitdistr(data, densfun = "log-normal" )
log_mean <- log_params$estimate[[1]]
log_sd <- log_params$estimate[[2]]

log_mean
log_sd

rnd_carat <- rlnorm(meanlog = log_mean, sdlog = log_sd, n=10000)
rnd_carat
plot(rnd_carat)


ggplot() +
  aes(x=data) +
  geom_histogram(aes(y=..density..), binwidth = 0.3, colour="white", fill="blue") +
  # geom_histogram(aes(x=rnd_carat, y=..density..), binwidth = 0.3, colour="white", fill="green") +
  geom_density(aes(x=rnd_carat),alpha=.2, fill="#FF6666")

# tjt za distribution fitting
# da radimo neke simulacije onda bismo koristili podatke o toj distr.













