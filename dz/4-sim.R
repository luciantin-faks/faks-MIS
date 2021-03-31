# volume depends on girth
# simulacija Volume
# prediktor Girth
# response variable (Y) is un/explained by the predictor (X)
# lm(dist ~ speed, data = cars)
# y=β0+β1x => dist = Intercept + (β * speed)
# X => Girth
# Y => Volume
# Volume = Intercept + (β * Girth)
# lm(Volume ~ Girth)

library(purrr)
library(ggplot2)

trees
cor(trees$Volume, trees$Girth)

cm <- lm(Volume ~ Girth, data = trees)
cm

coefs <- cm$coefficients
B_0 <- coefs[[1]]
B_1 <- coefs[[2]]

# odstupanje
cm_sigma <- sigma(cm)

x <- trees$Girth

sim_fun <- function (){
  y <- B_0 + B_1 * x + rnorm(length(x), 0, cm_sigma)
  fit <- lm(y~x)
  return(fit)
}


sim_fit <- sim_fun()

par(mfrow=(c(2,1)))

plot(trees$Girth,trees$Volume, col.main="blue", main="Trees", ylab = "Volume", xlab = "Girth")

abline(cm, col="blue")

abline(sim_fit, col="red", lty=2)

plot(sim_fit$model$x, sim_fit$model$y, col.main="blue", main="Simulated", ylab = "Volume", xlab = "Girth")

abline(cm, col="blue")

abline(sim_fit, col="red", lty=2)


## Sim

set.seed(1000)
sims <- rerun(1000, sim_fun())

# Estimates of the standard deviation
sims %>%
  map_dbl(~sigma(.)) %>%
  data.frame(sigma=.) %>%
  ggplot(aes(sigma)) +
  geom_density(fill="red", alpha=0.5) +
  geom_vline(xintercept = cm_sigma)
