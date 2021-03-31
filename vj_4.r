data(cars)

plot(cars,
     main = "Stopping distance ~ Speed ",
     xlab = "Speed", ylab = "Distance")

cor(cars$speed, cars$dist)
cor(cars)

cm <- lm(dist ~ speed, data = cars)
# y = -17.5 + 3.9 * speed + error
# vraca predvideni stopping distance kada stavim speed

summary(cm)

one <- 1:100
two <- 1:100
cm <- lm(one ~ two)
summary(cm)



cm <- lm(dist ~ speed, data = cars)
# y = -17.5 + 3.9 * speed + error
# vraca predvideni stopping distance kada stavim speed

summary(cm)

library(purrr)
library(broom)
library(dplyr)
library(ggplot2)

coefs <- cm$coefficients
B_0 <- coefs[[1]]
B_1 <- coefs[[2]]

# odstupanje
cm_sigma <- sigma(cm)

x <- cars$speed

sim_fun <- function (){
  y <- B_0 + B_1 * x + rnorm(length(x), 0, cm_sigma)
  fit <- lm(y~x)
  return(fit)
}


sim_fit <- sim_fun()

par(mfrow=(c(2,1)))

plot(cars, col.main="blue", main="Cars")
abline(cm, col="blue")
# plot(cars, col.main="blue")
abline(sim_fit, col="red", lty=2)

plot(sim_fit$model$x, sim_fit$model$y, col.main="blue", main="Simulated")
abline(cm, col="blue")
# plot(cars, col.main="blue")
abline(sim_fit, col="red", lty=2)


set.seed(1000)
sims <- rerun(1000, sim_fun())

sims %>%
  map_dbl(~sigma(.)) %>%
  data.frame(sigma=.) %>%
  ggplot(aes(sigma)) +
  geom_density(fill="red", alpha=0.5) +
  geom_vline(xintercept = cm_sigma)

sims %>%
  map_df(glance) %>%
  pull(p.value) %>%
  {. < 0.05} %>%
  mean()


summary(cm)$coef[, "Pr(>|t|)"]

intercept_t <- summary(cm)$coef[, "Pr(>|t|)"][[1]]
speed_t <- summary(cm)$coef[, "Pr(>|t|)"][[2]]

speed_t
intercept_t


sims %>%
  map_dbl(~summary(.x)$coef[, "Pr(>|t|)"][[1]]) %>%
  {. < 0.05} %>%
  mean()


sims %>%
  map_dbl(~summary(.x)$coef[, "Pr(>|t|)"][[2]]) %>%
  {. < 0.05} %>%
  mean()
