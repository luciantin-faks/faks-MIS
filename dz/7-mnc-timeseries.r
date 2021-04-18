library(ggplot2)
library(reshape2)

data <- nhtemp

change <- diff(data)

plot(density(change))

value_start <- data[1]

simulate <- function(x){
  change.sim <- rnorm(length(x), mean = mean(x), sd = sd(x))
  data.prices <- cumsum( c(value_start, change.sim) )
  return(data.prices)
}

simulations <- replicate(10, simulate(change))

colnames(simulations) <- sapply(seq_len(ncol(simulations)), function(x) paste0("Sim ", x))

simulations.data <- melt(simulations)

colnames(simulations.data) <- c("Year", "Simulation", "Temp")

head(simulations.data)
ggplot() +
  geom_line(data = simulations.data,
            aes(x = Year,
                y = Temp,
                group = Simulation,
                color = Simulation),
            show.legend = FALSE) +
  geom_line(aes(x = seq_along(data), y = data), col="white") +
  xlab("Year") +
  ylab("Price")