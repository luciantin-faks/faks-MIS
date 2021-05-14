library(simecol)
library(ggplot2)

plotPredPrey <- function (data){
  ggplot(data, aes(time)) +
    geom_line(aes(y=Pd, colour="Fox pop.")) +
    geom_line(aes(y=Py, colour="Rabbit pop.")) +
    labs(y="Population", x="Time")
}


PredPrey <- function(time, init, parms) {
  with(as.list(c(init, parms)), {
    dPy <- a1 * Py - a2 * Py * Pd - c1 * Py
    dPd <- -b1 * Pd + b2 * Py * Pd - c2 * Pd
    list(c(dPy, dPd))
  })
}


PP <- odeModel(main = PredPrey,
                parms = c(a1 = 0.57, a2 = 0.017, b1 = 0.98, b2 = 0.017, c1 = 0.1, c2 = 0.1),
                times = c(from=0, to=30, by=1),
                init = c(Py = 60, Pd = 16)
                )


PP <- sim(PP)
PP.data <- out(PP)

plotPredPrey(PP.data)
