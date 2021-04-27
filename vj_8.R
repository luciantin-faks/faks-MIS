
library(simecol)

param_values <- c(
  beta = 0.004, # infection rate
  gama = 0.5 # recovery rate
)

init_values <- c(
  S = 999,
  I = 1,
  R = 0
)

times <- c(from = 0, to = 10, by = 1) # num of days

sir_model <- function (time, init, parms) {
  with( as.list(c(init, parms)), {
    dS <- - beta * S * I
    dI <- beta * S * I - gama * I
    dR <- gama * I

    # povratne vrijednosti su promijene u vremenu
    return(list(c(dS, dI, dR)))
  })
}

SIR <- odeModel(main = sir_model,
                parms = param_values,
                init = init_values,
                times = times)

SIR <- sim(SIR)

SIR.data <- out(SIR)

SIR.data

library(ggplot2)

ggplot(SIR.data, aes(time)) +
  geom_line(aes(y=S, colour="Susceptible" )) +
  geom_line(aes(y=I, colour="Infected" )) +
  geom_line(aes(y=R, colour="Recovered" ))