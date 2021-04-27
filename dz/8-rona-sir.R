
library(simecol)

PopSt <- 44520404

param_values <- c(
  beta = 0.000000355 / (PopSt / 1000000), # infection rate
  gama = 0.2 # recovery rate
)

init_values <- c(
  S = PopSt - 1,
  I = 1,
  R = 0
)

# Algeria
# start = 25.2.2020
# end = 27.4.2021
# diff = 427

times <- c(from = 0, to = 427, by = 1) # num of days

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


total_sim <- tail(SIR.data, 1)["I"] + tail(SIR.data, 1)["R"]
Rnul <- (PopSt * param_values["beta"] ) / param_values["gama"]

print("Total real : 121,112")
print(paste("Total sim : ", total_sim))
print(Rnul)
