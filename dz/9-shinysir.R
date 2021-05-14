# library(shinySIR)
#
# logistic <- function (time, init, params) {
#   with(as.list(c(init, params)), {
#     dx <- a * x * ( 1 - x/K )
#   })
# }
#
# run_shiny( model = "Rabbitss",
#            neweqns = logistic,
#            ics = c(x=1),
#            parm0 = c(a=0.1, K=20),
#            parm_names = c("Grow rate", "Carry Cap."),
#            )

library(shinySIR)


mySIRS <- function(time, init, parms) {

    with(as.list(c(init, parms)),{

        # Change in Susceptibles
        dS <- - beta * S * I + delta * R

        # Change in Infected's
        dI <- beta * S * I - gamma * I

        # Change in Recovered's
        dR <- gamma * I - delta * R

    return(list(c(dS, dI, dR)))
    })
}

run_shiny(model = "SIRS (w/out demography)", # We need to set custom model name of our model
          neweqns = mySIRS,

          ics = c(S = 9999, I = 1, R = 0),  # Susceptible, Infected, Recovered

          parm0 = c(beta = 5e-5, gamma = 0.15, delta = 0.1),
          parm_names = c("Transmission rate", "Recovery rate", "Loss of immunity"),

          parm_min = c(beta = 1e-5, gamma = 0.06, delta = 0), # Min value for each parameter
          parm_max = c(beta = 9e-5, gamma = 1 , delta = 1) # Max value for each parameter
          )