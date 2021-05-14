library(shinySIR)


logistic <- function(time, init, parms) {
  with(as.list(c(init, parms)), {
    dx <- a * x * (1 - x / K)
    list(dx)
  })
}

run_shiny(model = "asdasd",
          neweqns = logistic,
          ics = c(x = 1),
          parm0 = c(K = 1, a = 0.1),
          parm_names = c("Carrying capacity", "Growth"),
          parm_min = c(K = 0, a = 0),
          parm_max = c(K = 20, a = 1)
          )