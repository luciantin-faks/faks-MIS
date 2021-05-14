library(simmer)
library(simmer.plot)

set.seed(2)

NUM_SALTERA <- 4
SIM_TIME <- 60

env <- simmer("Banka")

korisnik <- trajectory("korisnik trajectory") %>%
  log_("arrives at the Banka") %>%
  seize("salter", 1) %>%
  log_("Banka station") %>%
  timeout(function () round(rnorm(1, mean = 10, sd = 5), digits = 0)) %>%
  release("salter",1) %>%
  log_("leaves banka")

env %>%
  add_resource("salter", NUM_SALTERA) %>%
  add_generator("korisnik", korisnik, function () round(rnorm(1, mean = 7, sd = 2), digits = 0), 1) %>%
  run(SIM_TIME)

plot(korisnik, metric = "activity_time")
plot(korisnik, metric = "waiting_time")
plot(korisnik, metric = "flow_time")



