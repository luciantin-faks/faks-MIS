library(simmer)

set.seed(2)

NUM_SALTERA <- 4
NUM_LISTIC <- 1
SIM_TIME <- 60

DOLAZAK_KORISNIKA <- function() round(rnorm(1, mean = 7, sd = 2), 0)
OBRADA_ZAHTJEVA <- function() round(rnorm(1, mean = 10, sd = 5), 1)
UZIMANJE_LISTICA <- function() round(rnorm(1, mean = 0.5, sd = 0.2), 1)

env <- simmer("Banka")

korisnik <- trajectory("korisnik trajectory") %>%

  log_("arrives at the Banka") %>%
  seize("listic", 1) %>%
  timeout(UZIMANJE_LISTICA) %>%
  release("listic",1) %>%
  log_("uzme listic") %>%

  log_("Banka station") %>%
  seize("salter", 1) %>%
  timeout(OBRADA_ZAHTJEVA) %>%
  release("salter",1) %>%
  log_("leaves banka")

env %>%
  add_resource("salter", NUM_SALTERA) %>%
  add_resource("listic", NUM_LISTIC) %>%
  add_generator("korisnik", korisnik, DOLAZAK_KORISNIKA, 1) %>%
  run(SIM_TIME)

