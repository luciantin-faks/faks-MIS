library(simmer)
library(simmer.plot)

set.seed(2)


NUM_MACHINES <- 2
WASHTIME <- 5
T_INTER <- 7
SIM_TIME <- 60

env <- simmer("Carwash")

car <- trajectory("car trajectory") %>%
  log_("arrives at the carwash") %>%
  seize("wash", 1) %>%
  log_("enters the carwash station") %>%
  timeout(WASHTIME) %>%
  set_attribute("dirt_removed", function () sample(50:90,1)) %>%
  log_(function () as.character(get_attribute(env, "dirt_removed")) ) %>%
  release("wash",1) %>%
  log_("leaves washstation")


# plot(car, verbose = TRUE)

env %>%
  add_resource("wash", NUM_MACHINES) %>%
  add_generator("car_intial", car, at(rep(0, 4))) %>%
  add_generator("car", car, function () sample( (T_INTER-2):(T_INTER+2), 1)) %>%
  run(SIM_TIME)

cars <- get_mon_arrivals(env)
head(cars)

washers <- get_mon_resources(env)
head(washers)


plot(cars, metric = "activity_time")
plot(cars, metric = "waiting_time")
plot(cars, metric = "flow_time")

plot(washers, metric = "utilization")
plot(washers, metric = "usage", items=c("queue", "server"))



