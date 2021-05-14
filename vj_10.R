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
    dPy <- a1 * Py - a2 * Py * Pd
    dPd <- -b1 * Pd + b2 * Py * Pd
    list(c(dPy, dPd))
  })
}



PP <- odeModel(main = PredPrey,
                parms = c(a1 = 0.5, a2 = 0.02, b1 = 0.3, b2 = 0.01),
                times = c(from=0, to=30, by=1),
                init = c(Py = 30, Pd = 4)
                )




PP <- sim(PP)
PP.data <- out(PP)
head(PP.data)

plotPredPrey(PP.data)



obs <- read.csv2("./rabbit_fox.csv", header = TRUE)
names(obs) <- c("time", "Py", "Pd")
print(obs)
plotPredPrey(obs)


init(PP) <- c(Py=obs$Py[1], Pd=obs$Pd[1])
times(PP) <- c(from=obs$time[1], to=obs$time[length(obs$time)], by=0.5)

PP_FIT <- fitOdeModel(simObj = PP,
                      whichpar = c("a1","a2","b1","b2"),
                      obstime = obs$time,
                      yobs = obs[, c("Py","Pd")],
                      method = "BFGS",
                      lower = c(a1=0,a2=0,b1=0,b2=0),
                      upper = c(a1=1,a2=1,b1=1,b2=1))

PP_FIT$par
parms(PP) <- PP_FIT$par
parms(PP)
# parms(PP)[c("b1","b2")] <- PP_FIT$par # za posebno params mijenjanje



PP <- sim(PP)
PP.data <- out(PP)

plotPredPrey(PP.data)



obs$Type <- "Obs"
PP.data$Type <- "Sim"

allData <- rbind(obs, PP.data)

names(allData) <- c("Year", "Rabbit", "Fox", "Type")

ggplot(allData, aes(Year, group=Type)) +
  geom_line(aes(y=Rabbit, colour="Rabbit")) +
  geom_line(aes(y=Fox, colour="Fox")) +
  labs(y="Population", x="Time") +
  facet_wrap(~Type)











