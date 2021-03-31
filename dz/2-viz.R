# Title     : TODO
# Objective : TODO
# Created by: tin
# Created on: 31. 03. 2021.

data(faithful)
summary(faithful)

plot(faithful$waiting,faithful$eruptions, ylab = "Eruption time in mins", xlab = "Waiting time to next eruption (in mins)")
