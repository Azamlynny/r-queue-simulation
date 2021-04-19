library("ggplot2")
library("dplyr")

serverNumber = 30
serverSkill = rnorm(serverNumber) + 5 # Generates an array of normal distribution values 
qplot(serverSkill)

mainEPM = 2 # Main entrance per minute
carEPM = 1 # Car entrance per minute
simulationMinutes = 720 # 12 hours
mainEntry = rpois(simulationMinutes, mainEPM)
carEntry = rpois(simulationMinutes, carEPM)
qplot(mainEntry)
qplot(carEntry)

dflogs <- data.frame ()