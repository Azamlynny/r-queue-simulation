library("ggplot2")
library("dplyr")

serverNumber = 30
serverSkill = rnorm(serverNumber) + 5 # Generates an array of normal distribution values 
qplot(serverSkill)

mainEPM = 2 # Main entrance per minute
carEPM = 1 # Car entrance per minute
simulationMinutes = 720 # 720 minutes = 12 hours
mainEntry = rpois(simulationMinutes, mainEPM)
carEntry = rpois(simulationMinutes, carEPM)

totalPeople = sum(mainEntry) + sum(carEntry)
print(totalPeople)

dflogs <- data.frame () # Dataframe to track queues
mainQueue = 0
carQueue = 0
sales = 0
salaries = 0
profits = 0

for(t in 1:simulationMinutes){
  
  mainQueue <- mainQueue + mainEntry[t]
  carQueue <- carQueue + carEntry[t]
  
  profits <- sales - salaries 
  
  dflogs <- rbind(dflogs, c(t, mainQueue, carQueue, sales, salaries, profits))
}

colnames(dflogs) <- c('Minute','Main Queue', 'Car Queue')
