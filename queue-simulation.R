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

mainMealPrice = round(rnorm(sum(mainEntry)) * 3 + 10,2) 
carMealPrice = round(rnorm(sum(carEntry)) * 3 + 12,2) 

qplot(mainMealPrice)
qplot(carMealPrice)

dflogs <- data.frame () # Dataframe to track queues
mainQueue = 0
carQueue = 0
sales = 0
salaries = 0
profits = 0

dfmain <- data.frame () # Dataframe to track main queue
dfcar <- data.frame () # Dataframe to track car queue
mainClientNumber = 1
carClientNumber = 1


for(t in 1:simulationMinutes){
  
  if(mainEntry[t] != 0){
    for(i in 1:mainEntry[t]){
      dfmain <- rbind(dfmain, c(mainClientNumber, mainMealPrice[mainClientNumber], 'Ordering', t, '', ''))
      mainClientNumber <- mainClientNumber + 1
      carClientNumber <- carClientNumber + 1
    }
  }
  
  mainQueue <- mainQueue + mainEntry[t]
  carQueue <- carQueue + carEntry[t]
  
  profits <- sales - salaries 
  
  dflogs <- rbind(dflogs, c(t, mainQueue, carQueue, sales, salaries, profits))
}

colnames(dflogs) <- c('Minute','Main Queue', 'Car Queue', 'Sales', 'Salaries', 'Profits')
colnames(dfmain) <- c('Client Number','Meal Price', 'State', "Entry Time", "Order Time", "Served Time")
colnames(dfcar) <- c('Client Number','Meal Price', 'State', "Entry Time", "Order Time", "Served Time")

print(sum(mainEntry))

