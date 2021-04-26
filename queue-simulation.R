library("ggplot2")
library("dplyr")

serverNumber = 2
serverSkill = rnorm(serverNumber) + 3 # Generates an array of normal distribution values 
qplot(serverSkill)

cookNumber = 5
cookSkill = rnorm(cookNumber) + 5
qplot(cookSkill)

storageCapacity = 100
storage = 0

mainEPM = 2 # Main entrance per minute
carEPM = 1 # Car entrance per minute
simulationMinutes = 720 # 720 minutes = 12 hours
mainEntry = rpois(simulationMinutes, mainEPM)
carEntry = rpois(simulationMinutes, carEPM)

avgTimeToLeave = 12
mainLeave = ceiling(rexp(sum(mainEntry),rate = 1/avgTimeToLeave))
carLeave = ceiling(rexp(sum(carEntry),rate = 1/avgTimeToLeave))
qplot(mainLeave)

mainMealPrice = round(rnorm(sum(mainEntry)) * 3 + 10,2) 
carMealPrice = round(rnorm(sum(carEntry)) * 3 + 12,2) 

qplot(carEntry)
qplot(carMealPrice)

dflogs <- data.frame () # Dataframe to track queues
mainQueue = 0
carQueue = 0
sales = 0
salaries = 0
profits = 0

dfmain <- data.frame () # Dataframe to track main queue
dfcar <- data.frame () # Dataframe to track car queue
mainClientNumber = 0
carClientNumber = 0


mainInitialized = FALSE
carInitialized = FALSE

for(t in 1:simulationMinutes){
  
  if(mainEntry[t] != 0){
    for(i in 1:mainEntry[t]){
      mainClientNumber <- mainClientNumber + 1
      dfmain <- rbind(dfmain, c(mainClientNumber, mainMealPrice[mainClientNumber], 'Ordering', t, '', ''))
      if(!mainInitialized){
        mainInitialized <- TRUE
      }
    }
  }
  
  if(carEntry[t] != 0){
    for(i in 1:carEntry[t]){
      carClientNumber <- carClientNumber + 1
      dfcar <- rbind(dfcar, c(carClientNumber, carMealPrice[carClientNumber], 'Ordering', t, '', ''))
      if(!carInitialized){
        carInitialized <- TRUE
      }
    }
  }
  
  if(mainInitialized){
    colnames(dfmain) <- c('Client Number','Meal Price', 'State', "Entry Time", "Order Time", "Served Time")
  }
  if(carInitialized){
    colnames(dfcar) <- c('Client Number','Meal Price', 'State', "Entry Time", "Order Time", "Served Time")
  }
  
  mainQueue <- mainQueue + mainEntry[t]
  carQueue <- carQueue + carEntry[t]

  
  profits <- sales - salaries 
  
  dflogs <- rbind(dflogs, c(t, mainQueue, carQueue, sales, salaries, profits))
  
  if(mainInitialized){
    for(i in 1:length(dfmain[,1])){
      if(dfmain[i,3] == 'Ordering'){
        if((t - as.integer(dfmain[i,4])) >= mainLeave[i]){
          mainQueue <- mainQueue - 1
          dfmain[i,3] <- 'Left'
          dfmain[i,6] <- t
        }
      }
    }
  }
  
}

colnames(dflogs) <- c('Minute','Main Queue', 'Car Queue', 'Sales', 'Salaries', 'Profits')

print(sum(mainEntry))

y_rexp <- ceiling(rexp(1, rate = 0.2))
hist(y_rexp)
print(y_rexp)
