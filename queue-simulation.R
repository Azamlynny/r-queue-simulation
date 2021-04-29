library("ggplot2")
library("dplyr")

pricePerMeal = 4.00 # Price one meal unit is sold at
costPerMeal = 2.80 # Price to produce one meal unit
wageRate = 8.00 

mainServerNumber = 1
carServerNumber = 1
mainServerSkill = rnorm(mainServerNumber) + 3 # Generates an array of normal distribution values 
carServerSkill = rnorm(carServerNumber) + 3

cookNumber = 50
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
materials = 0
profits = 0

dfmain <- data.frame () # Dataframe to track main queue
dfcar <- data.frame () # Dataframe to track car queue
mainClientNumber = 0
carClientNumber = 0


mainInitialized = FALSE
carInitialized = FALSE

dfmainserver <- data.frame () # Dataframe to track servers in main entrance
dfcarserver <- data.frame () # Dataframe to track servers in main entrance
dfcook <- data.frame () # Dataframe to track cooks
for(i in 1:mainServerNumber){
  dfmainserver <- rbind(dfmainserver, ceiling(rexp(720,rate = 1/mainServerSkill[i])))
}
hist(as.integer(dfmainserver[1,]))
for(i in 1:mainServerNumber){
  dfcarserver <- rbind(dfcarserver, ceiling(rexp(720,rate = 1/carServerSkill[i])))
}
hist(as.integer(dfcarserver[1,]))

for(i in 1:cookNumber){
  dfcook <- rbind(dfcook, ceiling(rexp(720,rate = 1/cookSkill[i])))
}
hist(as.integer(dfcook[1,]))


# Convert to timestamp notation
for(i in 1:cookNumber){
  for(j in 1:(length(dfcook[i,]) - 1)){
    dfcook[i,j+1] <- dfcook[i,j+1] + dfcook[i,j] 
  }
}
for(i in 1:mainServerNumber){
  for(j in 1:(length(dfmainserver[i,]) -1)){
    dfmainserver[i,j+1] <- dfmainserver[i,j+1] + dfmainserver[i,j]
  }
}
for(i in 1:carServerNumber){
  for(j in 1:(length(dfcarserver[i,]) -1)){
    dfcarserver[i,j+1] <- dfcarserver[i,j+1] + dfcarserver[i,j]
  }
}

mainPlace <- 1
carPlace <- 1

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
  
  # Prevent errors with indexing
  if(mainInitialized){
    colnames(dfmain) <- c('Client Number','Meal Price', 'State', "Entry Time", "Order Time", "Served Time")
  }
  if(carInitialized){
    colnames(dfcar) <- c('Client Number','Meal Price', 'State', "Entry Time", "Order Time", "Served Time")
  }

  # Cook modeling
  for(i in 1:length(dfcook[,1])){
    for(j in 1:length(dfcook[i,])){
      if(dfcook[i,j] > t){
        break
      }
      if(dfcook[i,j] == t){
        if(storage < storageCapacity){
          storage <- storage + 1
        }
        materials <- materials + costPerMeal
      }    
    }
  }
  
  # Server modeling
  if(mainInitialized){
    for(i in 1:length(dfmain[,1])){
      if(dfmain[i,3] == "Ordering" && storage >= round((as.numeric(dfmain[i,2]) / pricePerMeal),2)){
        for(j in 1:length(dfmainserver[,1])){
          for(k in 1:length(dfmainserver[j,])){
            if(dfmainserver[j,k] > t){
              break
            }
            if(dfmainserver[j,k] == t){
              mainPlace <- mainPlace + 1
              sales <- sales + as.numeric(dfmain[i,2])
              storage <- round(storage - (as.numeric(dfmain[i,2]) / pricePerMeal), 2)
              dfmain[i,6] <- t
              dfmain[i,3] <- "Served"
              mainQueue <- mainQueue - 1
            }
          }
        }
      }
    }
  }
  

  
  mainQueue <- mainQueue + mainEntry[t]
  carQueue <- carQueue + carEntry[t]

  wages <- round((carServerNumber + mainServerNumber + cookNumber) * wageRate * (t/60),2)
  
  profits <- sales - materials - wages
  
  dflogs <- rbind(dflogs, c(t, mainQueue, carQueue, storage, sales, materials, wages, profits))
  
  # Main and car queue leaving modeled by exponential distribution
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
  if(carInitialized){
    for(i in 1:length(dfcar[,1])){
      if(dfcar[i,3] == 'Ordering'){
        if((t - as.integer(dfcar[i,4])) >= carLeave[i]){
          carQueue <- carQueue - 1
          dfcar[i,3] <- 'Left'
          dfcar[i,6] <- t
        }
      }
    }
  }
  print(cat(t, "/","720"))
  # if(t %% 60 == 0){
  #   print(cat(t, "/","720"))
  # }
}

colnames(dflogs) <- c('Minute','Main Queue', 'Car Queue', 'storage', 'Sales', 'Materials', 'Wages', 'Profits')

print(sum(mainEntry))

y_rexp <- ceiling(rexp(1, rate = 0.2))
hist(y_rexp)
print(y_rexp)

qplot(dflogs$Minute, dflogs$storage)

qplot(dflogs$Minute, dflogs$`Main Queue`)
