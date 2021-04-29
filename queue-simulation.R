library("ggplot2")
library("dplyr")

##########################  GENERAL SETTINGS  #############################

simulationMinutes = 720 # 720 minutes = 12 hours - 8:00 AM to 8:00 PM
wageRate = 8.00 # Wage paid per hour
pricePerMeal = 4.00 # Price one meal unit is sold at
costPerMeal = 2.80 # Price to produce one meal unit

##########################  CUSTOMER SETTINGS  ############################

mainEPM = 2 # Average number of main entrance customers per minute
carEPM = 1 # Average number of car entrance customers per minute
avgTimeToLeave = 12 # Average time for customer to leave the queue

##########################  COOK SETTINGS  ################################

cookNumber = 50 # Number of cooks
storageCapacity = 100 # Maximum storage capacity 
storage = 0 # Storage at the beginning of the day

##########################  SERVER SETTINGS  ##############################

mainServerNumber = 1 # Number of servers in the main entrance
carServerNumber = 1 # Number of servers in the car entrance

##########################  WORKER SKILLSETS  #############################

# Normal distributions for the skillsets of workers - Constants for Exponential distributions
mainServerSkill = rnorm(mainServerNumber) + 3
carServerSkill = rnorm(carServerNumber) + 3
cookSkill = rnorm(cookNumber) + 5

##########################  MEAL DISTRIBUTION  ############################

# Normal distributions for meals purchased by customers
mainMealPrice = round(rnorm(sum(mainEntry)) * 3 + 10,2) 
carMealPrice = round(rnorm(sum(carEntry)) * 3 + 12,2) 

###########################################################################
###########################################################################
###########################################################################

start_time <- Sys.time() # Execution time tracking

mainEntry = rpois(simulationMinutes, mainEPM)
carEntry = rpois(simulationMinutes, carEPM)

mainLeave = ceiling(rexp(sum(mainEntry),rate = 1/avgTimeToLeave))
carLeave = ceiling(rexp(sum(carEntry),rate = 1/avgTimeToLeave))


dflogs <- data.frame () # Dataframe to track queues
# Temporary variables used in dflogs
mainQueue = 0
carQueue = 0
sales = 0
materials = 0
profits = 0

dfmain <- data.frame () # Tracks main queue
dfcar <- data.frame () # Tracks car queue
dfmainserver <- data.frame () # Tracks servers in main entrance
dfcarserver <- data.frame () # Tracks servers in main entrance
dfcook <- data.frame () # Tracks cooks

# Temprorary variables for simulation
mainClientNumber = 0
carClientNumber = 0
mainPlace <- 1
carPlace <- 1
mainInitialized = FALSE
carInitialized = FALSE

# Generate exponential distributions for servers and cooks
for(i in 1:mainServerNumber){
  dfmainserver <- rbind(dfmainserver, ceiling(rexp(720,rate = 1/mainServerSkill[i])))
}
for(i in 1:mainServerNumber){
  dfcarserver <- rbind(dfcarserver, ceiling(rexp(720,rate = 1/carServerSkill[i])))
}
for(i in 1:cookNumber){
  dfcook <- rbind(dfcook, ceiling(rexp(720,rate = 1/cookSkill[i])))
}

# Convert exponential distribution to timestamp notation for iterative scheduling 
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

# Iterative time scheduling simulation
for(t in 1:simulationMinutes){
  
  # Entrance of customers
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
  if(carInitialized){
    for(i in 1:length(dfcar[,1])){
      if(dfcar[i,3] == "Ordering" && storage >= round((as.numeric(dfcar[i,2]) / pricePerMeal),2)){
        for(j in 1:length(dfcarserver[,1])){
          for(k in 1:length(dfcarserver[j,])){
            if(dfcarserver[j,k] > t){
              break
            }
            if(dfcarserver[j,k] == t){
              carPlace <- carPlace + 1
              sales <- sales + as.numeric(dfcar[i,2])
              storage <- round(storage - (as.numeric(dfcar[i,2]) / pricePerMeal), 2)
              dfcar[i,6] <- t
              dfcar[i,3] <- "Served"
              carQueue <- carQueue - 1
            }
          }
        }
      }
    }
  }
  
  # Statistic tracking 
  mainQueue <- mainQueue + mainEntry[t]
  carQueue <- carQueue + carEntry[t]
  wages <- round((carServerNumber + mainServerNumber + cookNumber) * wageRate * (t/60),2)
  profits <- sales - materials - wages
  
  # Statistics stored in dflogs for graphing
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
  
  # Print Simulation Progress
  cat(t, "/","720\n")
}

# Label dflogs data
colnames(dflogs) <- c('Minute','Main Queue', 'Car Queue', 'storage', 'Sales', 'Materials', 'Wages', 'Profits')

# Data plotting
qplot(dflogs$Minute, dflogs$storage)

qplot(dflogs$Minute, dflogs$`Main Queue`)

# Print program execution time
end_time <- Sys.time()
cat("Execution time:", end_time - start_time, "minutes")