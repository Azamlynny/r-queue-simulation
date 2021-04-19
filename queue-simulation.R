library("ggplot2")
library("dplyr")

serverNumber = 30
serverSkill = rnorm(serverNumber) + 5 # Generates an array of normal distribution values 
qplot(serverSkill)
