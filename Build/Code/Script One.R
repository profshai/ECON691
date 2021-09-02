# INTRODUCING SCRIPTS
# Created on August 25, 2021 by Shaibu

# Clear environment
rm(list=ls())

# Load packages
# install.packages("tidyverse")

library(tidyverse)

#Function####

delta = function(x) {
  temp = (x-lag(x)/lag(x))
  return(temp)
}

covidIL = read.csv("./Data/ILCovid19.csv")

covidIL = covidIL %>%
  mutate(pc_test =  delta(Tests),
         pc_cases = delta(Cases),
         pc_deaths = delta(Deaths))

#Alternatively:
#covidIL$pc_test = delta(covidIL$Tests)

#covidIL$pc_deaths[is.infinite(covidIL$pc_deaths)] = NA

covidIL$Date = as.Date(covidIL$Date, ,format="%m/%d/%Y")

plot(covidIL$Date, covidIL$pc_cases,
     main = "Cases",
     #xlab = "",
     #ylab = "",
     type = "l",
     #color = "blue"
     )

#Summary Statistics
summary(covidIL)
