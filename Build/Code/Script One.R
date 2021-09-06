# INTRODUCING SCRIPTS
# Created on August 25, 2021 by Shaibu

# Clear environment
rm(list=ls())

# Load packages
# install.packages("tidyverse")

library(tidyverse)

#Function####

# Difference between old and new called diff
diff = function(x) {
  df = (x-lag(x))
  return(df)
}

# Percentage change in diff
delta = function(x) {
  temp = (x-lag(x))/lag(x)
  return(temp)
}
  
covidIL = read.csv("./Data/ILCovid19.csv")

covidIL = covidIL %>%
  mutate(df_test =  diff(Tests),
         df_cases = diff(Cases),
         df_deaths = diff(Deaths))

covidIL = covidIL %>%
  mutate(pc_test =  delta(df_test),
         pc_cases = delta(df_cases),
         pc_deaths = delta(df_deaths))

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
