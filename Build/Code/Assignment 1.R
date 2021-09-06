# ASSIGNMENT ONE

# Clear environment
rm(list=ls())

# Load packages
library(tidyverse)

# Import data
covidIL = read.csv("./Data/ILCovid19.csv")

# DIF Function to calculate the number of daily new tests, new cases, and new deaths
dif = function(x) {
  df = (x- lag(x))
  return(df)
}

# Adding to data frame with the mutate function
covidIL = covidIL %>%
  mutate(df_test =  dif(Tests),
         df_cases = dif(Cases),
         df_deaths = dif(Deaths))

# Delta function to calculate the percentage changes
delta = function(x) {
  change = (x - lag(x))/lag(x)
  return(change)
}

# Daily new tests, new cases, and new deaths
covidIL = covidIL %>%
  mutate(df_test =  dif(Tests),
         df_cases = dif(Cases),
         df_deaths = dif(Deaths))


# Daily Percentage Change in New Cases, Daily Percentage Change in
# Tests, Daily Percentage Change in Deaths
covidIL = covidIL %>%
  mutate(pc_test =  delta(df_tests),
         pc_cases = delta(df_cases),
         pc_deaths = delta(df_deaths))

# Converting Date to date type
covidIL$Date = as.Date(covidIL$Date, format="%m/%d/%Y")

# Plotting the daily percentage change in new cases
plot(covidIL$Date, covidIL$pc_cases,
     main = "new COVID Cases",
     xlab = "Date",
     ylab = "COVID cases",
     type = "l",
     col = "blue"
)

# Plotting the daily percentage change in new tests
plot(covidIL$Date, covidIL$pc_tests,
     main = "new COVID tests",
     xlab = "Date",
     ylab = "COVID tests",
     type = "l",
     col = "green"
)

# Plotting the daily percentage change in new deaths
plot(covidIL$Date, covidIL$pc_deaths,
     main = "new COVID deaths",
     xlab = "Date",
     ylab = "COVID deaths",
     type = "l",
     col = "red"
)

#Summary Statistics
summary(covidIL)
