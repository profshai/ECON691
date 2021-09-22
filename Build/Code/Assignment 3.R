# This is assignment 3
# Shaibu Yahaya
# September 14, 2021

rm(list=ls())

# Import libraries
library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(tidycensus)
library(stringr)
library(ggplot2)
library(sf)
library(cowplot)
library(stargazer)

######################## PART 1 ########################

change = function(x, y) {
  diff = y-x
  return(diff)
}

# Importing data
df = read.csv("./Data/df.csv")
load("./Build/Output/votes.RData") # from assignment two

# Order data
votes = with(votes, votes[order(votes$state, votes$County),])
df = with(df, df[order(df$state, df$County),])

# Change in Democrats votes
df = df %>%
  mutate(c_Democrats =  change(votes$Clinton, df$Democrats))

# Change in Republicans votes
D_VOTES = df %>%
  mutate(c_Republicans =  change(votes$Trump, df$Republicans))


######################## PART 2 ########################

# Load and order CENSUS.2 data
load("./Build/Output/CENSUS.2.RData") # from assignment two

CENSUS.2 = with(CENSUS.2, CENSUS.2[order(CENSUS.2$state, CENSUS.2$County),])

# Merging D_VOTES and CENSUS.2
core = cbind(CENSUS.2, D_VOTES)

load("./Build/Output/States.Rda") 

# Graph for change in Republican votes
map3 = ggplot(core) + 
  geom_sf(aes(fill = c_Republicans))+
  scale_fill_gradient(low="white",high="red",
                      aes(name="Change in Republican votes"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = States,
    fill = NA,
    colour = "black",
    size=1,
    inherit.aes = FALSE
  )

map3

ggsave("./Plots/republican_votes_change.eps", plot = map3, device = "eps")

# Graph for change in Democrats votes
map4 = ggplot(core) + 
  geom_sf(aes(fill = c_Democrats))+
  scale_fill_gradient(low="white",high="blue",
                      aes(name="Change in Democrats votes"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())+
  geom_sf(
    data = States,
    fill = NA,
    colour = "black",
    size=1,
    inherit.aes = FALSE
  )

map4

ggsave("./Plots/democrats_votes_change.eps", plot = map4, device = "eps")

# According to Bloomberg, at least 161 million Americans voted 2020
# compared to nearly 139 million citizens in 2016.
# This reflected in a positive change in votes in almost all counties considered


######################## PART 3 ########################

# Load and order CENSUS.3 data
load("./Build/Output/CENSUS.3.RData") # from assignment two

CENSUS.3 = with(CENSUS.3, CENSUS.3[order(CENSUS.3$state, CENSUS.3$County),])

# Merging core and CENSUS.3
reg_data = cbind(CENSUS.3, core)

# Drop repeated columns
reg_data = subset(reg_data, select = -c(County.1,County.2, state.1, state.2, geometry.1))
summary(reg_data)

# Regression models
# Change in Republican votes against the percentage of the 
# 2019 population that is Male and White
mod1=lm(c_Republicans~perWhite+perMale, data=reg_data)
summary(mod1)

# Change in Democrats votes against the percentage of the 
# 2019 population that is Male and White
mod2=lm(c_Democrats~perWhite+perMale, data=reg_data)
summary(mod2)

# Change in Republican votes against the change in the percentage of the 
# population between 2016 and 2019 that is Male and White
mod3=lm(c_Republicans~pc_White+pc_Male, data=reg_data)
summary(mod3)

# Change in Democrats votes against the change in the percentage of the 
# population between 2016 and 2019 that is Male and White
mod4=lm(c_Democrats~pc_White+pc_Male, data=reg_data)
summary(mod4)

# Change in Republican votes against the change in the percentage of the 
# population between 2016 and 2019 that is Male and White
# With state fixed effects
mod5=lm(c_Republicans~pc_White+pc_Male+factor(state)-1, data=reg_data)
summary(mod5)

# Change in Democrats votes against the change in the percentage of the 
# population between 2016 and 2019 that is Male and White
# With state fixed effects
mod6=lm(c_Democrats~pc_White+pc_Male+factor(state)-1, data=reg_data)
summary(mod6)



