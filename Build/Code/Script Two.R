################## WORKING WITH APIS #################### 
# Created on September 1, 2021 by Shaibu

rm(list=ls())

# Load packages
# install.packages("tidyverse")
#install.packages("rvest")

library(tidyverse)
library(rvest)
library(httr)
library(jsonlite)
library(sf)

################## ELECTION RESULTS ##################

url = "https://www.nytimes.com/elections/2016/results/illinois"
webpage = read_html(url)
tables = webpage %>%
  html_nodes("table")

# tables

# Put result in a data frame

results = tables[2] %>%
  html_table(fill=TRUE, header=TRUE) %>%
  as.data.frame()

head(results)

# Illinois Results Only 

Illinois = results %>%
  rename("County" = "Vote.by.county") %>%
  mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
         "Trump" = as.numeric(gsub(",","", Trump)),
         "pctClinton" = (Clinton)/(Clinton + Trump),
         "pctTrump" = (Trump)/(Clinton + Trump))

#view(Illinois)

# Loop for all selected states

rm(list=ls())

library(tidyverse)
library(rvest)

states = c("illinois", "indiana", "kentucky", "missouri", "wisconsin", "iowa")

for (i in states){
  url = paste0("https://www.nytimes.com/elections/2016/results/",i)
  webpage = read_html(url)
  tables = webpage %>%
    html_nodes("table")
  
  results = as.data.frame(html_table(tables[2], header=TRUE, fill=TRUE))
  
  temp = results %>%
    rename("County" = "Vote.by.county") %>%
    mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
           "Trump" = as.numeric(gsub(",","", Trump)),
           "pctClinton" = (Clinton)/(Clinton + Trump),
           "pctTrump" = (Trump)/(Clinton + Trump),
           "state" = i)
  
  assign(i, temp)
}

# Combine the data
votes = rbind(illinois, indiana, kentucky, missouri, wisconsin, iowa)

# Save the data for future use
save(votes, file="./Build/Output/votes.RData")


##################### CENSUS DATA API #####################
##################### CREATE CENSUS DATA #####################
#install.packages("tidycensus")

rm(list=ls())
library(tidycensus)

#Pre-defining variables to be used in loop
vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003",
        "B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033",
        "B07001_049","B07001_065","B07001_081" )

states<-c("kentucky","indiana","illinois","missouri","wisconsin","iowa")

fips<-c(21,18,17,29,55,19)

# API Command
k = 1

#i in fips)
for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars,
               state = i,
               year = 2016,
               geometry = TRUE)
  temp = acs %>%
    mutate(variable2 = case_when(variable=="B01001_001" ~ "TotPop",
                                 variable=="B01001_002" ~ "Male",
                                 variable=="B02001_001" ~ "TotRace",
                                 variable=="B02001_002" ~ "White",
                                 variable=="B02001_003" ~ "Black",
                                 variable=="B05001_001" ~ "TotCit",
                                 variable=="B05001_006" ~ "NonCit",
                                 variable=="B07001_001" ~ "TotMob",
                                 variable=="B07001_017" ~ "Stay",
                                 variable=="B07001_033" ~ "SameCounty",
                                 variable=="B07001_049" ~ "SameSt",
                                 variable=="B07001_065" ~ "OthState",
                                 variable=="B07001_081" ~ "Abroad",
                                 TRUE ~ "other")) %>%
    select(!c(moe,variable)) %>%
    spread(key=variable2, value=estimate) %>%
    mutate(perMale = Male/TotPop,
           perWhite = White/TotPop,
           perBlack = Black/TotPop,
           perCit = 1-(NonCit/TotCit),
           perStay = Stay/TotMob,
           perSameCounty = SameCounty/TotMob,
           perSameSt = SameSt/TotMob,
           perOthState = OthState/TotMob,
           perAbroad = Abroad/TotMob) %>%
    select("GEOID","NAME",starts_with("per"),"geometry") %>%
    mutate(state = states[k])
  
  #ifelse(k==1, census = temp, census = rbind(census, temp))
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area<-st_area(temp)
  map = temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  #ifelse(k==1, MAP=map, MAP=rbind(MAP, map))
  
  assign(paste0(states[k],"map"),map)
  
  k = k+1
  rm(temp, map)
}

census<-rbind(illinoiscensus,iowacensus,kentuckycensus,wisconsincensus,
              missouricensus,indianacensus)
states<-rbind(illinoismap,iowamap,kentuckymap,wisconsinmap,missourimap,
              indianamap)

census$NAME<-as.data.frame(str_split_fixed(census$NAME, ",", 2))[,1]
census$NAME<-trimws(gsub(" County","",census$NAME))

# Save map data for future use
rm(fips, i, k, states, vars, acs)
save.image(file="./Build/Output/census.RData")

