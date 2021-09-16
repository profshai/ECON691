# This is assignment 2
# Shaibu Yahaya
# September 14, 2021


rm(list=ls())
######################## PART 1 ########################
# Scraping the 2016 Presidential Election Result data from the N.Y. Times’ website
library(tidyverse)
library(rvest)

states = c("minnesota", "north-dakota", "south-dakota", "iowa", "wisconsin")

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
votes = rbind(minnesota,`north-dakota`,`south-dakota`,iowa,wisconsin)

# Save the data for future use
save(votes, file="./Build/Output/votes.RData")

######################## PART 2 ########################
library(tidycensus)
#Pre-defining variables to be used in loop
vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003",
        "B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033",
        "B07001_049","B07001_065","B07001_081" )

fips<-c(27,38,46,19,55)

# API Command
k = 1

# For 2016 census
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
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area<-st_area(temp)
  map = temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map"),map)
  
  k = k+1
  rm(temp, map)
}

CENSUS.1<-rbind(minnesotacensus,`north-dakotacensus`,`south-dakotacensus`,
                iowacensus,wisconsincensus)
states<-rbind(minnesotamap,`north-dakotamap`,`south-dakotamap`,iowamap,
              wisconsinmap)

CENSUS.1$NAME<-as.data.frame(str_split_fixed(CENSUS.1$NAME, ",", 2))[,1]
CENSUS.1$NAME<-trimws(gsub(" County","",CENSUS.1$NAME))

# Save map data for future use
rm(fips, i, k, states, vars, acs)
save.image(file="./Build/Output/CENSUS.1.RData")

# For 2019 census
#rm(list=ls())

library(tidycensus)
#Pre-defining variables to be used in loop

vars<-c("B01001_001","B01001_002","B02001_001","B02001_002", "B02001_003",
        "B05001_001","B05001_006","B07001_001", "B07001_017","B07001_033",
        "B07001_049","B07001_065","B07001_081" )

states = c("minnesota", "north-dakota", "south-dakota", "iowa", "wisconsin")

fips<-c(27,38,46,19,55)

# API Command
k = 1

for(i in fips){
  acs<-get_acs(geography="county",
               variables = vars,
               state = i,
               year = 2019,
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
  
  assign(paste0(states[k],"census"),temp)
  
  temp$area<-st_area(temp)
  map = temp %>%
    summarise(area = sum(area)) %>%
    mutate(state = states[k])
  
  assign(paste0(states[k],"map"),map)
  
  k = k+1
  rm(temp, map)
}

CENSUS.2<-rbind(minnesotacensus,`north-dakotacensus`,`south-dakotacensus`,
                iowacensus,wisconsincensus)
states<-rbind(minnesotamap,`north-dakotamap`,`south-dakotamap`,iowamap,
              wisconsinmap)

CENSUS.2$NAME<-as.data.frame(str_split_fixed(CENSUS.2$NAME, ",", 2))[,1]
CENSUS.2$NAME<-trimws(gsub(" County","",CENSUS.2$NAME))

# Save map data for future use
rm(fips, i, k, states, vars, acs)
save.image(file="./Build/Output/CENSUS.2.RData")


# Finding percentage change in each country and creating a new dataframe
#rm(list=ls())

#load("./Build/Output/CENSUS.1.RData") # from script two
#load("./Build/Output/CENSUS.2.RData") # from script two

# Percentage change between 2016 and 2019
# x is 2016 and y is 2019
delta = function(x,y) {
  change = (y-x)/x
  return(change)
}

# Merging the two datasets
merged = cbind(CENSUS.1, CENSUS.2)

merged = merged %>%
  mutate(pc_Male =  delta(merged$perMale, merged$perMale.1),
         pc_White = delta(merged$perWhite, merged$perWhite.1),
         pc_Black = delta(merged$perBlack, merged$perBlack.1),
         pc_Cit = delta(merged$perCit, merged$perCit.1),
         pc_Stay = delta(merged$perStay, merged$perStay.1),
         pc_SameCounty = delta(merged$perSameCounty, merged$perSameCounty.1),
         pc_SameSt = delta(merged$perSameSt, merged$perSameSt.1),
         pc_OthState= delta(merged$perOthState, merged$perOthState.1),
         pc_Abroad = delta(merged$perAbroad, merged$perAbroad.1))

# Creating CENSUS.3 from merged dataset and keeping the following variables
CENSUS.3 = merged[c("NAME", "state", "pc_Male", "pc_White", "pc_Black", "pc_Cit",
                    "pc_Stay", "pc_SameCounty", "pc_SameSt", "pc_OthState",
                    "pc_Abroad", "geometry")]

# Sorting the data
#load("./Build/Output/votes.RData")

votes = with(votes, votes[order(votes$state, votes$County),])

CENSUS.3 = with(CENSUS.3, CENSUS.3[order(CENSUS.3$state, CENSUS.3$NAME),])

# Combine the sorted data

core = cbind(CENSUS.3, votes)

summary(core)
              
######################## PART 3 ########################
# Map creation using ggplot and cowplot
# Side By Side Graphs

library(cowplot)

p1<-ggplot(core)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
        
p2<-ggplot(core)+ 
  geom_sf(aes(fill = pc_White))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent  White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1,p2)

# It turns out that in many cases, the more black a county is, 
# the more votes Clinton had in the elections.