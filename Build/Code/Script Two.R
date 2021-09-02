# WORKING WITH APIs
# Created on September 1, 2021 by Shaibu

# Clear environment
rm(list=ls())

# Load packages
# install.packages("tidyverse")
#install.packages("rvest")

library(tidyverse)
library(rvest)

url = "https://www.nytimes.com/elections/2016/results/illinois"
webpage = read_html(url)
tables = webpage %>%
  html_nodes("table")
# tables

#results1 = as.data.frame(html_table(tables[2], header=TRUE, fill=TRUE))

results = tables[2] %>%
  html_table(fill=TRUE, header=TRUE) %>%
  as.data.frame()

head(results)

Illinois = results %>%
  rename("County" = "Vote.by.county") %>%
  mutate("Clinton" = as.numeric(gsub(",", "", Clinton)),
         "Trump" = as.numeric(gsub(",","", Trump)),
         "pctClinton" = (Clinton)/(Clinton + Trump),
         "pctTrump" = (Trump)/(Clinton + Trump))

view(Illinois)

# Using a loop
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




