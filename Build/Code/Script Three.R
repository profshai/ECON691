# This script is about regression using data created in script two. 
# Shaibu Yahaya
# September 8, 2021

rm(list=ls())

library(tidyverse)

######################## LOAD EXISTING DATA ########################

load("./Build/Output/census.RData") # from script two
load("./Build/Output/votes.RData") # from script two

######################## DATA CLEANING ########################
# Join the two datasets by county
#Split them into two at the comma and keep only the first column
#Trim white spaces before "county" and get rid of everything 
# after the white space

census = census %>%
  mutate(county = as.data.frame(str_split_fixed(NAME, ",", 2))[,1],
         county = trimws(gsub(" County", "", county)))

votes$County[which(votes$County=="DeWitt")]<-"De Witt"
votes$County[which(votes$County=="JoDaviess")]<-"Jo Daviess" 
votes$County[which(votes$County=="LaClede")]<-"Laclede" 
votes$County[which(votes$County=="LaRue")]<-"Larue" 
votes$County[which(votes$County=="St. Louis City")]<-"St. Louis city" 
census$county[which(census$county=="St. Louis")]<-"St. Louis County"

# Merge data, keeping everything else that don't match
core<-merge(census, votes, by.x=c("state","county"), by.y=c("state","County"),all=TRUE)
summary(core)


######################## REGRESSIONS ########################
mod1<-lm(pctClinton~perWhite, data=core)
summary(mod1)

mod2<-lm(pctClinton~perWhite-1, data=core)
summary(mod2)

mod3<-lm(pctClinton~perWhite+perMale+perSameCounty+perSameSt+
           perOthState+perAbroad-1, data=core)
summary(mod3)

# Add a dummy for each state (states fixed effects)
mod4<-lm(pctClinton~perWhite+perMale+perSameCounty+perSameSt+
           perOthState+perAbroad+factor(state)-1, data=core)
summary(mod4)

mod5<-lm(pctClinton~perWhite+perMale+perSameCounty+perSameSt+
           perOthState+perAbroad+factor(state)-1, data=core)
summary(mod5)

# Import the results (latex, html, text options)
library(stargazer)

stargazer(mod1, mod2, mod3, mod4, mod5, type="html", 
          out="./Build/Output/table1.html")
stargazer(core, type="html", out="./Build/Output/summ.html")

# Always dont forget Summary Statistics

################# PLOTS ########################
# Plotting other elements of the mod List
p1<-ggplot(core, aes(x=perWhite))+
  geom_point(aes(y=pctClinton))+
  xlim(0,1)+
  xlab("Percent Population White")+
  ylim(0,1)+
  ylab("Percent Clinton")+
  geom_line(aes(y=mod1$fitted.values), color="red")+
  theme_bw()

p1

p2<-ggplot(mod1, aes(x=seq(1,600)))+
  geom_line(aes(y=mod1$residuals), color="blue")+
  geom_line(aes(y=0), color="black")+
  theme_light()
p2

ggplot(core)+ 
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),
                      aes(name="Percent Clinton"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank()) +
  geom_sf(
    data = census,
    fill=NA, colour="black",
    size=1,
    inherit.aes=FALSE
  )


# Map creation
# Side By Side Graphs

library(cowplot)

il.acs<-core %>%
  subset(state=="illinois")

p1<-ggplot(il.acs)+
  geom_sf(aes(fill = pctClinton))+
  scale_fill_gradient(low="white",high="blue",limits=c(0,1),aes(name="Percent Clinton"))+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())
p1

p2<-ggplot(il.acs)+ 
  geom_sf(aes(fill = perWhite))+
  scale_fill_gradient(low="black",high="white",limits=c(0,1),aes(name="Percent  White"))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank())

plot_grid(p1,p2)



