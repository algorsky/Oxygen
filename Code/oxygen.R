library(tidyverse)
library(devtools)
library(lubridate)

setwd("/Users/adriannagorsky/Documents/Research/Oxygen")

## Load the data into R 
oxygen <- read.csv('oxygen.csv', sep = ",")
hypso <- read.csv('tbhypo.csv', sep = ",")
winter <- read.csv('winter.csv', sep = ",")
ice <- read.csv('iceoniceoff.csv', sep = ",")

#Convert to Dates
oxygen$sampledate = mdy(oxygen$sampledate)
oxygen$datefirstopen = mdy(oxygen$datefirstopen)

#Subset winter sampling 
#winter<- oxygen %>% 
  #filter(sampledate < datefirstopen)
#winter <- na.omit(winter)
#write.table(winter, '~/Downloads/winter.csv', sep="\t")

#Calculate Hypsometrically Weighted Oxygen rates
winteroxy<- left_join(winter, hypso, by = "depth")

winteroxy<- winteroxy %>%
  mutate("multipliedVol" = volume * o2)

winteroxy<- winteroxy %>%
  group_by(sampledate) %>%
  mutate("oxygenMass" = sum(multipliedVol/61693.5))

#Subset Ice
ice<- subset(ice, lakeid == "TB")
ice$datefirstopen = mdy(ice$datefirstopen)
winteroxy$datefirstopen = mdy(winteroxy$datefirstopen)
winteroxy<- left_join(winteroxy, ice, by = "datefirstopen")
winteroxy$datefirstice = mdy(winteroxy$datefirstice)


#Last Days since freeze up
winteroxy<- winteroxy %>%
  mutate("lastdays" = datefirstice - datefirstopen)

winteroxy$year.x <- factor(winteroxy$year.x)
ggplot(winteroxy, aes(x = lastdays, y = winteroxy$oxygenMass)) +
    geom_point()+
    g
  
