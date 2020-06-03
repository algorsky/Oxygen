#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)

## Load the data from Github
hypso <- read.csv('Data/tbhypo.csv', sep = ",")
winter <- read.csv('Data/winter.csv', sep = ",")
ice <- read.csv('Data/iceoniceoff.csv', sep = ",")
ice<- subset(ice, lakeid == "TB")

#Join Hypsometry and Ice datasheets
winteroxy<- left_join(winter, hypso, by = "depth")
winteroxy<- left_join(winteroxy, ice, by = "year")

#Convert to Dates
winteroxy$datefirstice = mdy(winteroxy$datefirstice)
winteroxy$sampledate = mdy(winteroxy$sampledate)

#Calculate Hypsometrically Weighted Oxygen rates (need help in matching Timothy's protocols)
winteroxy<- winteroxy %>%
  mutate("multipliedVol" = volume * o2)

winteroxy<- winteroxy %>%
  group_by(sampledate) %>%
  mutate("oxygenMass" = sum(multipliedVol/61693.5))

#Last Days since freeze up
winteroxy<- winteroxy %>%
  mutate("lastdays" = (sampledate - datefirstice ))

#Example 1982 y = -0.03x - 6.05 (slightly different than Timothy's)
year1982<- subset(winteroxy, year == 1982)
lm(oxygenMass~lastdays, data =year1982)
