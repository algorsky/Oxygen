#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)

## Load the data from Github and check structure
hypo <- read_csv('Data/hypso.csv')
str(hypo)
hypo<- hypo %>% select(c(lakeid, depth, volume))
ice <- read_csv('Data/ice.csv')
# Shift ice on by a year
ice <- ice %>%
  mutate(yearfirstice=lag(datefirstice))
str(ice)
oxygen <- read_csv('Data/o2_data_cleaned.csv')
str(oxygen)
#Remove unnecessary variables
oxygen<- oxygen %>% select(lakeid:o2sat)%>%
  rename(year = year4) 

#Join datasets
oxygenjoin <- left_join(oxygen, hypo, by = c("lakeid" = "lakeid", "depth" = "depth")) %>% 
  left_join(ice, by = c("lakeid" = "lakeid", "year" = "year")) %>%
#Add hydro year column
  mutate(hydroyear = 
           as.Date(ifelse(month(sampledate)<=10, sampledate, sampledate+years(1)),origin = "1970-01-01")) 

#Work with just Winter Dataset
winteroxy<- oxygenjoin %>%
  filter(sampledate >= datefirstice | sampledate <= datefirstopen) %>%
#Calculate Hypsometrically Weighted o2 rates
  mutate(multipliedVol = volume * o2) %>%
  group_by(lakeid, sampledate) %>%
  mutate(lakevolume = sum(volume))%>%
  mutate(oxygenMass = sum(multipliedVol/(sum(volume)))) %>% 
  ungroup() %>% 
  mutate(lastdays = (sampledate - yearfirstice))



