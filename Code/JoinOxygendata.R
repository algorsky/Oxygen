#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)

## Load the data from Github and check structure
hypo <- read_csv('Data/hypso.csv')
str(hypo)
ice <- read_csv('Data/ice.csv')
str(ice)
oxygen <- read_csv('Data/o2_data_cleaned.csv')
str(oxygen)
oxygen<- oxygen %>% select(lakeid:o2sat)%>%
  rename(year = year4) %>% left_join(oxygen, hypo, by = "depth") %>% 
  left_join(ice, by = "year") 