#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)
library(ggplot2)

## Load the data from Github and check structure
doc <- read_csv('Data/doc_data_cleaned.csv')
str(doc)


ggplot(doc, aes(x = sampledate, y = mean_DOC, shape = lakeid, color = lakeid))+
  geom_point()+
  xlab("Sample date")+
  facet_wrap(~lakeid)
