#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)
library(ggplot2)

## Load the data from Github and check structure
wodr <- read_csv('Data/regressionOxy.csv')
str(wodr)

#Add a column to distiguish from Tim's data
wodr <- wodr%>%
  mutate(name = "AG")

#Load Tim's data
tim<- read_csv('Data/tim_oxy.csv')

#Join my dataset with Tim's
wodr <- rbind(wodr, tim)

# From your regression dataframe, pull out the slopes for each year and plot distributions for each lake
wodr %>%
  ggplot(aes(x = estimate, fill = name))+
           geom_histogram()+
  facet_wrap(~lakeid)

#For each lake, pull the slope for each year and plot it as a timeseries. Is there a trend?

wodr %>%
  filter(name == "AG") %>%
  ggplot(aes(x = hydro, y = estimate))+
  geom_point()+
  geom_smooth(method = lm, se = F, alpha = 0.1)+
  facet_wrap(~lakeid)

#potential trend in Trout Bog and Trout Lake?

#With Tim's data also plotted
wodr%>%
  ggplot(aes(x = hydro, y = estimate, shape = name, color = name))+
  geom_point()+
  facet_wrap(~lakeid)




