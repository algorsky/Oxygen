#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)

## Load the data from Github

# HD: I find the read_csv function in readr package (part of tidyverse) much easier to work with 90% of the time. 
# Very good with timestamps, and defaults strings to characters (not factors!)
hypo <- read_csv('Data/tbhypo.csv')
ice = read_csv('Data/iceoniceoff.csv') %>% filter(lakeid == 'TB') 

hypso <- read.csv('Data/tbhypo.csv', sep = ",")
winter <- read.csv('Data/winter.csv', sep = ",")
ice <- read.csv('Data/iceoniceoff.csv', sep = ",")
ice <- subset(ice, lakeid == "TB")

#Add a Year to Ice before Joining it to winter oxygen
ice$year <- ice$year + 1

#Join Hypsometry and Ice datasheets
winteroxy<- left_join(winter, hypso, by = "depth")
winteroxy<- left_join(winteroxy, ice, by = "year")

# HD: Can double these up with a pipe 
winteroxy <- left_join(winter, hypso, by = "depth") %>% 
  left_join(ice, by = "year") 

#Convert to Dates
winteroxy$datefirstice = mdy(winteroxy$datefirstice)
winteroxy$sampledate = mdy(winteroxy$sampledate)

#Calculate Hypsometrically Weighted Oxygen rates (need help in matching Timothy's protocols)
# HD: Don't need the quotes around multipledVol

#Do I do anything with depth 8m? How to compensate for Ice thickness and change in lake volume?
winteroxy<- winteroxy %>%
  mutate("multipliedVol" = volume * o2)

winteroxy<- winteroxy %>%
  group_by(sampledate) %>%
  mutate("oxygenMass" = sum(multipliedVol/61693.5))

#Last Days since freeze up
winteroxy <- winteroxy %>%
  mutate("lastdays" = (sampledate - datefirstice))

#Example 1982 y = -0.03x - 6.05 (slightly different than Timothy's)
year1982 <- subset(winteroxy, year == 1982)
lm(oxygenMass~lastdays, data = year1982)


# HD: Alternative. It's good to use different object names to be able to troubleshoot. Here it's hard to investigate winteroxy because we keep rewriting it
# Although now that I run this, I don't think this is the code that you want. 

# HD: Can double these up with a pipe 
winteroxy.group <- left_join(winter, hypso, by = "depth") %>% 
  left_join(ice, by = "year") %>% 
  mutate(datefirstice = mdy(datefirstice), sampledate = mdy(sampledate)) %>%  #Convert to Dates
  mutate("multipliedVol" = volume * o2) %>% #Calculate Hypsometrically Weighted Oxygen rates (need help in matching Timothy's protocols) 
  group_by(sampledate) %>%
  mutate(oxygenMass = sum(multipliedVol/61693.5)) %>% 
  ungroup() %>% 
  mutate(lastdays = as.numeric(sampledate - datefirstice))
str(winteroxy.group) # HD: Always good to look at your dataframe structure to make sure it did what you wanted

# Plot regressions
ggplot(winteroxy.group) + geom_point(aes(x = lastdays, y = oxygenMass)) +
  geom_smooth(aes(x = lastdays, y = oxygenMass), method = lm) +
  facet_wrap(~year)

# HD: How to handle multiple regressions at once
library(broom)
options(scipen = 999) #HD: how to turn scientific notation off
winteroxy.group %>% 
  nest(-year) %>% 
  mutate(
    fit = map(data, ~ lm(oxygenMass ~ lastdays, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied)


#Tim's subset
thesis <- winteroxy %>%
  filter(year >1981, year <1992)

#Average Mimicking Table 10b
wholelake<- thesis %>%
  group_by(lakeid.x)%>%
  summarise(Mean = mean(oxygenMass),
            sd = sd(oxygenMass),
            min = min(oxygenMass),
            max = max(oxygenMass))

#Example 1982 (slightly different than Timothy's)
year1982<- subset(winteroxy, year == 1982)
summary(lm(oxygenMass~lastdays, data =year1982))

