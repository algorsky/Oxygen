#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)

## Load the data from Github and check structure
hypo <- read_csv('Data/hypso.csv')
str(hypo)
hypo<- hypo %>% select(c(lakeid, depth, area, volume))
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
  mutate(multipliedVol = volume * o2) %>% #grams
  group_by(lakeid, sampledate) %>%
  mutate(lakevolume = sum(volume))%>% #m3
  mutate(lakeArea = sum(area))%>% #m2
  mutate(oxygenMass = sum(multipliedVol/(sum(volume)))) %>%
  mutate(lakeO2Mass = (sum(multipliedVol))/lakeArea)%>%
  ungroup() %>% 
  mutate(lastdays = as.numeric(ifelse((month(sampledate)>= 10), sampledate - datefirstice, sampledate-yearfirstice))) 

#Plot lakeO2Mass by last days
winteroxy%>%
  filter(lakeid == "TB") %>%
  ggplot() + geom_point(aes(x = lastdays, y = lakeO2Mass, color = hydroyear))

# Probably best practice to group and summarise so you're not plotting multiple of the samepoints
# You might want to keep different columns for different reasons, but simple for now
winteroxy_grp = winteroxy %>% 
  group_by(lakeid, sampledate) %>%
  summarise(oxygenMass = mean(oxygenMass), lastdays = mean(lastdays), hydroyear = mean(hydroyear))
winteroxy_g_m2 = winteroxy %>% 
  group_by(lakeid, sampledate) %>%
  summarise(oxygenMass = mean(lakeO2Mass), lastdays = mean(lastdays), hydroyear = mean(hydroyear))
winteroxy_g_m2 %>%
  filter(lakeid == "TB") %>%
  ggplot() + geom_point(aes(x = lastdays, y = oxygenMass)) +
  geom_smooth(aes(x = lastdays, y = oxygenMass), method = lm) +
  facet_wrap(~year(hydroyear))
# Plot regressions
winteroxy_grp %>%
  filter(lakeid == "TB") %>%
  ggplot() + geom_point(aes(x = lastdays, y = oxygenMass)) +
  geom_smooth(aes(x = lastdays, y = oxygenMass), method = lm) +
  facet_wrap(~year(hydroyear))
# You're getting that error because there are some years with only two values

# HD: All regressions together
winteroxy_grp %>%
  ggplot(aes(x = lastdays, y = oxygenMass, color = year(hydroyear), group = year(hydroyear))) + 
  geom_point() +
  geom_smooth(method = lm, se = F, alpha = 0.1) +
  scale_colour_viridis_c(name = 'year') +
  facet_wrap(~lakeid)

# HD: All regressions together, free scales
winteroxy_grp %>%
  ggplot(aes(x = lastdays, y = oxygenMass, color = year(hydroyear), group = year(hydroyear))) + 
  geom_point(alpha = 0.6) +
  geom_smooth(method = lm, se = F, alpha = 0.1) + # Turn SE off
  scale_colour_viridis_c(name = 'year') +
  facet_wrap(~lakeid, scales = 'free')

#Multiple Regressions
library(broom)
options(scipen = 999) #HD: how to turn scientific notation off
#Create a new Column with Hydro Year
winteroxy_grp <- winteroxy_grp %>%
  mutate(hydro = year(hydroyear))

#Run Multiple Regressions (HD)
ALLregression <- winteroxy_grp %>% 
  nest(-(c(hydro,lakeid))) %>% 
  mutate(
    fit = map(data, ~ lm(oxygenMass ~ lastdays, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>%
  filter(term == "lastdays")%>%
  select(hydro, estimate) 

#Run Multiple Regressions
TBregression <- winteroxy_grp %>% 
  filter(lakeid == "TB") %>%
  nest(-(hydro))%>% 
  mutate(
    fit = map(data, ~ lm(oxygenMass ~ lastdays, data = .x)),
    tidied = map(fit, tidy)
  ) %>% 
  unnest(tidied) %>%
  filter(term == "lastdays")%>%
  select(hydro, estimate) %>%
  mutate(lakeid = "TB")

write.csv(ALLregression,"/Users/adriannagorsky/Documents/Research/Oxygen/Oxygen/Data/regressionOxy.csv", row.names = FALSE)
