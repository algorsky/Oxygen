library(tidyverse)
library(lubridate)
library(zoo)

# Package ID: knb-lter-ntl.29.27 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Physical Limnology of Primary Study Lakes 1981 - current.
inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/29/27/03e232a1b362900e0f059859abe8eb97" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

# This works, but can also use the code below to specify column names.
dt2 = read_csv(infile1) 
str(dt2) # Always check structure to make sure classes are correct
# 
# dt2 = read_csv(infile1, skip = 1, col_names = c("lakeid", "year4","daynum","sampledate","depth",     
#   "rep",     
#   "sta",     
#   "event",     
#   "wtemp",     
#   "o2",     
#   "o2sat",     
#   "deck",     
#   "light",     
#   "frlight",     
#   "flagdepth",     
#   "flagwtemp",     
#   "flago2",     
#   "flago2sat",     
#   "flagdeck",     
#   "flaglight",     
#   "flagfrlight"))


# Filter for northern lakes
dt2 = dt2 %>% filter(lakeid %in% c('AL','CB','CR','SP','TB','TR','BM')) 

# Check for duplicate sampling of oxygen (first filter NAs, then group by lake/date/depth)
dt2 %>% 
  filter(!is.na(o2)) %>% 
  group_by(lakeid, sampledate, depth) %>% 
  filter(n()>1) 

# You can see that most are the same except for the SP 2003-05-14 sample. Hmmmm
# Lets take a look at the whole profile 
dt2 %>% 
  filter(!is.na(o2)) %>% 
  filter(sampledate == '2003-05-14', lakeid == 'SP')
# Ok, so the 2nd rep here looks wrong to me. Because of this, I would just filter out all replicates, 
# becauase that solves two problems at once. This won't always be the case

dt3 = dt2 %>% filter(rep == 1)

# Only line 37 we filtered for NA values. Let's do that again without that filter. 
dt3 %>% 
  group_by(lakeid, sampledate, depth) %>% 
  filter(n()>1)

# Look, we have about 11 duplicates. Let's remove those from our main dataset 
otherDuplicates = dt3 %>% 
  group_by(lakeid, sampledate, depth) %>% 
  filter(n()>1, is.na(o2)) 
  
dt3 = dt2 %>% filter(rep == 1) %>% 
  anti_join(otherDuplicates)

# Ok, now let's get rid of sampledates when there is no oxygen data 
dt4 = dt3 %>% group_by(lakeid, sampledate) %>% 
  filter(any(!is.na(o2)))
# You can see we've lost about 2k datapoints

# Now let's check for NAs in profiles (at this point we're still grouped, so don't need to run that command)
dt5 = dt4 %>% filter(any(is.na(o2))) # there are 1647 groups. That's a lot. But looking at the data
# straight away you see there are a bunch of 0.5 m depths were no oxygen is taken. Let's get rid of all depths but integers. 

dt5 = dt4 %>% 
  filter(depth %in% c(1:40)) %>% 
  ungroup()

dt5 %>% group_by(lakeid, sampledate) %>% 
  filter(any(is.na(o2))) # Ok, now only 141 groups with some NAs. But most of the NAs are the maximum depth

# Let's find the depth at which there are more NAs than data for each lake 
maxDepths = dt5 %>% group_by(lakeid, depth) %>% mutate(o2 = as.logical(o2)) %>% 
  count(o2) %>% # Count the number of o2 data vs NA
  top_n(1, n) %>% # This keeps the row that has the highest value in each group (here TRUE vs NA) 
  filter(is.na(o2)) # These are the depths we want to get rid of 
# Ha, all that just to get the max sampling depth we want

# Get rid of the these depths
dt6 = dt5 %>% anti_join(maxDepths)

# Let's see what NAs we're STILL dealing with. 116 damn profiles. At this point I look at the date closely to decide what to do. 
remainingNAs = dt6 %>% group_by(lakeid, sampledate) %>%  
  filter(any(is.na(o2)))
View(remainingNAs)

# Let's see which of these profiles is in the winter. So at this point, since that's your focus. 
View(remainingNAs %>% filter(month(sampledate) == 12 | month(sampledate) <= 3))
# There are 22 profiles in winter. Most are just missing the max depth. Only 
# 2005-03-31 TB is weird (gotta garbage that one. ) 

# let's just interpolate the winter profiles and move on 
library(zoo)
newProfiles = remainingNAs %>% group_by(lakeid, sampledate) %>%  
  filter(any(is.na(o2))) %>% 
  filter(month(sampledate) == 12 | month(sampledate) <= 3) %>% 
  filter(!(lakeid == 'TB' & sampledate == '2005-03-31')) %>%  # get rid of garbage profile
  mutate(o2 = na.approx(o2, maxgap = 2, rule = 2)) # Look up how to use this function!!!! Especially the 'rule'

# Ok, so let's filter out those problems profiles for now (summer and winter), and add this fixed winter ones back in
dt7 = dt6 %>% anti_join(remainingNAs) %>% 
  bind_rows(newProfiles) %>% 
  arrange(lakeid, sampledate, depth)
write.csv(dt7,"/Users/adriannagorsky/Documents/Research/Oxygen/Oxygen/Data/o2_data_cleaned.csv", row.names = FALSE)
# THE END! 