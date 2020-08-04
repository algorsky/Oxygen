library(tidyverse)
library(lubridate)
library(zoo)

# Package ID: knb-lter-ntl.1.52 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Chemical Limnology of Primary Study Lakes: Nutrients, pH and Carbon 1981 - current.
# Data set creator:  NTL Lead PI - University of Wisconsin 
# Data set creator:  John Magnuson - University of Wisconsin 
# Data set creator:  Stephen Carpenter - University of Wisconsin 
# Data set creator:  Emily Stanley - University of Wisconsin 
# Metadata Provider:  NTL Information Manager - University of Wisconsin 
# Contact:  NTL Information Manager -  University of Wisconsin  - ntl.infomgr@gmail.com
# Contact:  NTL Lead PI -  University of Wisconsin  - ntl.leadpi@gmail.com
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/1/52/802d63a4c35050b09ef6d1e7da3efd3f" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

# This works, but can also use the code below to specify column names.
dt2 = read_csv(infile1) 
str(dt2) # Always check structure to make sure classes are correct

# Filter for northern lakes
dt2 = dt2 %>% filter(lakeid %in% c('AL','CB','CR','SP','TB','TR','BM'))
dt3<- dt2[c(1:8,14)] #Only keep important columns

# Check for duplicate sampling of oxygen (first filter NAs, then group by lake/date/depth)
#Also filter out -99.00 values
data<-dt3 %>% 
  filter(!is.na(doc)) %>% 
  group_by(lakeid, sampledate, depth) %>% 
  filter(n()>1) 
data<-data %>% 
  filter(doc != -99.00) %>% 
  group_by(lakeid, sampledate, depth)

# Look, we have about 11 duplicates. Let's remove those from our main dataset 
otherDuplicates = data %>% 
  group_by(lakeid, sampledate, depth) %>% 
  filter(n()>1) 

orh

datafull = data %>% filter(rep == 1) %>% 
  anti_join(otherDuplicates)

# Ok, now let's get rid of sampledates when there is no oxygen data 
dt5 = dt4 %>% group_by(lakeid, sampledate) %>% 
  filter(any(!is.na(doc)))
# You can see we've lost about 2k datapoints

# Now let's check for NAs in profiles (at this point we're still grouped, so don't need to run that command)
dt6 = dt5 %>% filter(any(is.na(doc))) # there are 1647 groups. That's a lot. But looking at the data
# straight away you see there are a bunch of 0.5 m depths were no oxygen is taken. Let's get rid of all depths but integers. 

dt6 = dt5 %>% 
  filter(depth %in% c(0:40)) %>% 
  ungroup()

dt6 %>% group_by(lakeid, sampledate) %>% 
  filter(any(is.na(doc))) # Ok, now only 141 groups with some NAs. But most of the NAs are the maximum depth

# Let's find the depth at which there are more NAs than data for each lake 
maxDepths = dt6 %>% group_by(lakeid, depth) %>% mutate(doc = as.logical(doc)) %>% 
  count(doc) %>% # Count the number of doc data vs NA
  top_n(1, n) %>% # This keeps the row that has the highest value in each group (here TRUE vs NA) 
  filter(is.na(doc)) # These are the depths we want to get rid of 
# Ha, all that just to get the max sampling depth we want

# Get rid of the these depths
dt6 = dt5 %>% anti_join(maxDepths)

# Let's see what NAs we're STILL dealing with. 116 damn profiles. At this point I look at the date closely to decide what to do. 
remainingNAs = dt6 %>% group_by(lakeid, sampledate) %>%  
  filter(any(is.na(doc)))
View(remainingNAs)

# Let's see which of these profiles is in the winter. So at this point, since that's your focus. 
View(remainingNAs %>% filter(month(sampledate) == 12 | month(sampledate) <= 3))
# There are 22 profiles in winter. Most are just missing the max depth. Only 
# 2005-03-31 TB is weird (gotta garbage that one. ) 

# let's just interpolate the winter profiles and move on 
library(zoo)
newProfiles = remainingNAs %>% group_by(lakeid, sampledate) %>%  
  filter(any(is.na(doc))) %>% 
  filter(month(sampledate) == 12 | month(sampledate) <= 3) %>% 
  filter(!(lakeid == 'TB' & sampledate == '2005-03-31')) %>%  # get rid of garbage profile
  mutate(doc = na.approx(doc, maxgap = 2, rule = 2)) # Look up how to use this function!!!! Especially the 'rule'

# Ok, so let's filter out those problems profiles for now (summer and winter), and add this fixed winter ones back in
dt7 = dt6 %>% anti_join(remainingNAs) %>% 
  bind_rows(newProfiles) %>% 
  arrange(lakeid, sampledate, depth)
write.csv(dt7,"/Users/adriannagorsky/Documents/Research/Oxygen/Oxygen/Data/doc_data_cleaned.csv", row.names = FALSE)
# THE END! 


