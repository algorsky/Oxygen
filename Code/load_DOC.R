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
  filter(!is.na(doc))
data<-data %>% 
  filter(doc != -99.00)

# Take the average of DOC by sampling date
avgData <- data%>%
  group_by(lakeid, sampledate)%>%
  summarize(mean_DOC = mean(doc))

write.csv(avgData,"Data/doc_data_cleaned.csv", row.names = FALSE)



