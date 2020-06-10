library(tidyverse)
library(lubridate)
library(zoo)
library(hydrostats)

# Package ID: knb-lter-ntl.32.27 Cataloging System:https://pasta.edirepository.org.
# Data set title: North Temperate Lakes LTER: Ice Duration - Trout Lake Area 1981 - current.

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/32/27/e57a6b46a237355214844e2c76fa8aa5" 
infile1 <- tempfile()
download.file(inUrl1,infile1,method="curl")

ice = read_csv(infile1) 
str(ice) # Check Structure
#Filter for northern lakes
ice = ice %>% filter(lakeid %in% c('AL','CB','CR','SP','TB','TR','BM'))
write.csv(ice,"/Users/adriannagorsky/Documents/Research/Oxygen/Oxygen/Data/ice.csv", row.names = FALSE)


# Package ID: knb-lter-ntl.301.1 Cataloging System:https://pasta.lternet.edu.
# Data set title: North Temperate Lakes LTER Morphometry and Hypsometry data for core study lakes.

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-ntl/301/1/a6905b8cdf899fe706972eee9cdfeffb" 
infile2 <- tempfile()
download.file(inUrl2,infile2,method="curl")

hypso = read_csv(infile2)
str(hypso)
hypso = hypso %>% filter(lakeid %in% c('AL','CB','CR','SP','TB','TR','BM')) 
write.csv(hypso,"/Users/adriannagorsky/Documents/Research/Oxygen/Oxygen/Data/hypso.csv", row.names = FALSE)
