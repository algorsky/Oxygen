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
str(ice) # Always check structure to make sure classes are correct

