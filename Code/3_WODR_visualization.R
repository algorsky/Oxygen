#Load Packages
library(tidyverse)
library(devtools)
library(lubridate)
library(ggplot2)
library(gridExtra)

## Load the data from Github and check structure
wodr <- read_csv('Data/regressionOxy.csv')
str(wodr)

## Load DOC data
doc <- read_csv('Data/doc_data_cleaned.csv')
str(doc)

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
wodr_facet<-wodr%>%
  ggplot(aes(x = hydro, y = estimate, shape = name, color = name))+
  geom_point()+
  geom_smooth(method = lm, se = F, alpha = 0.1)+
  facet_wrap(~lakeid)

ggsave("Figures/WODRTrend.png", width = 6.5, height = 6, units = 'in', wodr_facet)

# Correlation Matrix Plot (must change to wide format from long)
wodr.wide <- read_csv('Data/regressionOxy.csv') %>% 
  pivot_wider(names_from = lakeid, values_from = estimate)
dat.cor = round(cor(wodr.wide,use = 'complete.obs'),2)

library(ggcorrplot)
ggcorrplot(dat.cor, type = "lower",
           lab = TRUE) 
#ggsave('Figures/correlationPlot_ggcorrplot.png', width = 5, height = 5)

library(corrplot)
#png(file = "Figures/correlationPlot.png", width = 5, height = 5, units = 'in', res = 300)
corrplot(dat.cor, method = "ellipse",type = 'upper', tl.col = 'black')
dev.off()


#Trout Bog WODR with DOC trend

wodr<-wodr%>%
  filter(lakeid == "TB" & name == "AG") %>%
  ggplot(aes(x = hydro, y = estimate, shape = name, color = name))+
  geom_point()+
  geom_path()+
  xlab("")+
  ylab("Winter Oxygen Depletion Rate")+
  theme_bw()+
  theme(legend.position = "none")

doc<-doc%>%
    filter(lakeid == "TB")%>%
    ggplot(aes(x = sampledate, y = mean_DOC, shape = lakeid, color = lakeid))+
    geom_point()+
  xlab("")+
    ylab("Dissolved Organic Carbon (mg/L)")+
    theme_bw()+
    theme(legend.position = "none")
  
doc<- grid.arrange(wodr, doc, ncol = 2, nrow = 1,
             top = ("Trout Bog Trend"),
             bottom = ("Sample Date"))
ggsave("/Figures/WodrDoc.png", width = 10, height = 6, units = 'in', doc)
