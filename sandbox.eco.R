#Declaration of library
library(dplyr) # I use that for manipulate data
library(data.table) #I use that for faster read csv file
library(lubridate) #I use that for manipulate date/time
library(ggplot2) #I use that for plot
library(knitr) #I use that for knitr function like 'kable' for example
library(stringdist)
#dfStorm <- fread("repdata-data-StormData.csv") #read file

expList <- c("K","M","B")
expMult <- c(10^3,10^6,10^9)

dfs.prop <- dfStorm %>%
  filter(PROPDMGEXP %in% expList & PROPDMG > 0) %>%
dfs.prop.err <- dfStorm %>% 
  filter(!(PROPDMGEXP %in% expList) & PROPDMG > 0) %>%
  mutate(YEAR = year((as.Date(BGN_DATE,"%m/%d/%Y"))))
dfs.prop.err.year <- unique(dfs.prop.err$YEAR)
dfs.prop.err.sum <- summary(dfs.prop.err$PROPDMG)
dfs.prop.group <- dfs.prop %>%
  group_by(EVTYPE,PROPDMGEXP) %>% 
  summarize(PROP = sum(PROPDMG)) 

dfs.crop <- dfStorm %>%
  filter(CROPDMGEXP %in% expList & CROPDMG > 0) 
dfs.crop.err <- dfStorm %>% 
  filter(!(CROPDMGEXP %in% expList) & CROPDMG > 0) %>%
  mutate(YEAR = year((as.Date(BGN_DATE,"%m/%d/%Y"))))
dfs.crop.err.year <- unique(dfs.crop.err$YEAR)
dfs.crop.err.sum <- summary(dfs.crop.err$PROPDMG)


