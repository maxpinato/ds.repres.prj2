#Declaration of library
library(dplyr) # I use that for manipulate data
library(data.table) #I use that for faster read csv file
library(lubridate) #I use that for manipulate date/time
library(ggplot2) #I use that for plot
library(knitr) #I use that for knitr function like 'kable' for example
#dfStorm <- fread("repdata-data-StormData.csv") #read file
dfStorm.overall <- dfStorm %>%
  group_by(EVTYPE) %>%
  summarize(FAT = sum(FATALITIES), INJ = sum(INJURIES)) 
dfStorm.overall.top10FAT <- dfStorm.overall %>% 
  arrange(desc(FAT)) %>%
  top_n(10,FAT) %>% 
  mutate(EVTYPE.ABR = abbreviate(EVTYPE,minlength=3))
g.fat <- ggplot(dfStorm.overall.top10FAT,aes(x=reorder(EVTYPE.ABR,FAT),y=FAT)) + 
  geom_bar(stat="identity") + 
  xlab("Event Types") + ylab("Fatalities") + labs(title="Top 10 events for fatalities")

dfStorm.overall.top10INJ <- dfStorm.overall %>% 
  arrange(desc(INJ)) %>%
  top_n(10,INJ) %>% 
  mutate(EVTYPE.ABR = abbreviate(EVTYPE,minlength=3))
g.inj <- ggplot(dfStorm.overall.top10INJ,aes(x=reorder(EVTYPE.ABR,INJ),y=INJ)) +
  geom_bar(stat="identity") + 
  xlab("Event Types") + ylab("Injurious") + labs(title="Top 10 events for injurious")

## ECONOMICS

