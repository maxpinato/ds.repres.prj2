library(dplyr) # I use that for manipulate data
library(data.table) #I use that for faster read csv file
library(lubridate) #I use that for manipulate date/time
library(ggplot2) #I use that for plot
library(knitr) #I use that for knitr function like 'kable' for example
dfStorm <- fread("repdata-data-StormData.csv") #read file
dfStorm.overall <- dfStorm %>% 
  group_by(EVTYPE) %>% #group by event type
  summarize(FAT = sum(FATALITIES),
            INJ = sum(INJURIES)
            ) %>% 
  mutate(
    EVTYPE.ABBR = abbreviate(EVTYPE,minlegth=3)
  )
g <- ggplot(dfStorm.overall,aes(x = EVTYPE.ABBR,y = INJ,label=EVTYPE.ABBR)) + 
      geom_point(aes(size=FAT))