library(dplyr) # I use that for manipulate data
library(data.table) #I use that for faster read csv file
library(lubridate) #I use that for manipulate date/time
library(ggplot2) #I use that for plot
library(knitr) #I use that for knitr function like 'kable' for example
dfStorm <- fread("repdata-data-StormData.csv") #read file
dfStorm.overall <- dfStorm %>% 
  group_by(EVTYPE) %>% #group by event type
  summarize(FAT = sum(FATALITIES)) #calculate total of fatalities for each event
summary(dfStorm.overall) #I want to view how are the events
dfStorm.overall.zero <- dfStorm.overall %>% 
  filter(FAT > 0) 
summary(dfStorm.overall.zero) #I want to view how are the events
dfStorm.overall.20 <- dfStorm.overall %>% 
  filter(FAT > 19.75) 
summary(dfStorm.overall.20) #I want to view how are the events
dfStorm.overall.mt205 <- dfStorm.overall %>% #events with more than 205 fatalities
  filter(FAT > 205) 
dfStorm.overall.lt205 <- dfStorm.overall %>% #events with less than 205 fatalities
  filter(FAT <= 205) 
#calculate total and event numbers of 'more than 205 fatalities'
fat.mt205 <- sum(dfStorm.overall.mt205$FAT) 
fat.mt205.cnt <- length(dfStorm.overall.mt205$EVTYPE)
#calculate total and event numbers of 'less than 205 fatalities'
fat.lt205 <- sum(dfStorm.overall.lt205$FAT)
fat.lt205.cnt <- length(dfStorm.overall.lt205$EVTYPE)
#prepare a dataframe in order to better present the data
df205 <- as.data.frame(
  rbind( 
    c("More than 205 fatalities",fat.mt205,fat.mt205.cnt),
    c("Less than 205 fatalities",fat.lt205,fat.lt205.cnt) 
  )
)
names(df205) <- c("GROUP","FATALITIES","EVTYPE.NUMBER")
dfStorm.overall.mt205 <- dfStorm.overall.mt205 %>% 
  arrange(desc(FAT)) 
g <- ggplot(dfStorm.overall.mt205,aes(x=reorder(EVTYPE,FAT),y=FAT))
g <- g +  geom_bar(stat="identity",size=10)
g <- g + geom_text(aes(label=EVTYPE),vjust=-0.2)
g


