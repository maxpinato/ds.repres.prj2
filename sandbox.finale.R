#sandbox.finale
library(plyr)
library(dplyr) # I use that for manipulate data
library(data.table) #I use that for faster read csv file
library(lubridate) #I use that for manipulate date/time
library(ggplot2) #I use that for plot
library(knitr) #I use that for knitr function like 'kable' for example
library(stringdist)
library(stringr)
library(cowplot)

#FUNCTIONS -----------
fn.getEventClusterOffset <- function(events,lstHC,offset){
  
  lst_hc <- as.data.frame(cutree(lstHC,h=offset))
  lst_hc$EVTYPE <- events
  colnames(lst_hc) <- c("Cluster","EVTYPE")
  lst_hc <- lst_hc %>% arrange(Cluster)
  return(lst_hc)
}


#First Data Read
#dfStorm <- fread("repdata-data-StormData.csv") #read file
#Date Format and Filter
dfStorm.flt <- dfStorm %>% 
  mutate( BGN_DATE = as.Date(as.character(BGN_DATE),"%m/%d/%Y %H:%M:%S") ) %>%
  filter(year(BGN_DATE) >= 1996)
#Valuate PROPDMGEXP. I need to use explicit library prefix because n() is in conflicts
#between plyr and dplyr (and I need both the library)
dfPExp <- dfStorm.flt %>%
  group_by(PROPDMGEXP) %>%
  dplyr::summarise(    CNT = n(),    VAL = sum(PROPDMG)  )
#Valuate CROPDMGEXP
dfCExp <- dfStorm.flt %>%
  group_by(CROPDMGEXP) %>%
  dplyr::summarise(CNT = n(),VAL = sum(CROPDMG))
#In both the case, values are empty or irrilevant, but cases number is 
#relevant.
#According to data dictionary, when the exponential is empty or different,
#we have to see the remarks.
#Reading remarks, we cannot assume to have zero damage, but we have to do because of
#missing data.
#So, I filter only data with valid exponential.
#But those errors haven't to envolve consideration about fatalities and injurius.

#Before any consideration about dangerous and economics, I have to 
#cluster the EVTYPE because there are different string of the same event.

lstEv <- unique(dfStorm.flt$EVTYPE)
dstEv <- stringdistmatrix(lstEv,lstEv,method="jw")
rownames(dstEv) <- lstEv
lstEv_hc <- hclust(as.dist(dstEv))
#Effettuo tre carotaggi, a 0.10, 0.15 e 0.20.
lstEv_hc.c10 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.10)
lstEv_hc.c15 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.15)
lstEv_hc.c20 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.20)
lstEv_hc.c25 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.25)
lstEv_hc.c30 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.30)
lstEv_hc.c35 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.35)
lstEv_hc.c40 <- fn.getEventClusterOffset(lstEv,lstEv_hc,0.40)
#0.40 fails on cluster 2
#0.35 fails on cluster 36
#0.30 fails on cluster 168
#0.25 seems to be a valid offset.
lstEv_hc.optimal <- lstEv_hc.c25 %>% group_by(Cluster) %>% mutate(EVTYPE.CLUSTER = min(EVTYPE)) %>% ungroup()
dfStorm.clustered <- merge(dfStorm.flt,lstEv_hc.optimal,
    by.x = "EVTYPE",by.y = "EVTYPE",all.x = T,all.y = F) 

#So, I divide in two df, one for dangerous and one for economics.
#Dangerous Dataframe
dfStorm.dang <- dfStorm.clustered %>% 
  select(EVTYPE.CLUSTER,FATALITIES,INJURIES) %>% 
  group_by(EVTYPE.CLUSTER) %>% 
  dplyr::summarise(
    FAT.TOT = sum(as.numeric(as.character(FATALITIES))),
    INJ.TOT = sum(as.numeric(as.character(INJURIES)))
  )

dfStorm.dang.top5Fatal <- dfStorm.dang %>%
  top_n(5,FAT.TOT) %>%
  arrange(desc(FAT.TOT)) %>%
  select(EVTYPE.CLUSTER,FAT.TOT) %>%
  mutate(EVTYPE.ABBR = abbreviate(EVTYPE.CLUSTER,minlength=6))

dfStorm.dang.top5Injur <- dfStorm.dang %>%
  top_n(5,INJ.TOT) %>%
  arrange(desc(INJ.TOT)) %>%
  select(EVTYPE.CLUSTER,INJ.TOT) %>%
  mutate(EVTYPE.ABBR = abbreviate(EVTYPE.CLUSTER,minlength=6))

g.fatal <- ggplot(dfStorm.dang.top5Fatal, aes(x = EVTYPE.ABBR,y = FAT.TOT)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Most dangerous - Fatalities" ) + 
  labs(x="Events (clustered)",y="Fatalities")
g.injuries <- ggplot(dfStorm.dang.top5Injur, aes(x = EVTYPE.ABBR,y = INJ.TOT)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Most dangerous - Injuries" ) + 
  labs(x="Events (clustered)",y="Injuries")


#Economics Dataframe
dfStorm.economics <- dfStorm.clustered %>% select(EVTYPE.CLUSTER,CROPDMG,CROPDMGEXP,PROPDMG,PROPDMGEXP)
dfStorm.fltValidCash <- dfStorm.economics %>%
  filter( CROPDMGEXP %in% c("K","M","B") & PROPDMGEXP %in% c("K","M","B") )
#Calculate Cash Exponential
lstMapping <- c("K" = 10^3,"M" = 10^6,"B" = 10^9)
dfStorm.wCashExp <- dfStorm.fltValidCash %>% 
  mutate(
    CEXP = revalue(CROPDMGEXP ,lstMapping),
    PEXP = revalue(PROPDMGEXP ,lstMapping)
  )
#Calculate correct cash and overall cash
dfStorm.wCash <- dfStorm.wCashExp %>% 
  mutate(
    CROP.CASH = as.numeric(CROPDMG) * as.numeric(CEXP),
    PROP.CASH = as.numeric(PROPDMG) * as.numeric(PEXP)
  )
dfStorm.wCash <- dfStorm.wCash %>%
  mutate( OVER.CASH = CROP.CASH + PROP.CASH) %>%
  group_by(EVTYPE.CLUSTER) %>%
  dplyr::summarise(
    CROP.CASH.TOT = sum(CROP.CASH),
    PROP.CASH.TOT = sum(PROP.CASH),
    OVER.CASH.TOT = sum(OVER.CASH)
  )

dfStorm.eco.top5Crop <- dfStorm.wCash %>%
  top_n(5,CROP.CASH.TOT) %>%
  arrange(desc(CROP.CASH.TOT)) %>%
  select(EVTYPE.CLUSTER,CROP.CASH.TOT) %>%
  mutate(EVTYPE.ABBR = abbreviate(EVTYPE.CLUSTER,minlength=6))

dfStorm.eco.top5Prop <- dfStorm.wCash %>%
  top_n(5,PROP.CASH.TOT) %>%
  arrange(desc(PROP.CASH.TOT)) %>%
  select(EVTYPE.CLUSTER,PROP.CASH.TOT) %>%
  mutate(EVTYPE.ABBR = abbreviate(EVTYPE.CLUSTER,minlength=6))

dfStorm.eco.top5Overall <- dfStorm.wCash %>%
  top_n(5,OVER.CASH.TOT) %>%
  arrange(desc(OVER.CASH.TOT)) %>%
  select(EVTYPE.CLUSTER,OVER.CASH.TOT) %>%
  mutate(EVTYPE.ABBR = abbreviate(EVTYPE.CLUSTER,minlength=6))

g.ecoCrop <- ggplot(dfStorm.eco.top5Crop, aes(x = EVTYPE.ABBR,y = CROP.CASH.TOT)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Most expansive - Crop Damage" ) + 
  labs(x="Events (clustered)",y="Crop Damage")

g.ecoProp <- ggplot(dfStorm.eco.top5Prop, aes(x = EVTYPE.ABBR,y = PROP.CASH.TOT)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Most expansive - Property Damage" ) + 
  labs(x="Events (clustered)",y="Property Damage")

g.ecoOver <- ggplot(dfStorm.eco.top5Overall, aes(x = EVTYPE.ABBR,y = OVER.CASH.TOT)) + 
  geom_bar(stat = "identity") + 
  ggtitle("Most expansive - Overall Damage" ) + 
  labs(x="Events (clustered)",y="Overall Damage")