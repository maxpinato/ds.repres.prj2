#sandbox.esempioDaRete
library(dplyr) # I use that for manipulate data
library(data.table) #I use that for faster read csv file
library(lubridate) #I use that for manipulate date/time
library(ggplot2) #I use that for plot
library(knitr) #I use that for knitr function like 'kable' for example
library(stringdist)
library(stringr)
storm_data <- fread("repdata-data-StormData.csv") #read file
storm_data$BGN_DATE <- as.POSIXct(storm_data$BGN_DATE, format="%m/%d/%Y %H:%M:%S")
storm_data <- storm_data %>% filter(BGN_DATE >= as.POSIXct('1/1/1996', format="%m/%d/%Y")) 
storm_data <- storm_data %>% mutate(EVTYPE = str_trim(toupper(EVTYPE)), 
                                    PROPDMGEXP = ifelse(PROPDMGEXP == 'B',1E9,
                                                        ifelse(PROPDMGEXP == 'K', 1E3,
                                                               ifelse(PROPDMGEXP == 'M', 1E6,0))),
                                    CROPDMGEXP = ifelse(CROPDMGEXP == 'B', 1E9,
                                                        ifelse(CROPDMGEXP == 'K', 1E3,
                                                               ifelse(CROPDMGEXP == 'M', 1E6,0))))

storm_data <- storm_data %>% mutate(PROPDMG_CASH = PROPDMG*PROPDMGEXP, CROPDMG_CASH = CROPDMG*CROPDMGEXP) %>% 
  arrange(BGN_DATE)
set.seed(42)
# There is no point clustering multiple instances of the same EVTYPE, so let's limit it to the unique events. 
EVTYPES <- unique(storm_data$EVTYPE)
# Create the distance matrix with the Jaro-Winkler distance metric. It's normalized on string distance so a single metric will work for strings of various lengths like we have in our data set. 
distance_matrix <- stringdistmatrix(EVTYPES,EVTYPES,method = "jw")
# We put the names back into the distance matrix so we can plot it. 
rownames(distance_matrix) <- EVTYPES
EVTYPES_hc <- hclust(as.dist(distance_matrix))

# Let's grab clusters at height 0.14 in the hierarchical tree. 
EV_cuts <- cutree(EVTYPES_hc, h=0.14)
EV_cuts <- as.data.frame(EV_cuts)
EV_cuts$Event_Type <- attr(EV_cuts, 'row.names')
colnames(EV_cuts) <- c("Cluster","Event_Type")

storm_data <- merge(storm_data, EV_cuts, by.x="EVTYPE", by.y="Event_Type", all.x=T, all.y=F)

cluster_demo <- storm_data %>% group_by(EVTYPE, Cluster) %>% summarize(COUNT = n()) %>% ungroup() %>% 
  arrange(Cluster)

storm_data_summarized <- storm_data %>% group_by(Cluster) %>% 
  summarize(EVTYPE = first(EVTYPE), COUNT = n(), FATALITIES = sum(FATALITIES, na.rm=T), 
            INJURIES = sum(INJURIES, na.rm=T), PROPDMG = sum(PROPDMG_CASH, na.rm=T), 
            CROPDMG = sum(CROPDMG_CASH, na.rm=T)) %>% ungroup()
