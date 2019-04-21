#PJTN feature developmnet 

library(glookoGEO)

#Exploratory analysis for one months worth of data
start <- '2016-01-01 00:00:00'
end <- '2016-01-01 23:59:59'
klystron <- 'li22_31'

#PJTN
query <- paste(list("select * from test.pjtn where timestamp >= '", start, "' and timestamp <= '", end, "' and klystron = ", klystron), collapse="")
#query <- paste(list("select * from test.pjtn where timestamp >= '", start, "' and timestamp <= '", end, "'"), collapse="")
pjtn <- glookoGetQuery(query)

#exploratory analysis of pjtn
max(pjtn$timestamp, na.rm=TRUE) #6/30/2017
min(pjtn$timestamp, na.rm=TRUE) #12/20/2016

summary(pjtn$value) #max 10.6 , mean .1, median .1, 21% of values missing... are these contiguous?
nrow(pjtn[pjtn$value > 0.2,])/nrow(pjtn) #22.6% of rows above 0.2 

unique(pjtn$status) #0, 17 --> all NA values
unique(pjtn$severity) #0, 3 --> all NA values

#remove NAs from the data set
pjtn <- pjtn[!is.na(pjtn$value),]

##lets start with li_23 only
pjtn <- pjtn[pjtn$klystron=='li22_31',]


jitter_list <- lapply(unique(pjtn$klystron), function(i) {
  pjtn_klystron <- pjtn[pjtn$klystron==i,]
  jitter_events <- pvEvent(pjtn_klystron, threshold=.15, event_duration = 5)
  return(jitter_events)
})

jitter_events <- do.call(rbind, jitter_list)
write.csv(jitter_events, '~/Documents/GitHub/slac-capstone/vikram/jitter_events_5_minutes.csv')

#extract features to predict jitter events
jitter_feature_list <- lapply(unique(jitter_events$klystron), function(i) {
  je_klystron <- jitter_events[jitter_events$klystron==i,]
  pjtn_klystron <- pjtn[pjtn$klystron==i,]
  features_klystron <- timeSeriesFeatureGeneration(pjtn_klystron, je_klystron)
  return(features_klystron)
})

#Now we're going to want to something similar with the following: 
#sigma, mod_thy_resv, mod_thy_htr, swrd, focus_i, room



#extract time into variables
library(lubridate)
hour_data <-  hour(pjtn$timestamp)
day_data <- day(pjtn$timestamp)
month_data <- month(pjtn$timestamp)
year_data <- year(pjtn$timestamp)
minute_data <- minute(pjtn$timestamp)
pjtn <- cbind(pjtn, minute_data, hour_data, day_data, month_data, year_data)

#Do the same with the jitter data
hour_data <-  hour(jitter_events$timestamp)
day_data <- day(jitter_events$timestamp)
month_data <- month(jitter_events$timestamp)
year_data <- year(jitter_events$timestamp)
minute_data <- minute(jitter_events$timestamp)
jitter_events <- cbind(jitter_events, minute_data, hour_data, day_data, month_data, year_data)
write.csv(jitter_events, '~/Documents/GitHub/slac-capstone/vikram/jitter_events_5_minutes_time_features.csv')

##More exploratory analysis
#91% of the days had a jitter event... that means yesterdays data probably isn't going to be particularly predictive
#... needs to be more granular, or the PV is always fucked up at this machine
#every hour has a jitter event... is there a distribution for this though
#the distribution seems to basically be even across the day too, ranging between.03-.06... jitter does seem to rise
#a slight bit in the middle of the night ... but is that even relevant given they're using this during the day

#what is going on w/ these jitter events... 
## what % of records are jitter ... 1.84% .. so there is class imbalance here
## what % of records are above .2 ... .012%  ... so that is basically nothing
#.. these jitter events don't last long, they're every day, and every hour... it seems something much more granular
#is going to need to be done... 

### hmm... there are 41 events at
##hmmm... I'm kind of thinking we should use mini jitters to predict and extended jitter... 
# and then potentially use other PVs to predict a mini jitter + the extended jitter

library(glookoGEO)

#Exploratory analysis for one months worth of data
start <- '2016-01-01 00:00:00'
end <- '2016-12-31 23:59:59'
klystron <- 'li24_61'

#PJTN
query <- paste(list("select * from test.pjtn where timestamp >= '", start, "' and timestamp <= '", end, "' and klystron = ", klystron), collapse="")
#query <- paste(list("select * from test.pjtn where timestamp >= '", start, "' and timestamp <= '", end, "'"), collapse="")
pjtn2 <- glookoGetQuery(query)

#15 events have last more than 83 minutes.. which is 7% of the 217, 5 minute events
"select count(*), date_trunc('dayofweek', timestamp) from test.pjtn where timestamp "

query <- paste(list("select count(*), date_trunc('month', timestamp) from test.pjtn where timestamp >= '", start, 
           "' and timestamp <= '", end, "' and klystron = '", klystron, "'", " and value >= 0.15 group by date_trunc('month', timestamp)"), collapse="")