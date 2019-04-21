setwd('../feature_engineering_functions/') #find the function folder
source('feature_times.R')
source('pvEvent.R')
source('hourly_metrics.R')

##SET WORKING DIRECTORY TO KLYSTON
setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li28_31/')

for (i in 1:length(feature_variables)) {
  
  start <- feature_variables[[i]][1]
  month_start <- feature_variables[[i]][2]
  end <- feature_variables[[i]][3]
  klystron <- feature_variables[[i]][4]
  month <- feature_variables[[i]][5]
  
  print(month)
  
  #PJTN
  query <- paste(list("select * from test.pjtn where timestamp >= '", 
                      start, "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  pjtn <- glookoGetQuery(query)
  
  write.csv(pjtn, paste('raw_pjtn_', month, '.csv', sep=""))
  
  #remove NAs from the data set
  pjtn <- pjtn[!is.na(pjtn$value),]
  
  #process jitter events
  jitter_events_15 <- pvEvent(pjtn[pjtn$timestamp >= as.POSIXct(month_start),], threshold=.15, event_duration = 15)
  jitter_events_60 <- pvEvent(pjtn[pjtn$timestamp >= as.POSIXct(month_start),], threshold=.15, event_duration = 60)
  jitter_events_90 <- pvEvent(pjtn[pjtn$timestamp >= as.POSIXct(month_start),], threshold=.15, event_duration = 90)
  jitter_events_120 <- pvEvent(pjtn[pjtn$timestamp >= as.POSIXct(month_start),], threshold=.15, event_duration = 120)
  jitter_events_180 <- pvEvent(pjtn[pjtn$timestamp >= as.POSIXct(month_start),], threshold=.15, event_duration = 180)
  
  jitter_events_15 <- cbind(jitter_events_15, 'threshold'=.15, 'event_duration'=15, month)
  jitter_events_60 <- cbind(jitter_events_60, 'threshold'=.15, 'event_duration'=60, month)
  jitter_events_90 <- cbind(jitter_events_90, 'threshold'=.15, 'event_duration'=90, month)
  jitter_events_120 <- cbind(jitter_events_120, 'threshold'=.15, 'event_duration'=120, month)
  jitter_events_180 <- cbind(jitter_events_180, 'threshold'=.15, 'event_duration'=180, month)
  
  jitter_events <- rbind(jitter_events_15, jitter_events_60,jitter_events_90,
                            jitter_events_120,jitter_events_180)
  
  th15 <- cbind(month, 
                'je_15'=nrow(jitter_events_15), 
                'je_60'=nrow(jitter_events_60),
                'je_90'=nrow(jitter_events_90), 
                'je_120'=nrow(jitter_events_120), 'je_180'=nrow(jitter_events_180))
  
  hourly_metrics <- hourlyMetrics(pjtn) #Hourly Metrics
  
  write.csv(jitter_events, paste0('jitter_events/jitter_events_', month, '.csv', sep=""))
  write.csv(hourly_metrics, paste0('hourly_metrics/hourly_metrics_', month, '.csv', sep=""))
  write.csv(th15, paste0('jitter_event_counts/jitter_event_counts_', month, '.csv', sep=""))
}