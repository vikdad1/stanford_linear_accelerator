#This script creates a feature/response data set, where all features are
#extracted from the process variables of yesteday

lapply(1:length(feature_variables), function(i) {
  
  start <- feature_variables[[i]][1]
  month_start <- feature_variables[[i]][2]
  end <- feature_variables[[i]][3]
  klystron <- feature_variables[[i]][4]
  month <- feature_variables[[i]][5]
  
  print('month')
  
  #read in jitter, hourly metrics, pjtn data
  jitter <- read.csv(paste0("jitter_events/jitter_events_", month, ".csv", sep=""))
  #hm <- read.csv(paste0("hourly_metrics/hourly_metrics_", month, ".csv", sep=""))
  #pjtn <- read.csv(paste0("raw_pjtn/raw_pjtn_", month, ".csv", sep=""))
  
  #ensure timestamp data type
  jitter$timestamp <- as.POSIXct(as.character(jitter$timestamp))
  #pjtn$timestamp <- as.POSIXct(as.character(pjtn$timestamp))
  #hm$timestamp <- as.POSIXct(as.character(hm$timestamp))
  
  #consider all of these different classes of jitter events
  jitter_events <- summary((subset(jitter, average_value >= .15 & event_duration == 60 & threshold==.15)$event_duration_seconds/60))
  jitter_events_2 <- summary((subset(jitter, average_value >= .2 & event_duration == 60 & threshold==.15)$event_duration_seconds/60))
  jitter_events_120 <- summary((subset(jitter, average_value >= .15 & event_duration == 120 & threshold==.15)$event_duration_seconds/60))
  jitter_events_120_2 <- summary((subset(jitter, average_value >= .2 & event_duration == 120 & threshold==.15)$event_duration_seconds/60))
  jitter_events_180 <- summary((subset(jitter, average_value >= .15 & event_duration == 180 & threshold==.15)$event_duration_seconds/60))
  jitter_events_180_2 <- summary((subset(jitter, average_value >= .2 & event_duration == 180 & threshold==.15)$event_duration_seconds/60))
  
  je <- as.data.frame(rbind(jitter_events, jitter_events_2, jitter_events_120, jitter_events_120_2, jitter_events_180,
        jitter_events_180_2))
  
  return(je)  

})

#findings: 
#li28_31 - the duration of the event is pretty much as long as the threshold that is set
#li22_31 - some of the durations are shorter.. like 30 min .. but for the most part, I don't think this is a huge issue
#li24_61 - some of the durations are shorter.. like 30 min .. but for the most part, I don't think this is a huge issue
