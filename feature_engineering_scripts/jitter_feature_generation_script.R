#This script creates a feature/response data set, where all features are
#extracted from the process variables of yesteday

for (i in 1:length(feature_variables)) {
  
  start <- feature_variables[[i]][1]
  month_start <- feature_variables[[i]][2]
  end <- feature_variables[[i]][3]
  klystron <- feature_variables[[i]][4]
  month <- feature_variables[[i]][5]
  
  print('month')

  #read in jitter, hourly metrics, pjtn data
  jitter <- read.csv(paste0("jitter_events/jitter_events_", month, ".csv", sep=""))
  hm <- read.csv(paste0("hourly_metrics/hourly_metrics_", month, ".csv", sep=""))
  pjtn <- read.csv(paste0("raw_pjtn/raw_pjtn_", month, ".csv", sep=""))
  
  #ensure timestamp data type
  jitter$timestamp <- as.POSIXct(as.character(jitter$timestamp))
  pjtn$timestamp <- as.POSIXct(as.character(pjtn$timestamp))
  hm$timestamp <- as.POSIXct(as.character(hm$timestamp))
  
  #consider all of these different classes of jitter events
  jitter_events <- subset(jitter, average_value >= .15 & event_duration == 60 & threshold==.15)
  jitter_events_2 <- subset(jitter, average_value >= .2 & event_duration == 60 & threshold==.15)
  jitter_events_120 <- subset(jitter, average_value >= .15 & event_duration == 120 & threshold==.15)
  jitter_events_120_2 <- subset(jitter, average_value >= .2 & event_duration == 120 & threshold==.15)
  jitter_events_180 <- subset(jitter, average_value >= .15 & event_duration == 180 & threshold==.15)
  jitter_events_180_2 <- subset(jitter, average_value >= .2 & event_duration == 180 & threshold==.15)
  
  #extract all hours that had an average of less than <.15 - this is going to be non-jitter
  no_jitter_events <- subset(hm, average_value < .15)
  
  #for all unique dates within the hourly metrics data set
  unique_dates <- unique(as.Date(hm$timestamp))
  unique_dates <- unique_dates[order(unique_dates)]
  
  #response variable generation - 1 hour
  response_data <- lapply(1:length(unique_dates), function(i) {
    
    end_time <- unique_dates[i]
    hour(end_time) <- 23; minute(end_time) <- 59; second(end_time) <- 59
  
    temp <- subset(jitter_events, timestamp >= unique_dates[i] & timestamp < end_time)
    jitter_event_60m_15v_count <- nrow(temp)
    jitter_60m_15v <- ifelse(nrow(temp) > 0, 1, 0) #binary classification
    
    temp <- subset(jitter_events_2, timestamp >= unique_dates[i] & timestamp < end_time)
    jitter_event_60m_20v_count <- nrow(temp)
    jitter_60m_20v <- ifelse(nrow(temp) > 0, 1, 0) #binary classification
    
    temp <- subset(jitter_events_120, timestamp >= unique_dates[i] & timestamp < end_time)
    jitter_event_120m_15v_count <- nrow(temp)
    jitter_120m_15v <- ifelse(nrow(temp) > 0, 1, 0) #binary classification
    
    temp <- subset(jitter_events_120_2, timestamp >= unique_dates[i] & timestamp < end_time)
    jitter_event_120m_20v_count <- nrow(temp)
    jitter_120m_20v <- ifelse(nrow(temp) > 0, 1, 0) #binary classification
    
    temp <- subset(jitter_events_180, timestamp >= unique_dates[i] & timestamp < end_time)
    jitter_event_180m_15v_count <- nrow(temp)
    jitter_180m_15v <- ifelse(nrow(temp) > 0, 1, 0) #binary classification
    
    temp <- subset(jitter_events_180_2, timestamp >= unique_dates[i] & timestamp < end_time)
    jitter_event_180m_20v_count <- nrow(temp)
    jitter_180m_20v <- ifelse(nrow(temp) > 0, 1, 0) #binary classification
    
    temp <- subset(hm, average_value < .15 & timestamp >= unique_dates[i] & timestamp < end_time)
    no_jitter_60m_count <- nrow(temp)
    no_jitter_60m_15v <- ifelse(nrow(temp) > 0, 1, 0)
    
    response_data <- cbind(jitter_event_60m_15v_count, jitter_60m_15v,  no_jitter_60m_count, 
                           no_jitter_60m_15v, jitter_event_60m_20v_count, jitter_60m_20v,
          jitter_event_120m_15v_count, jitter_120m_15v, jitter_event_120m_20v_count, jitter_120m_20v,
          jitter_event_180m_15v_count, jitter_180m_15v, jitter_event_180m_20v_count, jitter_180m_20v , 
          'timestamp'=as.character(unique_dates[i]))
    
    return(response_data)
  })

  #use response data to generate the relevant features
  response_data <- as.data.frame(do.call(rbind, response_data))
  response_data$timestamp  <- as.Date(response_data$timestamp)
  

  #extract pjtn features to predict pjtn events
  jitter_features <- yesterdayFeatureGeneration(jitter, response_data, 'pjtn')
  print('pjtn_done')
  
  #now get sigma
  query <- paste(list("select * from test.sigma where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  sigma_data <- glookoGetQuery(query) 
  
  if (nrow(sigma_data) > 0) {
    sigma_data <- sigma_data[!is.na(sigma_data$value),] #0.01068 0.07749 0.08538 0.08875 0.09523 0.29290 , min 1q med mean 3q max
    print('sigma_event_start')
    sigma_events <- pvEvent(sigma_data, .12, 5)
    print('sigma_event_end')
    sigma_features <- yesterdayFeatureGeneration(sigma_events, response_data, 'sigma')
  } else { 
    sigma_features <- featureExtractionFunctions[['sigma']](data.frame())
  }
  print('sigma_done')
  
  #now get mod_thy_resv
  query <- paste(list("select * from test.mod_thy_resv where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  mod_thy_resv <- glookoGetQuery(query)
  
  if (nrow(mod_thy_resv) > 0) {
    mod_thy_resv <- mod_thy_resv[!is.na(mod_thy_resv$value),] #0.144   5.100   5.100   4.877   5.100   5.400 , min 1q med mean 3q max
    mod_thy_resv_features <- yesterdayFeatureGeneration(mod_thy_resv, response_data, 'thy_resv')
  } else {
    mod_thy_resv_features <- featureExtractionFunctions[['thy_resv']](data.frame())
  }
  print('mod_thy_resv_done')
    
  #now get swrd 
  query <- paste(list("select * from test.swrd where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  swrd <- glookoGetQuery(query) #unique value: 1 (11) or 0 (12) 
  
  if (nrow(swrd) > 0) {
    swrd <- swrd[!is.na(swrd$value),]
    swrd_features <- yesterdayFeatureGeneration(swrd, response_data, 'swrd')
  } else {
    swrd_features <- featureExtractionFunctions[['swrd']](data.frame())
  }
  print('swrd_done')
  
  #now get room 
  query <- paste(list("select * from test.room where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  room <- glookoGetQuery(query)
  if (nrow(room) > 0) {
    room <- room[!is.na(room$value),] #44.53   59.47   68.61   69.03   77.55  108.10, min 1q med mean 3q max
    room_features <- yesterdayFeatureGeneration(room, response_data, 'temp')
  } else {
    room_features <- featureExtractionFunctions[['temp']](data.frame())
  }
  print('room_done')
  
  #now get mod dqi
  query <- paste(list("select * from test.mod_dqi where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  mod_dqi <- glookoGetQuery(query)
  if (nrow(mod_dqi) > 0) {
    mod_dqi <- mod_dqi[!is.na(mod_dqi$value),] #44.53   59.47   68.61   69.03   77.55  108.10, min 1q med mean 3q max
    mod_dqi_features <- yesterdayFeatureGeneration(mod_dqi, response_data, 'dqi')
  } else {
    mod_dqi_features <- featureExtractionFunctions[['dqi']](data.frame())
  }

  #now get mod hvv
  query <- paste(list("select * from test.mod_hvv where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  mod_hvv <- glookoGetQuery(query)
  if (nrow(mod_hvv) > 0) {
    mod_hvv <- mod_hvv[!is.na(mod_hvv$value),] #44.53   59.47   68.61   69.03   77.55  108.10, min 1q med mean 3q max
    mod_hvv_features <- yesterdayFeatureGeneration(mod_hvv, response_data, 'hvv')
  } else {
    mod_hvv_features <- featureExtractionFunctions[['hvv']](data.frame())
  }
  
  #now get wndw
  query <- paste(list("select * from test.wndw where timestamp >= '", start, 
                      "' and timestamp <= '", end, "' and klystron = '", klystron, "'"), collapse="")
  wndw <- glookoGetQuery(query)
  if (nrow(wndw) > 0) {
    wndw <- wndw[!is.na(wndw$value),] #44.53   59.47   68.61   69.03   77.55  108.10, min 1q med mean 3q max
    wndw_features <- yesterdayFeatureGeneration(wndw, response_data, 'wndw')
  } else {
    wndw_features <- featureExtractionFunctions[['wndw']](data.frame())
  }
  
  #now need to merge the data sets together
  jitter_final <- cbind(jitter_features, sigma_features, mod_thy_resv_features, 
                    swrd_features, room_features, mod_dqi_features, mod_hvv_features,
                    wndw_features, response_data)
  
  write.csv(jitter_final, paste0("jitter_ml_data_v2/jitter_ml_data_", month, ".csv", sep=""))
  print('data_written')
}