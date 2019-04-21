library(lubridate)

setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li22_31')

je_count_list <- lapply(1:length(feature_variables), function(i) {
  month_start <- feature_variables[[i]][2]
  month <- feature_variables[[i]][5]
  events <- read.csv(paste0('jitter_ml_data_v2/jitter_ml_data_', month, '.csv', sep=""))
  
  jitter_events <- events[,grep('jitter_event', colnames(events), value = T)]
  je_count <- colSums(jitter_events)
  
  pjtn <- read.csv(paste0('raw_pjtn/raw_pjtn_', month, '.csv', sep=""))
  pjtn$timestamp <- as.Date(pjtn$timestamp)
  pjtn <- subset(pjtn, timestamp >= as.POSIXct(month_start))
  
  day_count <- length(unique(day(pjtn$timestamp)))
  je <- cbind(je_count, day_count, month)
})

je_count <- do.call(rbind, je_count_list)
write.csv(je_count, 'je_count.csv')
