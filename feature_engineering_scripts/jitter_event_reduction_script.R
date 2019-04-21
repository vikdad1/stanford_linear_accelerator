#Further processing the jitter events to be meaningful
library(lubridate)

setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/')
mld_2231 <- read.csv('li22_31/ml_data_set_complete_cases.csv')
mld_2461 <- read.csv('li24_61/ml_data_set_complete_cases.csv')
mld_2831 <- read.csv('li28_31/ml_data_set_complete_cases.csv')

### Let's say you can only have 5 days in a row where it happened - and then, break
mld_2831$timestamp <- as.Date(as.character(mld_2831$timestamp), '%m/%d/%y')
mld_2831 <- mld_2831[order(mld_2831$timestamp),]
mld_2231$timestamp <- as.Date(as.character(mld_2231$timestamp))
mld_2231 <- mld_2231[order(mld_2231$timestamp),]
mld_2461$timestamp <- as.Date(as.character(mld_2461$timestamp))
mld_2461 <- mld_2461[order(mld_2461$timestamp),]


response <- 'jitter_180m_15v' 
data <- mld_2461

keep_nje <- data.frame()
keep_je <- data.frame()
last_je_timestamp <- data[data[,response]==1,'timestamp'][1]
last_kept_je_timestamp <- data[data[,response]==1,'timestamp'][1]
  
for (i in 1:nrow(data)) {
  if (data[i,response] == 0) {
    keep_nje <- rbind(keep_nje, data[i,])
  }
  
  #if it is a 1, then check if it is within 5 days of the last event logged
  #and check if it within the last event that was found. if so, then keep
  
  #if it is a positive response
  if (data[i,response]==1) {
    #check if the event is consecutive from the last event
    if (last_je_timestamp >= (data[i,'timestamp'] - days(1))) {
      
      #if it is consecutive, then check if it is within the last 5 days of the last returned event
      if (length(last_kept_je_timestamp[last_kept_je_timestamp >= (data[i,'timestamp'] - days(4))]) ==1 ) {
        #if it is, the return the event as a record
        keep_je <- rbind(keep_je, data[i,])
      }
    } else {
      keep_je <- rbind(keep_je, data[i,])
      last_kept_je_timestamp <- data[i,'timestamp']     
    }
    last_je_timestamp <- data[i,'timestamp']
  }
}

new <- rbind(keep_je, keep_nje)
write.csv(new, 'li24_61/ml_data_reduced_response_3h.csv')
