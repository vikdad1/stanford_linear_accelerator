#pjtn feature extraction
pjtnFeatureExtraction <- function(data, pv_name='pjtn') { 
  
  #data = pjtn data

  if (nrow(data)==0) {
    max_value <- NA
    min_value <- NA
    median_value <- NA
    average_value <- NA
    event_count <- NA
    event_max_value <- NA 
    event_median_value <- NA
    event_average_value <- NA
    
    list_names <- c('max_value', 'min_value', 'median_value', 'average_value',
                    'event_count', 'event_max_value', 'event_median_value',
                    'event_average_value')
    final <- cbind(max_value, min_value, median_value, average_value,
                   event_count, event_max_value, event_median_value,
                   event_average_value)
    
    list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
    colnames(final) <- list_names 
    
    return(final)
    
  } else {
    max_value <- max(data$max_value) #max value
    min_value <- min(data$min_value) #min value
    median_value <- median(data$max_value) #median value
    average_value <- mean(data$average_value) #average value
  }

  #This could be done more scalably by actually calling pvEvent, but doing it for efficiency right now
  events <- subset(data, average_value >= .15 & event_duration==15)
  
  if (length(events) == 0) {
    event_count <- NA
    event_max_value <- NA 
    event_median_value <- NA
    event_average_value <- NA
  } else {
    event_count <- nrow(events)
    event_max_value <- max(events$max_value) #max value
    event_median_value <- median(events$max_value) #median value
    event_average_value <- mean(events$max_value) #average value
  }
  
  list_names <- c('max_value', 'min_value', 'median_value', 'average_value',
                  'event_count', 'event_max_value', 'event_median_value',
                  'event_average_value')
  final <- cbind(max_value, min_value, median_value, average_value,
                 event_count, event_max_value, event_median_value,
                 event_average_value)
  
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  colnames(final) <- list_names 
  
  return(final)
}

#thy resv feature extraction
thyresvFeatureExtraction <- function(data, pv_name='thy_resv') { 
  
  if (nrow(data)==0) {
    median_value <- NA
    average_value <- NA
    high_var <- NA
    
    list_names <- c('median_value', 'average_value', 'high_var')
    final <- cbind(median_value, average_value, high_var)
    
    list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
    colnames(final) <- list_names 
    return(final)
  } else {
    median_value <- round(median(data$value)) #median value
    average_value <- round(mean(data$value)) #average value
  }
  
  #This is about whether the variability is high or not .. but I think this logic should be refined
  stdev <- sd(data$value)
  high_var <- ifelse(stdev >= .01, T, F)
  
  list_names <- c('median_value', 'average_value', 'high_var')
  final <- cbind(median_value, average_value, high_var)
  
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  colnames(final) <- list_names 
  return(final)
}

#sigma feature extraction
#has pretty high variability
sigmaFeatureExtraction <- function(data, pv_name='sigma') {
  
  if (nrow(data)==0) {
    max_value <- NA
    min_value <- NA
    median_value <- NA
    average_value <- NA
    event_count <- NA
    event_max_value <- NA 
    event_median_value <- NA
    event_average_value <- NA
    
    list_names <- c('max_value', 'min_value', 'median_value', 'average_value',
                    'event_count', 'event_max_value', 'event_median_value',
                    'event_average_value')
    final <- cbind(max_value, min_value, median_value, average_value,
                   event_count, event_max_value, event_median_value,
                   event_average_value)
    
    list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
    colnames(final) <- list_names 
    return(final)
    
  } else {
    max_value <- max(data$max_value) #max value
    min_value <- min(data$min_value) #min value
    median_value <- median(data$max_value) #median value
    average_value <- mean(data$average_value) #average value
  }
  
  if (length(data) == 0) {
    event_count <- NA
    event_max_value <- NA 
    event_median_value <- NA
    event_average_value <- NA
  } else {
    event_count <- nrow(data)
    event_max_value <- max(data$max_value) #max value
    event_median_value <- median(data$max_value) #median value
    event_average_value <- mean(data$max_value) #average value
  }
  
  list_names <- c('max_value', 'min_value', 'median_value', 'average_value',
                  'event_count', 'event_max_value', 'event_median_value',
                  'event_average_value')
  final <- cbind(max_value, min_value, median_value, average_value,
                 event_count, event_max_value, event_median_value,
                 event_average_value)
  
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  colnames(final) <- list_names 
  return(final)
}

#swrd feature extraction
swrdFeatureExtraction <- function(data, pv_name = 'swrd') {
  
  if (nrow(data) == 0) {
    swrd_trip_count <- NA
    return(as.data.frame(swrd_trip_count))
  }
  
  #how many trips were there
  swrd_trip_count <- nrow(data[data$value==1,])
  return(as.data.frame(swrd_trip_count))
}

#temp feature extraction
tempFeatureExtraction <- function(data, pv_name = 'temp') {
  
  if (nrow(data)==0) {
   final <-  cbind(NA, NA, NA, NA)
   list_names <- c('max_value', 'min_value', 'median_value', 'average_value')
   list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
   colnames(final) <- list_names
   
   return(final)
  }
  
  breaks_vector <- seq(30, 120, by=5)
  temp_data <- c(max(data$value), min(data$value), median(data$value), mean(data$value))
  final <- rbind(as.character(cut(temp_data, breaks_vector, include.lowest=T, ordered_result = T)))
  list_names <- c('max_value', 'min_value', 'median_value', 'average_value')
  
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  colnames(final) <- list_names
  
  return(final)

}

#mod dqi
dqiFeatureExtraction <- function(data, pv_name='dqi') {
  
  if (nrow(data)==0) {
    max_value <- NA
    min_value <- NA
    median_value <- NA
    average_value <- NA
    
  } else {
    max_value <- max(data$value) #max value
    min_value <- min(data$value) #min value
    median_value <- median(data$value) #median value
    average_value <- mean(data$value) #average value
  }
  
  list_names <- c('max_value', 'min_value', 'median_value', 'average_value')
  final <- cbind(max_value, min_value, median_value, average_value)
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  
  colnames(final) <- list_names 
  return(final)
}

#mod hvv
hvvFeatureExtraction <- function(data, pv_name='hvv') {
  
  if (nrow(data)==0) {
    max_value <- NA
    min_value <- NA
    median_value <- NA
    average_value <- NA
    
  } else {
    max_value <- max(data$value) #max value
    min_value <- min(data$value) #min value
    median_value <- median(data$value) #median value
    average_value <- mean(data$value) #average value
  }
  
  list_names <- c('max_value', 'min_value', 'median_value', 'average_value')
  final <- cbind(max_value, min_value, median_value, average_value)
  
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  colnames(final) <- list_names 
  return(final)
}

#wndw
wndwFeatureExtraction <- function(data, pv_name='wndw') {
  
  if (nrow(data)==0) {
    max_value <- NA
    min_value <- NA
    median_value <- NA
    average_value <- NA
    
  } else {
    max_value <- max(data$value) #max value
    min_value <- min(data$value) #min value
    median_value <- median(data$value) #median value
    average_value <- mean(data$value) #average value
  }
  
  list_names <- c('max_value', 'min_value', 'median_value', 'average_value')
  final <- cbind(max_value, min_value, median_value, average_value)
  
  list_names <- sapply(list_names, function(i) paste0(pv_name, '_', i))
  colnames(final) <- list_names 
  return(final)
}
