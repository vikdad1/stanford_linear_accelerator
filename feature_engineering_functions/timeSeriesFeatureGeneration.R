#function to do precursor predictions
timeSeriesFeatureGeneration <- function(data, event_data, pv_name) {
  
  #data = data set to use for feature generation
  #event_data = the response variable/events for which features are to be built around
  #pv_name = the process variable for which features are being generated 
  
  final_list <- lapply(1:nrow(event_data), function(i) {
    event <- event_data[i,]
    start_time <- event$timestamp - hours(24)
    hour(start_time) <- 0; minute(start_time) <- 0; second(start_time) <- 0
    end_time <- event$timestamp - hours(24)
    hour(end_time) <- 23; minute(end_time) <- 59; second(end_time) <- 59
    
    #get yesterday, calendar day, data
    yesterday <- subset(data, timestamp >= start_time & timestamp <= end_time)
    
    #get data by hours since event
    twenty_four_hours <- subset(data, timestamp >= (event$timestamp) - hours(24) & timestamp < event$timestamp)
    twelve_hours <- subset(data, timestamp >= (event$timestamp) - hours(12) & timestamp < event$timestamp)
    six_hours <- subset(data, timestamp >= (event$timestamp) - hours(6) & timestamp < event$timestamp)
    two_hours <- subset(data, timestamp >= (event$timestamp) - hours(2) & timestamp < event$timestamp)
    one_hour <- subset(data, timestamp >= (event$timestamp) - hours(1) & timestamp < event$timestamp)
    
    list_names <- c('yesterday', '24H', '12H', '6H', '2H', '1H')
    #create a list
    time_chunks <- list(yesterday, twenty_four_hours, twelve_hours, six_hours, two_hours, one_hour)

    time_features <- lapply(time_chunks, function(i) featureExtractionFunctions[[pv_name]](i))

    for (i in 1:length(time_features)) {
      old_names <- colnames(time_features[[i]])
      new_names <- sapply(old_names, function(x) paste0(pv_name, '_', list_names[i],'_', x))
      colnames(time_features[[i]]) <- new_names                    
    }
    
    return(do.call(cbind, time_features))
    
    return(do.call(cbind, time_features))
  })
  return(do.call(rbind,final_list))
}

