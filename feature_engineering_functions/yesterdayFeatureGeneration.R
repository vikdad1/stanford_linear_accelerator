#This function is being used to generate features on data from 'yesterday', 
#where yesteday is the day prior to a jitter/non-jitter event.
yesterdayFeatureGeneration <- function(data, event_data, pv_name) {
  
  #data = data set to use for feature generation
  #event_data = the response variable/events for which features are to be built around
  #pv_name = the process variable for which features are being generated 
  
  final_list <- lapply(1:nrow(event_data), function(i) {
    
    #for each event
    event <- event_data[i,]

    #find the start time and end time of yesteday
    start_time <- event$timestamp - hours(24)
    hour(start_time) <- 0; minute(start_time) <- 0; second(start_time) <- 0
    end_time <- event$timestamp - hours(24)
    hour(end_time) <- 23; minute(end_time) <- 59; second(end_time) <- 59
    
    #get yesterday, calendar day, data
    yesterday <- subset(data, timestamp >= start_time & timestamp <= end_time)
    
    #if multiple time chunks, can be added in sclaable way
    list_names <- c('yesterday')
    time_chunks <- list(yesterday)    
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
