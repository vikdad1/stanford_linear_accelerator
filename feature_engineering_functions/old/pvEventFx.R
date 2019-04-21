##find threshold data
pvEvent <- function(data, threshold, event_duration) {
  
  #data = the data to be used for feature generation
  #threshold = the value for which a reading could be considered an event
  #event_duration = the maximum length of an event
  
  threshold_data <- subset(data, value >= threshold) 
  non_threshold_data <- subset(data, value < threshold)
  
  #if (nrow(threshold_data) <= 1) {do something}
  
  #order data 
  threshold_data <- threshold_data[order(threshold_data$secondspastepoch, decreasing=F),]
  
  timestamps <- as.numeric(threshold_data[,'secondspastepoch'])
  timestamps_selected <- vector('numeric', length(timestamps)) ##Create empty events vector
  timestamps_selected <- timestamps[1] #Input first value into dataframe (by default within first event)
  events <- list() ##Create empty events list
  
  timestamp_index = 2
  event_start_index = 1

  while (timestamp_index <= length(timestamps)) { #For all timestamps except the first/most distant 1
    #Determine whether there are readings within 'event_duration' minutes prior
    test <- subset(timestamps_selected, (timestamps[timestamp_index]-(event_duration*60)) <= timestamps_selected &
                     timestamps_selected <= timestamps[timestamp_index])
    
    if (length(test)==0) { #If no, then the current reading is the start of a new event
      timestamps_selected[timestamp_index] <- timestamps[timestamp_index]
      events[[length(events)+1]] <- threshold_data[event_start_index:(timestamp_index-1),]
      event_start_index <- timestamp_index
    }
    print(timestamp_index)
    timestamp_index = timestamp_index + 1
  }
  
  #For each event, extract relevant features from data 
  if (length(events) > 0) {
    
    events <- lapply(1:length(events), function(i) {
      max_value <- max(events[[i]]$value) #max value
      min_value <- min(events[[i]]$value) #min value
      median_value <- median(events[[i]]$value) #median value
      average_value <- mean(events[[i]]$value) #average value 
      event_duration_seconds <- max(events[[i]]$secondspastepoch) - min(events[[i]]$secondspastepoch) #duration of event
      first_record <- events[[i]][1,] #extract first record
      #create data frame of data
      processed <- as.data.frame(cbind(first_record, max_value, min_value, median_value, average_value, event_duration_seconds))
      return(processed)
    })
  }
  
  #return data 
  events <- do.call(rbind, events)
  return(events)
}