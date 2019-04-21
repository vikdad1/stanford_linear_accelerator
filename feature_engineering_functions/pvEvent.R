#A method for identifying 'jitter events', where the value threshold of jitter
#and the maximum desired event duration of jitter can be defined.

pvEvent <- function(data, threshold, event_duration) {
  
  #data = the data to be used for feature generation
  #threshold = the value for which a reading could be considered an event
  #event_duration = the maximum length of an event
  
  #order data 
  data <- data[order(data$secondspastepoch, decreasing=F),]
  rownames(data) <- 1:nrow(data)
  
  #create data w/ an index
  new_data <- data
  new_data[,'index'] <- as.numeric(rownames(data))
  
  #develop threshold data set
  threshold_data <- subset(new_data, value >= threshold) 

  #if (nrow(threshold_data) <= 1) {do something}
  events <- list() ##Create empty events list
  
  #set the first index to be first value that is below threshold 
  index <- min(threshold_data$index)
  
  #while the index is <= than the max index of readings below threshold
  while (index <= max(threshold_data$index)) {

    #extract the time associated with the index
    the_timestamp <- threshold_data[threshold_data$index==index,'secondspastepoch']
    
    #Grab data within the event duration
    event_data <- subset(data, secondspastepoch >= the_timestamp & secondspastepoch <= 
                           (the_timestamp + (event_duration*60)))
    
    events[[length(events)+1]] <- event_data
    #find the data that is beyond the event duration
    temp <- subset(threshold_data, secondspastepoch > (the_timestamp + (event_duration*60)))
    
    if(nrow(temp) == 0) {
      break
    } else {
      index <- min(temp[,'index']) #return index
    }
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