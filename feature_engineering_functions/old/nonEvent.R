hourlyMetrics <- function(data) {
  library(lubridate)
  
  hour_data <-  hour(data$timestamp)
  day_data <- day(data$timestamp)
  year_data <- year(data$timestamp)
  
  new_data <- cbind(data, hour_data, day_data, year_data)
  
  hour_day <- expand.grid('hour'=unique(hour_data), 'day'=unique(day_data), 'year'=unique(year_data))
  
  events <- lapply(1:nrow(hour_day), function(i) {
    temp <- subset(new_data, hour_data == hour_day$hour[i] & day_data == hour_day$day[i] & year_data == hour_day$year[i])
    return(temp)
  })
  
  events <- events[-which(sapply(events, is.null))]
  events <- events[sapply(events, function(x) dim(x)[1]) > 0]
  print(events)
  #For each event, extract relevant features from data 
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
  
  #return data 
  events <- do.call(rbind, events)
  return(events)
}