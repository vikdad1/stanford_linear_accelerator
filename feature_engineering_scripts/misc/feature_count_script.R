#Jitter Event Exploration...
#concept here is it might not be possible to find enough 'non-jitter' events.

setwd("~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li22_31/")

threshold_list <- list()
hm_list <- list()

for (i in 1:length(feature_variables)) {
  
  month <- feature_variables[[i]][5]
  jitter <- read.csv(paste0("jitter_events_", month, ".csv", sep=""))
  hm <- read.csv(paste0("hourly_metrics_", month, ".csv", sep=""))
  
  grid <- expand.grid(unique(jitter$threshold), unique(jitter$event_duration))
  names(grid) <- c('threshold', 'event_duration')
  
  test <- lapply(1:nrow(grid), function(i) {
    data <- subset(jitter, threshold == grid[i,'threshold'] & 
                     event_duration == grid[i, 'event_duration'])
    
    total_events <- nrow(data)
    average_above_threshold <- nrow(data[data$average_value >=  grid[i,'threshold'],])
    return(cbind(threshold=grid[i,'threshold'], event_duration=grid[i, 'event_duration'], 
                 total_events, average_above_threshold))
  })
  
  test <- do.call(rbind, test) #makes me that 60 min - 120 min, .15, is the way 
  hm_count <- nrow(subset(hm, average_value < .15)) #results in 172 events... I like the results from this, good balance
  
  threshold_list[[i]] <- test
  hm_list[[i]] <- hm_count
}

jitter <- read.csv("jitter_events_nov_2016.csv")
hm <- read.csv("hourly_metrics_nov_2016.csv")

#jitter events
#jitter <- jitter_events_th15 
grid <- expand.grid(unique(jitter$threshold), unique(jitter$event_duration))
names(grid) <- c('threshold', 'event_duration')

test <- lapply(1:nrow(grid), function(i) {
  data <- subset(jitter, threshold == grid[i,'threshold'] & 
           event_duration == grid[i, 'event_duration'])
  
  total_events <- nrow(data)
  average_above_threshold <- nrow(data[data$median_value >=  grid[i,'threshold'],])
  return(cbind(threshold=grid[i,'threshold'], event_duration=grid[i, 'event_duration'], 
               total_events, average_above_threshold))
})

do.call(rbind, test) #makes me that 60 min - 120 min, .15, is the way 

nrow(subset(hm, average_value < .15)) #results in 172 events... I like the results from this, good balance

#Based on this data, I want to do 60 min events where average

#may - 172 hours without events, ~200 60-120 events
#june - 660 hours without events, < 5 events 1 hour - 2 hours long ... and only 8 for 15
#july - 741 hours without events, <=5 total even with 5 events... so basically none. major class imbalance. 
#aug - 736 hours without evens, <= 5 total events .. yikes
#sept - 710 events, <=2 total 

#oct 2016 - 1050 events, 644 hours
#nov 
