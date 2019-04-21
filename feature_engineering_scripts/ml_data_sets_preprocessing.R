#Training on li_22_31 - independently 

#set working directory
setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li22_31/jitter_ml_data_v2/')

#Combined ML data set

ml_data_list <- lapply(1:length(feature_variables), function(i) {
  month <- feature_variables[[i]][5]
  print(month)
  ml_data <- read.csv(paste0("jitter_ml_data_", month, ".csv"))
  
  #pjtn processing
  for (i in grep('pjtn_min_value', colnames(ml_data[,grep('pjtn', colnames(ml_data), value=T)]),value = T, invert = T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA 
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T))
  }
  
  for (i in grep('pjtn_min_value', colnames(ml_data[,grep('pjtn', colnames(ml_data), value=T)]),value = T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA 
    ml_data[is.na(ml_data[,i]),i] <- 0
  }
  
  #sigma processing
  for (i in grep('sigma', colnames(ml_data), value=T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA 
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T))
  }
  
  #thy_resv
  for (i in grep('thy_resv_high_var', colnames(ml_data[,grep('thy_resv', colnames(ml_data), value=T)]), value = T,invert=T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA  
    ml_data[is.nan(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T))
  }
  
  #thy_resv
  for (i in grep('thy_resv_high_var', colnames(ml_data), value = T)) {
    ml_data[is.na(ml_data[,i]),i] <- 0
    ml_data[ml_data[,i]!=1,i] <- 0
    ml_data[is.infinite(ml_data[,i]),i] <- 0 
    ml_data[is.nan(ml_data[,i]),i] <- 0 #if there is no data, then there was no tri
  }
  
  if ('thy_resv_high_var' %in% colnames(ml_data)) {
    colnames(ml_data)[grep('thy_resv_high_var', colnames(ml_data))] <- "thy_resv_yesterday_thy_resv_high_var"
    colnames(ml_data)[grep('thy_resv_median_value', colnames(ml_data))] <- "thy_resv_yesterday_thy_resv_median_value"
    colnames(ml_data)[grep('thy_resv_average_value', colnames(ml_data))] <- "thy_resv_yesterday_thy_resv_average_value"
  }
  
  #swrd
  for (i in grep('swrd', colnames(ml_data), value=T)) {
    ml_data[is.na(ml_data[,i]),i] <- 0 #if there is no data, then there was no trip
    ml_data[is.infinite(ml_data[,i]),i] <- 0
  }

  if ('swrd_trip_count' %in% colnames(ml_data)) {
    colnames(ml_data)[grep('swrd_trip_count', colnames(ml_data))] <- "swrd_yesterday_swrd_trip_count"
  }
  
  #temp
  for (i in grep('temp', colnames(ml_data), value=T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T)) #if there is no data, then there was no trip
  }
  
  #dqi
  for (i in grep('dqi', colnames(ml_data), value=T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    ml_data[is.nan(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T)) #if there is no data, then there was no trip
  }
  
  if ('dqi_max_value' %in% colnames(ml_data)) {
    colnames(ml_data)[grep('dqi_max_value', colnames(ml_data))] <- "dqi_yesterday_dqi_max_value"
    colnames(ml_data)[grep('dqi_min_value', colnames(ml_data))] <- "dqi_yesterday_dqi_min_value"
    colnames(ml_data)[grep('dqi_median_value', colnames(ml_data))] <- "dqi_yesterday_dqi_median_value"
    colnames(ml_data)[grep('dqi_average_value', colnames(ml_data))] <- "dqi_yesterday_dqi_average_value"
  }
  
  for (i in grep('hvv', colnames(ml_data), value=T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    ml_data[is.nan(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T)) #if there is no data, then there was no trip
  }
  
  if ('hvv_max_value' %in% colnames(ml_data)) {
    colnames(ml_data)[grep('hvv_max_value', colnames(ml_data))] <- "hvv_yesterday_hvv_max_value"
    colnames(ml_data)[grep('hvv_min_value', colnames(ml_data))] <- "hvv_yesterday_hvv_min_value"
    colnames(ml_data)[grep('hvv_median_value', colnames(ml_data))] <- "hvv_yesterday_hvv_median_value"
    colnames(ml_data)[grep('hvv_average_value', colnames(ml_data))] <- "hvv_yesterday_hvv_average_value"
  }
  
  for (i in grep('wndw', colnames(ml_data), value=T)) {
    ml_data[is.infinite(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    ml_data[is.nan(ml_data[,i]),i] <- NA #if there is no data, then there was no tri
    ml_data[is.na(ml_data[,i]),i] <- as.vector(mean(ml_data[,i], na.rm=T)) #if there is no data, then there was no trip
  }
  
  return(ml_data)
})

ml_data <- do.call(rbind, ml_data_list)
ml_data_li24_61 <- ml_data
ml_data_li22_31 <- ml_data
ml_data_li28_31 <- ml_data #- this is garbage right now 

#Exploratory analysis of each dataset
table(complete.cases(ml_data_li22_31)) #485 true, 286 false
table(complete.cases(ml_data_li24_61)) #794 false
table(complete.cases(ml_data_li28_31)) #775 false

####LI 22 31 deep dive
#li 22 31 deep dive - there is no columsn that are completely missing
for (i in 1:ncol(ml_data_li22_31)) {
  if (is.numeric(ml_data_li22_31[,i])) {
    ml_data_li22_31[is.na(ml_data_li22_31[,i]), i] <- mean(ml_data_li22_31[,i], na.rm = T)
  }
}

#for the factors, replace with most popular variable
for (i in 1:ncol(ml_data_li22_31)) {
  if (is.factor(ml_data_li22_31[,i])) {
    ml_data_li22_31[is.na(ml_data_li22_31[,i]),i] <- names(which.max(table(ml_data_li22_31[,i])))
  }
}

###check that cases are complete ... yes, all complete cases
table(complete.cases(ml_data_li22_31))

###LI 24 61 deep dive
unique(ml_data_li24_61$temp_median_value) #all temperature data is missing. should we impute with the temps from other klystron?

temp <- merge(ml_data_li22_31[,c(grep('temp', colnames(ml_data_li22_31), value = T), 'timestamp')], 
      ml_data_li24_61[,c('X','timestamp')], by='timestamp', all.y=T)

colnames(ml_data_li24_61)[grep('temp', colnames(ml_data_li24_61))] <- c('temp_yesterday_temp_max_value', 'temp_yesterday_temp_min_value', 
                                                                'temp_yesterday_temp_median_value', 'temp_yesterday_temp_average_value')

ml_data_li24_61$temp_yesterday_temp_max_value <- temp$temp_yesterday_temp_max_value
ml_data_li24_61$temp_yesterday_temp_median_value <- temp$temp_yesterday_temp_median_value
ml_data_li24_61$temp_yesterday_temp_average_value <- temp$temp_yesterday_temp_average_value
ml_data_li24_61$temp_yesterday_temp_min_value <- temp$temp_yesterday_temp_min_value

table(complete.cases(ml_data_li24_61)) #693 true, 101 false

#for all values that are NA, imput with the average
for (i in 1:ncol(ml_data_li24_61)) {
  if (is.numeric(ml_data_li24_61[,i])) {
    ml_data[is.na(ml_data_li24_61[,i]), i] <- mean(ml_data_li24_61[,i], na.rm = T)
    ml_data[is.nan(ml_data_li24_61[,i]), i] <- mean(ml_data_li24_61[,i], na.rm = T)
  }
}

#for the factors, replace with most popular variable
for (i in 1:ncol(ml_data_li24_61)) {
  if (is.factor(ml_data_li24_61[,i])) {
    ml_data_li24_61[is.na(ml_data_li24_61[,i]),i] <- names(which.max(table(ml_data_li24_61[,i])))
  }
}

table(complete.cases(ml_data_li24_61)) #706 true, 88 false

#what else is missing
ml_data_li24_61[!complete.cases(ml_data_li24_61),]

#dqi
for (i in grep('dqi', colnames(ml_data_li24_61), value=T)) {
  ml_data_li24_61[is.infinite(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.nan(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.na(ml_data_li24_61[,i]),i] <- as.vector(mean(ml_data_li24_61[,i], na.rm=T)) #if there is no data, then there was no trip
}

#hvv
for (i in grep('hvv', colnames(ml_data_li24_61), value=T)) {
  ml_data_li24_61[is.infinite(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.nan(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.na(ml_data_li24_61[,i]),i] <- as.vector(mean(ml_data_li24_61[,i], na.rm=T)) #if there is no data, then there was no trip
}

#thy_resv
for (i in grep('thy_resv', colnames(ml_data_li24_61), value=T)) {
  ml_data_li24_61[is.infinite(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.nan(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.na(ml_data_li24_61[,i]),i] <- as.vector(mean(ml_data_li24_61[,i], na.rm=T)) #if there is no data, then there was no trip
}

#thy_resv
for (i in grep('pjtn', colnames(ml_data_li24_61), value=T)) {
  ml_data_li24_61[is.infinite(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.nan(ml_data_li24_61[,i]),i] <- NA #if there is no data, then there was no tri
  ml_data_li24_61[is.na(ml_data_li24_61[,i]),i] <- as.vector(mean(ml_data_li24_61[,i], na.rm=T)) #if there is no data, then there was no trip
}

ml_data_li24_61[!complete.cases(ml_data_li24_61),]

ml_data_li22_31 <- ml_data_li22_31[,-1]
ml_data_li24_61 <- ml_data_li24_61[,-1]

#Had to do a lot of pre-processing to get us there for li24_61

setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li22_31/')
write.csv(ml_data_li22_31, 'ml_data_set_complete_cases.csv', row.names = F)
setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li24_61/')
write.csv(ml_data_li24_61, 'ml_data_set_complete_cases.csv', row.names = F)


###do some variable processing
###temperature needs to be treated correctly
temp_levels <- levels(cut(1, breaks_vector))
ml_data[,grep('temp', colnames(ml_data), value=T)] <- lapply(ml_data[,grep('temp', colnames(ml_data), value=T)], 
                                                             function(x) ordered(x, levels=temp_levels))
ml_data[,grep('thy_resv_high_var', colnames(ml_data), value=T)] <- lapply(ml_data[,grep('thy_resv_high_var', colnames(ml_data), value=T)], factor)
ml_data <- ml_data[,-grep("X", colnames(ml_data))]
ml_data <- ml_data[,-grep("timestamp", colnames(ml_data))]


####LI 2831 deep dive
#li 22 31 deep dive - there is no columsn that are completely missing
for (i in 1:ncol(ml_data_li28_31)) {
  if (is.numeric(ml_data_li28_31[,i])) {
    ml_data_li28_31[is.na(ml_data_li28_31[,i]), i] <- mean(ml_data_li28_31[,i], na.rm = T)
  }
}

#for the factors, replace with most popular variable
for (i in 1:ncol(ml_data_li28_31)) {
  if (is.factor(ml_data_li28_31[,i])) {
    ml_data_li28_31[is.na(ml_data_li28_31[,i]),i] <- names(which.max(table(ml_data_li28_31[,i])))
  }
}

###check that cases are complete ... yes, all complete cases
table(complete.cases(ml_data_li28_31))

###LI 24 61 deep dive
unique(ml_data_li28_31$temp_median_value) #all temperature data is missing. should we impute with the temps from other klystron?

temp <- merge(ml_data_li22_31[,c(grep('temp', colnames(ml_data_li22_31), value = T), 'timestamp')], 
              ml_data_li28_31[,c('X','timestamp')], by='timestamp', all.y=T)

colnames(ml_data_li28_31)[grep('temp', colnames(ml_data_li28_31))] <- c('temp_yesterday_temp_max_value', 'temp_yesterday_temp_min_value', 
                                                                        'temp_yesterday_temp_median_value', 'temp_yesterday_temp_average_value')

ml_data_li28_31$temp_yesterday_temp_max_value <- temp$temp_yesterday_temp_max_value
ml_data_li28_31$temp_yesterday_temp_median_value <- temp$temp_yesterday_temp_median_value
ml_data_li28_31$temp_yesterday_temp_average_value <- temp$temp_yesterday_temp_average_value
ml_data_li28_31$temp_yesterday_temp_min_value <- temp$temp_yesterday_temp_min_value

table(complete.cases(ml_data_li28_31)) #693 true, 101 false

#for all values that are NA, imput with the average
for (i in 1:ncol(ml_data_li28_31)) {
  if (is.numeric(ml_data_li28_31[,i])) {
    ml_data[is.na(ml_data_li28_31[,i]), i] <- mean(ml_data_li28_31[,i], na.rm = T)
    ml_data[is.nan(ml_data_li28_31[,i]), i] <- mean(ml_data_li28_31[,i], na.rm = T)
  }
}

#for the factors, replace with most popular variable
for (i in 1:ncol(ml_data_li28_31)) {
  if (is.factor(ml_data_li28_31[,i])) {
    ml_data_li28_31[is.na(ml_data_li28_31[,i]),i] <- names(which.max(table(ml_data_li28_31[,i])))
  }
}

table(complete.cases(ml_data_li28_31)) #775 true

ml_data_li22_31 <- ml_data_li22_31[,-1]
ml_data_li24_61 <- ml_data_li24_61[,-1]
ml_data_li28_31 <- ml_data_li28_31[,-1]

#Had to do a lot of pre-processing to get us there for li24_61

setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li22_31/')
write.csv(ml_data_li22_31, 'ml_data_set_complete_cases.csv', row.names = F)
setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li24_61/')
write.csv(ml_data_li24_61, 'ml_data_set_complete_cases.csv', row.names = F)
setwd('~/Documents/GitHub/slac-capstone/vikram/jitter_data_set/li28_31/')
write.csv(ml_data_li28_31, 'ml_data_set_complete_cases.csv', row.names = F)

###do some variable processing
###temperature needs to be treated correctly
temp_levels <- levels(cut(1, breaks_vector))
ml_data[,grep('temp', colnames(ml_data), value=T)] <- lapply(ml_data[,grep('temp', colnames(ml_data), value=T)], 
                                                             function(x) ordered(x, levels=temp_levels))
ml_data[,grep('thy_resv_high_var', colnames(ml_data), value=T)] <- lapply(ml_data[,grep('thy_resv_high_var', colnames(ml_data), value=T)], factor)
ml_data <- ml_data[,-grep("X", colnames(ml_data))]
ml_data <- ml_data[,-grep("timestamp", colnames(ml_data))]