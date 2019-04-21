#Baseline model testing

mld2 <- mld
mld2$timestamp <- as.Date(as.character(mld$timestamp))
mld2$klystron[mld2$k]
test <- sapply(unique(mld2$klystron), function(i) {
  klystron <- subset(mld2, klystron==i)
  klystron <- klystron[order(klystron$timestamp),]

  result <- sapply(1:nrow(klystron), function(j) {
      yesterday_timestamp <- klystron$timestamp[j] - days(1)
      yesterday <- subset(klystron, timestamp==yesterday_timestamp)
      if (nrow(yesterday) > 0) {
        ifelse(yesterday$jitter==1, 1, 0)
      } else {
        NA
      }
  })
  
  return(result)
})

li2231 <- subset(mld, klystron=='li22_31')
li2461 <- subset(mld, klystron=='li24_61')
li2831 <- subset(mld, klystron=='li28_31')

li2231 <- cbind(li2231, predicted=test[[1]])
li2461 <- cbind(li2461, predicted=do.call(rbind, test[[2]])[,1])
li2831 <- cbind(li2831, predicted=do.call(rbind, test[[3]])[,1])

final <- rbind(li2231, li2461, li2831)
final <- final[complete.cases(final),]
table('actual'=final$jitter, 'predicted'=final$predicted)
