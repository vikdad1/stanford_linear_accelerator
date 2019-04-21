library(rpart)
library(rpart.plot)
library(caret)
library(randomForest)
library(gbm)
library(caTools)
library(dplyr)
library(ggplot2)

#SET WORKING DIRECTORY TO WHERE JITTER DATA SET FOLDER LIVES
setwd('~/jitter_data_set/')

mld_2231 <- read.csv('li22_31/ml_data_reduced_response.csv')
mld_2461 <- read.csv('li24_61/ml_data_reduced_response.csv')
mld_2831 <- read.csv('li28_31/ml_data_reduced_response.csv')

###Do some variable processing
mld_2231 <- cbind(mld_2231, 'klystron'='li22_31')
mld_2461 <- cbind(mld_2461, 'klystron'='li24_61')
mld_2831 <- cbind(mld_2831, 'klystron'='li28_31')

mld <- rbind(mld_2231, mld_2461, mld_2831)

###do some variable processing
###temperature needs to be treated correctly
breaks_vector <- seq(30, 120, by=5)
temp_levels <- levels(cut(1, breaks_vector, include.lowest=T))

#convert temp data into ordered factor 
mld[,grep('temp', colnames(mld), value=T)] <- lapply(mld[,grep('temp', colnames(mld), value=T)], 
                                                     function(x) ordered(x, levels=temp_levels))

#convert high variability to a factor
mld[,grep('thy_resv_high_var', colnames(mld), value=T)] <- unlist(lapply(mld[,grep('thy_resv_high_var', 
                                                                                   colnames(mld), value=T)], factor))
mld_backup <- mld

#let's convert the count variable
breaks_vector <- seq(0, 24, by=12)
jitter_levels <- levels(cut(0, breaks_vector, include.lowest=F))
jitter <- as.character(cut(mld_backup$jitter_event_60m_15v_count, breaks_vector, include.lowest = F))
jitter[is.na(jitter)] <- '0'
jitter <- ordered(jitter, levels=c(0, jitter_levels))
###now set the seed 

mld <- mld_backup
mld <- mld[,-grep("X", colnames(mld))]
mld <- mld[,-grep("timestamp", colnames(mld))]
mld <- mld[,-grep("klystron", colnames(mld))]
#mld <- mld[,-grep("pjtn", colnames(mld))]
#mld <- mld[,-grep('temp', colnames(mld))]
mld <- mld[,-grep('jitter', colnames(mld),value=F)]
mld <- cbind(mld, jitter)

#break into train/test set 
set.seed(144)
train.ids = sample(nrow(mld), 0.7*nrow(mld))
train.ctr = mld[train.ids,]
test.ctr = mld[-train.ids,]

# Let's try random forest first
set.seed(144)
mod.rf <- randomForest(jitter ~ ., data = train.ctr, mtry = 5, nodesize = 5, ntree = 500)

# what are the defaults for the 3 params?
pred.rf <- predict(mod.rf, newdata = test.ctr) # just to illustrate
tableAccuracy(test.ctr$jitter, pred.rf) #(.97 accuracy)
table('actual'=test.ctr$jitter, 'prediction'=pred.rf) #woah

# syntax is very similar to CART training
set.seed(99)
train.rf <- train(jitter ~ .,
                  data = train.ctr,
                  method = "rf",
                  tuneGrid = data.frame(mtry=1:16),
                  trControl = trainControl(method="cv", number=5, verboseIter = TRUE),
                  metric = "Accuracy")

# RMSE or Rsquared doesn't matter actually -- both will be generated for regression problems
train.rf$results
train.rf
train.ctr.mm = as.data.frame(model.matrix(jitter~.+0, data=train.ctr))
pred.best.rf <- predict(best.rf, newdata = train.ctr.mm, type='class') # can use same model matrix
tableAccuracy(train.ctr$jitter, pred.best.rf) #woah
table('actual'=train.ctr$jitter, 'prediction'=pred.best.rf) #woah

test.ctr.mm = as.data.frame(model.matrix(jitter~.+0, data=test.ctr))
pred.best.rf <- predict(best.rf, newdata = test.ctr.mm, type='class') # can use same model matrix
tableAccuracy(test.ctr$jitter, pred.best.rf) #woah 
table('actual'=test.ctr$jitter, 'prediction'=pred.best.rf) #woah
67/(67+54)
25/(25+386)

# bagging can be done just by setting mtry = p = 17
# need to use model matrix because of how factor x variables are handled
test.ctr.mm = as.data.frame(model.matrix(jitter ~ . + 0, data = test.ctr))
set.seed(3432)
pred.best.rf <- predict(pred.best.rf, newdata = test.ctr.mm, type='class') # can use same model matrix


mod.bag <- randomForest(x = train.ctr.mm, y = train.ctr$CTR, data = train.ctr, mtry = 17, nodesize = 5, ntree = 500)
pred.bag <- predict(mod.bag, newdata = test.ctr.mm)