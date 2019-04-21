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

#mld <- rbind(mld_2231, mld_2461)
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


###now set the seed 

mld <- mld_backup
mld <- mld[,-grep("X", colnames(mld))]
mld <- mld[,-grep("timestamp", colnames(mld))]
mld <- mld[,-grep("klystron", colnames(mld))]
#mld <- mld[,-grep("pjtn", colnames(mld))]
mld <- mld[,-grep('temp', colnames(mld))]
jitter_60m_15v <- mld[,'jitter_60m_15v']
mld <- mld[,-grep('jitter', colnames(mld),value=F)]
mld <- cbind(mld, jitter_60m_15v)
mld$jitter_60m_15v <- as.factor(mld$jitter_60m_15v)
colnames(mld)[ncol(mld)] <- 'jitter'

set.seed(144)
train.ids = sample(nrow(mld), 0.7*nrow(mld))
train.ctr = mld[train.ids,]
test.ctr = mld[-train.ids,]

# Let's try basic CART first
set.seed(33)
train.cart = train(jitter ~ .,
                   data = train.ctr,
                   method = "rpart",
                   tuneGrid = data.frame(cp=seq(0, 0.1, 0.005)),
                   trControl = trainControl(method="cv", number=5),
                   metric = "Accuracy")
train.cart$results
train.cart

mod <- rpart(jitter ~ .,
             data = train.ctr, method="class", 
             minbucket=50, cp = 0.025)
mod
prp(mod) # plots the tree. Might be slow/crash -- big tree! 
predTrain_mod <- predict(mod, type='class')
tableAccuracy(train.ctr$jitter, predTrain_mod) #(.8426 accuracy)
table('actual'=train.ctr$jitter, 'prediction'=predTrain_mod)
134/(134+160) #.45 fpr
35/(35+910) #.03 fpr

# now train with cross validation!
ctrl <- trainControl(method='repeatedcv', number=50, savePredictions = T)

test <- train(jitter ~ .,
                 data=train.ctr,
                 method="rpart",
                 family='binomial',
                 trControl=ctrl,
                 tuneGrid=.025, 
                 metric="Accuracy")

#mod 
# Let's incorporate a loss matrix
loss.mat <- cbind(c(0, 2), c(1, 0)) # cbind is column bind, rbind is row bind

# adding loss function to a list of "parms"
mod2 = rpart(jitter ~ .,
             data = train.ctr, method="class", 
             parms=list(loss = loss.mat),
             minbucket = 10, cp = 0.025)
prp(mod2, main='CART with PJTN & Loss Matrix')

pred2 <- predict(mod2, newdata = train.ctr, type = "class")
tableAccuracy(train.ctr$jitter, pred2)
180/(180+114) #.61
138/(807+138) #.14

#let's run it back, without pjtn events
pred_test = predict(mod, newdata = test.ctr, type='class')
tableAccuracy(test.ctr$jitter, pred_test) #(.8426 accuracy)
table('actual'=test.ctr$jitter, 'prediction'=pred_test)
57/(57+64) #.471
18/(18+393) #.037

pred_test_loss = predict(mod2, newdata = test.ctr, type='class')
tableAccuracy(test.ctr$jitter, pred_test_loss) #(.827 accuracy)
table('actual'=test.ctr$jitter, 'prediction'=pred_test_loss)
75/(75+46) # 0.6198347
46/(46+365) #.037
