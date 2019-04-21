#Machine Learning Script
library(ROCR)
library(MASS)
library(caret) # cross vxxalidation
library(rpart)
library(car)
library(stats)
library(glookoGEO)


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
mld[,grep('thy_resv_high_var', colnames(mld), value=T)] <- unlist(lapply(mld[,grep('thy_resv_high_var', colnames(mld), value=T)], factor))
mld_backup <- mld

#Approach 1
###In this approach, following considerations:
#1. removing component of seasonality by excluding timestamp as a variable in test/train split
#2. mixing data from klystrons, including klystron as a variable that could be used to predict - potentially worth removing
#3. Going to simply predict if day will have a jitter event, where a jitter event is any day with 1, 60 minute incident of jitter 
#### question - when I'm gathering the jitter events, am I actually getting many events that are less than an hour? Perhaps I should be including event_duration in my query

##step model is showing that klystron value could play a role, especially in the step model. Would like this
#to be generalizable, so I'm going to remove the klytron

# mld <- mld_backup
# mld <- mld[,-grep("timestamp", colnames(mld))]
# mld <- mld[,-grep("klystron", colnames(mld))]
# jitter_60m_15v <- mld[,'jitter_60m_15v']
# mld <- mld[,-grep('jitter', colnames(mld),value=F)]
# mld <- cbind(mld, jitter_60m_15v)
# mld$jitter_60m_15v <- as.factor(mld$jitter_60m_15v)
# colnames(mld)[ncol(mld)] <- 'jitter'
# 
# set.seed(144)
# 
# #split the data w/o seasonality in train, validation, test sets, 6/2/2 split
# train_ids <- sample(nrow(mld), 0.70*nrow(mld))
# train_set <- mld[train_ids,]
# test_set <- mld[-train_ids,]
# 
# ##Logistic Regression
# mod <- glm(jitter ~ ., data=train_set, family="binomial")
# #glm.fit: fitted probabilities numerically 0 or 1 occurred
# #glm.fit: algorithm didn't converge. 
# #information on complete separation: https://bscheng.com/2016/12/11/modeling-completely-separated-data-in-r/
# 
# #understand the model
# summary(mod)
##showing that yesterday's PJTN event count is significant, as well as the average PJTN value of yesterday.
#sigma median value seems to have minor importance 
#temp median value, where the encoding is 6, showing significance - who knows though...
#wndw min value, average value showing exterme significance 

#singularity issue, something wrong with my dummy variables: https://stats.stackexchange.com/questions/25839/logistic-regression-in-r-returning-na-values
#more: https://stackoverflow.com/questions/7337761/linear-regression-na-estimate-just-for-last-coefficient
#https://en.wikipedia.org/wiki/Linear_independence
#https://en.wikipedia.org/wiki/Dummy_variable_%28statistics%29

#vif(mod) #there are aliased coefficients
#alias(mod) #shows sigma_yesterday_sigma_event_max_value, sigma_yesterday_sigma_event_median value, temp_values_^8/9/10 

#ok, based on the method for how sigma events are currently being processed, the maximum value of the sigma event will
#necessarily be the same max value of yesterday... same thing goes with median

##Information on AIC: https://en.wikipedia.org/wiki/Akaike_information_criterion
###Try removing temperature 
mld <- mld_backup
mld <- mld[,-grep('X', colnames(mld))]
mld <- mld[,-grep('temp', colnames(mld))]
#mld <- mld[,-grep("timestamp", colnames(mld))]
#mld <- mld[,-grep("klystron", colnames(mld))]
mld <- mld[,-grep("pjtn", colnames(mld))]
jitter_60m_15v <- mld[,'jitter_60m_15v']
mld <- mld[,-grep('jitter', colnames(mld),value=F)]
mld <- cbind(mld, jitter_60m_15v)
mld$jitter_60m_15v <- as.factor(mld$jitter_60m_15v)
colnames(mld)[ncol(mld)] <- 'jitter'

set.seed(144)

#split the data w/o seasonality in train, validation, test sets, 6/2/2 split
train_ids <- sample(nrow(mld), 0.70*nrow(mld))
train_set <- mld[train_ids,]
test_set <- mld[-train_ids,]

#there is something wrong with wndx, removing extreme values
train_set <- subset(train_set, wndw_yesterday_wndw_min_value >= .01)
train_set <- subset(train_set, wndw_yesterday_wndw_average_value >= .01)

alias(mod) #shows sigma_yesterday_sigma_event_max_value, sigma_yesterday_sigma_event_median value, temp_values_^8/9/10 
train_set <- train_set[,-grep('sigma_yesterday_sigma_event_max_value', colnames(train_set))]
train_set <- train_set[,-grep('sigma_yesterday_sigma_event_median_value', colnames(train_set))]

##Logistic Regression
mod <- glm(jitter ~ ., data=train_set, family="binomial")
#glm.fit: fitted probabilities numerically 0 or 1 occurred
#glm.fit: algorithm didn't converge. 
#information on complete separation: https://bscheng.com/2016/12/11/modeling-completely-separated-data-in-r/

#understand the model
summary(mod)
alias(mod)
vif_df <- as.data.frame(vif(mod)) ## remove high variance value?
train_set <- train_set[,-grep('wndw_yesterday_wndw_average_value', colnames(train_set))]
train_set <- train_set[,-grep('dqi_yesterday_dqi_average_value', colnames(train_set))]
train_set <- train_set[,-grep('thy_resv_yesterday_thy_resv_average_value', colnames(train_set))]
train_set <- train_set[,-grep('sigma_yesterday_sigma_event_average_value', colnames(train_set))]
train_set <- train_set[,-grep('sigma_yesterday_sigma_average_value', colnames(train_set))]

#re-run the mod now that I've removed features from train set 
mod <- glm(jitter ~ ., data=train_set, family="binomial")

#define table accuracy function
tableAccuracy <- function(test, pred) {
  t = table(test, pred)
  a = sum(diag(t))/length(test)
  return(a)
}

#Try making a prediction on the train set, see how we do
predTrain_mod <- predict(mod, data=train_set, type="response")
tableAccuracy(train_set$jitter, predTrain_mod > 0.5) #(.8890 accuracy)
table('actual'=train_set$jitter, 'predicted'=predTrain_mod > 0.5)
47/(47+242) #TPR = .16
13/(13+914) #FPR = .107

#Try making a prediction via step 
mod_stBoth <- step(mod, direction='both')
predTrain_mod_stBoth <- predict(mod_stBoth, data=train_set, type="response")
tableAccuracy(train_set$jitter, predTrain_mod_stBoth > 0.5) #(.876 accuracy)
table(train_set$jitter, predTrain_mod_stBoth > 0.5)
48/(241+48) #TPR = .16
14/(14+913) #FPR = .015

##Evaluating out-of-sample performance using k-fold validation
ctrl <- trainControl(method='repeatedcv', number=50, savePredictions = T)

#All variables
mod_k_fold <- train(jitter ~ ., data=train_set, method='glm', family='binomial',
                    trControl = ctrl, tuneLength=5) #Predicted accuracy is .69, kappa=.2

#For variables identified by step
mod_k_fold_step <- train(jitter ~ ., data=mod_stBoth$model, method='glm', family='binomial',
                         trControl = ctrl, tuneLength=5) #Predicted accuracy is .77, kappa=.217

###Create some ROC curves, see what we can find 
# ROC curves
par(mfcol=c(1,2))
##Baseline logistic regression model
mod_rocr <- prediction(predTrain_mod, train_set$jitter)
mod_performance <- performance(mod_rocr, "tpr", "fpr")
plot(mod_performance, colorize = TRUE)
abline(0, 1)
title("Logistic Regression ROC")
as.numeric(performance(mod_rocr, "auc")@y.values) #.66

mod_rocr <- prediction(predTrain_mod_stBoth, train_set$jitter)
mod_performance <- performance(mod_rocr, "tpr", "fpr")
plot(mod_performance, colorize = TRUE)
abline(0, 1)
title("Logistic Regression Step ROC")
as.numeric(performance(mod_rocr, "auc")@y.values) #.66

###Let's try against the validation set
###Alas - the test set predictions & Accuracy
predVal_mod <- predict(mod_stBoth, newdata=test_set, type="response")
tableAccuracy(test_set$jitter, predVal_mod > 0.5) #.832 accuracy
table(actual=test_set$jitter, prediction=predVal_mod > .5) 
(8)/(113+8) #.80 TPR
11/(400+11) #.319 FPR

###Let's try against the validation set
###Alas - the test set predictions & Accuracy
predVal_mod <- predict(mod, newdata=test_set, type="response")
tableAccuracy(test_set$jitter, predVal_mod > 0.5) #.832 accuracy
table(actual=test_set$jitter, prediction=predVal_mod > .2) 
(72)/(49+72) #.59 TPR
197/(197+214) #.47 FPR

