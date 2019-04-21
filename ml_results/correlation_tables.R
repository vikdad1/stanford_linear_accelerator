#Correlation table
test <- mld[,c('pjtn_yesterday_pjtn_average_value', 'sigma_yesterday_sigma_average_value',
       'thy_resv_yesterday_thy_resv_average_value', 'swrd_yesterday_swrd_trip_count',
       'dqi_yesterday_dqi_average_value',
       'hvv_yesterday_hvv_average_value', 'wndw_yesterday_wndw_average_value', 'jitter')]

test$jitter <- as.numeric(as.character(test$jitter))

correlation_table <- cor(test, method="spearman")
correlation_table <- round(correlation_table, 2)

install.packages("Hmisc")
library(Hmisc)
res2 <- rcorr(as.matrix(test), type='spearman')


install.packages("corrplot")
library(corrplot)


colnames(correlation_table) <- c('pjtn', 'sigma', 'thy resv', 'swrd', 
                                'dqi', 'hvv', 'wndw', 'jitter')
rownames(correlation_table) <- c('pjtn', 'sigma', 'thy resv', 'swrd', 
                                'dqi', 'hvv', 'wndw', 'jitter')

corrplot(correlation_table, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45, outline=T)

#.7894 p value for the correlation between sigma and jitter... no huge corrrelations between average

#Do correlations for day of week and hour per day
setwd('li22_31/')
dow <- read.csv("dow_record_distribution.csv")
dow$day 

week <- c(Mon = 401, Tue = 199, Wed = 187, Thur = 202, Fri = 240, Sat = 212, Sun = 244)
names(week) <- NULL
observed <- week
expected <- rep(1/length(week), length(week))
chisq.test(week, p = expected)

chisq.test(dow$record_count, p=expected)

#hour per day
hour <- read.csv("hr_record_distribution.csv")
expected <- rep(1/nrow(hour), nrow(hour))
chisq.test(hour$jitter_records, p=expected)

hour$jitter_records
