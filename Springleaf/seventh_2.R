library(caret); library(lubridate) ; library(readr) ; library(xgboost)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

##REMOVE RESPONSE AND ID VARIABLES

response <- train$target

train_ID <- train$ID

test_ID <- test$ID

##MODIFY train and test set

training <- subset(train, select = -c(ID, target))

testing <- subset(test, select = -c(ID))

dim(training); dim(testing)

########################################################################################
tmp <- rbind(training, testing)

tmp_num <- tmp[, sapply(tmp, is.numeric)] 

dim(tmp_num)

tmp_char <- tmp[,sapply(tmp, is.character)]

dim(tmp_char)

numeric_ele <- (lapply(tmp_num, function(x) length(unique(x))))

char_ele <- (lapply(tmp_char, function(x) length(unique(x))))

numeric_one <- subset(numeric_ele , subset  = c(numeric_ele == 1))
names(numeric_one)

ids <- c("VAR_0227", "VAR_0228")

remove_col <- c(names(numeric_one), ids)

tmp <- tmp[, !(names(tmp) %in% (remove_col))]

dim(tmp)

######################################################################################

tmp_num <- tmp[, sapply(tmp, is.numeric)] 

tmp_char <- tmp[,sapply(tmp, is.character)]

#####################################################################################

tmp_date <- tmp_char[, grep("JAN1|FEB1|MAR1", tmp_char)]

dim(tmp_date)


tmp_dates <- data.frame(sapply(tmp_date, function(x) strptime(x, "%d%B%y :%H:%M:%S")))

tmp_year <- data.frame(sapply(tmp_dates, function(x) year(x)))

names(tmp_year) <- paste("YEAR_", names(tmp_year), sep = "") 

tmp_wday <- data.frame(sapply(tmp_dates, function(x) wday(x)))

names(tmp_wday) <- paste("WDAY_", names(tmp_wday), sep = "") 

tmp_month <- data.frame(sapply(tmp_dates, function(x) month(x)))

names(tmp_month) <- paste("MONTH_", names(tmp_month), sep = "") 

tmp_yday <- data.frame(sapply(tmp_dates, function(x) yday(x)))

names(tmp_yday) <- paste("YDAY_", names(tmp_yday), sep = "") 

tmp_mday <- data.frame(sapply(tmp_dates, function(x) mday(x)))

names(tmp_mday) <- paste("MDAY_", names(tmp_mday), sep = "") 

tmp_weekdays <- data.frame(sapply(tmp_dates, function(x) weekdays(x)))

names(tmp_weekdays) <- paste("YEAR_", names(tmp_weekdays), sep = "") 

tmp_hour <- data.frame(sapply(tmp_dates, function(x) hour(x)))

names(tmp_hour) <- paste("HOUR_", names(tmp_hour), sep = "") 

tmp_minute <- data.frame(sapply(tmp_dates, function(x) minute(x)))

names(tmp_minute) <- paste("MINUTE_", names(tmp_minute), sep = "") 

tmp_second <- data.frame(sapply(tmp_dates, function(x) second(x)))

names(tmp_second) <- paste("SECOND_", names(tmp_second), sep = "") 

#################################################################################
tmp <- tmp[, !(names(tmp) %in% names(tmp_date))]

dim(tmp)

dates <- cbind( tmp_year, tmp_yday, tmp_weekdays, tmp_wday, tmp_second, 
               tmp_hour, tmp_minute)


##further modification when will people most likely take a loan
######################################################################################

training <- tmp[1:145231,]

testing <- tmp[(nrow(training)+1): nrow(tmp), ]

dim(training); dim(testing)

feature.names <- names(training)

for (f in feature.names) {
  
  if (class(training[[f]])=="character") {
    
    levels <- unique(c(training[[f]], testing[[f]]))
    
    training[[f]] <- as.integer(factor(training[[f]], levels=levels))
    
    testing[[f]]  <- as.integer(factor(testing[[f]],  levels=levels))
    
  }
}
  
training[is.na(training)] <- -1

testing[is.na(testing)]   <- -1

dates[is.na(dates)] <- -1

###############################################################################
tmp <- rbind(training, testing)

tmp <- cbind(tmp, dates)

training <- tmp[1:145231,]

testing <- tmp[(nrow(training)+1): nrow(tmp), ]

dim(training); dim(testing)
gc(); rm(train);rm(test)
#############################################################################

benchmark <- read_csv("D:/kaggle/Springleaf/SUBMISSION/second.csv")

first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third_first.csv")

third <- read_csv("D:/kaggle/Springleaf/SUBMISSION/fourth.csv")

fourth <- read_csv("D:/kaggle/Springleaf/sixth.csv")

fifth <- read_csv("D:/kaggle/Springleaf/eight.csv")

feature_1 <- benchmark$target[1:145231] 

training$feature1 <- feature_1

testing$feature1 <- benchmark$target

feature_2 <- first$target[1:145231] 

training$feature2 <- feature_2

testing$feature2 <- first$target

feature_3 <- second$target[1:145231] 

training$feature3 <- feature_3

testing$feature3 <- second$target

feature_4 <- third$target[1:145231] 

training$feature4 <- feature_4

testing$feature4 <- third$target

feature_5 <- fourth$target[1:145231] 

training$feature5 <- feature_5

testing$feature5 <- fourth$target


dtraining <- xgb.DMatrix(data.matrix(training[,feature.names]), label= response)

param <- list(  objective   = "binary:logistic", 
                
                eta                 = 0.014,
                
                max_depth           = 10,
                
                subsample           = 0.7,
                
                colsample_bytree    = 0.7,
                
                eval_metric         = "auc"
)




#cv <- xgb.cv(params = param,data =  dtraining,nrounds = 700, nfold = 5, showsd = T, metrics = "auc"
#, verbose = 2, maximize = TRUE)

clf_first <- xgb.train( params = param, 
                        
                        data                = dtraining, 
                        
                        nrounds             = 2000, # changed from 300
                        
                        verbose             = 2,
                        
                        maximize = TRUE)

submission_second <- data.frame(ID=test_ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf_first, data.matrix(testing[,feature.names]))

write_csv(submission_second, "ten.csv")


