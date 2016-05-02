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

numeric_one <- subset(numeric_ele , subset  = c(numeric_ele == 1))

ids <- c("VAR_0227", "VAR_0228")

remove_col <- c(names(numeric_one), ids)

tmp <- tmp[, !(names(tmp) %in% (remove_col))]

######################################################################################

tmp_num <- tmp[, sapply(tmp, is.numeric)] 

tmp_char <- tmp[,sapply(tmp, is.character)]

#####################################################################################

tmp_date <- tmp_char[, grep("JAN1|FEB1|MAR1", tmp_char)]

tmp_dates <- data.frame(sapply(tmp_date, function(x) strptime(x, "%d%B%y :%H:%M:%S")))

tmp_year <- data.frame(sapply(tmp_dates, function(x) year(x)))

names(tmp_year) <- paste("YEAR_", names(tmp_year), sep = "") 

tmp_wday <- data.frame(sapply(tmp_dates, function(x) wday(x)))

names(tmp_wday) <- paste("WDAY_", names(tmp_wday), sep = "") 

tmp_month <- data.frame(sapply(tmp_dates, function(x) month(x)))

names(tmp_month) <- paste("MONTH_", names(tmp_month), sep = "") 


tmp_mday <- data.frame(sapply(tmp_dates, function(x) mday(x)))

names(tmp_mday) <- paste("MDAY_", names(tmp_mday), sep = "") 


tmp_hour <- data.frame(sapply(tmp_dates, function(x) hour(x)))

names(tmp_hour) <- paste("HOUR_", names(tmp_hour), sep = "") 

tmp_minute <- data.frame(sapply(tmp_dates, function(x) minute(x)))

names(tmp_minute) <- paste("MINUTE_", names(tmp_minute), sep = "") 

#################################################################################
tmp <- tmp[, !(names(tmp) %in% names(tmp_date))]

dim(tmp)

dates <- cbind( tmp_year, tmp_wday , tmp_month, tmp_mday, tmp_hour,  )


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

dtraining <- xgb.DMatrix(data.matrix(training[,feature.names]), label= response)

param <- list(
  
  "objective"  = "binary:logistic"
  , "eval_metric" = "auc"
  , "eta" = 0.01
  , "subsample" = 0.7
  , "colsample_bytree" = 0.5
  , "min_child_weight" =6
  , "max_depth" = 9
  , "alpha" = 4
  , "nthreads" = 2
  ,  eval_metric         = "auc"
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

write_csv(submission_second, "eleven.csv")


