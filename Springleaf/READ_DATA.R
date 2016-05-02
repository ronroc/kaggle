library(caret)

library(readr)

library(xgboost)

#escape file path to read data into file (memory)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")


feature.names <- names(train)[2:(ncol(train)-1)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
  
      levels <- unique(c(train[[f]], test[[f]]))
    
      train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
      test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  
      }
}

train[is.na(train)] <- -1

test[is.na(test)]   <- -1

split <- createDataPartition(train$target, p = .75, list = FALSE)

training <- train[ split,]

testing  <- train[-split,]

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label= training$target)

dtest <- xgb.DMatrix(data.matrix(testing[,feature.names]), label= testing$target)

watchlist <- list(train = dtrain, test = dtest)

param <- list(  objective   = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.001,
                max_depth           = 13,  # changed from default of 6
                subsample           = 0.6,
                colsample_bytree    = 0.75,
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
)


clf <- xgb.train(   params              = param, 
                   data                = dtrain, 
                   nrounds             = 50, # changed from 300
                   verbose             = 2, 
                   
                   watchlist = watchlist,
                   
                   nthread = 2,
                   
                   maximize = TRUE)

submission <- data.frame(ID=test$ID)

submission$target <- NA 

submission[,"target"] <- predict(clf, data.matrix(test[,feature.names]))

write_csv(submission, "benchmark.csv")

xgb.dump(model = clf, "benchmark.txt", with.stats = T)

xgb.save(clf, "xgb_benchmark.R")

###############################################################################################

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

feature.names <- names(train)[2:(ncol(train)-1)]

cat("assuming text variables are categorical & replacing them with numeric ids\n")

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    
  }
}

train[is.na(train)] <- -1

test[is.na(test)]   <- -1

dim(train)

feature_1 <- submission$target[1:145231] 

train$feature1 <- feature_1

test$feature1 <- submission$target

################################################################################################

#train again using new features 

split <- createDataPartition(train$target, p = .75, list = FALSE)

response <- train$target

train$target <- NULL

feature.names <- names(train)[2:(ncol(train))]

training <- train[ split,]

testing  <- train[-split,]

response_testing <-  response[-split]

response_training <- response[split]

dtrain <- xgb.DMatrix(data.matrix(training[,feature.names]), label= response_training)

dval <- xgb.DMatrix(data.matrix(testing[,feature.names]), label= response_testing)

watchlist <- list(train = dtrain, test = dval)

param <- list(  objective   = "binary:logistic", 
                # booster = "gblinear",
                eta                 = ( 0.013),
                max_depth           = ( 16),  # changed from default of 8
                subsample           = (0.7),
                colsample_bytree    = 0.7
)


clf_first <- xgb.train( params = param, 
                          
                          data                = dtrain, 
                          
                          nrounds             = 50, # changed from 300
                          
                          verbose             = 2, 
                          
                          watchlist = watchlist,
                          
                          nthread = 2,
                          
                          maximize = TRUE)
submission_second <- data.frame(ID=test$ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf_first, data.matrix(test[,feature.names]))

write_csv(submission, "second.csv")

xgb.dump(model = clf_first, "second.txt", with.stats = T)

xgb.save(clf_first, "xgb_second.R")
