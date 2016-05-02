library(caret)

library(readr)

library(xgboost)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

feature.names <- names(train)[2:(ncol(train)-1)]


for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    
  }
}

train[is.na(train)] <- -1

test[is.na(test)]   <- -1

benchmark <- read.csv("D/kaggle/Springleaf/SUBMISSION/second.csv")

first <- read.csv("D/kaggle/Springleaf/SUBMISSION/third.csv")

second <- read.csv("D/kaggle/Springleaf/SUBMISSION/third_first.csv")

third <- read.csv("D/kaggle/Springleaf/SUBMISSION/fourth.csv")

feature_1 <- benchmark$target[1:145231] 

train$feature1 <- feature_1

test$feature1 <- benchmark$target

feature_2 <- first$target[1:145231] 

train$feature2 <- feature_2

test$feature2 <- first$target

feature_3 <- second$target[1:145231] 

train$feature3 <- feature_3

test$feature3 <- second$target

feature_4 <- third$target[1:145231] 

train$feature4 <- feature_4

test$feature4 <- third$target

response <- train$target

train$target <- NULL

feature.names <- names(train)[2:(ncol(train))]

dtrain <- xgb.DMatrix(data.matrix(train[,feature.names]), label= response)

param <- list(  objective   = "binary:logistic", 
                
                eta                 = 0.014,
                
                max_depth           = 10,
                
                subsample           = 0.7,
                
                colsample_bytree    = 0.7,
                
                
                
                eval_metric         = "auc"
)


#cv <- xgb.cv(params = param,data =  dtrain,nrounds = 700, nfold = 5, showsd = T, metrics = "auc"
              #, verbose = 2, maximize = TRUE)

clf_first <- xgb.train( params = param, 
                        
                        data                = dtrain, 
                        
                        nrounds             = 700, # changed from 300
                        
                        verbose             = 2, 
                        
                        
                        
                        nthread = 2,
                        
                        maximize = TRUE)

submission_second <- data.frame(ID=test$ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf_first, data.matrix(test[,feature.names]))

write_csv(submission_second, "fifth.csv")

xgb.save(clf_first, "xgb_fifth.R")



