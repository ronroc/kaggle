rm(list = ls())

library(readr); library(xgboost); library(doParallel); library(caret) 

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(8675309) 

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

# save ID and response

train_target <- train$target

train$target <- NULL

train_ID <- train$ID

train$ID <- NULL

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

# save ID and response

test_ID <- test$ID

test$ID <- NULL

print(dim(train)); print(dim(test))

#To find date columns -- really didn`t understand how

dates = names(train[,grep("JAN1|FEB1|MAR1", train), ])

datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", 
                "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", 
                "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

tmp <- rbind(train, test)

tmp_cropped <- tmp[datecolumns]

tmp_cc <- data.frame(apply(tmp_cropped, 
                             2, 
                             function(x) as.double(strptime(x, 
                                                            format='%d%b%y:%H:%M:%S', tz="UTC"))))
for (dc in datecolumns){
  
  tmp[dc] <- NULL
  
  tmp[dc] <- tmp_cc[dc]
}

tmp_cc <- NULL

tmp_cropped <- NULL

gc()

test_cropped <- test[datecolumns]

test_cc <- data.frame(apply(test_cropped, 
                            2, 
                            function(x) as.double(strptime(x, 
                                                           format='%d%b%y:%H:%M:%S', tz="UTC"))))

for (dc in datecolumns){
  
  test[dc] <- NULL
  
  test[dc] <- test_cc[dc]
}

test_cc <- NULL

test_cropped <- NULL

gc()

feature.names <- names(train)[1:ncol(train)]

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
  
}



require(caret)

train_test_1 <-  preProcess(train[datecolumns], method = ("BoxCox"))

train_test <- predict(train_test_1, train[datecolumns])

train[datecolumns] <- train_test


train_test_2 <-  preProcess(test[datecolumns], method = ("BoxCox"))

train_test1 <- predict(train_test_2, test[datecolumns])

test[datecolumns] <- train_test1

nzv <- nearZeroVar(train)

nzv_test <- nearZeroVar(test)

train <- train[, -nzv]

test <- test[, -nzv_test]

train[is.na(train)] <- -1

test[is.na(test)]   <- -1


feature.names <- names(train)[1:ncol(train)]

dtrain <- xgb.DMatrix(data.matrix(train[,feature.names]), label=train_target)

gc()

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.014, #0.06, #0.01,
                max_depth           = 10,  # changed from default of 8
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                eval_metric         = "auc",
                nthreads = -1
                # alpha = 0.0001, 
                # lambda = 1
)

library(doParallel)

cl <- makeCluster(2)

registerDoParallel(cl)

#clf_cv <- xgb.cv(params = param, data = dtrain, nrounds = 1500, nfold = 4, 
                 
#                 showsd = T, verbose = T, maximize = T)



clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2500, #300, #280, #125, #250, # changed from 300
                    verbose             = 2, 
                    
                    maximize            = TRUE)


dtrain=0
gc()

submission_second <- data.frame(ID=test_ID)

submission_second$target <- NA 

submission_second[,"target"] <- pred_2$h2o.deeplearning.wrapper

write_csv(submission_second, "D:/kaggle/Springleaf/SUBMISSION/09242015_5.csv")

######################################################################################
first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09192015_3.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/09242015_1.csv")

submission_second <- data.frame(ID=test_ID)

submission_second$target <- NA 

third <- cbind(first$target, second$target)

submission_second$target <- apply(X = third, MARGIN = 1, FUN = mean) 

write_csv(submission_second, "D:/kaggle/Springleaf/SUBMISSION/09242015_mean.csv")
