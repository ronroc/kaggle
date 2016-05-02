rm(list = ls())

library(readr)

library(xgboost)

library(doParallel)

cl <- makeCluster(2)

registerDoParallel(cl)

set.seed(8675309) 

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

train_target <- train$target

train$target <- NULL

train_ID <- train$ID

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

test_ID <- test$ID

train.unique.count <- lapply(train, function(x) length(unique(x)))

train.unique.count_1 <- unlist(train.unique.count[unlist(train.unique.count) == 1])

train.unique.count_2=unlist(train.unique.count[unlist(train.unique.count)==2])

train.unique.count_2=train.unique.count_2[-which(names(train.unique.count_2)=='target')]

delete_const <- names(train.unique.count_1)

#safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS

delete_NA56=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145175))

delete_NA89=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145142))

delete_NA918=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==144313))

print(length(c(delete_const,delete_NA56,delete_NA89,delete_NA918)))

# Update train and test data

cat("Update train and test data\n")

train <- train[,!(names(train) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]

test  <- test[,!(names(test) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]

print(dim(train))

print(dim(test))

rm(train.unique.count, train.unique.count_1, delete_const)

gc()


datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", 
                "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", 
                "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

train_cropped <- train[datecolumns]

train_cc <- data.frame(apply(train_cropped, 
                             2, 
                             function(x) as.double(strptime(x, 
                                                            format='%d%b%y:%H:%M:%S', tz="UTC"))))


for (dc in datecolumns){
  
  train[dc] <- NULL

    train[dc] <- train_cc[dc]
}

train_cc <- NULL

train_cropped <- NULL

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

# Convert factor columns to numeric values

cat("\n assuming text variables are categorical & replacing them with numeric ids\n")

feature.names <- setdiff(names(train)[-1], 'target')

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



clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 2500, #300, #280, #125, #250, # changed from 300
                    verbose             = 2, 
                    
                    maximize            = TRUE)


dtrain=0
gc()

submission_second <- data.frame(ID=test_ID)

submission_second$target <- NA 

submission_second[,"target"] <- predict(clf, data.matrix(test[,feature.names]))

write_csv(submission_second, "09152015_2.csv")

