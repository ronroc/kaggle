rm(list = ls())

library(doParallel)

cl <- makeCluster(2)

registerDoParallel(cl)

library(xgboost)

library(readr)

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
  
)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

y <- train$target

train$target <- NULL

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

test_ID <- test$ID

train.unique.count=lapply(train, function(x) length(unique(x)))

train.unique.count_1=unlist(train.unique.count[unlist(train.unique.count)==1])

train.unique.count_2=unlist(train.unique.count[unlist(train.unique.count)==2])

train.unique.count_2=train.unique.count_2[-which(names(train.unique.count_2)=='target')]

delete_const=names(train.unique.count_1)

delete_NA56=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145175))

delete_NA89=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==145142))

delete_NA918=names(which(unlist(lapply(train[,(names(train) %in% names(train.unique.count_2))], function(x) max(table(x,useNA='always'))))==144313))

#VARS to delete

#safe to remove VARS with 56, 89 and 918 NA's as they are covered by other VARS

print(length(c(delete_const,delete_NA56,delete_NA89,delete_NA918)))

train=train[,!(names(train) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]

test=test[,!(names(test) %in% c(delete_const,delete_NA56,delete_NA89,delete_NA918))]

print(dim(train))

print(dim(test))

train <- train[,-1]

test <- test[,-1]

levels_TR <- unique(train$VAR_0241)

levels_tt <- unique(test$VAR_0241)

train$VAR_0241 <- as.integer(factor(train$VAR_0241, levels=levels_TR))

test$VAR_0241  <- as.integer(factor(test$VAR_0241,  levels=levels_tt))

feature.names <- names(train)

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
    
  }
}

        
train[is.na(train)] <- -1

test[is.na(test)] <- -1

benchmark <- read_csv("D:/kaggle/Springleaf/SUBMISSION/second.csv")

first <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third.csv")

second <- read_csv("D:/kaggle/Springleaf/SUBMISSION/third_first.csv")

third <- read_csv("D:/kaggle/Springleaf/SUBMISSION/fourth.csv")

fourth <- read_csv("D:/kaggle/Springleaf/sixth.csv")

fifth <- read_csv("D:/kaggle/Springleaf/eight.csv")

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

feature_5 <- fourth$target[1:145231] 

train$feature5 <- feature_5

test$feature5 <- fourth$target



  xgtrain = xgb.DMatrix(as.matrix(train), label = y, missing = NA)

    gc()
  
      model = xgb.train(
    
        nrounds = 2000   # increase for more results at home
    
        , params = param
    
        , data = xgtrain
        ,verbose = 2
    
        
  )
  
      
    rm("train")
    
    gc()
    
    train_val <-predict(model, newdata=xgtrain)
    
    rm("xgtrain")
    
    xgtest <- xgb.DMatrix(as.matrix(test), missing = NA)
    
    preds_out <- predict(model, xgtest)
    
    
    submission_second <- data.frame(ID=test_ID)
    
    submission_second$target <- preds_out 
    
    write_csv(submission_second, "eleven.csv")
    