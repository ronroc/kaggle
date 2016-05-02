rm(list = ls())

require(doParallel); cl <- makeCluster(2)

registerDoParallel(cl)

require(readr)

train <- read_csv("D:/kaggle/Springleaf/DATA/CSV/train.csv")

test <- read_csv("D:/kaggle/Springleaf/DATA/CSV/test.csv")

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

# From manual data analysis
datecolumns = c("VAR_0073", "VAR_0075", "VAR_0156", "VAR_0157", "VAR_0158", "VAR_0159", "VAR_0166", "VAR_0167", "VAR_0168", "VAR_0176", "VAR_0177", "VAR_0178", "VAR_0179", "VAR_0204", "VAR_0217")

train_cropped <- train[datecolumns]

train_cc <- data.frame(apply(train_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  
  train[dc] <- NULL
  
  train[dc] <- train_cc[dc]
}

train_cc <- NULL

train_cropped <- NULL

gc()

test_cropped <- test[datecolumns]

test_cc <- data.frame(apply(test_cropped, 2, function(x) as.double(strptime(x, format='%d%b%y:%H:%M:%S', tz="UTC")))) #2 = columnwise

for (dc in datecolumns){
  
  test[dc] <- NULL
  
  test[dc] <- test_cc[dc]
}

test_cc <- NULL

test_cropped <- NULL

gc()


# safe target and put it at the end again

train_target <- train$target

train$target <- NULL

feature.names <- names(train)[2:ncol(train)]
# names(train)  # 1934 variables

for (f in feature.names) {
  
  if (class(train[[f]])=="character") {
    
    levels <- unique(c(train[[f]], test[[f]]))
    
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("replacing missing values with -1\n")
train[is.na(train)] <- -1

test[is.na(test)]   <- -1


gc()

require(caret)

split <- createDataPartition(y = train_target, p = 0.5, list = F )

train_a <- train[split, ]

train_b <- train[-split, ]

feature.names <- names(train)[2:ncol(train)]

dtrain_a <- xgb.DMatrix(data.matrix(train_a[,feature.names]), label=train_target[split])

dtrain_b <- xgb.DMatrix(data.matrix(train_b[,feature.names]), label=train_target[-split])

gc()

param <- list(  objective           = "binary:logistic", 
                # booster = "gblinear",
                eta                 = 0.01, #0.06, #0.01,
                max_depth           = 8,  # changed from default of 8
                subsample           = 0.7,
                colsample_bytree    = 0.7,
                eval_metric         = "auc"
                # alpha = 0.0001, 
                # lambda = 1
)


clf_a <- xgb.train(   params              = param, 
                      data                = dtrain_a, 
                      nrounds             = 1000, #300, #280, #125, #250, # changed from 300
                      verbose             = 2, 
                      
                      maximize            = TRUE)




clf_b <- xgb.train(   params              = param, 
                      data                = dtrain_b, 
                      nrounds             = 1000, #300, #280, #125, #250, # changed from 300
                      verbose             = 2, 
                      
                      maximize            = TRUE)

xgb.save(model = clf_a, fname = "train_a"); xgb.save(model = clf_b, fname = "train_b")

pred_a <- predict(clf_a, data.matrix(train_b[,feature.names]))

pred_b <- predict(clf_b, data.matrix(train_a[,feature.names]))

pred <- predict(clf_a, data.matrix(train[ ,feature.names]))

stack_gbm <- data.frame(x = pred_a, y = pred_b[1:72615], z = (train_target[-split]))

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities

                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)
stacker <- train(z ~ ., data = stack_gbm, method = 'gbm', verbose = T, metric = "roc") 

prediction <- predict(stacker, newdata=test)

submission_second <- data.frame(ID=test$ID)
length(test$ID)
