
library(readr); require(doParallel); library(xgboost); require(data.table)

#my favorite seed^^

cat("reading the train and test data\n")
train <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

test  <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\test.csv", data.table = F)

# There are some NAs in the integer columns so conversion to zero
train[is.na(train)]   <- -1
test[is.na(test)]   <- -1



str(train, list.len = 350)

a <- lapply(train, function(x) unique(x))

a[3:35]


cat("train data column names and details\n")
names(train)
str(train)
summary(train)
cat("test data column names and details\n")
names(test)
str(test)
summary(test)


# seperating out the elements of the date column for the train set


train$Original_Quote_Date <- as.Date(train$Original_Quote_Date)
train$month <- as.integer(format(train$Original_Quote_Date, "%m"))
train$year <- as.integer(format(train$Original_Quote_Date, "%y"))
train$day <- weekdays(as.Date(train$Original_Quote_Date))

# removing the date column
train <- train[,-c(2)]

# seperating out the elements of the date column for the train set
test$Original_Quote_Date <- as.Date(test$Original_Quote_Date)
test$month <- as.integer(format(test$Original_Quote_Date, "%m"))
test$year <- as.integer(format(test$Original_Quote_Date, "%y"))
test$day <- weekdays(as.Date(test$Original_Quote_Date))

# removing the date column
test <- test[,-c(2)]


feature.names <- names(train)[c(3:301)]
cat("Feature Names\n")
feature.names

cat("assuming text variables are categorical & replacing them with numeric ids\n")
for (f in feature.names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

cat("train data column names after slight feature engineering\n")
names(train)
cat("test data column names after slight feature engineering\n")
names(test)
tra<-train[,feature.names]

nrow(train)
h<-sample(nrow(train),2000)

dval<-xgb.DMatrix(data=data.matrix(tra[h,]),label=train$QuoteConversion_Flag[h])
#dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=train$QuoteConversion_Flag[-h])
dtrain<-xgb.DMatrix(data=data.matrix(tra),label=train$QuoteConversion_Flag)

watchlist<-list(val=dval,train=dtrain)

param <- list(  objective           = "binary:logistic", 
                booster = "gbtree",
                eval_metric = "auc",
                eta                 = 0.02, # 0.06, #0.01,
                max_depth           = 7, #changed from default of 8
                subsample           = 0.82, # 0.7
                colsample_bytree    = 0.66, # 0.7
                num_parallel_tree   = 2
                # alpha = 0.0001, 
                # lambda = 1
)

cl <- makeCluster(2); registerDoParallel(cl) 

best <- read_csv("D:\\kaggle\\HOMESITE\\11182015.csv")

submission_bag <- best$QuoteConversion_Flag

submission_bag <- data.frame(pred = submission_bag)

  for(i in c(1:5)){
  
  
print(paste("currently training", i, "th",  "model"))
      
set.seed(11*21*i*15)


clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 10, 
                    verbose             = 1,  #1
                    #early.stop.round    = 150,
                    watchlist           = watchlist,
                    maximize            = FALSE,
                    
                    nthread = 2
                    )

pred <- predict(clf, data.matrix(test[,feature.names]))

pred <- data.frame(pred)

names(pred) <- paste0("pred", "_", i)

submission_bag <- cbind(submission_bag, pred)

}


pred1 <- apply(submission_bag[, c(2:6)], 1, mean)


submission <- data.frame(QuoteNumber=test$QuoteNumber, QuoteConversion_Flag=pred1)

cat("saving the submission file\n")

write_csv(submission_bag, "D:\\kaggle\\HOMESITE\\submission\\sub_bag.csv")

names=c(names(tmp_new), names(tmp_count))

importance_matrix <- xgb.importance(feature_names = names, model = clf)



xgb.plot.importance(importance_matrix = importance_matrix[1:10, ])

# run intial stuff for 3 rounds with all params = 1 before going all out


dtrain <- xgb.DMatrix(data.matrix(train), label = response)


cl <- makeCluster(2); registerDoParallel(cl) 

start <- Sys.time()

i = 0; j = 0; k = 0

for( gamma in c(3, 6, 9)){
  
  i = i + 1
  
  for(min_child_weight in c(3, 6, 9)){
    
    j = j + 1
    
    for(max_delta_step in c(3, 6)){
      
      k = k + 1
      
      set.seed((gamma*min_child_weight*max_delta_step)*100)
      
      param <- list( objective = "binary:logistic", 
                     
                     booster = "gbtree",
                     
                     eval_metric = "auc",
                     
                     eta = 0.02, # 0.06, #0.01,
                     
                     max_depth = 7, #changed from default of 8
                     
                     subsample = 0.82, # 0.7
                     
                     colsample_bytree = 0.66,
                     
                     gamma = gamma,
                     
                     min_child_weight = min_child_weight,
                     
                     max_delta_step = max_delta_step,
                     
                     # 0.7
                     
                     num_parallel_tree   = 2
                     
                     # alpha = 0.0001, 
                     
                     # lambda = 1
                     
      )
      
      
      
      clf_i_j_k <- xgb.cv(   params              = param, 
                             
                             data                = dtrain, 
                             
                             nrounds             = 1800, 
                             
                             verbose             = 1,  #1
                             #early.stop.round    = 150,
                             #watchlist           = watchlist,
                             
                             maximize            = FALSE,
                             
                             nthread = 2, 
                             
                             nfold = 5
      )
      
      write_csv(clf_i_j_k, paste0( "D:\\kaggle\\HOMESITE\\models\\", "clf_", i, "_", j,"_",k,".csv"))
      
    }
    
  }
  
}




time_taken <- Sys.time() - start