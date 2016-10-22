
# seperate tmp into seperate df's for char, num, binary, dates -- manipulate -- combine

require(data.table); require(lubridate); require(caret); require(sqldf); require(xgboost); require(sqldf); require(xlsx); require(Matrix)

train_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\train.csv", data.table = F)

response <- train_raw$QuoteConversion_Flag

train_raw$QuoteConversion_Flag <- NULL

train_raw$QuoteNumber <- NULL



test_raw <- fread(input = "D:\\kaggle\\HOMESITE\\Data\\test.csv", data.table = F)

id <- test_raw$QuoteNumber

test_raw$QuoteNumber <- NULL


tmp <- rbind(train_raw, test_raw)

tmp$Original_Quote_Date <- as.Date(tmp$Original_Quote_Date)

tmp$month <- as.integer(format(tmp$Original_Quote_Date, "%m"))

tmp$year <- as.integer(format(tmp$Original_Quote_Date, "%y"))

tmp$day <- weekdays(as.Date(tmp$Original_Quote_Date))


####################################################################################################

# remove non zero variance predictors

nzv <- nearZeroVar(tmp)

tmp_raw <- tmp[, -nzv]

str(tmp_raw, list.len = 250)


for (f in names(tmp_raw)) {
  
  if (class(tmp_raw[[f]])=="character") {
    
    levels <- unique(tmp_raw[[f]])
    
    tmp_raw[[f]] <- as.integer(factor(tmp_raw[[f]], levels=levels))
    
  }
  
}




tmp_new <- cbind(tmp_raw)#, tmp_count)

train <- tmp_new[c(1:260753), ]

test <- tmp_new[c(260754:434589), ]

gc()

train[is.na(train)] <- 0

test[is.na(test)] <- 0

gc()

#train <- train[sample(1:nrow(train), nrow(train)), ]

#test <- test[sample(1:nrow(test), nrow(test)), ]

feature.names <- names(train)

h <- sample(nrow(train),2000)

dval <- xgb.DMatrix(data=data.matrix(train[h,]),label=response[h])

#dtrain<-xgb.DMatrix(data=data.matrix(tra[-h,]),label=train$QuoteConversion_Flag[-h])

dtrain <- xgb.DMatrix(data=data.matrix(train),label=response)

watchlist <- list(val=dval,train=dtrain)

param <- list(objective           = "binary:logistic",
              
              booster = "gbtree",
              
              eval_metric = "auc",
              
              eta = 0.02, # 0.06, #0.01,
              
              max_depth = 7, #changed from default of 8
              
              subsample = 0.86, # 0.7
              
              colsample_bytree = 0.68, # 0.7
              
              num_parallel_tree = 2
              
              # alpha = 0.0001,
              
              # lambda = 1
              
)

cl <- makeCluster(2); registerDoParallel(cl)

set.seed(11*23*15)

start <- Sys.time()

clf <- xgb.train( params              = param,
                  
                  data                = dtrain, 
                  
                  label               = response,
                  
                  nrounds             = 1900,
                  
                  verbose             = 1,  #1
                  
                  #early.stop.round    = 150,
                  
                  watchlist           = watchlist,
                  
                  maximize            = FALSE,
                  
                  nthread = 2)

pred <- predict(clf, data.matrix(test[,feature.names]))

submission <- data.frame(QuoteNumber = id, QuoteConversion_Flag = pred)

write_csv(submission, "D:\\kaggle\\HOMESITE\\submission\\11232015.csv")

time_taken <- Sys.time() - start
